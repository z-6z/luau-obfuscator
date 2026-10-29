const { lex } = require("./lexer");
const { OP, RKBIT } = require("./opcodes");

const BINOPCODE = {
  "+": OP.ADD,
  "-": OP.SUB,
  "*": OP.MUL,
  "/": OP.DIV,
  "%": OP.MOD,
  "^": OP.POW,
};

const CMPOP = {
  "<": [OP.LT, false],
  ">": [OP.LT, true],
  "<=": [OP.LE, false],
  ">=": [OP.LE, true],
  "==": [OP.EQ, false],
  "~=": [OP.NEQ, false],
};

class FnState {
  constructor(parent, name) {
    this.parent = parent;
    this.regTop = 0;
    this.scopes = [];
    this.upvals = [];
    this.loops = [];
    this.proto = {
      name,
      numParams: 0,
      hasVararg: false,
      code: [],
      k: [],
      kmap: new Map(),
      protos: [],
      upvals: [],
      maxStack: 2,
    };
  }
}

class Compiler {
  compile(ast) {
    const fs = new FnState(null, "main");
    this.compileBlock(fs, ast);
    this.emit(fs, OP.RETURN, 0, 1, 0);
    return fs.proto;
  }

  emit(fs, op, a = 0, b = 0, c = 0) {
    fs.proto.code.push([op, a, b, c]);
    return fs.proto.code.length - 1;
  }

  pc(fs) {
    return fs.proto.code.length;
  }

  patchJmp(fs, idx, target) {
    fs.proto.code[idx][2] = target - (idx + 1);
  }

  bump(fs, n) {
    if (n > fs.proto.maxStack) fs.proto.maxStack = n;
  }

  k(fs, v) {
    const key = typeof v + ":" + String(v);
    if (fs.proto.kmap.has(key)) return fs.proto.kmap.get(key);
    fs.proto.k.push(v);
    const idx = fs.proto.k.length - 1;
    fs.proto.kmap.set(key, idx);
    return idx;
  }

  openScope(fs) {
    fs.scopes.push({ vars: new Map(), base: fs.regTop });
  }

  closeScope(fs) {
    const sc = fs.scopes.pop();
    fs.regTop = sc.base;
  }

  declareLocal(fs, name, reg) {
    fs.scopes[fs.scopes.length - 1].vars.set(name, reg);
    this.bump(fs, reg + 1);
  }

  resolveLocal(fs, name) {
    for (let i = fs.scopes.length - 1; i >= 0; i--) {
      if (fs.scopes[i].vars.has(name)) return fs.scopes[i].vars.get(name);
    }
    return null;
  }

  resolveUpval(fs, name) {
    const existing = fs.upvals.findIndex((u) => u.name === name);
    if (existing >= 0) return existing;
    if (!fs.parent) return null;
    const r = this.resolveLocal(fs.parent, name);
    if (r !== null) {
      fs.upvals.push({ name, instack: true, idx: r });
      return fs.upvals.length - 1;
    }
    const u = this.resolveUpval(fs.parent, name);
    if (u !== null) {
      fs.upvals.push({ name, instack: false, idx: u });
      return fs.upvals.length - 1;
    }
    return null;
  }

  compileBlock(fs, block) {
    this.openScope(fs);
    for (const st of block.stats) this.compileStat(fs, st);
    this.closeScope(fs);
  }

  compileStat(fs, st) {
    switch (st.kind) {
      case "Local":
        this.compileLocal(fs, st);
        break;
      case "LocalFunction": {
        const reg = fs.regTop++;
        this.declareLocal(fs, st.name, reg);
        const proto = this.compileFunction(fs, st.func, st.name);
        this.emit(fs, OP.NEWCLOSURE, reg, fs.proto.protos.length, 0);
        fs.proto.protos.push(proto);
        break;
      }
      case "Assign":
        this.compileAssign(fs, st);
        break;
      case "Call":
      case "MethodCall":
        this.expToReg(fs, st, fs.regTop, 1);
        break;
      case "If":
        this.compileIf(fs, st);
        break;
      case "While":
        this.compileWhile(fs, st);
        break;
      case "Repeat":
        this.compileRepeat(fs, st);
        break;
      case "NumericFor":
        this.compileNumericFor(fs, st);
        break;
      case "GenericFor":
        this.compileGenericFor(fs, st);
        break;
      case "Do":
        this.compileBlock(fs, st.body);
        break;
      case "Return":
        this.compileReturn(fs, st);
        break;
      case "Break": {
        if (fs.loops.length === 0) throw new Error("break outside loop");
        const idx = this.emit(fs, OP.JMP);
        fs.loops[fs.loops.length - 1].breaks.push(idx);
        break;
      }
      default:
        throw new Error("compileStat: unknown " + st.kind);
    }
  }

  compileFunction(fs, func, name) {
    const child = new FnState(fs, name);
    child.proto.numParams = func.params.length;
    child.proto.hasVararg = func.hasVararg;
    const s = { vars: new Map(), base: 0 };
    for (let i = 0; i < func.params.length; i++) s.vars.set(func.params[i], i);
    child.scopes.push(s);
    child.regTop = func.params.length;
    this.compileBlock(child, func.body);
    this.emit(child, OP.RETURN, 0, 1, 0);
    child.proto.maxStack = Math.max(child.proto.maxStack, child.regTop + 1);
    child.proto.upvals = child.upvals;
    return child.proto;
  }

  compileLocal(fs, st) {
    const names = st.names;
    const vals = st.exprs;
    const base = fs.regTop;
    const nNames = names.length;
    const nVals = vals.length;
    if (nVals === 0) {
      if (nNames > 0) this.emit(fs, OP.LOADNIL, base, nNames - 1, 0);
    } else {
      for (let i = 0; i < nVals - 1; i++) {
        fs.regTop = base + i + 1;
        this.expToReg(fs, vals[i], base + i, 1);
      }
      const last = vals[nVals - 1];
      const isMult = this.isMultret(last);
      const want = isMult ? Math.max(1, nNames - (nVals - 1)) : 1;
      fs.regTop = base + nVals;
      this.expToReg(fs, last, base + nVals - 1, want);
      const filled = isMult ? (nVals - 1) + want : nVals;
      if (filled < nNames) this.emit(fs, OP.LOADNIL, base + filled, nNames - filled - 1, 0);
    }
    for (let j = 0; j < nNames; j++) this.declareLocal(fs, names[j], base + j);
    fs.regTop = base + nNames;
    this.bump(fs, base + nNames);
  }

  compileAssign(fs, st) {
    const targets = st.targets;
    const vals = st.values;
    const nT = targets.length;
    const nVals = vals.length;
    const base = fs.regTop;
    let filled = 0;
    for (let i = 0; i < nVals - 1; i++) {
      fs.regTop = base + i + 1;
      this.expToReg(fs, vals[i], base + i, 1);
    }
    if (nVals > 0) {
      const last = vals[nVals - 1];
      const isMult = this.isMultret(last);
      const want = isMult ? Math.max(1, nT - (nVals - 1)) : 1;
      fs.regTop = base + nVals;
      this.expToReg(fs, last, base + nVals - 1, want);
      filled = isMult ? (nVals - 1) + want : nVals;
    }
    for (let j = 0; j < nT; j++) {
      const src = base + Math.min(j, Math.max(filled - 1, 0));
      this.assignFromReg(fs, targets[j], src);
    }
    fs.regTop = base;
  }

  assignFromReg(fs, target, src) {
    if (target.kind === "Name") {
      const r = this.resolveLocal(fs, target.name);
      if (r !== null) {
        if (r !== src) this.emit(fs, OP.MOVE, r, src, 0);
        return;
      }
      const u = this.resolveUpval(fs, target.name);
      if (u !== null) {
        this.emit(fs, OP.SETUPVAL, src, u, 0);
        return;
      }
      this.emit(fs, OP.SETGLOBAL, src, this.k(fs, target.name), 0);
      return;
    }
    if (target.kind === "Index") {
      const saved = fs.regTop;
      const o = fs.regTop++;
      this.bump(fs, o + 1);
      this.expToReg(fs, target.obj, o, 1);
      const keyRK = this.operandRK(fs, target.idx);
      this.emit(fs, OP.SETTABLE, o, keyRK, src);
      fs.regTop = saved;
      return;
    }
    throw new Error("assignFromReg: bad target");
  }

  compileReturn(fs, st) {
    const args = st.args;
    const base = fs.regTop;
    const n = args.length;
    if (n === 0) {
      this.emit(fs, OP.RETURN, base, 1, 0);
      return;
    }
    for (let i = 0; i < n - 1; i++) {
      fs.regTop = base + i + 1;
      this.expToReg(fs, args[i], base + i, 1);
    }
    const last = args[n - 1];
    const isMult = this.isMultret(last);
    fs.regTop = base + n;
    this.expToReg(fs, last, base + n - 1, isMult ? 0 : 1);
    if (isMult) this.emit(fs, OP.RETURN, base, 0, 0);
    else this.emit(fs, OP.RETURN, base, n + 1, 0);
    fs.regTop = base;
  }

  compileIf(fs, st) {
    const jEnds = [];
    for (const cl of st.clauses) {
      this.openScope(fs);
      this.expToReg(fs, cl.cond, fs.regTop, 1);
      this.emit(fs, OP.TEST, fs.regTop, 0, 1);
      const jElse = this.emit(fs, OP.JMP);
      this.compileBlock(fs, cl.body);
      this.closeScope(fs);
      jEnds.push(this.emit(fs, OP.JMP));
      this.patchJmp(fs, jElse, this.pc(fs));
    }
    if (st.orelse) this.compileBlock(fs, st.orelse);
    const end = this.pc(fs);
    for (const j of jEnds) this.patchJmp(fs, j, end);
  }

  compileWhile(fs, st) {
    const jCond = this.emit(fs, OP.JMP);
    fs.loops.push({ breaks: [] });
    const bodyStart = this.pc(fs);
    this.compileBlock(fs, st.body);
    const condPos = this.pc(fs);
    this.expToReg(fs, st.cond, fs.regTop, 1);
    this.emit(fs, OP.TEST, fs.regTop, 0, 0);
    const jBack = this.emit(fs, OP.JMP);
    this.patchJmp(fs, jBack, bodyStart);
    const end = this.pc(fs);
    for (const b of fs.loops.pop().breaks) this.patchJmp(fs, b, end);
    this.patchJmp(fs, jCond, condPos);
  }

  compileRepeat(fs, st) {
    fs.loops.push({ breaks: [] });
    const bodyStart = this.pc(fs);
    this.openScope(fs);
    for (const s of st.body.stats) this.compileStat(fs, s);
    this.expToReg(fs, st.cond, fs.regTop, 1);
    this.emit(fs, OP.TEST, fs.regTop, 0, 1);
    const jBack = this.emit(fs, OP.JMP);
    this.patchJmp(fs, jBack, bodyStart);
    this.closeScope(fs);
    const end = this.pc(fs);
    for (const b of fs.loops.pop().breaks) this.patchJmp(fs, b, end);
  }

  compileNumericFor(fs, st) {
    const A = fs.regTop;
    fs.regTop = A + 3;
    this.bump(fs, A + 4);
    this.expToReg(fs, st.start, A, 1);
    this.expToReg(fs, st.stop, A + 1, 1);
    this.expToReg(fs, st.step, A + 2, 1);
    const forprep = this.emit(fs, OP.FORPREP, A, 0, 0);
    const bodyStart = this.pc(fs);
    fs.loops.push({ breaks: [] });
    this.openScope(fs);
    this.declareLocal(fs, st.name, A + 3);
    fs.regTop = A + 4;
    for (const s of st.body.stats) this.compileStat(fs, s);
    this.closeScope(fs);
    const forloop = this.emit(fs, OP.FORLOOP, A, 0, 0);
    this.patchJmp(fs, forloop, bodyStart);
    this.patchJmp(fs, forprep, forloop);
    const end = this.pc(fs);
    for (const b of fs.loops.pop().breaks) this.patchJmp(fs, b, end);
    fs.regTop = A;
  }

  compileGenericFor(fs, st) {
    const A = fs.regTop;
    const nvars = st.names.length;
    fs.regTop = A + 3;
    this.bump(fs, A + 3 + nvars);
    const n = st.exprs.length;
    for (let i = 0; i < n; i++) {
      const want = i === n - 1 ? Math.max(1, 3 - i) : 1;
      fs.regTop = A + i + 1;
      this.expToReg(fs, st.exprs[i], A + i, want);
    }
    const jCond = this.emit(fs, OP.JMP);
    const bodyStart = this.pc(fs);
    fs.loops.push({ breaks: [] });
    this.openScope(fs);
    for (let i = 0; i < nvars; i++) this.declareLocal(fs, st.names[i], A + 3 + i);
    fs.regTop = A + 3 + nvars;
    for (const s of st.body.stats) this.compileStat(fs, s);
    this.closeScope(fs);
    const tfor = this.emit(fs, OP.TFORLOOP, A, nvars, 0);
    const jBack = this.emit(fs, OP.JMP);
    this.patchJmp(fs, jBack, bodyStart);
    this.patchJmp(fs, jCond, tfor);
    const end = this.pc(fs);
    for (const b of fs.loops.pop().breaks) this.patchJmp(fs, b, end);
    fs.regTop = A;
  }

  isMultret(e) {
    return e.kind === "Call" || e.kind === "MethodCall" || e.kind === "Vararg";
  }

  operandRK(fs, n) {
    if (n.kind === "Number" || n.kind === "String" || n.kind === "Boolean") {
      return this.k(fs, n.v) | RKBIT;
    }
    if (n.kind === "Nil") {
      const t = fs.regTop++;
      this.bump(fs, t + 1);
      this.emit(fs, OP.LOADNIL, t, 0, 0);
      return t;
    }
    if (n.kind === "Name") {
      const r = this.resolveLocal(fs, n.name);
      if (r !== null) return r;
      const u = this.resolveUpval(fs, n.name);
      const t = fs.regTop++;
      this.bump(fs, t + 1);
      if (u !== null) this.emit(fs, OP.GETUPVAL, t, u, 0);
      else this.emit(fs, OP.GETGLOBAL, t, this.k(fs, n.name), 0);
      return t;
    }
    const t = fs.regTop++;
    this.bump(fs, t + 1);
    this.expToReg(fs, n, t, 1);
    return t;
  }

  expToReg(fs, node, reg, nres = 1) {
    const savedTop = fs.regTop;
    this.bump(fs, reg + 2);
    switch (node.kind) {
      case "Number":
        this.emit(fs, OP.LOADK, reg, this.k(fs, node.v), 0);
        break;
      case "String":
        this.emit(fs, OP.LOADK, reg, this.k(fs, node.v), 0);
        break;
      case "Boolean":
        this.emit(fs, OP.LOADBOOL, reg, node.v ? 1 : 0, 0);
        break;
      case "Nil":
        this.emit(fs, OP.LOADNIL, reg, 0, 0);
        break;
      case "Name": {
        const r = this.resolveLocal(fs, node.name);
        if (r !== null) {
          if (r !== reg) this.emit(fs, OP.MOVE, reg, r, 0);
          break;
        }
        const u = this.resolveUpval(fs, node.name);
        if (u !== null) {
          this.emit(fs, OP.GETUPVAL, reg, u, 0);
          break;
        }
        this.emit(fs, OP.GETGLOBAL, reg, this.k(fs, node.name), 0);
        break;
      }
      case "Index": {
        const o = fs.regTop++;
        this.bump(fs, o + 1);
        this.expToReg(fs, node.obj, o, 1);
        const key = this.operandRK(fs, node.idx);
        this.emit(fs, OP.GETTABLE, reg, o, key);
        break;
      }
      case "Binop": {
        const op = node.op;
        if (op === "..") {
          fs.regTop = Math.max(fs.regTop, reg + 2);
          this.bump(fs, reg + 2);
          this.expToReg(fs, node.left, reg, 1);
          this.expToReg(fs, node.right, reg + 1, 1);
          this.emit(fs, OP.CONCAT, reg, reg, reg + 1);
          break;
        }
        if (CMPOP[op] !== undefined) {
          const [cop, swap] = CMPOP[op];
          const b = this.operandRK(fs, node.left);
          const c = this.operandRK(fs, node.right);
          this.emit(fs, cop, reg, swap ? c : b, swap ? b : c);
          break;
        }
        const b = this.operandRK(fs, node.left);
        const c = this.operandRK(fs, node.right);
        this.emit(fs, BINOPCODE[op], reg, b, c);
        break;
      }
      case "Unop": {
        const b = this.operandRK(fs, node.e);
        const op = node.op === "-" ? OP.UNM : node.op === "not" ? OP.NOT : OP.LEN;
        this.emit(fs, op, reg, b, 0);
        break;
      }
      case "Logical": {
        this.expToReg(fs, node.left, reg, 1);
        this.emit(fs, OP.TEST, reg, 0, node.op === "and" ? 1 : 0);
        const j = this.emit(fs, OP.JMP);
        this.expToReg(fs, node.right, reg, 1);
        this.patchJmp(fs, j, this.pc(fs));
        break;
      }
      case "Function": {
        const proto = this.compileFunction(fs, node, "");
        this.emit(fs, OP.NEWCLOSURE, reg, fs.proto.protos.length, 0);
        fs.proto.protos.push(proto);
        break;
      }
      case "Table":
        this.compileTable(fs, node, reg);
        break;
      case "Vararg": {
        if (!fs.proto.hasVararg) throw new Error("'...' outside vararg function");
        this.emit(fs, OP.VARARG, reg, nres === 0 ? 0 : nres, 0);
        break;
      }
      case "Call":
        this.compileCall(fs, node, reg, nres, 0);
        break;
      case "MethodCall":
        this.compileCall(fs, node, reg, nres, 1);
        break;
      default:
        throw new Error("expToReg: unknown " + node.kind);
    }
    fs.regTop = savedTop;
  }

  compileCall(fs, node, reg, nres, isMethod) {
    const savedTop = fs.regTop;
    let argBase;
    if (isMethod) {
      argBase = reg + 2;
      this.expToReg(fs, node.obj, reg + 1, 1);
      this.emit(fs, OP.SELF, reg, reg + 1, this.k(fs, node.name) | RKBIT);
    } else {
      argBase = reg + 1;
      this.expToReg(fs, node.fn, reg, 1);
    }
    const n = node.args.length;
    const lastIsMult = n > 0 && this.isMultret(node.args[n - 1]);
    let b = n + 1 + isMethod;
    for (let i = 0; i < n - 1; i++) {
      fs.regTop = argBase + i + 1;
      this.expToReg(fs, node.args[i], argBase + i, 1);
    }
    if (n > 0) {
      const last = node.args[n - 1];
      const want = lastIsMult ? 0 : 1;
      fs.regTop = argBase + n;
      this.expToReg(fs, last, argBase + n - 1, want);
      if (lastIsMult) b = 0;
    }
    this.bump(fs, argBase + n + 1);
    const c = nres === 0 ? 0 : nres + 1;
    this.emit(fs, OP.CALL, reg, b, c);
    fs.regTop = savedTop;
  }

  compileTable(fs, node, reg) {
    this.emit(fs, OP.NEWTABLE, reg, 0, 0);
    fs.regTop = reg + 1;
    let nArr = 0;
    let batch = 0;
    const flush = () => {
      if (nArr === 0) return;
      batch++;
      this.emit(fs, OP.SETLIST, reg, nArr, batch);
      nArr = 0;
    };
    for (const item of node.items) {
      if (item.type === "array") {
        const isLast = item === node.items[node.items.length - 1];
        if (isLast && this.isMultret(item.value)) {
          this.expToReg(fs, item.value, fs.regTop, 0);
          batch++;
          this.emit(fs, OP.SETLIST, reg, 0, batch);
          nArr = 0;
          fs.regTop = reg + 1;
          continue;
        }
        this.expToReg(fs, item.value, fs.regTop, 1);
        fs.regTop++;
        nArr++;
        this.bump(fs, fs.regTop);
        if (nArr === 50) flush();
      } else {
        flush();
        const keyNode = item.type === "named" ? { kind: "String", v: item.key, line: node.line } : item.key;
        const key = this.operandRK(fs, keyNode);
        const temp = fs.regTop;
        this.bump(fs, temp + 1);
        this.expToReg(fs, item.value, temp, 1);
        this.emit(fs, OP.SETTABLE, reg, key, temp);
      }
    }
    flush();
    fs.regTop = reg + 1;
  }
}

function compile(src) {
  const { parse } = require("./parser");
  return new Compiler().compile(parse(src));
}

module.exports = { Compiler, compile };
