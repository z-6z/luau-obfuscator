const { lex } = require("./lexer");

const BINPREC = {
  "<": 3, ">": 3, "<=": 3, ">=": 3, "~=": 3, "==": 3,
  "..": 5,
  "+": 6, "-": 6,
  "*": 7, "/": 7, "%": 7,
};

class Parser {
  constructor(src) {
    this.toks = lex(src);
    this.pos = 0;
  }

  peek(k = 0) { return this.toks[this.pos + k]; }
  next() { return this.toks[this.pos++]; }
  check(t) { return this.peek().t === t; }
  accept(t) { if (this.check(t)) { this.pos++; return true; } return false; }

  expect(t) {
    const tk = this.next();
    if (tk.t !== t) {
      throw new Error(`Line ${tk.line}: expected '${t}', got '${tk.t}'${tk.v ? " '" + tk.v + "'" : ""}`);
    }
    return tk;
  }

  expectName() {
    const tk = this.next();
    if (tk.t !== "name") throw new Error(`Line ${tk.line}: expected name`);
    return tk.v;
  }

  parseBlock() {
    const stats = [];
    while (true) {
      const t = this.peek().t;
      if (["eof", "end", "else", "elseif", "until"].includes(t)) break;

      if (t === "return") {
        const line = this.next().line;
        let exprs = [];
        if (!["eof", "end", "else", "elseif", "until", ";"].includes(this.peek().t)) {
          exprs = this.parseExprList();
        }
        this.accept(";");
        stats.push({ kind: "Return", args: exprs, line });
        break;
      }
      if (t === "break") {
        const line = this.next().line;
        this.accept(";");
        stats.push({ kind: "Break", line });
        continue;
      }
      if (t === ";") { this.pos++; continue; }
      if (this.peek().t === "eof") break;

      stats.push(this.parseStat());
    }
    return { kind: "Block", stats };
  }

  parseStat() {
    const tk = this.peek();
    switch (tk.t) {
      case "local":    return this.parseLocal();
      case "function": return this.parseFunctionStat();
      case "if":       return this.parseIf();
      case "while":    return this.parseWhile();
      case "for":      return this.parseFor();
      case "repeat":   return this.parseRepeat();
      case "do": {
        this.next();
        const body = this.parseBlock();
        this.expect("end");
        return { kind: "Do", body, line: tk.line };
      }
    }

    const e = this.parseSuffixedExp();

    if (this.check("=")) {
      this.next();
      const targets = [e];
      while (this.accept(",")) targets.push(this.parseSuffixedExp());
      const values = this.parseExprList();
      return { kind: "Assign", targets, values, line: tk.line };
    }

    for (const op of ["+=", "-=", "*=", "/=", "%=", "^=", "..="]) {
      if (this.check(op)) {
        this.next();
        const value = this.parseExpr();
        const binop = op.slice(0, -1);
        return {
          kind: "Assign",
          targets: [e],
          values: [{ kind: "Binop", op: binop, left: e, right: value, line: tk.line }],
          line: tk.line,
        };
      }
    }

    if (e.kind === "Call" || e.kind === "MethodCall") return e;
    throw new Error(`Line ${tk.line}: syntax error near '${this.peek().v ?? this.peek().t}'`);
  }

  parseLocal() {
    const line = this.expect("local").line;
    if (this.accept("function")) {
      const name = this.expectName();
      const func = this.parseFuncBody(line);
      return { kind: "LocalFunction", name, func, line };
    }
    const names = [this.expectName()];
    while (this.accept(",")) names.push(this.expectName());
    let exprs = [];
    if (this.accept("=")) exprs = this.parseExprList();
    return { kind: "Local", names, exprs, line };
  }

  parseFunctionStat() {
    const line = this.expect("function").line;
    let target = { kind: "Name", name: this.expectName(), line };
    let isMethod = false;
    while (this.check(".") || this.check(":")) {
      const isM = this.next().t === ":";
      const name = this.expectName();
      target = { kind: "Index", obj: target, idx: { kind: "String", v: name, line }, line };
      if (isM) { isMethod = true; break; }
    }
    const func = this.parseFuncBody(line, isMethod);
    return { kind: "Assign", targets: [target], values: [func], line };
  }

  parseFuncBody(line, isMethod = false) {
    this.expect("(");
    const params = [];
    if (isMethod) params.push("self");
    let hasVararg = false;
    if (!this.check(")")) {
      while (true) {
        if (this.accept("...")) { hasVararg = true; break; }
        params.push(this.expectName());
        if (!this.accept(",")) break;
      }
    }
    this.expect(")");
    const body = this.parseBlock();
    this.expect("end");
    return { kind: "Function", params, hasVararg, body, line };
  }

  parseIf() {
    const line = this.expect("if").line;
    const clauses = [];
    const cond = this.parseExpr();
    this.expect("then");
    clauses.push({ cond, body: this.parseBlock() });
    while (this.check("elseif")) {
      this.next();
      const c = this.parseExpr();
      this.expect("then");
      clauses.push({ cond: c, body: this.parseBlock() });
    }
    let orelse = null;
    if (this.accept("else")) orelse = this.parseBlock();
    this.expect("end");
    return { kind: "If", clauses, orelse, line };
  }

  parseWhile() {
    const line = this.expect("while").line;
    const cond = this.parseExpr();
    this.expect("do");
    const body = this.parseBlock();
    this.expect("end");
    return { kind: "While", cond, body, line };
  }

  parseRepeat() {
    const line = this.expect("repeat").line;
    const body = this.parseBlock();
    const cond = (this.expect("until"), this.parseExpr());
    return { kind: "Repeat", body, cond, line };
  }

  parseFor() {
    const line = this.expect("for").line;
    const names = [this.expectName()];
    while (this.accept(",")) names.push(this.expectName());

    if (this.accept("=")) {
      const start = this.parseExpr();
      this.expect(",");
      const stop = this.parseExpr();
      const step = this.accept(",") ? this.parseExpr() : { kind: "Number", v: 1, line };
      this.expect("do");
      const body = this.parseBlock();
      this.expect("end");
      return { kind: "NumericFor", name: names[0], start, stop, step, body, line };
    }

    this.expect("in");
    const exprs = this.parseExprList();
    this.expect("do");
    const body = this.parseBlock();
    this.expect("end");
    return { kind: "GenericFor", names, exprs, body, line };
  }

  parseExprList() {
    const list = [this.parseExpr()];
    while (this.accept(",")) list.push(this.parseExpr());
    return list;
  }

  parseExpr() {
    let left = this.parseSimpleExp();
    while (true) {
      const t = this.peek().t;

      if (t === "^") {
        this.next();
        const right = this.parseExpr();
        left = { kind: "Binop", op: "^", left, right, line: this.peek().line };
        continue;
      }

      if (t === "or" || t === "and") {
        this.next();
        const right = this.parseExpr();
        left = { kind: "Logical", op: t, left, right, line: this.peek().line };
        continue;
      }

      const prec = BINPREC[t];
      if (prec) {
        this.next();
        const right = this.parseBinopRhs(prec);
        left = { kind: "Binop", op: t, left, right, line: this.peek().line };
        continue;
      }
      break;
    }
    return left;
  }

  parseBinopRhs(limit) {
    let left = this.parseSimpleExp();
    while (true) {
      const t = this.peek().t;
      const prec = BINPREC[t];
      if (!prec || prec <= limit) break;
      this.next();
      const right = this.parseBinopRhs(prec);
      left = { kind: "Binop", op: t, left, right, line: this.peek().line };
    }
    return left;
  }

  parseSimpleExp() {
    const tk = this.peek();
    switch (tk.t) {
      case "number": this.next(); return { kind: "Number", v: tk.v, line: tk.line };
      case "string": this.next(); return { kind: "String", v: tk.v, line: tk.line };
      case "nil":    this.next(); return { kind: "Nil", line: tk.line };
      case "true":   this.next(); return { kind: "Boolean", v: true, line: tk.line };
      case "false":  this.next(); return { kind: "Boolean", v: false, line: tk.line };
      case "...":    this.next(); return { kind: "Vararg", line: tk.line };
      case "not": this.next(); return { kind: "Unop", op: "not", e: this.parseSimpleExp(), line: tk.line };
      case "-":   this.next(); return { kind: "Unop", op: "-",   e: this.parseSimpleExp(), line: tk.line };
      case "#":   this.next(); return { kind: "Unop", op: "#",   e: this.parseSimpleExp(), line: tk.line };
      case "{":   return this.parseTable();
      case "function": {
        this.next();
        return this.parseFuncBody(tk.line);
      }
      case "(": {
        this.next();
        const e = this.parseExpr();
        this.expect(")");
        return e;
      }
    }
    if (tk.t === "name") return this.parseSuffixedExp();
    throw new Error(`Line ${tk.line}: unexpected symbol '${tk.v ?? tk.t}'`);
  }

  parseSuffixedExp() {
    const nameTk = this.peek();
    let e = { kind: "Name", name: this.expectName(), line: nameTk.line };
    while (true) {
      const t = this.peek().t;
      if (t === ".") {
        this.next();
        const name = this.expectName();
        e = { kind: "Index", obj: e, idx: { kind: "String", v: name, line: this.peek().line }, line: this.peek().line };
      } else if (t === "[") {
        this.next();
        const idx = this.parseExpr();
        this.expect("]");
        e = { kind: "Index", obj: e, idx, line: this.peek().line };
      } else if (t === ":") {
        this.next();
        const mname = this.expectName();
        const args = this.parseCallArgs();
        e = { kind: "MethodCall", obj: e, name: mname, args, line: this.peek().line };
      } else if (t === "(" || t === "string" || t === "{") {
        const args = this.parseCallArgs();
        e = { kind: "Call", fn: e, args, line: this.peek().line };
      } else break;
    }
    return e;
  }

  parseCallArgs() {
    const t = this.peek().t;
    if (t === "string") {
      this.next();
      return [{ kind: "String", v: this.toks[this.pos - 1].v, line: this.toks[this.pos - 1].line }];
    }
    if (t === "{") return [this.parseTable()];
    this.expect("(");
    const args = [];
    if (!this.check(")")) args.push(...this.parseExprList());
    this.expect(")");
    return args;
  }

  parseTable() {
    const line = this.expect("{").line;
    const items = [];
    while (!this.check("}")) {
      if (this.check("[")) {
        this.next();
        const key = this.parseExpr();
        this.expect("]");
        this.expect("=");
        items.push({ type: "indexed", key, value: this.parseExpr() });
      } else if (this.check("name") && this.toks[this.pos + 1].t === "=") {
        const key = this.expectName();
        this.next();
        items.push({ type: "named", key, value: this.parseExpr() });
      } else {
        items.push({ type: "array", value: this.parseExpr() });
      }
      if (!this.accept(",") && !this.accept(";")) break;
    }
    this.expect("}");
    return { kind: "Table", items, line };
  }
}

function parse(src) {
  const p = new Parser(src);
  const ast = p.parseBlock();
  if (!p.check("eof")) {
    throw new Error("Unexpected trailing tokens near line " + p.peek().line);
  }
  return ast;
}

module.exports = { Parser, parse };
