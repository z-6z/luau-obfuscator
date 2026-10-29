function toUtf8(s) {
  const out = [];
  for (let j = 0; j < s.length; j++) {
    const cp = s.codePointAt(j);
    if (cp > 0xffff) j++;
    if (cp < 0x80) out.push(cp);
    else if (cp < 0x800) out.push(0xc0 | (cp >> 6), 0x80 | (cp & 0x3f));
    else if (cp < 0x10000) out.push(0xe0 | (cp >> 12), 0x80 | ((cp >> 6) & 0x3f), 0x80 | (cp & 0x3f));
    else out.push(0xf0 | (cp >> 18), 0x80 | ((cp >> 12) & 0x3f), 0x80 | ((cp >> 6) & 0x3f), 0x80 | (cp & 0x3f));
  }
  return out;
}

function luaStr(s) {
  const bytes = toUtf8(s);
  let out = '"';
  for (const b of bytes) {
    if (b === 34) out += '\\"';
    else if (b === 92) out += "\\\\";
    else if (b === 10) out += "\\n";
    else if (b === 9) out += "\\t";
    else if (b === 13) out += "\\r";
    else if (b >= 32 && b <= 126) out += String.fromCharCode(b);
    else out += "\\" + String(b).padStart(3, "0");
  }
  return out + '"';
}

function luaConst(v) {
  if (typeof v === "number") return String(v);
  if (typeof v === "boolean") return v ? "true" : "false";
  return luaStr(String(v));
}

function serializeProto(proto) {
  const parts = [];
  parts.push(luaStr(proto.name));
  parts.push(String(proto.numParams));
  parts.push(proto.hasVararg ? "true" : "false");
  parts.push("{" + proto.code.map((ins) => "{" + ins.join(",") + "}").join(",") + "}");
  parts.push("{" + proto.k.map(luaConst).join(",") + "}");
  parts.push("{" + proto.protos.map(serializeProto).join(",") + "}");
  parts.push("{" + proto.upvals.map((u) => "{" + (u.instack ? 1 : 0) + "," + u.idx + "}").join(",") + "}");
  parts.push(String(proto.maxStack));
  return "{" + parts.join(",") + "}";
}

function serializeAll(proto) {
  return "return " + serializeProto(proto) + "\n";
}

module.exports = { luaStr, serializeProto, serializeAll };
