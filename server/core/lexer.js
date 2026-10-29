const KEYWORDS = new Set([
  "local", "function", "end", "if", "then", "else", "elseif",
  "while", "do", "for", "in", "repeat", "until", "return",
  "break", "true", "false", "nil", "and", "or", "not",
]);

function lex(src) {
  const toks = [];
  let i = 0;
  let line = 1;

  const isDigit = (c) => c >= "0" && c <= "9";
  const isAlpha = (c) => /[A-Za-z_]/.test(c);

  while (i < src.length) {
    const c = src[i];

    if (c === "\n") { line++; i++; continue; }
    if (c === " " || c === "\t" || c === "\r") { i++; continue; }

    if (c === "-" && src[i + 1] === "-") {
      if (src.slice(i + 2, i + 3) === "[") {
        const j = src.indexOf("]]", i);
        if (j === -1) throw new Error("Unterminated long comment at line " + line);
        for (let k = i; k < j; k++) if (src[k] === "\n") line++;
        i = j + 2;
        continue;
      }
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }

    if (c === "[" && src.slice(i, i + 2) === "[[") {
      const j = src.indexOf("]]", i);
      if (j === -1) throw new Error("Unterminated long string at line " + line);
      const s = src.slice(i + 2, j);
      for (let k = i; k < j; k++) if (src[k] === "\n") line++;
      toks.push({ t: "string", v: s, line });
      i = j + 2;
      continue;
    }

    if (isDigit(c) || (c === "." && isDigit(src[i + 1]))) {
      let j = i;
      while (j < src.length && /[0-9.eExXa-fA-F]/.test(src[j])) j++;
      let str = src.slice(i, j);
      if ((str.includes("e") || str.includes("E")) && /[+-]/.test(src[j] || "")) {
        str += src[j] + (src.slice(j + 1).match(/[0-9]+/)?.[0] || "");
        j = i + str.length;
      }
      toks.push({ t: "number", v: Number(str), line });
      i = j;
      continue;
    }

    if (c === '"' || c === "'") {
      const q = c;
      let j = i + 1;
      let s = "";
      while (j < src.length && src[j] !== q) {
        if (src[j] === "\\") {
          const e = src[j + 1];
          const map = { n: "\n", t: "\t", r: "\r", "\\": "\\", '"': '"', "'": "'" };
          s += map[e] !== undefined ? map[e] : e;
          j += 2;
        } else {
          s += src[j++];
        }
      }
      toks.push({ t: "string", v: s, line });
      i = j + 1;
      continue;
    }

    if (isAlpha(c)) {
      let j = i;
      while (j < src.length && /[A-Za-z0-9_]/.test(src[j])) j++;
      const w = src.slice(i, j);
      toks.push({ t: KEYWORDS.has(w) ? w : "name", v: w, line });
      i = j;
      continue;
    }

    const three = src.slice(i, i + 3);
    const two = src.slice(i, i + 2);
    if (three === "...") { toks.push({ t: "...", line }); i += 3; continue; }
    if (["==", "~=", "<=", ">=", "..", "+=", "-=", "*=", "/=", "%=", "^=", "..="].includes(two)) {
      toks.push({ t: two, line });
      i += 2;
      continue;
    }
    toks.push({ t: c, line });
    i++;
  }

  toks.push({ t: "eof", line });
  return toks;
}

module.exports = { lex, KEYWORDS };
