// One random type, spelled three ways: the Sury schema under test, the Rust
// type the oracle compiles, and `dump`, the canonical text of a value both
// sides print. The dump is written from this model, never from Sury's own
// layout, so it is an independent reading of the bytes.

import * as S from "sury";

export type Num = "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" | "u128" | "i128" | "U256" | "f32" | "f64";

export type T =
  | { k: "num"; r: Num; as: "int32" | "number" | "bigint" | "integer"; nan?: boolean }
  | { k: "bool" }
  | { k: "char" }
  | { k: "str" }
  | { k: "bytes" }
  | { k: "vec"; i: T }
  | { k: "opt"; i: T; none: null | undefined }
  | { k: "box"; i: T }
  | { k: "struct"; name: string; f: [string, T][]; rec?: boolean }
  | { k: "tuple"; i: T[] }
  | { k: "uenum"; name: string; lits: (string | number | boolean)[] }
  | { k: "denum"; name: string; tag: string; vars: { lit: string; f: [string, T][] | null }[] }
  | { k: "lit"; v: string | number | boolean }
  | { k: "self"; name: string };

export type Rng = { next: () => number; int: (lo: number, hi: number) => number; pick: <X>(xs: readonly X[]) => X; chance: (p: number) => boolean };

export const rng = (seed: number): Rng => {
  let a = seed >>> 0;
  const next = () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const int = (lo: number, hi: number) => lo + Math.floor(next() * (hi - lo + 1));
  return { next, int, pick: (xs) => xs[int(0, xs.length - 1)]!, chance: (p) => next() < p };
};

// ── types ───────────────────────────────────────────────────────────────────

let names = 0;
const name = (prefix: string) => `${prefix}${names++}`;

const numberTypes: [Num, "int32" | "number" | "bigint" | "integer"][] = [
  ["u8", "int32"],
  ["i8", "int32"],
  ["u16", "int32"],
  ["i16", "int32"],
  ["i32", "int32"],
  ["u32", "number"],
  ["f32", "number"],
  ["f64", "number"],
  ["i64", "number"],
  ["u64", "number"],
  ["i64", "integer"],
  ["u64", "bigint"],
  ["i64", "bigint"],
  ["u128", "bigint"],
  ["i128", "bigint"],
  ["U256", "bigint"],
];

const key = (r: Rng, idx: number) => `${r.pick(["a", "id", "name", "x", "value", "items", "next"])}${idx}`;

const fields = (r: Rng, depth: number, self?: string): [string, T][] => {
  const count = r.int(0, 4);
  const out: [string, T][] = [];
  for (let idx = 0; idx < count; idx++) out.push([key(r, idx), genType(r, depth + 1, self)]);
  return out;
};

export const genType = (r: Rng, depth = 0, self?: string): T => {
  const leafOnly = depth >= 3;
  const roll = r.int(0, leafOnly ? 6 : 15);
  switch (roll) {
    case 0:
    case 1: {
      const [type, as] = r.pick(numberTypes);
      return { k: "num", r: type, as, nan: type === "f64" && as === "number" && r.chance(0.3) };
    }
    case 2:
      return { k: "bool" };
    case 3:
      return r.chance(0.3) ? { k: "char" } : { k: "str" };
    case 4:
      return { k: "str" };
    case 5:
      return r.chance(0.5) ? { k: "bytes" } : { k: "lit", v: r.pick(["kind", 7, true, ""]) };
    case 6:
      return { k: "uenum", name: name("E"), lits: r.pick([["a", "b"], ["x", "y", "z"], [1, 2, 3], [true, false], ["on", 0]]) };
    case 7:
    case 8: {
      // S.rkyv refuses a list of zero-sized items: rkyv reads any length of
      // them, more than a JS array holds.
      let item = genType(r, depth + 1, self);
      while (zeroSized(item)) item = genType(r, depth + 1, self);
      return { k: "vec", i: item };
    }
    case 9: {
      const none = r.chance(0.7) ? undefined : null;
      let inner = genType(r, depth + 1, self);
      // `S.optional` of an optional is the same union, so only the one spelling
      // of two nested options exists: undefined outside, null inside.
      while (inner.k === "opt" && !(none === undefined && inner.none === null && inner.i.k !== "opt")) inner = genType(r, depth + 1, self);
      return { k: "opt", i: inner, none };
    }
    case 10:
      return { k: "box", i: genType(r, depth + 1, self) };
    case 11:
    case 12:
      return { k: "struct", name: name("S"), f: fields(r, depth, self) };
    case 13: {
      const count = r.int(0, 4);
      return { k: "tuple", i: Array.from({ length: count }, () => genType(r, depth + 1, self)) };
    }
    case 14: {
      const tag = r.pick(["type", "kind", "tag"]);
      const count = r.int(2, 4);
      return {
        k: "denum",
        name: name("D"),
        tag,
        vars: Array.from({ length: count }, (_, idx) => ({
          lit: `v${idx}`,
          f: r.chance(0.2) ? null : fields(r, depth, self).filter(([k]) => k !== tag),
        })),
      };
    }
  }
  // A recursive struct, reaching itself through a list or a boxed option.
  if (self !== U) return { k: "str" };
  const structName = name("R");
  const own = fields(r, depth + 1, structName);
  own.push([`kids${own.length}`, r.chance(0.5) ? { k: "vec", i: { k: "self", name: structName } } : { k: "opt", none: undefined, i: { k: "box", i: { k: "self", name: structName } } }]);
  return { k: "struct", name: structName, f: own, rec: true };
};

const U = undefined;

const zeroSized = (t: T): boolean =>
  t.k === "lit" || (t.k === "tuple" && t.i.every(zeroSized)) || (t.k === "struct" && t.f.every(([, f]) => zeroSized(f)));

// ── Sury ────────────────────────────────────────────────────────────────────

export const toSchema = (t: T, selves: Map<string, unknown> = new Map()): any => {
  switch (t.k) {
    case "num": {
      let schema: any =
        t.as === "int32" ? S.int32 : t.as === "number" ? S.number : t.as === "integer" ? S.integer : S.bigint;
      const implied = t.as === "int32" ? "i32" : t.as === "number" ? "f64" : "i64";
      if (t.r !== implied) schema = schema.with(S.rkyvType, t.r);
      return t.nan ? S.union([schema, S.schema(NaN)]) : schema;
    }
    case "bool":
      return S.boolean;
    case "char":
      return S.string.with(S.rkyvType, "char");
    case "str":
      return S.string;
    case "bytes":
      return S.uint8Array;
    case "vec":
      return S.array(toSchema(t.i, selves));
    case "opt":
      return t.none === undefined ? S.optional(toSchema(t.i, selves)) : S.nullable(toSchema(t.i, selves));
    case "box":
      return toSchema(t.i, selves).with(S.rkyvType, "Box");
    case "struct": {
      const build = (): any => {
        const shape: Record<string, unknown> = {};
        for (const [k, f] of t.f) shape[k] = toSchema(f, selves);
        return S.schema(shape);
      };
      if (!t.rec) return build();
      return S.recursive(t.name, (self: unknown) => {
        selves.set(t.name, self);
        return build();
      });
    }
    case "tuple":
      return S.tuple(t.i.map((item) => toSchema(item, selves)));
    case "uenum":
      return S.union(t.lits as any);
    case "denum":
      return S.union(
        t.vars.map((v) => {
          if (v.f === null) return S.schema(v.lit);
          const shape: Record<string, unknown> = { [t.tag]: v.lit };
          for (const [k, f] of v.f) shape[k] = toSchema(f, selves);
          return S.schema(shape);
        }) as any,
      );
    case "lit":
      return S.schema(t.v);
    case "self":
      return selves.get(t.name);
  }
};

// ── Rust ────────────────────────────────────────────────────────────────────

const derive = "#[derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]";
const recursiveAttrs =
  "#[rkyv(serialize_bounds(__S: rkyv::ser::Writer + rkyv::ser::Allocator, __S::Error: rkyv::rancor::Source))]\n" +
  "#[rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source))]\n" +
  "#[rkyv(bytecheck(bounds(__C: rkyv::validation::ArchiveContext, __C::Error: rkyv::rancor::Source)))]";

const mentions = (t: T, self: string): boolean => {
  switch (t.k) {
    case "self":
      return t.name === self;
    case "vec":
    case "opt":
    case "box":
      return mentions(t.i, self);
    case "tuple":
      return t.i.some((i) => mentions(i, self));
    case "struct":
      return t.f.some(([, f]) => mentions(f, self));
    case "denum":
      return t.vars.some((v) => v.f?.some(([, f]) => mentions(f, self)) ?? false);
  }
  return false;
};

export const toRust = (t: T, defs: string[], self?: string): string => {
  switch (t.k) {
    case "num":
      return t.r === "U256" ? "ruint::aliases::U256" : t.r;
    case "bool":
      return "bool";
    case "char":
      return "char";
    case "str":
      return "String";
    case "bytes":
      return "Vec<u8>";
    case "vec":
      return `Vec<${toRust(t.i, defs, self)}>`;
    case "opt":
      return `Option<${toRust(t.i, defs, self)}>`;
    case "box":
      return `Box<${toRust(t.i, defs, self)}>`;
    case "tuple":
      return t.i.length === 0 ? "()" : `(${t.i.map((i) => toRust(i, defs, self)).join(", ")},)`;
    case "lit":
      return "()";
    case "self":
      return t.name;
    case "uenum":
      defs.push(
        `${derive}\npub enum ${t.name} { ${t.lits.map((_, idx) => `V${idx}`).join(", ")} }\n` +
          `impl Dump for ${t.name} { fn dump(&self, o: &mut String) { match self { ${t.lits.map((_, idx) => `${t.name}::V${idx} => o.push_str("E${idx}")`).join(", ")} } } }`,
      );
      return t.name;
    case "struct": {
      const own = t.rec ? t.name : self;
      const fieldDecls = t.f.map(([k, f]) => `${own !== U && mentions(f, own) ? "#[rkyv(omit_bounds)] " : ""}pub k_${k}: ${toRust(f, defs, own)}`);
      defs.push(
        `${derive}\n${t.rec || own !== U ? recursiveAttrs + "\n" : ""}pub struct ${t.name} { ${fieldDecls.join(", ")} }\n` +
          `impl Dump for ${t.name} { fn dump(&self, o: &mut String) { o.push('{'); ${t.f.map(([k], idx) => `${idx ? "o.push(','); " : ""}self.k_${k}.dump(o);`).join(" ")} o.push('}'); } }`,
      );
      return t.name;
    }
    case "denum": {
      const variants = t.vars.map((v, idx) =>
        v.f === null
          ? `V${idx}`
          : `V${idx} { ${v.f.map(([k, f]) => `${self !== U && mentions(f, self) ? "#[rkyv(omit_bounds)] " : ""}k_${k}: ${toRust(f, defs, self)}`).join(", ")} }`,
      );
      const arms = t.vars.map((v, idx) =>
        v.f === null
          ? `${t.name}::V${idx} => o.push_str("E${idx}")`
          : `${t.name}::V${idx} { ${v.f.map(([k]) => `k_${k}`).join(", ")} } => { o.push_str("E${idx}{"); ${v.f
              .map(([k], j) => `${j ? "o.push(','); " : ""}k_${k}.dump(o);`)
              .join(" ")} o.push('}'); }`,
      );
      defs.push(
        `${derive}\n${self !== U ? recursiveAttrs + "\n" : ""}pub enum ${t.name} { ${variants.join(", ")} }\n` +
          `impl Dump for ${t.name} { fn dump(&self, o: &mut String) { match self { ${arms.join(", ")} } } }`,
      );
      return t.name;
    }
  }
};

// ── values ──────────────────────────────────────────────────────────────────

const intRanges: Record<string, [bigint, bigint]> = {
  u8: [0n, 255n],
  i8: [-128n, 127n],
  u16: [0n, 65535n],
  i16: [-32768n, 32767n],
  u32: [0n, 4294967295n],
  i32: [-2147483648n, 2147483647n],
  u64: [0n, 2n ** 64n - 1n],
  i64: [-(2n ** 63n), 2n ** 63n - 1n],
  u128: [0n, 2n ** 128n - 1n],
  i128: [-(2n ** 127n), 2n ** 127n - 1n],
  U256: [0n, 2n ** 256n - 1n],
};

const randomBig = (r: Rng, lo: bigint, hi: bigint): bigint => {
  const roll = r.int(0, 5);
  if (roll === 0) return lo;
  if (roll === 1) return hi;
  if (roll === 2) return lo < 0n ? -1n : 1n;
  if (roll === 3) return 0n;
  let x = 0n;
  for (let idx = 0; idx < 9; idx++) x = (x << 32n) | BigInt(r.int(0, 4294967295));
  return lo + (x % (hi - lo + 1n));
};

const floats = [0, -0, 1, -1, 0.5, 1e-310, 3.4028234663852886e38, 1e300, -1e-7, Infinity, -Infinity, 123456.789];

const strings = ["", "a", "12345678", "123456789", "hello world", "ß", "日本語テキスト", "😀", "a😀b", "x".repeat(70), "é".repeat(5)];

const randomString = (r: Rng): string => {
  if (r.chance(0.5)) return r.pick(strings);
  const len = r.int(0, 20);
  let s = "";
  for (let idx = 0; idx < len; idx++) {
    const roll = r.int(0, 9);
    s += roll < 6 ? String.fromCharCode(r.int(32, 126)) : roll < 8 ? String.fromCharCode(r.int(0x80, 0x7ff)) : roll < 9 ? String.fromCharCode(r.int(0xe000, 0xfffd)) : String.fromCodePoint(r.int(0x10000, 0x10ffff));
  }
  return s;
};

export const genValue = (r: Rng, t: T, depth = 0, selves: Map<string, T> = new Map()): unknown => {
  switch (t.k) {
    case "num": {
      if (t.r === "f32" || t.r === "f64") {
        if (t.nan && r.chance(0.2)) return NaN;
        const x = r.chance(0.5) ? r.pick(floats) : (r.next() - 0.5) * 10 ** r.int(-5, 30);
        return t.r === "f32" ? Math.fround(x) : x;
      }
      let [lo, hi] = intRanges[t.r]!;
      if (t.as !== "bigint") {
        if (lo < -(2n ** 53n)) lo = -(2n ** 53n) + 1n;
        if (hi > 2n ** 53n) hi = 2n ** 53n - 1n;
        return Number(randomBig(r, lo, hi));
      }
      return randomBig(r, lo, hi);
    }
    case "bool":
      return r.chance(0.5);
    case "char": {
      const roll = r.int(0, 3);
      return roll === 0 ? "a" : roll === 1 ? "é" : roll === 2 ? "😀" : String.fromCodePoint(r.chance(0.5) ? r.int(0, 0xd7ff) : r.int(0xe000, 0x10ffff));
    }
    case "str":
      return randomString(r);
    case "bytes":
      return Uint8Array.from({ length: r.int(0, 12) }, () => r.int(0, 255));
    case "vec":
      return Array.from({ length: depth > 4 ? 0 : r.int(0, 3) }, () => genValue(r, t.i, depth + 1, selves));
    case "opt":
      return depth > 4 || r.chance(0.3) ? t.none : genValue(r, t.i, depth + 1, selves);
    case "box":
      return genValue(r, t.i, depth + 1, selves);
    case "struct": {
      if (t.rec) selves.set(t.name, t);
      const out: Record<string, unknown> = {};
      for (const [k, f] of t.f) out[k] = genValue(r, f, depth + 1, selves);
      return out;
    }
    case "tuple":
      return t.i.map((item) => genValue(r, item, depth + 1, selves));
    case "uenum":
      return r.pick(t.lits);
    case "denum": {
      const v = r.pick(t.vars);
      if (v.f === null) return v.lit;
      const out: Record<string, unknown> = { [t.tag]: v.lit };
      for (const [k, f] of v.f) out[k] = genValue(r, f, depth + 1, selves);
      return out;
    }
    case "lit":
      return t.v;
    case "self":
      return genValue(r, selves.get(t.name)!, depth + 1, selves);
  }
};

// ── dump ────────────────────────────────────────────────────────────────────

const scratch = new DataView(new ArrayBuffer(8));
const hex = (bytes: Uint8Array) => Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");

export const dump = (t: T, v: any, selves: Map<string, T> = new Map()): string => {
  switch (t.k) {
    case "num":
      if (t.r === "f32" || t.r === "f64") {
        if (v !== v) return "nan";
        if (t.r === "f32") {
          scratch.setFloat32(0, v);
          return scratch.getUint32(0).toString(16).padStart(8, "0");
        }
        scratch.setFloat64(0, v);
        return scratch.getBigUint64(0).toString(16).padStart(16, "0");
      }
      return String(v);
    case "bool":
      return v ? "t" : "f";
    case "char":
      return `c${v.codePointAt(0)}`;
    case "str":
      return `s${hex(new TextEncoder().encode(v))}`;
    case "bytes":
      return `[${Array.from(v as Uint8Array).join(",")}]`;
    case "vec":
      return `[${(v as unknown[]).map((x) => dump(t.i, x, selves)).join(",")}]`;
    case "opt":
      return v === t.none ? "N" : `S${dump(t.i, v, selves)}`;
    case "box":
      return dump(t.i, v, selves);
    case "struct":
      if (t.rec) selves.set(t.name, t);
      return `{${t.f.map(([k, f]) => dump(f, v[k], selves)).join(",")}}`;
    case "tuple":
      return `(${t.i.map((item, idx) => dump(item, v[idx], selves)).join(",")})`;
    case "uenum":
      return `E${t.lits.findIndex((lit) => Object.is(lit, v))}`;
    case "denum": {
      const idx = t.vars.findIndex((variant) => (variant.f === null ? v === variant.lit : v?.[t.tag] === variant.lit));
      const variant = t.vars[idx]!;
      return variant.f === null ? `E${idx}` : `E${idx}{${variant.f.map(([k, f]) => dump(f, v[k], selves)).join(",")}}`;
    }
    case "lit":
      return "()";
    case "self":
      return dump(selves.get(t.name)!, v, selves);
  }
};

export const describe = (t: T): string => JSON.stringify(t, (_, v) => (typeof v === "number" && v !== v ? "NaN" : v));
