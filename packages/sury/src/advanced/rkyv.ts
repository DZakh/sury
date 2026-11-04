import {
  anyOfTag,
  arrayTag,
  bigintTag,
  booleanTag,
  type Builder,
  conversionSite,
  copySchema,
  type Encoder,
  type ErrorDetails,
  initSchema,
  inlinedProperty,
  inputExpression,
  instanceTag,
  type Internal,
  nanTag,
  nullTag,
  numberTag,
  objectTag,
  panic,
  type Path,
  pathConcat,
  refTag,
  stringify,
  stringTag,
  type SuryErrorRecord,
  tagFlags,
  U,
  undefinedTag,
  type Val,
  compilePath,
} from "../base";
import {
  _var,
  B_conversionFail,
  B_embedPure,
  B_fail,
  B_let,
  B_next,
  B_nextVar,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
} from "../builder";
import { arrayFactory, definitionToSchema } from "../composites";
import { instanceDecoder } from "../parse";
import { bigint, bool, float, int, integer, nan, nullLiteral, string, unit } from "../primitives";
import { port, uint8Array } from "../refinements";
import { unionFactory } from "../union";
import { recursive } from "./recursive";

export type RkyvType =
  | "u8"
  | "i8"
  | "u16"
  | "i16"
  | "u32"
  | "i32"
  | "u64"
  | "i64"
  | "u128"
  | "i128"
  | "f32"
  | "f64"
  | "char"
  | "String"
  | "Box";

// Kinds (`k`): 0 a number, 1 a bigint, 2 bool, 3 char, 4 String, 5 Vec,
// 6 Vec<u8> as a Uint8Array, 7 Option, 8 struct or tuple, 9 enum, 10 Box,
// 11 a zero-sized literal, 12 a 64-bit integer read as a number.
type Layout = {
  k: number;
  // The Rust type, for messages and the number reads.
  t: string;
  s: number;
  a: number;
  w: Internal;
  // Vec, Option and Box: what they hold.
  i?: Layout;
  // Option: what None reads as, and where Some's payload starts.
  n?: unknown;
  o?: number;
  // Struct: [key, field, offset]. Numeric keys make a tuple.
  f?: [key: string | number, field: Layout, offset: number][];
  // Enum: the key a variant's literal sits under (unset: the variants are
  // the literals themselves), and each variant's literal and body.
  tk?: string;
  v?: [literal: unknown, body: Layout | undefined][];
  // An integer read into a narrower number format: its bounds and name.
  m?: [min: number, max: number, format: string];
  // A literal's value; 1 on a float that refuses NaN.
  c?: unknown;
  // A recursive definition, compiled as its own function.
  r?: number;
};

// [size, read into a bigint, the range a number holds].
const numbers: Record<string, [size: number, big: boolean, min?: number, max?: number]> = {
  u8: [1, false, 0, 255],
  i8: [1, false, -128, 127],
  u16: [2, false, 0, 65535],
  i16: [2, false, -32768, 32767],
  u32: [4, false, 0, 4294967295],
  i32: [4, false, -2147483648, 2147483647],
  f32: [4, false],
  f64: [8, false],
  u64: [8, true],
  i64: [8, true],
  u128: [16, true],
  i128: [16, true],
};

const alignUp = (n: number, a: number): number => (n + a - 1) & -a;

type Ctx = { defs: Map<Internal, Layout>; fns: Layout[] };

const leaf = (k: number, t: string, size: number, w: Internal): Layout => ({ k, t, s: size, a: size, w });

const literal = (c: unknown, w: Internal): Layout => ({ k: 11, t: "()", s: 0, a: 1, w, c });

// A recursive definition is sized only once its body is known, so one that
// reaches itself inline, not through a pointer, has none yet.
const sized = (layout: Layout): Layout =>
  layout.s === U ? panic(`S.rkyv: a type can't hold itself inline. Wrap the recursive field with S.rkyvType(schema, "Box")`) : layout;

const struct = (t: string, keys: (string | number)[], fields: Layout[], w: Internal, start = 0): Layout => {
  let off = start,
    a = 1;
  const f: [string | number, Layout, number][] = [];
  for (let idx = 0; idx < fields.length; idx++) {
    const field = sized(fields[idx]!);
    off = alignUp(off, field.a);
    f.push([keys[idx]!, field, off]);
    off += field.s;
    if (field.a > a) a = field.a;
  }
  return { k: 8, t, s: alignUp(off, a), a, w, f };
};

const unrepresentable = (schema: Internal): never =>
  panic(`S.rkyv can't store ${inputExpression(schema)}`);

// The wire claims the target's own number format, so a Rust integer wider than
// it is range-checked as it's read.
const numberLayout = (schema: Internal, type: string | undefined, nan?: Internal): Layout => {
  const format = schema.format;
  if (type === U) type = format === "int32" ? "i32" : format === "port" ? "u16" : format === "integer" ? "i64" : "f64";
  const known = numbers[type];
  if (known === U) return panic(`S.rkyvType ${type} can't store a number`);
  const [size, big, min = -Infinity, max = Infinity] = known;
  if (size > 8) return panic(`S.rkyvType ${type} needs S.bigint`);
  if (type[0] === "f") {
    const layout = leaf(0, type, size, nan ?? float);
    // `S.number` refuses NaN, which a float holds like any other value.
    if (nan === U) layout.c = 1;
    return layout;
  }
  const wire = format === "int32" ? int : format === "port" ? port : format === "integer" ? integer : float;
  const layout = leaf(big ? 12 : 0, type, size, wire);
  if (wire !== float && (min < wire.minimum! || max > wire.maximum!)) layout.m = [wire.minimum as number, wire.maximum as number, format!];
  return layout;
};

const layoutOf = (schema: Internal, ctx: Ctx): Layout => {
  const type = schema.rkyvType as string | undefined;
  if (type?.startsWith("Box")) {
    const inner = copySchema(schema);
    if (type === "Box") delete inner.rkyvType;
    else inner.rkyvType = type.slice(4, -1);
    const held = layoutOf(inner, ctx);
    // A union describes itself, and its unboxed copy would nest differently in
    // a union around it than the schema the value is decoded to.
    return { k: 10, t: "Box", s: 4, a: 4, w: held.w === inner ? schema : held.w, i: held };
  }
  if ("const" in schema) return literal(schema.const, schema);
  switch (schema.type) {
    case refTag: {
      const def = schema.definition!()!;
      let found = ctx.defs.get(def);
      if (found === U) {
        // Filled in once its body is known: a reference inside the body sees
        // the same object, sized only after, so holding itself inline is
        // caught as a missing size.
        found = { k: 8, t: "", s: U as unknown as number, a: 1, w: U as unknown as Internal, r: ctx.fns.length };
        ctx.fns.push(found);
        ctx.defs.set(def, found);
        const self = found;
        let body: Layout | undefined;
        const wire = recursive(schema.name || "Self", (ref) => {
          self.w = ref;
          body = layoutOf(def, ctx);
          return body.w;
        });
        Object.assign(self, body!, { r: self.r, w: wire });
      }
      return found;
    }
    case stringTag:
      if (type === "char") return leaf(3, "char", 4, string);
      if (type !== U && type !== "String") return panic(`S.rkyvType ${type} can't store a string`);
      return { k: 4, t: "String", s: 8, a: 4, w: string };
    case numberTag:
      return numberLayout(schema, type);
    case bigintTag: {
      const t = type ?? "i64";
      if (numbers[t]?.[1] !== true) return panic(`S.rkyvType ${t} can't store a bigint`);
      return leaf(1, t, numbers[t]![0], bigint);
    }
    case booleanTag:
      return leaf(2, "bool", 1, bool);
    case instanceTag:
      if (schema.class === Uint8Array) return { k: 6, t: "Vec<u8>", s: 8, a: 4, w: uint8Array };
      return unrepresentable(schema);
    case arrayTag: {
      if (typeof schema.additionalItems === objectTag) {
        const item = layoutOf(schema.additionalItems as Internal, ctx);
        if (item.s === 0) return panic("S.rkyv can't store a list of zero-sized items");
        return { k: 5, t: "Vec", s: 8, a: 4, w: arrayFactory(item.w), i: item };
      }
      const items = schema.items!.map((item) => layoutOf(item, ctx));
      return struct(
        "tuple",
        items.map((_, idx) => idx),
        items,
        definitionToSchema(items.map((item) => item.w)),
      );
    }
    case objectTag: {
      if (typeof schema.additionalItems === objectTag) return panic("S.rkyv doesn't store maps yet. Use S.array(S.tuple([key, value]))");
      const keys = Object.keys(schema.properties!);
      const fields = keys.map((key) => layoutOf(schema.properties![key]!, ctx));
      return struct("struct", keys, fields, objectWire(keys, fields));
    }
    case anyOfTag:
      return unionLayout(schema, ctx);
    case undefinedTag:
    case nullTag:
    case nanTag:
      return literal(schema.const, schema);
  }
  return unrepresentable(schema);
};

const objectWire = (keys: string[], fields: Layout[]): Internal => {
  const definition: Record<string, Internal> = {};
  for (let idx = 0; idx < keys.length; idx++) definition[keys[idx]!] = fields[idx]!.w;
  return definitionToSchema(definition);
};

const unionLayout = (schema: Internal, ctx: Ctx): Layout => {
  const members = schema.anyOf!;
  const rest: Internal[] = [];
  let hasUndefined = false,
    hasNull = false,
    hasNaN = false;
  for (const member of members) {
    if (member.type === undefinedTag) hasUndefined = true;
    else if (member.type === nullTag) hasNull = true;
    else {
      if (member.type === nanTag) hasNaN = true;
      rest.push(member);
    }
  }
  if (rest.length && (hasUndefined || hasNull)) {
    let inner = layoutOf(unionFactory(rest), ctx);
    if (hasUndefined && hasNull) inner = option(inner, null, nullLiteral);
    return option(inner, hasUndefined ? U : null, hasUndefined ? unit : nullLiteral);
  }
  // `S.number` refuses NaN, so a double that holds it is spelled as a union.
  if (hasNaN && rest.length === 2) {
    const number = rest.find((member) => member.type === numberTag && !("const" in member));
    if (number !== U) return numberLayout(number, number.rkyvType as string | undefined, schema);
  }
  if (members.length > 256) return panic("S.rkyv stores an enum of at most 256 variants");
  if (members.every((member) => "const" in member)) {
    return { k: 9, t: "enum", s: 1, a: 1, w: schema, v: members.map((member) => [member.const, U]) };
  }
  const tk = tagKeyOf(members);
  if (tk === U) {
    return panic(
      `S.rkyv can't tell the variants of ${inputExpression(schema)} apart. Give each object a literal under the same key, such as type: "circle"`,
    );
  }
  let size = 1,
    a = 1;
  const variants: [unknown, Layout | undefined][] = [];
  const wires: Internal[] = [];
  for (const member of members) {
    if ("const" in member) {
      variants.push([member.const, U]);
      wires.push(member);
      continue;
    }
    const keys: string[] = [],
      fields: Layout[] = [];
    for (const key in member.properties!) {
      if (key === tk) continue;
      keys.push(key);
      fields.push(layoutOf(member.properties[key]!, ctx));
    }
    const body = struct("struct", keys, fields, U as unknown as Internal, 1);
    const definition: Record<string, unknown> = {};
    for (const key in member.properties!) {
      definition[key] = key === tk ? member.properties[key] : fields[keys.indexOf(key)]!.w;
    }
    wires.push(definitionToSchema(definition));
    variants.push([member.properties![tk]!.const, body]);
    if (body.s > size) size = body.s;
    if (body.a > a) a = body.a;
  }
  return { k: 9, t: "enum", s: alignUp(size, a), a, w: unionFactory(wires), tk, v: variants };
};

const tagKeyOf = (members: Internal[]): string | undefined => {
  const objects = members.filter((member) => !("const" in member));
  if (objects.some((member) => member.type !== objectTag || member.properties === U)) return U;
  candidates: for (const key in objects[0]!.properties!) {
    const seen = new Set<unknown>();
    for (const member of objects) {
      const property = member.properties![key];
      if (property === U || !("const" in property) || seen.has(property.const)) continue candidates;
      seen.add(property.const);
    }
    return key;
  }
  return U;
};

const option = (inner: Layout, none: unknown, noneWire: Internal): Layout => {
  sized(inner);
  const o = alignUp(1, inner.a);
  return {
    k: 7,
    t: "Option",
    s: alignUp(o + inner.s, inner.a),
    a: inner.a,
    w: unionFactory([inner.w, noneWire]),
    i: inner,
    n: none,
    o,
  };
};

// ── failures ────────────────────────────────────────────────────────────────

const unsited = /* @__PURE__ */ conversionSite(U as never, U as never);

const isFailure = (x: unknown): x is SuryErrorRecord => x != null && Object.getPrototypeOf(x) === unsited;

const fail = (reason: string): never => {
  const record = Object.create(unsited) as SuryErrorRecord;
  record.code = "invalid_conversion";
  record.path = [];
  record.reason = reason;
  throw record;
};

// A failure inside a scope gains the scope's place in front of what it
// already says. Anything else - a getter's throw - passes through.
const prefix = (x: unknown, at: Path): unknown => {
  if (isFailure(x)) x.path = pathConcat(at, x.path);
  return x;
};

const textDecoder = /* @__PURE__ */ new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const textEncoder = /* @__PURE__ */ new TextEncoder();

// ── code generation ─────────────────────────────────────────────────────────

const literalSource = (value: unknown): string =>
  value === U
    ? "void 0"
    : typeof value === "bigint"
      ? `${value}n`
      : typeof value === "number"
        ? Object.is(value, -0)
          ? "-0"
          : `${value}`
        : JSON.stringify(value);

const literalCheck = (value: unknown, v: string): string =>
  typeof value === "number" && value !== value ? `${v}!==${v}` : `${v}===${literalSource(value)}`;

// A stretch of generated code whose failures say where they happened: `f`
// names the place the last fallible step reached, `paths` lists them.
type Scope = { paths: Path[]; f: string };

type Gen = {
  vars: number;
  decls: string[];
  tables: Path[][];
};

const newVar = (gen: Gen): string => {
  const name = "v" + gen.vars++;
  gen.decls.push(name);
  return name;
};

const mark = (scope: Scope, path: Path): string => `${scope.f}=${scope.paths.push(path) - 1};`;

const newScope = (gen: Gen): Scope => ({ paths: [], f: newVar(gen) });

const scopeCatch = (gen: Gen, scope: Scope, index?: string): string => {
  const table = gen.tables.push(scope.paths) - 1;
  const at = `T[${table}][${scope.f}]`;
  return `catch(x){throw P(x,${index === U ? at : `[${index}].concat(${at})`})}`;
};

const reads: Record<string, (p: string) => string> = {
  u8: (p) => `b[${p}]`,
  i8: (p) => `b[${p}]<<24>>24`,
  u16: (p) => `d.getUint16(${p},true)`,
  i16: (p) => `d.getInt16(${p},true)`,
  u32: (p) => `d.getUint32(${p},true)`,
  i32: (p) => `d.getInt32(${p},true)`,
  f32: (p) => `d.getFloat32(${p},true)`,
  f64: (p) => `d.getFloat64(${p},true)`,
  u64: (p) => `d.getBigUint64(${p},true)`,
  i64: (p) => `d.getBigInt64(${p},true)`,
  u128: (p) => `(d.getBigUint64(${p}+8,true)<<64n|d.getBigUint64(${p},true))`,
  i128: (p) => `(d.getBigInt64(${p}+8,true)<<64n|d.getBigUint64(${p},true))`,
};

const at = (p: string, off: number): string => (off === 0 ? p : `${p}+${off}`);

// Emits the statements reading `layout` at `p`, whose out-of-line data lies
// below `hi`, and returns the expression of its value.
const emitRead = (gen: Gen, scope: Scope, layout: Layout, p: string, hi: string, path: Path): [code: string, value: string] => {
  if (layout.r !== U) {
    const x = newVar(gen);
    return [`${mark(scope, path)}${x}=r${layout.r}(${p},${hi});`, x];
  }
  switch (layout.k) {
    case 0:
      if (layout.c === 1) {
        const x = newVar(gen);
        return [`${mark(scope, path)}${x}=${reads[layout.t]!(p)};${x}===${x}||F("Expected number, received NaN");`, x];
      }
      if (layout.m !== U) {
        const x = newVar(gen);
        return [`${mark(scope, path)}${x}=${reads[layout.t]!(p)};${bounded(layout, x)}`, x];
      }
      return ["", reads[layout.t]!(p)];
    case 1:
      return ["", reads[layout.t]!(p)];
    case 12: {
      const x = newVar(gen);
      return [
        `${mark(scope, path)}${x}=d.${layout.t === "i64" ? "getInt32" : "getUint32"}(${p}+4,true)*4294967296+d.getUint32(${p},true);Number.isSafeInteger(${x})||F("rkyv ${layout.t} "+${layout.t === "i64" ? "d.getBigInt64" : "d.getBigUint64"}(${p},true)+" doesn't fit a number");${bounded(layout, x)}`,
        x,
      ];
    }
    case 2: {
      const x = newVar(gen);
      return [`${mark(scope, path)}${x}=b[${p}];${x}>1&&F("Invalid rkyv bool "+${x});`, `${x}===1`];
    }
    case 3: {
      const x = newVar(gen);
      return [
        `${mark(scope, path)}${x}=d.getUint32(${p},true);(${x}>1114111||${x}>55295&&${x}<57344)&&F("Invalid rkyv char "+${x});`,
        `String.fromCodePoint(${x})`,
      ];
    }
    case 4: {
      const x = newVar(gen);
      return [`${mark(scope, path)}${x}=S(${p},${hi});`, x];
    }
    case 6: {
      const q = newVar(gen),
        n = newVar(gen);
      return [
        `${mark(scope, path)}${q}=${p}+d.getInt32(${p},true);${n}=d.getUint32(${p}+4,true);C(${q},${n},1,${hi});lo=${q}+${n};`,
        `b.slice(${q},${q}+${n})`,
      ];
    }
    case 5: {
      const item = layout.i!;
      const q = newVar(gen),
        n = newVar(gen),
        x = newVar(gen),
        i = newVar(gen);
      const inner = newScope(gen);
      const o = newVar(gen);
      const [code, value] = emitRead(gen, inner, item, o, q, []);
      return [
        `${mark(scope, path)}${q}=${p}+d.getInt32(${p},true);${n}=d.getUint32(${p}+4,true);C(${q},${n}*${item.s},${item.a},${hi});${x}=new Array(${n});try{for(${i}=0,${o}=${q};${i}<${n};${i}++,${o}+=${item.s}){${code}${x}[${i}]=${value}}}${scopeCatch(gen, inner, i)}lo=${q}+${n}*${item.s};`,
        x,
      ];
    }
    case 7: {
      const t = newVar(gen),
        x = newVar(gen);
      const [code, value] = emitRead(gen, scope, layout.i!, at(p, layout.o!), hi, path);
      return [
        `${mark(scope, path)}${t}=b[${p}];if(${t}===0)${x}=${literalSource(layout.n)};else if(${t}===1){${code}${x}=${value}}else F("Invalid rkyv Option tag "+${t});`,
        x,
      ];
    }
    case 8: {
      let code = "",
        fields = "";
      const tuple = layout.t === "tuple";
      for (const [key, field, off] of layout.f!) {
        const [c, value] = emitRead(gen, scope, field, at(p, off), hi, [...path, key]);
        code += c;
        fields += tuple ? `${value},` : `${JSON.stringify(key)}:${value},`;
      }
      if (layout.f!.length === 0) return [code, tuple ? "[]" : "{}"];
      const x = newVar(gen);
      return [`${code}${x}=${tuple ? `[${fields}]` : `{${fields}}`};`, x];
    }
    case 9: {
      const t = newVar(gen),
        x = newVar(gen);
      let cases = "";
      layout.v!.forEach(([lit, body], idx) => {
        if (body === U) {
          cases += `case ${idx}:${x}=${literalSource(lit)};break;`;
          return;
        }
        let code = "",
          fields = `${JSON.stringify(layout.tk)}:${literalSource(lit)},`;
        for (const [key, field, off] of body.f!) {
          const [c, value] = emitRead(gen, scope, field, at(p, off), hi, [...path, key]);
          code += c;
          fields += `${JSON.stringify(key)}:${value},`;
        }
        cases += `case ${idx}:${code}${x}={${fields}};break;`;
      });
      return [`${mark(scope, path)}${t}=b[${p}];switch(${t}){${cases}default:F("Invalid rkyv enum tag "+${t})}`, x];
    }
    case 10: {
      const held = layout.i!;
      const q = newVar(gen);
      const [code, value] = emitRead(gen, scope, held, q, q, path);
      const x = newVar(gen);
      return [
        `${mark(scope, path)}${q}=${p}+d.getInt32(${p},true);C(${q},${held.s},${held.a},${hi});${code}${x}=${value};lo=${q}+${held.s};`,
        x,
      ];
    }
  }
  return ["", literalSource(layout.c)];
};

const bounded = (layout: Layout, x: string): string => {
  if (layout.m === U) return "";
  const [min, max, format] = layout.m;
  return `${x}>=${min}&&${x}<=${max}||F("Expected ${format}, received "+${x});`;
};

const fnBody = (gen: Gen, scope: Scope, code: string, ret: string): string =>
  `${gen.decls.length ? `var ${gen.decls.join(",")};` : ""}try{${code}}${scopeCatch(gen, scope)}return ${ret}`;

const freshGen = (tables: Path[][]): Gen => ({ vars: 0, decls: [], tables });

const compileReader = (root: Layout, fns: Layout[]): ((bytes: Uint8Array) => unknown) => {
  const tables: Path[][] = [];
  let src = "";
  for (const def of fns) {
    const gen = freshGen(tables);
    const scope = newScope(gen);
    const [code, value] = emitRead(gen, scope, { ...def, r: U }, "p", "hi", []);
    src += `function r${def.r}(p,hi){${fnBody(gen, scope, code, value)}}`;
  }
  const gen = freshGen(tables);
  const scope = newScope(gen);
  const [code, value] = emitRead(gen, scope, root, "p", "p", []);
  src += `return function(i){b=i;d=new DataView(i.buffer,i.byteOffset,i.byteLength);let p=i.length-${root.s};if(p<0)F("rkyv ${root.t} needs ${root.s} bytes, received "+i.length);if(p&${root.a - 1})F("Unaligned rkyv root");lo=0;${fnBody(gen, scope, code, value)}}`;
  const factory = new Function(
    "F",
    "P",
    "T",
    "D",
    `let b,d,lo;` +
      // Claims a subtree the way rkyv's validator does: past everything
      // claimed so far and below the pointer that reaches it.
      `const C=(q,n,a,hi)=>{if(q<lo||q+n>hi)F("rkyv pointer out of bounds");if(q&(a-1))F("Unaligned rkyv pointer")};` +
      `const S=(p,hi)=>{let n=0,q=p;if((b[p]&192)!==128){while(n<8&&b[p+n]!==255)n++}else{n=d.getUint32(p,true);n=(n&63)+(n>>>8)*64;if(n<9)F("Invalid rkyv string length");q=p+d.getInt32(p+4,true);C(q,n,1,hi);lo=q+n}if(n<13){let s="",c=0;for(let j=0;j<n;j++){c=b[q+j];if(c>127)return D(b,q,n);s+=String.fromCharCode(c)}return s}return D(b,q,n)};` +
      src,
  );
  return factory(fail, prefix, tables, utf8);
};

const utf8 = (bytes: Uint8Array, p: number, n: number): string => {
  try {
    return textDecoder.decode(bytes.subarray(p, p + n));
  } catch {
    return fail("rkyv string is not valid UTF-8");
  }
};

// ── writer ──────────────────────────────────────────────────────────────────

const writes: Record<string, (p: string, v: string) => string> = {
  u8: (p, v) => `w[${p}]=${v}`,
  i8: (p, v) => `w[${p}]=${v}`,
  u16: (p, v) => `wd.setUint16(${p},${v},true)`,
  i16: (p, v) => `wd.setInt16(${p},${v},true)`,
  u32: (p, v) => `wd.setUint32(${p},${v},true)`,
  i32: (p, v) => `wd.setInt32(${p},${v},true)`,
  f32: (p, v) => `wd.setFloat32(${p},${v},true)`,
  f64: (p, v) => `wd.setFloat64(${p},${v},true)`,
  u64: (p, v) => `wd.setBigUint64(${p},${v},true)`,
  i64: (p, v) => `wd.setBigInt64(${p},${v},true)`,
  u128: (p, v) => `wd.setBigUint64(${p},BigInt.asUintN(64,${v}),true),wd.setBigUint64(${p}+8,BigInt.asUintN(64,${v}>>64n),true)`,
  i128: (p, v) => `wd.setBigUint64(${p},BigInt.asUintN(64,${v}),true),wd.setBigInt64(${p}+8,BigInt.asIntN(64,${v}>>64n),true)`,
};

const ranges: Record<string, (v: string) => string> = {
  u8: (v) => `(${v}&255)===${v}`,
  i8: (v) => `(${v}<<24>>24)===${v}`,
  u16: (v) => `(${v}&65535)===${v}`,
  i16: (v) => `(${v}<<16>>16)===${v}`,
  u32: (v) => `${v}>>>0===${v}`,
  i32: (v) => `(${v}|0)===${v}`,
  u64: (v) => `BigInt.asUintN(64,${v})===${v}`,
  i64: (v) => `BigInt.asIntN(64,${v})===${v}`,
  u128: (v) => `BigInt.asUintN(128,${v})===${v}`,
  i128: (v) => `BigInt.asIntN(128,${v})===${v}`,
};

const rangeCheck = (scope: Scope, layout: Layout, v: string, path: Path): string => {
  // A typed value already holds its format's range.
  if ((layout.w === int && layout.t === "i32") || (layout.w === port && layout.t === "u16")) return "";
  const check = layout.k === 12 ? `Number.isSafeInteger(${v})${layout.t === "u64" ? `&&${v}>=0` : ""}` : ranges[layout.t]?.(v);
  return check === U ? "" : `${mark(scope, path)}${check}||E(${v},"${layout.t}");`;
};

const bind = (gen: Gen, v: string): [code: string, name: string] => {
  if (/^v\d+$/.test(v)) return ["", v];
  const x = newVar(gen);
  return [`${x}=${v};`, x];
};

// The out-of-line data `layout` reaches, written before the value itself as
// rkyv's serializer does, and the resolvers its own write needs.
const emitDeps = (gen: Gen, scope: Scope, layout: Layout, v: string, path: Path): [code: string, resolvers: string[]] => {
  if (layout.r !== U) {
    const r = newVar(gen);
    return [`${mark(scope, path)}${r}=D${layout.r}(${v});`, [r]];
  }
  switch (layout.k) {
    case 0:
    case 1:
    case 12:
      return [rangeCheck(scope, layout, v, path), []];
    case 3:
      return [`${mark(scope, path)}H(${v});`, []];
    case 4: {
      const r = newVar(gen),
        n = newVar(gen);
      return [`${mark(scope, path)}${r}=wp;${n}=W(${v});`, [r, n]];
    }
    case 6: {
      const r = newVar(gen);
      return [`${r}=wp;G(${v}.length);w.set(${v},wp);wp+=${v}.length;`, [r]];
    }
    case 5: {
      const item = layout.i!;
      const [bound, list] = bind(gen, v);
      const i = newVar(gen),
        r = newVar(gen);
      const inner = newScope(gen);
      const [deps, itemResolvers] = emitDeps(gen, inner, item, `${list}[${i}]`, []);
      let code = bound;
      const arrays = itemResolvers.map(() => newVar(gen));
      if (deps !== "") {
        code += `${arrays.map((a) => `${a}=new Array(${list}.length);`).join("")}try{for(${i}=0;${i}<${list}.length;${i}++){${deps}${arrays
          .map((a, idx) => `${a}[${i}]=${itemResolvers[idx]};`)
          .join("")}}}${scopeCatch(gen, inner, i)}`;
      }
      const o = newVar(gen),
        e = newVar(gen);
      const resolve = emitResolve(gen, item, e, arrays.map((a) => `${a}[${i}]`), o);
      code += `A(${item.a});${r}=wp;Z(${list}.length*${item.s});`;
      if (resolve !== "") code += `for(${i}=0,${o}=${r};${i}<${list}.length;${i}++,${o}+=${item.s}){${e}=${list}[${i}];${resolve}}`;
      return [code, [r, `${list}.length`]];
    }
    case 7: {
      const [bound, x] = bind(gen, v);
      const [deps, resolvers] = emitDeps(gen, scope, layout.i!, x, path);
      return [`${bound}${deps === "" ? "" : `if(${x}!==${literalSource(layout.n)}){${deps}}`}`, [x, ...resolvers]];
    }
    case 8: {
      const [bound, x] = bind(gen, v);
      let code = bound;
      const resolvers: string[] = [];
      for (const [key, field] of layout.f!) {
        const [deps, fieldResolvers] = emitDeps(gen, scope, field, property(x, key), [...path, key]);
        code += deps;
        resolvers.push(...fieldResolvers);
      }
      return [code, resolvers];
    }
    case 9: {
      const [bound, x] = bind(gen, v);
      const t = newVar(gen);
      const resolvers: string[] = [t];
      const read = layout.tk === U ? x : property(x, layout.tk);
      let code = `${bound}${mark(scope, path)}`;
      let cases = "";
      layout.v!.forEach(([lit, body], idx) => {
        let deps = "";
        if (body !== U) {
          for (const [key, field] of body.f!) {
            const [c, fieldResolvers] = emitDeps(gen, scope, field, property(x, key), [...path, key]);
            deps += c;
            resolvers.push(...fieldResolvers);
          }
        }
        cases += `if(${literalCheck(lit, body === U ? x : read)}){${t}=${idx};${deps}}else `;
      });
      code += `${cases}E(${read},"${layout.tk === U ? "enum" : `enum tag ${layout.tk}`}");`;
      return [code, resolvers];
    }
    case 10: {
      const held = layout.i!;
      const [deps, resolvers] = emitDeps(gen, scope, held, v, path);
      const r = newVar(gen);
      return [`${deps}A(${held.a});${r}=wp;Z(${held.s});${emitResolve(gen, held, v, resolvers, r)}`, [r]];
    }
  }
  return ["", []];
};

const property = (x: string, key: string | number): string =>
  typeof key === "number" ? `${x}[${key}]` : inlinedProperty(x, key);

// Writes `layout` itself at `p`, zeroed beforehand.
const emitResolve = (gen: Gen, layout: Layout, v: string, resolvers: string[], p: string): string => {
  if (layout.r !== U) return `R${layout.r}(${v},${resolvers[0]},${p});`;
  switch (layout.k) {
    case 0:
    case 1:
      return `${writes[layout.t]!(p, v)};`;
    case 12:
      return `wd.${layout.t === "i64" ? "setBigInt64" : "setBigUint64"}(${p},BigInt(${v}),true);`;
    case 2:
      return `${v}&&(w[${p}]=1);`;
    case 3:
      return `wd.setUint32(${p},${v}.codePointAt(0),true);`;
    case 4: {
      const [r, n] = resolvers;
      return `${n}<9?I(${v},${p}):(wd.setUint32(${p},(${n}&63|128)+(${n}>>>6)*256,true),wd.setInt32(${p}+4,${r}-(${p}),true));`;
    }
    case 5:
    case 6:
      return `wd.setInt32(${p},${resolvers[0]}-(${p}),true);wd.setUint32(${p}+4,${layout.k === 6 ? `${v}.length` : resolvers[1]},true);`;
    case 7: {
      const [x, ...rest] = resolvers;
      return `if(${x}!==${literalSource(layout.n)}){w[${p}]=1;${emitResolve(gen, layout.i!, x!, rest, at(p, layout.o!))}}`;
    }
    case 8: {
      let code = "";
      let cursor = 0;
      for (const [key, field, off] of layout.f!) {
        const count = resolverCount(field);
        code += emitResolve(gen, field, property(v, key), resolvers.slice(cursor, cursor + count), at(p, off));
        cursor += count;
      }
      return code;
    }
    case 9: {
      const [t] = resolvers;
      let cursor = 1;
      let cases = "";
      layout.v!.forEach(([, body], idx) => {
        let code = "";
        if (body !== U) {
          for (const [key, field, off] of body.f!) {
            const count = resolverCount(field);
            code += emitResolve(gen, field, property(v, key), resolvers.slice(cursor, cursor + count), at(p, off));
            cursor += count;
          }
        }
        cases += `case ${idx}:${code}break;`;
      });
      return `w[${p}]=${t};${layout.v!.every(([, body]) => body === U || body.f!.length === 0) ? "" : `switch(${t}){${cases}}`}`;
    }
    case 10:
      return `wd.setInt32(${p},${resolvers[0]}-(${p}),true);`;
  }
  return "";
};

const resolverCount = (layout: Layout): number => {
  if (layout.r !== U) return 1;
  switch (layout.k) {
    case 4:
      return 2;
    case 5:
      return 2;
    case 6:
    case 10:
      return 1;
    case 7:
      return 1 + resolverCount(layout.i!);
    case 8:
      return layout.f!.reduce((sum, [, field]) => sum + resolverCount(field), 0);
    case 9:
      return 1 + layout.v!.reduce((sum, [, body]) => sum + (body === U ? 0 : resolverCount(body)), 0);
  }
  return 0;
};

const compileWriter = (root: Layout, fns: Layout[]): ((value: unknown) => Uint8Array) => {
  const tables: Path[][] = [];
  let src = "";
  for (const def of fns) {
    const body = { ...def, r: U };
    let gen = freshGen(tables);
    let scope = newScope(gen);
    const [deps, resolvers] = emitDeps(gen, scope, body, "v", []);
    src += `function D${def.r}(v){${fnBody(gen, scope, deps, `[${resolvers.join(",")}]`)}}`;
    gen = freshGen(tables);
    const names = resolvers.map(() => newVar(gen));
    const resolve = emitResolve(gen, body, "v", names, "p");
    src += `function R${def.r}(v,rs,p){${names.length ? `let [${names.join(",")}]=rs;` : ""}${resolve}}`;
  }
  const gen = freshGen(tables);
  const scope = newScope(gen);
  const [deps, resolvers] = emitDeps(gen, scope, root, "v", []);
  const r = newVar(gen);
  const resolve = emitResolve(gen, root, "v", resolvers, r);
  src += `return function(v){wp=0;${fnBody(gen, scope, `${deps}A(${root.a});${r}=wp;Z(${root.s});${resolve}`, "w.slice(0,wp)")}}`;
  const factory = new Function(
    "E",
    "P",
    "T",
    "N",
    `let w=new Uint8Array(256),wd=new DataView(w.buffer),wp=0;` +
      `const G=(n)=>{if(wp+n>w.length){let x=new Uint8Array(Math.max(w.length*2,wp+n));x.set(w);w=x;wd=new DataView(w.buffer)}};` +
      `const A=(a)=>{G(a);while(wp&(a-1))w[wp++]=0};` +
      `const Z=(n)=>{G(n);w.fill(0,wp,wp+n);wp+=n};` +
      // Writes a string's UTF-8 at the cursor, keeping it there only when it
      // is too long to sit inline. Past a dozen characters TextEncoder
      // outruns a loop of charCodeAt.
      `const W=(s)=>{let l=s.length,n=0;G(l*3);if(l>12)n=N(s,w,wp);else for(;n<l;n++){let c=s.charCodeAt(n);if(c>127){n=N(s,w,wp);break}w[wp+n]=c}if(n>1073741823)E(s,"String");if(n>8)wp+=n;return n};` +
      `const I=(s,p)=>{w.fill(255,p,p+8);let l=s.length;for(let j=0;j<l;j++){let c=s.charCodeAt(j);if(c>127){N(s,w,p);return}w[p+j]=c}};` +
      `const H=(s)=>{let c=s.codePointAt(0);s.length===(c>65535?2:1)&&(c<55296||c>57343)||E(s,"char")};` +
      src,
  );
  // A getter that encodes again would write over the buffer the outer call
  // is using, so a call that finds it taken builds its own.
  let free: ((value: unknown) => Uint8Array) | undefined;
  const make = () => factory(invalid, prefix, tables, encodeInto);
  return (value) => {
    const write = free ?? make();
    free = U;
    try {
      return write(value);
    } finally {
      free = write;
    }
  };
};

const encodeInto = (s: string, w: Uint8Array, p: number): number =>
  textEncoder.encodeInto(s, w.subarray(p)).written;

const invalid = (value: unknown, type: string): never => fail(`Expected ${type}, received ${stringify(value)}`);

// ── schema glue ─────────────────────────────────────────────────────────────

type Compiled = { layout: Layout; fns: Layout[]; read?: (bytes: Uint8Array) => unknown; write?: (value: unknown) => Uint8Array };

const compiled = /* @__PURE__ */ new WeakMap<Internal, Compiled>();

const compiledOf = (schema: Internal): Compiled => {
  let entry = compiled.get(schema);
  if (entry === U) {
    const ctx: Ctx = { defs: new Map(), fns: [] };
    entry = { layout: layoutOf(schema, ctx), fns: ctx.fns };
    compiled.set(schema, entry);
  }
  return entry;
};

const guard = (input: Val, output: Val, target: Internal, call: string): string => {
  const foreign = B_conversionFail(input, target);
  let site: object | undefined;
  const failure = B_fail(
    output,
    (x: unknown, path?: Path) => {
      if (!isFailure(x)) return foreign(x, path);
      Object.setPrototypeOf(x, (site ??= conversionSite(input.s, target)));
      x.path = pathConcat(path ?? compilePath(input.path), x.path);
      return x as unknown as ErrorDetails;
    },
    "x",
  );
  return `${B_let(input.g, output.i)}try{${output.i}=${call}}catch(x){${failure}}`;
};

// Bytes to the target: the reader hands back the exact values the archive
// holds, typed by the layout's wire schema, and the parse loop converts them
// to the target like any typed value - so only what the target adds on top
// (a refinement, a transform) is emitted.
const rkyvEncoder: Encoder = (input, target) => {
  if (tagFlags[target.type]! & 8192 && target.rkyvType === U) return input;
  const entry = compiledOf(target);
  const read = (entry.read ??= compileReader(entry.layout, entry.fns));
  const output = B_nextVar(input, entry.layout.w, target);
  output.cp = guard(input, output, target, `${B_embedPure(input, read)}(${input.v()})`);
  return output;
};

const rkyvDecoder: Builder = (input) => {
  const sourceTagFlag = tagFlags[input.s.type]!;
  if (sourceTagFlag & 1) return instanceDecoder(input);
  // Bytes are an archive already, unless S.rkyvType says what they are.
  if (sourceTagFlag & 8192 && input.s.rkyvType === U)
    return input.s.class === Uint8Array || (input.s.class as Function).prototype instanceof Uint8Array
      ? B_refine(input, input.e)
      : B_unsupportedDecode(input, input.s, input.e);
  const entry = compiledOf(input.s);
  const write = (entry.write ??= compileWriter(entry.layout, entry.fns));
  const output = B_next(input, B_varWithoutAllocation(input.g), input.e, input.e);
  output.v = _var;
  output.cp = guard(input, output, input.e, `${B_embedPure(input, write)}(${input.v()})`);
  output.io = true;
  return output;
};

export const rkyv: Internal = /* @__PURE__ */ initSchema(instanceTag, rkyvDecoder, (schema) => {
  schema.class = Uint8Array;
  schema.encoder = rkyvEncoder;
  schema.flags = schema.flags | 65536;
});

// @__NO_SIDE_EFFECTS__
export const rkyvType = (schema: Internal, type: RkyvType): Internal => {
  if (!(type in numbers || type === "char" || type === "String" || type === "Box")) return panic(`S.rkyvType doesn't know the Rust type ${JSON.stringify(type)}`);
  const mut = copySchema(schema);
  // Boxing keeps the type it boxes: `Box<u64>`, spelled as Rust does.
  mut.rkyvType = type === "Box" && schema.rkyvType !== U ? `Box<${schema.rkyvType}>` : type;
  return mut;
};

