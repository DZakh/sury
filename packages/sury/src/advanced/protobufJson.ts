import { type Internal, numberTag, objectTag, stringify, U, unknown, unknownTag, type Val } from "../base";
import {
  _var,
  B_embed,
  B_embedPure,
  B_hoistDecl,
  B_let,
  B_merge,
  B_next,
  B_operationArg,
  B_refine,
  B_scope,
  B_varWithoutAllocation,
} from "../builder";
import { dictFactory, objectDecoder } from "../composites";
import { getOutputSchema } from "../parse";
import { stdCodec } from "../refinements";
import { json } from "./json";
import {
  bounds,
  compileMessage,
  type Encoding,
  emitDefault,
  encodeFrame,
  encodeHelpers,
  fail,
  type Field,
  fieldFailure,
  fieldLive,
  fieldValsOf,
  firstObject,
  guarded,
  isStruct,
  jsonFieldName,
  type Message,
  nameMessages,
  newCtx,
  objectSchemaOf,
  oneofGuards,
  printedName,
  protobufEnumOf,
  type Read,
  valueOf,
  wireFrame,
  writesOf,
} from "./protobuf";
import type { ProtobufType, WellKnownType } from "./protobufField";

// ProtoJSON (https://protobuf.dev/programming-guides/json) is another wire for
// the message `S.protobuf` compiles: the same fields, oneofs, Any members and
// well-known types, each read into the locals the binary reader fills and
// built into the value by the same `valueOf`, and written from the same
// `writesOf` reads. Only what a field's value looks like on the wire differs.

const bad = (type: string, value: unknown): never => fail(`Expected ${type}, received ${stringify(value)}`);

// A number may arrive as a JSON string too, in decimal: not padded with
// blanks, and not the hex `Number` would also read.
const numeric = (value: unknown): unknown =>
  typeof value === "string" && /^-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?$/.test(value) ? Number(value) : value;

const int32 = (value: unknown, min: number, max: number, type: string): number => {
  const n = numeric(value);
  return typeof n === "number" && Number.isInteger(n) && n >= min && n <= max ? n + 0 : bad(type, value);
};

const int64 = (value: unknown, min: bigint, max: bigint, type: string): bigint => {
  if (typeof value === "number" ? Number.isInteger(value) : typeof value === "string" && /^-?\d+$/.test(value)) {
    const n = BigInt(value as number);
    if (n >= min && n <= max) return n;
  }
  return bad(type, value);
};

// `finite` is the failure a NaN is to a number schema that refuses one.
const float = (value: unknown, f32: number, finite?: string): number => {
  let n = value === "NaN" ? NaN : value === "Infinity" ? Infinity : value === "-Infinity" ? -Infinity : U;
  if (n === U) {
    const parsed = numeric(value);
    n = typeof parsed === "number" && Number.isFinite(parsed) ? parsed : bad(f32 ? "float" : "double", value);
  }
  if (f32 && Math.abs(n) > 3.4028234663852886e38 && Number.isFinite(n)) bad("float", value);
  if (n !== n && finite !== U) fail(finite);
  return f32 ? Math.fround(n) : n;
};

const bool = (value: unknown): boolean => (value === true || value === false ? value : bad("bool", value));

const string = (value: unknown): string =>
  typeof value === "string" && (value as { isWellFormed?: () => boolean }).isWellFormed?.() !== false ? value : bad("string", value);

// Standard or URL-safe, padded or not.
const bytes = (value: unknown): Uint8Array => {
  const text = typeof value === "string" && /^[A-Za-z0-9+/_-]*=*$/.test(value) ? value.replace(/=+$/, "").replace(/-/g, "+").replace(/_/g, "/") : U;
  if (text === U || text.length % 4 === 1) return bad("bytes", value);
  return stdCodec.toBytes(text + "===".slice((text.length + 3) % 4));
};

// An enum by number, or by name where the schema names its values. An unknown
// name is a value the reader skips (`undefined`), unless `strict`.
const enumValue = (value: unknown, names: Record<string, number> | undefined, strict: number): number | undefined =>
  typeof value === "number" ? int32(value, -2147483648, 2147483647, "enum")
  : typeof value === "string" && names !== U && Object.hasOwn(names, value) ? names[value]
  : typeof value !== "string" || strict ? bad("enum", value)
  : U;

const isObject = (value: unknown): Record<string, unknown> => (isStruct(value) ? value : bad("object", value));

const isArray = (value: unknown): unknown[] => (Array.isArray(value) ? value : bad("array", value));

// A map key arrives as a property name, and lands as the one its type
// prints: `"01"` and `"1"` are one int32 key.
const mapKey = (key: string, type: ProtobufType): string => {
  if (type === "string") return key;
  if (type === "bool") return key === "true" || key === "false" ? key : bad("bool key", key);
  const [min, max] = bounds(type);
  return "" + (type.includes("64") ? int64(key, BigInt(min.slice(0, -1)), BigInt(max.slice(0, -1)), `${type} key`) : int32(key, +min, +max, `${type} key`));
};

const twice = (what: string): never => fail(`ProtoJSON ${what} is set more than once`);

const unknownKey = (key: string): never => fail(`Unrecognized ProtoJSON field "${key}"`);

// JSON as `S.json` holds it: no `undefined`, no non-finite number.
const jsonValue = (value: unknown, d: number): unknown => {
  if (d >= 100) fail("Protobuf message nesting limit exceeded");
  if (value === null || typeof value === "string" || typeof value === "boolean") return value;
  if (typeof value === "number") return Number.isFinite(value) ? value : bad("JSON", value);
  if (Array.isArray(value)) {
    for (let idx = 0; idx < value.length; idx++) jsonValue(value[idx], d + 1);
    return value;
  }
  if (isStruct(value)) {
    for (const key in value) jsonValue(value[key], d + 1);
    return value;
  }
  return bad("JSON", value);
};

const timestampMin = -62135596800;
const timestampMax = 253402300799;
const durationMax = 315576000000;

// RFC 3339 in UTC or with an offset, to the nanosecond.
const timestampOf = (value: unknown): [seconds: number, nanos: number] => {
  const match = typeof value === "string" ? /^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})(?:\.(\d{1,9}))?(Z|[+-]\d{2}:\d{2})$/.exec(value) : null;
  const ms = match === null ? NaN : Date.parse(match[1]! + match[3]);
  const seconds = ms / 1000;
  if (!(seconds >= timestampMin && seconds <= timestampMax)) return bad("Timestamp", value);
  return [seconds, match![2] === U ? 0 : +match![2].padEnd(9, "0")];
};

const durationOf = (value: unknown): [seconds: number, nanos: number] => {
  const match = typeof value === "string" ? /^(-)?(\d+)(?:\.(\d{1,9}))?s$/.exec(value) : null;
  const seconds = match === null ? NaN : +match[2]!;
  if (!(seconds <= durationMax)) return bad("Duration", value);
  const nanos = match![3] === U ? 0 : +match![3].padEnd(9, "0");
  return match![1] ? [-seconds, -nanos] : [seconds, nanos];
};

// Nanos print in groups of three digits, as many as they need.
const fraction = (nanos: number): string => {
  if (nanos === 0) return "";
  const digits = `${nanos + 1e9}`.slice(1);
  return "." + (digits.endsWith("000000") ? digits.slice(0, 3) : digits.endsWith("000") ? digits.slice(0, 6) : digits);
};

const timestampText = (seconds: unknown, nanos: unknown): string => {
  const s = Number(seconds);
  if (!(s >= timestampMin && s <= timestampMax) || !Number.isInteger(nanos) || (nanos as number) < 0 || (nanos as number) > 999999999) {
    return fail(`Expected Timestamp, received ${s}s ${stringify(nanos)}ns`);
  }
  return new Date(s * 1000).toISOString().slice(0, 19) + fraction(nanos as number) + "Z";
};

const durationText = (seconds: unknown, nanos: unknown): string => {
  const s = Number(seconds);
  const n = nanos as number;
  if (!(Math.abs(s) <= durationMax) || !Number.isInteger(n) || Math.abs(n) > 999999999 || (s > 0 && n < 0) || (s < 0 && n > 0)) {
    return fail(`Expected Duration, received ${s}s ${stringify(nanos)}ns`);
  }
  return (s === 0 && n < 0 ? "-" : "") + s + fraction(Math.abs(n)) + "s";
};

const dateText = (value: unknown): string => {
  const ms = value instanceof Date ? value.getTime() : NaN;
  const seconds = Math.floor(ms / 1000);
  if (!(seconds >= timestampMin && seconds <= timestampMax)) {
    return fail(`Expected Timestamp, received ${value instanceof Date ? (ms === ms ? value.toISOString() : "invalid Date") : stringify(value)}`);
  }
  return timestampText(seconds, (ms - seconds * 1000) * 1e6);
};

const readDate = (value: unknown): Date => {
  if (value === U) return new Date(0);
  const [seconds, nanos] = timestampOf(value);
  return new Date(seconds * 1000 + Math.trunc(nanos / 1e6));
};

// A FieldMask's paths print lowerCamelCase, which has to read back the same.
const snake = (path: string): string => path.replace(/[A-Z]/g, (letter) => "_" + letter.toLowerCase());

const maskText = (paths: unknown): string =>
  (paths as unknown[])
    .map((path) => {
      const camel = typeof path === "string" ? jsonFieldName(path) : U;
      return camel !== U && snake(camel) === path ? camel : fail(`Expected FieldMask path, received ${stringify(path)}`);
    })
    .join(",");

const readMask = (value: unknown): string[] => {
  if (value === U || value === "") return [];
  if (typeof value !== "string") return bad("FieldMask", value);
  return value.split(",").map((path) => (/_/.test(path) ? bad("FieldMask path", path) : snake(path)));
};

const bytesText = (value: unknown): string => (value instanceof Uint8Array ? stdCodec.fromBytes(value) : fieldFailure(value));

const stringText = (value: unknown): string => (typeof value === "string" ? value : fieldFailure(value));

const floatText = (value: unknown, f32?: number): number | string => {
  if (typeof value !== "number") return fieldFailure(value);
  const n = f32 ? Math.fround(value) : value;
  return Number.isFinite(n) ? n : n !== n ? "NaN" : n > 0 ? "Infinity" : "-Infinity";
};

// Reading nothing is the default instance, as it is on the binary wire.
const codecs: Partial<Record<WellKnownType, [read: (value: unknown, d: number) => unknown, write: (value: unknown) => unknown]>> = {
  "google.protobuf.Timestamp": [readDate, dateText],
  "google.protobuf.Value": [(value, d) => (value === U ? null : jsonValue(value, d)), (value) => jsonValue(value, 0)],
  "google.protobuf.Struct": [(value, d) => (value === U ? {} : jsonValue(isObject(value), d)), (value) => jsonValue(isObject(value), 0)],
  "google.protobuf.ListValue": [(value, d) => (value === U ? [] : jsonValue(isArray(value), d)), (value) => jsonValue(isArray(value), 0)],
};

const wrapperZero: Record<string, unknown> = {
  "google.protobuf.DoubleValue": 0,
  "google.protobuf.FloatValue": 0,
  "google.protobuf.Int64Value": "0",
  "google.protobuf.UInt64Value": "0",
  "google.protobuf.Int32Value": 0,
  "google.protobuf.UInt32Value": 0,
  "google.protobuf.BoolValue": false,
  "google.protobuf.StringValue": "",
  "google.protobuf.BytesValue": "",
};

type Form = [toFields: (value: unknown) => unknown, fromFields: (fields: Record<string, unknown>) => unknown];

const secondsNanosForm = (parse: (value: unknown) => [number, number], print: (seconds: unknown, nanos: unknown) => string): Form => [
  (value) => {
    if (value === U) return U;
    const [seconds, nanos] = parse(value);
    return { seconds, nanos };
  },
  (fields) => print(fields.seconds ?? 0, fields.nanos ?? 0),
];

const valueOfFields = (fields: Record<string, unknown>): unknown => {
  const keys = Object.keys(fields);
  if (keys.length !== 1) fail("Expected google.protobuf.Value to hold a value");
  const value = fields[keys[0]!];
  return typeof value === "string" && keys[0] === "numberValue" ? bad("JSON", value) : value;
};

// The well-known messages with a ProtoJSON of their own, when held as the
// message protobuf-es spells (sury/protobuf/wkt): read as the object of their
// fields that JSON stands for, and printed from it.
const forms: Record<string, Form> = /* @__PURE__ */ (() => {
  const table: Record<string, Form> = {
  "google.protobuf.Timestamp": secondsNanosForm(timestampOf, timestampText),
  "google.protobuf.Duration": secondsNanosForm(durationOf, durationText),
  "google.protobuf.FieldMask": [(value) => ({ paths: readMask(value) }), (fields) => maskText(fields.paths ?? [])],
  "google.protobuf.Struct": [(value) => (value === U ? U : { fields: isObject(value) }), (fields) => fields.fields ?? {}],
  "google.protobuf.ListValue": [(value) => (value === U ? U : { values: isArray(value) }), (fields) => fields.values ?? []],
  "google.protobuf.Value": [
    (value) =>
      value === U ? U
      : value === null ? { nullValue: null }
      : typeof value === "number" ? { numberValue: value }
      : typeof value === "string" ? { stringValue: value }
      : typeof value === "boolean" ? { boolValue: value }
      : Array.isArray(value) ? { listValue: value }
      : { structValue: isObject(value) },
    valueOfFields,
  ],
  };
  for (const name in wrapperZero) table[name] = [(value) => (value === U ? U : { value }), (fields) => ("value" in fields ? fields.value : wrapperZero[name])];
  return table;
})();

const formOf = (msg: Message): Form | undefined =>
  msg.codec !== U || msg.unwrap ? U : forms[(msg.wellKnown ?? msg.typeName)!];

const anyTypeName = "google.protobuf.Any";

// A `google.protobuf.Any` kept as its type URL and bytes: its ProtoJSON is the
// message it holds, which only a schema for that message can print or read.
const opaqueAny = (what: string) => fail(`ProtoJSON can't ${what} a google.protobuf.Any without the schema of the message it holds. Convert it with S.to to a message S.protobufTypeName names`);

const readOpaqueAny = (value: unknown): unknown => {
  if (value !== U && Object.keys(isObject(value)).length) opaqueAny("read");
  return { typeUrl: "", value: new Uint8Array(0) };
};

const writeOpaqueAny = (value: unknown): unknown =>
  (value as { typeUrl?: unknown }).typeUrl === "" && !(value as { value: Uint8Array }).value.length ? {} : opaqueAny("print");

const isOpaqueAny = (msg: Message): boolean => msg.typeName === anyTypeName && msg.any === U;

// A message that prints as something other than an object of its fields.
const custom = (msg: Message): boolean => (msg.wellKnown !== U && msg.wellKnown !== "google.protobuf.Empty") || formOf(msg) !== U;

const nullValueNames = { NULL_VALUE: 0 };

const isNullValue = (schema: Internal | undefined): boolean => findIn(schema, (s) => s.protobufTypeName === "google.protobuf.NullValue") !== U;

const findIn = (schema: Internal | undefined, test: (schema: Internal) => boolean, seen = new Set<Internal>()): Internal | undefined => {
  for (let current = schema; current !== U && !seen.has(current); current = current.to) {
    seen.add(current);
    if (test(current)) return current;
    const output = getOutputSchema(current);
    if (output !== current && test(output)) return output;
    for (const member of output.anyOf ?? []) {
      const found = findIn(member, test, seen);
      if (found !== U) return found;
    }
    if (typeof output.additionalItems === objectTag) {
      const found = findIn(output.additionalItems as Internal, test, seen);
      if (found !== U) return found;
    }
  }
  return U;
};

const enumNames = (field: Field): Record<string, number> | undefined => protobufEnumOf(field.armValue ?? field.held);

// `null` is a value only to a Value, and to the NullValue enum.
const nullIsValue = (field: Field): boolean =>
  (field.message?.wellKnown ?? field.message?.typeName) === "google.protobuf.Value" || (field.type === "enum" && isNullValue(field.armValue ?? field.held));

const property = (name: string): string => (/^[A-Za-z_$][\w$]*$/.test(name) ? `.${name}` : `[${JSON.stringify(name)}]`);

const jsonNames = (field: Field): string[] => {
  const printed = printedName(field);
  const json = jsonFieldName(printed);
  return json === printed ? [json] : [json, printed];
};

const decodeHelpers = {
  $i: int32,
  $l: int64,
  $f: float,
  $o: bool,
  $s: string,
  $b: bytes,
  $e: enumValue,
  $j: isObject,
  $a: isArray,
  $k: mapKey,
  $t: twice,
  $u: unknownKey,
  $m: readMask,
};
type DecodeHelper = keyof typeof decodeHelpers;

// The expression reading JSON `v` as a scalar of the field's type, into what
// the binary reader would have read: `undefined` for an enum name skipped.
const readScalar = (field: Field, type: ProtobufType, v: string, h: (helper: DecodeHelper) => string, nan: boolean, strict: number, emb: (value: unknown) => string): string => {
  if (type === "bool") return `${h("$o")}(${v})`;
  if (type === "string") return `${h("$s")}(${v})`;
  if (type === "bytes") return `${h("$b")}(${v})`;
  if (type === "double" || type === "float") return `${h("$f")}(${v},${type === "float" ? 1 : 0}${nan && field.finite !== U ? `,${JSON.stringify(field.finite)}` : ""})`;
  if (type === "enum") {
    if (isNullValue(field.armValue ?? field.held)) return `${v}===null?0:${h("$e")}(${v},${emb(nullValueNames)},1)`;
    const enumNamesOf = enumNames(field);
    return `${h("$e")}(${v},${enumNamesOf === U ? "void 0" : emb(enumNamesOf)},${strict})`;
  }
  const [min, max] = bounds(type);
  return type.includes("64") ? `${h("$l")}(${v},${min},${max},"${type}")` : `${h("$i")}(${v},${min},${max},"${type}")`;
};

// The read loop of a message: each property its field's name or JSON name
// reads into the local the binary reader would fill, so `valueOf` builds the
// value as it does from bytes. `t` is the tag of the field being read, which
// is what frames a failure (`wireFrame`), with `y` a map's key.
const readParts = (msg: Message, local: (kind: string, idx: number) => string, fn: (msg: Message) => string, nan: boolean, h: (helper: DecodeHelper) => string, emb: (value: unknown) => string) => {
  const fields = msg.fields;
  const strict = msg.object.additionalItems === "strict" ? 1 : 0;
  const locals: [name: string, init: string][] = [];
  const temps: string[] = [];
  const cases: string[] = [];
  const sets = new Map<string, string>();
  const oneofs = new Map<string, string>();
  const seen = new Map<string, Field>();
  let after = "";
  for (let idx = 0; idx < fields.length; idx++) {
    const field = fields[idx]!;
    const value = local("f", idx);
    const keys = jsonNames(field);
    for (const key of keys) {
      const other = seen.get(key);
      if (other !== U && other !== field) fail(`ProtoJSON reads "${key}" as two fields, ${printedName(other)} and ${printedName(field)}`);
      seen.set(key, field);
    }
    let head = `${keys.map((key) => `case ${JSON.stringify(key)}:`).join("")}t=${field.number * 8 + field.wire};v=j[k];`;
    if (keys.length > 1) {
      temps.push(`s${idx}=0`);
      head += `if(s${idx})${h("$t")}(${JSON.stringify(`field "${keys[0]}"`)});s${idx}=1;`;
    }
    const nulls = nullIsValue(field);
    const read = (v: string): string =>
      field.type === "message" ? `${fn(field.message!)}(${v},d+1)` : readScalar(field, field.type, v, h, nan, strict, emb);
    // Only an enum's reader can answer `undefined`: a name it skips.
    const skips = field.type === "enum" && !strict && !isNullValue(field.armValue ?? field.held);
    const present = (v: string, set: string): string =>
      skips ? `if((${v}=${read(v)})!==void 0){${set}}` : `${v}=${read(v)};${set};`;
    const unlessNull = (body: string): string => (nulls ? body : `if(v!==null){${body}}`);
    if (field.name !== U || field.oneof !== U) {
      // One member per oneof: a second one set is a message no reader agrees on.
      let set: string;
      let mark: string;
      if (field.name !== U) {
        set = sets.get(field.key)!;
        if (set === U) {
          sets.set(field.key, (set = local("c", idx)));
          locals.push([set, "0"]);
        }
        mark = `${set}=${field.number}`;
      } else {
        set = oneofs.get(field.oneof!)!;
        if (set === U) {
          oneofs.set(field.oneof!, (set = `o${idx}`));
          temps.push(`${set}=0`);
        }
        mark = `${set}=1`;
      }
      locals.push([value, field.name !== U ? "void 0" : emitDefault(field)]);
      const oneof = JSON.stringify(`oneof "${field.oneof ?? field.key}"`);
      cases.push(`${head}${unlessNull(present("v", `if(${set})${h("$t")}(${oneof});${mark};${value}=v`))}continue;`);
      continue;
    }
    locals.push([value, emitDefault(field)]);
    const item = (v: string, push: (value: string) => string, what: string): string =>
      (nulls ? "" : `if(${v}===null)fail("ProtoJSON ${what} is null");`) + present(v, push(v));
    if (field.map !== U) {
      const store = field.map === "string"
        ? `y==="__proto__"?Object.defineProperty(${value},y,{value:c,enumerable:!0,writable:!0,configurable:!0}):${value}[y]=c`
        : `if(Object.hasOwn(${value},y))${h("$t")}("map key "+y);${value}[y]=c`;
      cases.push(`${head}if(v!==null){${h("$j")}(v);for(p in v){y=void 0;c=v[p];y=${field.map === "string" ? "p" : `${h("$k")}(p,"${field.map}")`};${item("c", () => store, "map value")}}}continue;`);
    } else if (field.repeated) {
      cases.push(`${head}if(v!==null){${h("$a")}(v);for(n=0;n<v.length;n++){c=v[n];${item("c", (c) => `${value}.push(${c})`, "list item")}}}continue;`);
    } else {
      cases.push(`${head}${unlessNull(present("v", `${value}=v`))}continue;`);
      // A required message read from nothing is its default instance.
      if (field.type === "message" && !field.optional) after += `if(${value}===void 0){t=${field.number * 8 + 2};${value}=${fn(field.message!)}(void 0,d+1)}`;
    }
  }
  const lists = fields.flatMap((field, idx) => (field.repeated ? [`,${local("f", idx)}`] : [])).join("");
  const frame = lists || fields.some((field) => field.map !== U) ? `,y${lists}` : "";
  // An Any's member is read from the Any's own object, beside its "@type".
  const miss = strict ? `t=-1;x&&k==="@type"||${h("$u")}(k)` : "";
  const loop = `for(k in j){switch(k){${cases.join("")}}${miss}}${after}`;
  return { locals, temps, frame, loop };
};

// An Any's "@type" names the member its value is: the rest of the object, or
// its "value" for a message printed as something other than an object.
const anyParts = (msg: Message, fns: Map<Message, string>) => {
  const { fields, listed, other } = msg.any!;
  const expected = listed.map((field) => field.name).join(" or ");
  const read = (field: Field): string => {
    const idx = msg.fields.indexOf(field);
    return `t=${field.number * 8 + 2};c2=${field.number};f${idx}=${fns.get(field.message!)}(${field === other ? "void 0" : custom(field.message!) ? "j.value" : "j"},d+1,1)`;
  };
  const pick =
    listed.map((field) => `if(n===${JSON.stringify(field.name)}){${read(field)}}else `).join("") +
    `if(n===""${other !== U ? `){${read(other)}}else ` : "&&0)"}fail(n?${JSON.stringify("Protobuf Any holds ")}+n+${JSON.stringify(`, expected ${expected}`)}:${JSON.stringify(`Protobuf Any has no type URL, expected ${expected}`)})`;
  return {
    locals: [["c2", "0"], ...fields.map((_, idx) => [`f${idx + 2}`, "void 0"] as [string, string])] as [string, string][],
    temps: ['n=""'],
    frame: "",
    loop: `if(j!==void 0){$j(j);n=j["@type"];if(n===void 0&&Object.keys(j).length===0)n="";else if(typeof n!=="string"||!n)fail('Expected ProtoJSON Any to name its type in "@type"');n=n.slice(n.lastIndexOf("/")+1)}${pick}`,
  };
};

// Every message's reader but the root's, which reads in the operation unless a
// field under it refers back to it.
const compileDecoder = (root: Message, fns: Map<Message, string>, flag: number): Record<string, Function> => {
  let src = "";
  const messages: Message[] = [];
  const g = B_operationArg(unknown, unknown, flag).g;
  const params = ["at", "fail", "M", "e", ...Object.keys(decodeHelpers)];
  const args: unknown[] = [wireFrame, fail, messages, g.e, ...Object.values(decodeHelpers)];
  const embed = (value: unknown): string => {
    params.push(`K${params.length}`);
    args.push(value);
    return params[params.length - 1]!;
  };
  const names: string[] = [];
  const readers = new Map<Message, string>();
  const local = (kind: string, idx: number) => kind + idx;
  const own = (msg: Message): boolean => msg.codec === U && !msg.unwrap && !isOpaqueAny(msg);
  for (let changed = true; changed; ) {
    changed = false;
    fns.forEach((_, msg) => {
      if ((msg === root && root.recursive === U) || !own(msg)) return;
      const base = B_operationArg(unknown, unknown, flag);
      base.g = g;
      const value = valueOf(base, msg, local);
      readers.set(msg, `${B_merge(value)}return ${value.i}`);
      if (value.f & 1 && !msg.async) changed = msg.async = true;
    });
  }
  fns.forEach((name, msg) => {
    if (msg === root && root.recursive === U) return;
    names.push(name);
    if (msg.codec !== U || isOpaqueAny(msg)) {
      params.push(name);
      args.push(msg.codec !== U ? codecs[msg.wellKnown!]![0] : readOpaqueAny);
      return;
    }
    const slot = messages.push(msg) - 1;
    if (msg.unwrap) {
      const field = msg.fields[0]!;
      const read = msg.wellKnown === "google.protobuf.FieldMask" ? "$m(j)" : `j===void 0?${emitDefault({ ...field, optional: false })}:${readScalar(field, field.type, "j", (helper) => helper, !(flag & 2), 1, embed)}`;
      src += `function ${name}(j,d){try{return ${read}}catch(x){throw at(x,M[${slot}],${field.number * 8 + field.wire})}}`;
      return;
    }
    const parts = msg.any !== U ? anyParts(msg, fns) : readParts(msg, local, (m) => fns.get(m)!, !(flag & 2), (helper) => helper, embed);
    const vars = [...parts.locals.map(([l, init]) => `${l}=${init}`), ...parts.temps, "t=-1,k,v,c,n,p,y"];
    const form = formOf(msg);
    const check = msg.any !== U ? "" : `${form !== U ? `j=${embed(form[0])}(j);` : ""}if(j!==void 0)$j(j);`;
    src += `function ${name}(j,d,x){if(d>=100)fail("Protobuf message nesting limit exceeded");var ${vars};try{${check}${parts.loop}}catch(x){throw at(x,M[${slot}],t${parts.frame})}${readers.get(msg)}}`;
  });
  return new Function(...params, `${src}return {${names.join(",")}}`)(...args);
};

const encodeJsonHelpers = /* @__PURE__ */ Object.assign({ b64: bytesText, str: stringText, flt: floatText, mask: maskText }, encodeHelpers);
type JsonHelper = keyof typeof encodeJsonHelpers;

// The JSON of a scalar `v` of the field's type, checked as the binary writer
// checks it. `v` is read once.
const scalarJson = (field: Field, type: ProtobufType, v: string, enc: (helper: JsonHelper) => string, emb: (value: unknown) => string, known = 0): string => {
  if (type === "bool") return `!!${v}`;
  if (type === "string") return `${enc("str")}(${v})`;
  if (type === "bytes") return `${enc("b64")}(${v})`;
  if (type === "double" || type === "float") return `${enc("flt")}(${v}${type === "float" ? ",1" : ""})`;
  const [min, max] = bounds(type);
  if (type.includes("64")) return `""+${enc(field.int64String ? "digits" : "check")}(${v},${min},${max})`;
  const n = `${enc("check")}(${known ? v : `Number(${v})`},${min},${max})`;
  if (type !== "enum") return n;
  if (isNullValue(field.armValue ?? field.held)) return `(${n},null)`;
  const enumNamesOf = enumNames(field);
  if (enumNamesOf === U) return n;
  const printed: Record<number, string> = {};
  for (const name in enumNamesOf) printed[enumNamesOf[name]!] ??= name;
  return `(s=${n},${emb(printed)}[s]??s)`;
};

const encodeBody = (msg: Message, fns: Map<Message, string>, read: Read, enc: (helper: JsonHelper) => string, emb: (value: unknown) => string): string => {
  const [init, guard] = oneofGuards(msg, enc as Encoding);
  let body = init;
  for (let idx = 0; idx < msg.fields.length; idx++) {
    const field = msg.fields[idx]!;
    const { expr: src, numeric } = read(field);
    const out = `J${property(jsonNames(field)[0]!)}`;
    const value = (v: string): string =>
      field.type === "message" ? `${fns.get(field.message!)}(${v})` : scalarJson(field, field.type, v, enc, emb, field.int64String ? 0 : numeric ? 1 : 0);
    body += `f=${idx};v=${src};`;
    if (field.map !== U) {
      const key = field.map === "string" ? "k" : field.map === "bool" ? `""+(k==="true")` : `""+${enc("key")}(k,${bounds(field.map).join(",")})`;
      const store = field.map === "string"
        ? `k==="__proto__"?Object.defineProperty(q,k,{value:${value("c")},enumerable:!0,writable:!0,configurable:!0}):q[k]=${value("c")}`
        : `q[${key}]=${value("c")}`;
      body += `a=Object.keys(v);n=a.length;if(n){q={};j=0;while(j<n){k=a[j++];c=v[k];${store}}${out}=q}`;
    } else if (field.repeated) {
      body += `n=v.length;if(n){q=[];j=0;while(j<n)q.push(${value("v[j++]")});${out}=q}`;
    } else if (field.type === "message") {
      body += `if(${field.message!.codec !== U ? "v!==void 0" : "v!=null"}){${guard(field)}${out}=${value("v")}}`;
    } else {
      body += `if(${fieldLive(field, numeric)}){${guard(field)}${out}=${value("v")}}`;
    }
  }
  return body;
};

// The member set, as the message it holds with its type URL beside it.
const anyEncodeBody = (msg: Message, fns: Map<Message, string>, read: Read): string => {
  const { fields, other } = msg.any!;
  let body = "";
  fields.forEach((field, idx) => {
    const write = `${fns.get(field.message!)}(v)`;
    const url = JSON.stringify(`type.googleapis.com/${field.name}`);
    body += `f=${idx + 2};v=${read(field).expr};if(v!==void 0){J=${
      field === other ? write : custom(field.message!) ? `{"@type":${url},value:${write}}` : `Object.assign({"@type":${url}},${write})`
    }}`;
  });
  return body;
};

const encodeLocals = "var v,j,n,s,a,k,c,o,q,f,J={}";

// Message writers are built once per operation, as `S.protobuf`'s are.
const compileEncoders = (root: Message, fns: Map<Message, string>, flag: number): Record<string, Function> => {
  let src = "";
  const messages: Message[] = [];
  const g = B_operationArg(unknown, unknown, flag).g;
  const params: string[] = [...Object.keys(encodeJsonHelpers), "at", "M", "fail", "e"];
  const args: unknown[] = [...Object.values(encodeJsonHelpers), encodeFrame, messages, fail, g.e];
  const embed = (value: unknown): string => {
    const name = `K${args.push(value)}`;
    params.push(name);
    return name;
  };
  const names: string[] = [];
  fns.forEach((name, msg) => {
    if (msg === root && root.recursive === U) return;
    names.push(name);
    if (msg.codec !== U || isOpaqueAny(msg)) {
      params.push(name);
      args.push(msg.codec !== U ? codecs[msg.wellKnown!]![1] : writeOpaqueAny);
      return;
    }
    const slot = messages.push(msg) - 1;
    if (msg.unwrap) {
      const field = msg.fields[0]!;
      const write = msg.wellKnown === "google.protobuf.FieldMask" ? "mask(value)" : scalarJson(field, field.type, "value", (helper) => helper, embed);
      src += `function ${name}(value){var s;try{return ${write}}catch(x){throw at(x,M[${slot}],0,0)}}`;
      return;
    }
    const value = B_operationArg(msg.object, msg.object, flag);
    value.g = g;
    value.i = "value";
    const [head, reads] = writesOf(value, msg);
    const read: Read = (field) => ({ expr: reads.get(field)!, numeric: false });
    const body = msg.any !== U ? anyEncodeBody(msg, fns, read) : encodeBody(msg, fns, read, (helper) => helper, embed);
    const form = formOf(msg);
    src += `function ${name}(value){${head}${encodeLocals};try{${body}${form !== U ? `;J=${embed(form[1])}(J)` : ""}}catch(x){throw at(x,M[${slot}],f,j,a)}return J}`;
  });
  return new Function(...params, `${src}return {${names.join(",")}}`)(...args);
};

const isMessage = (object: Internal | undefined): boolean =>
  object !== U && object.properties !== U && typeof object.additionalItems !== objectTag && !(object.flags & 256);

const leadsToMessage = (schema: Internal | undefined): boolean => isMessage(schema && firstObject(schema, newCtx()));

// A value into ProtoJSON. From `unknown` it is JSON to read on: the reader
// after checks it field by field, and with no message after it checks as the
// object of JSON it is.
const protobufJsonDecoder = (input: Val): Val => {
  if (input.s.type === unknownTag || input.s.flags & 256) {
    return input.s.flags & 256 || leadsToMessage(input.e.to) ? B_refine(input, input.e) : objectDecoder(input);
  }
  const source = objectSchemaOf(input);
  if (!leadsToMessage(source)) return objectDecoder(input);
  const message = compileMessage(source, newCtx(false, true))!;
  const fns = new Map<Message, string>();
  nameMessages(message, fns);
  const encoders = compileEncoders(message, fns, input.g.o);
  const names = new Map<Message, string>();
  fns.forEach((name, msg) => {
    const fn = encoders[name];
    names.set(msg, fn === U ? name : B_embedPure(input, fn));
  });
  const validated = fieldValsOf(input);
  const value = B_scope(input);
  value.s = message.object;
  value.d = validated;
  const [head, reads] = writesOf(value, message);
  const readRoot: Read = (field) => {
    const fv = validated?.[field.path[0]!];
    return { expr: reads.get(field)!, numeric: field.path.length === 1 && fv !== U && fv.s.type === numberTag && fv.s.format !== U };
  };
  const embeds: Partial<Record<JsonHelper, string>> = {};
  const body = encodeBody(message, names, readRoot, (helper) => (embeds[helper] ??= B_embed(input, encodeJsonHelpers[helper])), (v) => B_embedPure(input, v));
  const fails = message.fields.length > 0;
  const outVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outVar, input.e, input.e);
  output.v = _var;
  const frame = fails ? `x=${B_embedPure(input, encodeFrame)}(x,${B_embedPure(input, message)},f,j,a);` : "";
  output.cp = `${head}${B_let(input.g, outVar)}{let v,j,n,s,a,k,c,o,q,J={}${fails ? ",f" : ""};${guarded(input, output, input.e, `${body};${outVar}=J`, frame)}}`;
  output.io = true;
  return output;
};

// ProtoJSON into a message. A target that is no message takes the JSON as it
// is, which is how `S.protobufJson.with(S.to, S.jsonString)` prints it.
const protobufJsonEncoder = (input: Val, target: Internal): Val => {
  if (!leadsToMessage(target)) return input;
  const message = compileMessage(target, newCtx(true))!;
  const fns = new Map<Message, string>();
  nameMessages(message, fns);
  const readers = compileDecoder(message, fns, input.g.o);
  const locals: Record<string, string> = Object.create(null);
  const local = (kind: string, idx: number): string => (locals[kind + idx] ??= B_varWithoutAllocation(input.g));
  let declared = `,d=0,fail=${B_embedPure(input, fail)}`;
  const used = new Set<string>();
  const h = (helper: DecodeHelper): string => {
    if (!used.has(helper)) {
      used.add(helper);
      declared += `,${helper}=${B_embedPure(input, decodeHelpers[helper])}`;
    }
    return helper;
  };
  const { locals: inits, temps, frame, loop } = readParts(message, local, (msg) => {
    const name = fns.get(msg)!;
    if (!used.has(name)) {
      used.add(name);
      declared += `,${name}=${B_embedPure(input, readers[name])}`;
    }
    return name;
  }, !(input.g.o & 2), h, (v) => B_embedPure(input, v));
  for (const [name, init] of inits) B_hoistDecl(input, name, init);
  const at = B_embedPure(input, message);
  const read = B_next(input, "", target, target);
  const code = guarded(input, read, target, `${h("$j")}(j);${loop}`, `x=${B_embedPure(input, wireFrame)}(x,${at},t${frame});`);
  read.cp = `{let j=${input.v()},t=-1,k,v,c,n,p,y,x${temps.map((temp) => `,${temp}`).join("")}${declared};${code}}`;
  return valueOf(read, message, local);
};

// `S.protobuf`'s message as ProtoJSON: a JSON object, which converts with
// `S.to` into the message and out of it.
export const protobufJson: Internal = /* @__PURE__ */ (() => {
  const schema = dictFactory(json);
  schema.decoder = protobufJsonDecoder;
  schema.encoder = protobufJsonEncoder;
  schema.flags = schema.flags | 256;
  schema.name = "ProtoJSON";
  return schema;
})();
