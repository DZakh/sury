// Generates message graphs the way `fuzz:eq` generates schemas, so the
// combinations the case table never names get asked too: a map of a message
// holding a oneof, a packed field beside an unpacked one of the same type, a
// message that reaches itself two fields down.
//
// Two properties per graph. Every generated value goes through `runRoundTrip`,
// the check each corpus round trip gets: Sury's bytes equal protobufjs's, both
// decode back to the value, and protobuf-es reads Sury's bytes and the printed
// `.proto` alike. Then the valid encoding is mutated - a byte flipped, a
// truncation, a slice repeated or spliced in - and Sury and protobuf-es each
// decode the result: they must agree on whether it is a message, and where
// both say it is, re-encode it to the same bytes.
import { create, toBinary, fromBinary, fromJson, toJson, type DescMessage } from "@bufbuild/protobuf";
import * as wkt from "@bufbuild/protobuf/wkt";
import * as S from "sury";
import { type FieldDef, suryMessage } from "./cases";
import { protobufEsType } from "./reference";
import { bytesOf, equalValue, runRoundTrip, show } from "./runner";

type Random = () => number;

// mulberry32: a seed names a run exactly, so a finding can be replayed.
const rng = (seed: number): Random => {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
};

const int = (r: Random, n: number): number => Math.floor(r() * n);
const pick = <T>(r: Random, xs: readonly T[]): T => xs[int(r, xs.length)]!;
const chance = (r: Random, p: number): boolean => r() < p;

type Scalar = Exclude<S.ProtobufType, "message">;
const SCALARS: Scalar[] = [
  "int32", "int64", "uint32", "uint64", "sint32", "sint64", "fixed32", "fixed64",
  "sfixed32", "sfixed64", "float", "double", "bool", "string", "bytes", "enum",
];
const KEYS: Scalar[] = [
  "int32", "int64", "uint32", "uint64", "sint32", "sint64", "fixed32", "fixed64",
  "sfixed32", "sfixed64", "bool", "string",
];
const PACKABLE = (type: S.ProtobufType): boolean => type !== "string" && type !== "bytes" && type !== "message";

// 1-15 take a one-byte tag, up to 2047 two, and the rest reach the 29-bit
// ceiling; 19000-19999 is reserved to the implementation and never generated.
const fieldNumbers = (r: Random, n: number): number[] => {
  const out = new Set<number>();
  while (out.size < n) {
    const band = int(r, 4);
    const number =
      band < 2 ? 1 + int(r, 15)
      : band === 2 ? 16 + int(r, 2032)
      : chance(r, 0.5) ? 536870911 - int(r, 4) : 20000 + int(r, 1e6);
    out.add(number);
  }
  return [...out].sort((a, b) => a - b);
};

// A message is a field table. `ancestors` are the tables still being filled on
// the way down, which a field may name again - a cycle - only where the value
// can end: optional, repeated, a map or a oneof member.
const genMessage = (r: Random, depth: number, ancestors: FieldDef[][], budget: { messages: number }): FieldDef[] => {
  const fields: FieldDef[] = [];
  budget.messages--;
  const size = int(r, depth === 0 ? 7 : 5);
  const numbers = fieldNumbers(r, size);
  const scope = [...ancestors, fields];
  let oneofs = 0;
  const nestedMessage = (canEnd: boolean): FieldDef[] => {
    if (canEnd && chance(r, 0.35)) return pick(r, scope);
    if (depth >= 3 || budget.messages <= 0) return [];
    return genMessage(r, depth + 1, scope, budget);
  };
  for (let idx = 0; idx < numbers.length; idx++) {
    const number = numbers[idx]!;
    const key = `f${idx}`;
    const kind = int(r, 10);
    const type: S.ProtobufType = chance(r, 0.25) ? "message" : pick(r, SCALARS);
    if (kind < 2 && idx + 1 < numbers.length) {
      // A oneof takes this number and the next one or two.
      const name = `o${oneofs++}`;
      const members = Math.min(numbers.length - idx, 2 + int(r, 2));
      for (let m = 0; m < members; m++) {
        const memberType: S.ProtobufType = chance(r, 0.3) ? "message" : pick(r, SCALARS);
        const def: FieldDef = { key: `f${idx + m}`, number: numbers[idx + m]!, type: memberType, oneof: name };
        if (memberType === "message") def.fields = nestedMessage(true);
        fields.push(def);
      }
      idx += members - 1;
      continue;
    }
    const def: FieldDef = { key, number, type };
    if (kind === 2) def.optional = true;
    else if (kind === 3 || kind === 4) {
      def.repeated = true;
      if (PACKABLE(type) && chance(r, 0.4)) def.packed = false;
    } else if (kind === 5) def.map = pick(r, KEYS);
    if (type === "message") def.fields = nestedMessage(!!(def.optional || def.repeated || def.map));
    fields.push(def);
  }
  return fields;
};

const INT32 = [0, 1, -1, 127, 128, 300, 16383, 16384, 2147483647, -2147483648, -64, 63, 64];
const UINT32 = [0, 1, 127, 128, 16384, 2097151, 2097152, 268435455, 268435456, 4294967295];
const INT64 = [0n, 1n, -1n, 127n, 128n, 2n ** 31n, -(2n ** 31n), 2n ** 53n, -(2n ** 53n), 2n ** 53n + 1n, 9223372036854775807n, -9223372036854775808n];
const UINT64 = [0n, 1n, 127n, 128n, 2n ** 32n, 2n ** 53n + 1n, 2n ** 63n, 18446744073709551615n];
const FLOATS = [0, -0, 1, -1, 0.5, NaN, Infinity, -Infinity, 1.401298464324817e-45, 3.4028234663852886e38, -3.4028234663852886e38, 1.1754943508222875e-38];
const DOUBLES = [0, -0, 1, -1, 0.1, NaN, Infinity, -Infinity, 5e-324, Number.MAX_VALUE, -Number.MAX_VALUE, 2 ** 53, 1e21];
// No string opens with U+FEFF: protobuf-es strips a leading BOM (the corpus
// case "official/string-bom" holds that one), and the finding would say so
// on every graph that drew it.
const STRINGS = ["", "a", "__", "hello world", "é", "€", "😀", "a﻿", "\u0000", "x".repeat(47), "y".repeat(48), "z".repeat(130), "é".repeat(70), "a😀".repeat(40)];

const genScalar = (r: Random, type: Scalar): unknown => {
  switch (type) {
    case "int32": case "sint32": case "sfixed32": case "enum":
      return chance(r, 0.6) ? pick(r, INT32) : (int(r, 2 ** 32) | 0);
    case "uint32": case "fixed32":
      return chance(r, 0.6) ? pick(r, UINT32) : int(r, 2 ** 32);
    case "int64": case "sint64": case "sfixed64":
      return chance(r, 0.6) ? pick(r, INT64) : BigInt.asIntN(64, BigInt(int(r, 2 ** 32)) * BigInt(int(r, 2 ** 32)));
    case "uint64": case "fixed64":
      return chance(r, 0.6) ? pick(r, UINT64) : BigInt(int(r, 2 ** 32)) * BigInt(int(r, 2 ** 32));
    case "float":
      return chance(r, 0.6) ? pick(r, FLOATS) : Math.fround((r() - 0.5) * 10 ** int(r, 30));
    case "double":
      return chance(r, 0.6) ? pick(r, DOUBLES) : (r() - 0.5) * 10 ** int(r, 300);
    case "bool":
      return chance(r, 0.5);
    case "string":
      return chance(r, 0.7) ? pick(r, STRINGS) : String.fromCodePoint(...Array.from({ length: int(r, 20) }, () => pick(r, [int(r, 128), int(r, 0xd7ff), 0x10000 + int(r, 0xfffff)]))).replace(/^\ufeff/, "");
    case "bytes":
      return Uint8Array.from({ length: pick(r, [0, 1, 5, 127, 128, 300]) }, () => int(r, 256));
  }
};

// A map key is the property name a decode writes, so the generator writes
// the one the key type reads back to.
const genKey = (r: Random, type: Scalar): string => {
  if (type === "string") return pick(r, ["", "a", "key", "é", "0", "10", "constructor", "toString"]);
  if (type === "bool") return String(chance(r, 0.5));
  return String(genScalar(r, type));
};

const genValue = (r: Random, fields: FieldDef[], depth: number): Record<string, unknown> => {
  const value: Record<string, unknown> = {};
  const deep = depth > 4;
  const groups = new Map<string, FieldDef[]>();
  for (const def of fields) {
    if (def.oneof) {
      groups.set(def.oneof, [...(groups.get(def.oneof) ?? []), def]);
      continue;
    }
    const one = (): unknown =>
      def.type === "message" ? genValue(r, def.fields!, depth + 1) : genScalar(r, def.type as Scalar);
    // Twenty items only at the root: a list of messages that reach themselves
    // multiplies by its length at every level.
    if (def.repeated) value[def.key] = deep ? [] : Array.from({ length: pick(r, depth === 0 ? [0, 1, 2, 3, 20] : [0, 1, 2, 3]) }, one);
    else if (def.map) {
      const map: Record<string, unknown> = {};
      if (!deep) for (let n = int(r, 4); n > 0; n--) map[genKey(r, def.map)] = one();
      value[def.key] = map;
    } else if (def.optional) {
      if (!deep && chance(r, 0.6)) value[def.key] = one();
    } else value[def.key] = one();
  }
  groups.forEach((members) => {
    if (deep || chance(r, 0.3)) return;
    const def = pick(r, members);
    value[def.key] = def.type === "message" ? genValue(r, def.fields!, depth + 1) : genScalar(r, def.type as Scalar);
  });
  return value;
};

const mutate = (r: Random, bytes: Uint8Array): Uint8Array => {
  const src = Array.from(bytes);
  const at = int(r, src.length + 1);
  switch (int(r, 6)) {
    case 0:
      if (src.length) src[Math.min(at, src.length - 1)] ^= 1 << int(r, 8);
      break;
    case 1:
      src.length = at;
      break;
    case 2:
      src.splice(at, 0, int(r, 256));
      break;
    case 3: {
      const end = at + int(r, 8);
      src.splice(end, 0, ...src.slice(at, end));
      break;
    }
    case 4:
      src.splice(at, 1 + int(r, 3));
      break;
    default:
      // A tag with a random wire type, which a field of another type must
      // treat as unknown rather than misread.
      src.splice(at, 0, (1 + int(r, 15)) * 8 + int(r, 8), int(r, 256));
  }
  return Uint8Array.from(src);
};

export type Finding = { kind: string; seed: number; detail: string; proto?: string };

// Cases known not to hold, keyed by what the run prints, each with the reason
// written by hand. The run fails on a finding with no entry here, and on an
// entry no finding matched: an entry that has started to hold is removed.
//
// Every entry below was put to Google's own parser (upb, through the
// `protobuf` Python package) on the bytes the run printed, and Google sided
// with Sury on each.
const ES_LENIENT = "protobuf-es reads on where Google's parser rejects the bytes as corrupt";
const ES_STRICT = "protobuf-es rejects bytes Google's parser reads, as Sury does";
const KNOWN: Record<string, string> = {
  "acceptance: only protobuf-es accepts (truncated protobuf message)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (unmatched protobuf end group)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (mismatched protobuf end group)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (unterminated protobuf group)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (invalid protobuf field number)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (invalid protobuf wire type)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (invalid protobuf tag)": ES_LENIENT,
  "acceptance: only protobuf-es accepts (varint exceeds # bytes)": ES_LENIENT,
  "acceptance: only sury accepts (illegal tag: varint overflows uint#)": ES_STRICT,
  "acceptance: only sury accepts (illegal tag: field no # wire type #)": ES_STRICT,
  "acceptance: only sury accepts (premature EOF)": ES_STRICT,
  "acceptance: only sury accepts (Offset is outside the bounds of the DataView)": ES_STRICT,
  "acceptance: only sury accepts (cant skip wire type #)": ES_STRICT,
  "reencode: sury reads protobuf-es's re-encode as another value":
    "protobuf-es misreads a 32-bit varint written in more than five bytes, and writes back the value it misread",
};

// Reached only by a sweep wider than the default, so a default run has no way
// to tell one has started to hold: listed apart and never reported stale.
const KNOWN_WIDE: Record<string, string> = {
  "acceptance: only sury accepts (The encoded data was not valid for encoding utf-#)": ES_STRICT,
};

// Fewer seeds than the default can miss a listed case without it having
// started to hold, so only a full run reports one as stale.
export const DEFAULT_SEEDS = 600;

const esDecode = (type: DescMessage, bytes: Uint8Array): Uint8Array | Error => {
  try {
    return toBinary(type, fromBinary(type, bytes), { writeUnknownFields: false });
  } catch (e) {
    return e as Error;
  }
};

const suryRead = (decode: (b: Uint8Array) => unknown, bytes: Uint8Array): { value: unknown } | Error => {
  try {
    return { value: decode(bytes) };
  } catch (e) {
    return e as Error;
  }
};

// The graph a seed generates, drawn first so a replay reaches it without
// running the values after it.
export const graphOf = (seed: number): { r: Random; fields: FieldDef[] } => {
  const r = rng(seed);
  return { r, fields: genMessage(r, 0, [], { messages: 6 }) };
};

// A JSON value for `google.protobuf.Value`. No `__proto__` key: protobuf-es
// keeps a Struct's fields on a plain object and loses it (see runner.ts).
const genJson = (r: Random, depth: number): unknown => {
  const roll = int(r, depth > 3 ? 4 : 6);
  if (roll === 0) return null;
  if (roll === 1) return chance(r, 0.5) ? pick(r, [0, -0, 1, -1, 0.5, 1e21, 5e-324, Number.MAX_VALUE]) : (r() - 0.5) * 10 ** int(r, 30);
  if (roll === 2) return genScalar(r, "string");
  if (roll === 3) return chance(r, 0.5);
  if (roll === 4) return Array.from({ length: int(r, 4) }, () => genJson(r, depth + 1));
  const out: Record<string, unknown> = {};
  for (let n = int(r, 4); n > 0; n--) out[pick(r, ["a", "", "1", "b c", "\u00e9", "constructor", "10"])] = genJson(r, depth + 1);
  return out;
};

// The field's payload: Sury writes a present well-known value as field 1, so
// the bytes after its tag and length are the message.
const payload = (bytes: Uint8Array): Uint8Array => {
  let at = 1;
  while (bytes[at]! > 127) at++;
  return bytes.subarray(at + 1);
};

type Message = Record<string, unknown>;

// One well-known type against protobuf-es's own implementation of it: the
// schema Sury reads it into, a value to write, the message protobuf-es builds
// for that value and the value it reads back out, and what a comparison looks
// at (a Date by its time, since two Dates have no fields to differ in).
type WellKnownCase = {
  type: string;
  schema: S.Schema<unknown, unknown>;
  es: DescMessage;
  gen: (r: Random) => unknown;
  toEs: (value: unknown) => Message;
  fromEs: (message: Message) => unknown;
  key?: (value: unknown) => unknown;
};

const field = (schema: S.Schema<unknown, unknown>, type?: string): S.Schema<unknown, unknown> =>
  S.schema({ v: schema.with(S.protobufField, type === undefined ? 1 : { number: 1, type: type as S.ProtobufWellKnownType }) }) as never;

const secondsNanos = S.schema({ seconds: S.bigint, nanos: S.int32 }) as S.Schema<unknown, unknown>;
const genSecondsNanos = (r: Random) => ({
  seconds: chance(r, 0.3) ? pick(r, [0n, 1n, -1n, 253402300799n, -62135596800n, 9223372036854775807n]) : BigInt(int(r, 2 ** 40)) - 2n ** 39n,
  nanos: chance(r, 0.3) ? pick(r, [0, 1, -1, 999999999, -999999999]) : int(r, 2 ** 32) | 0,
});
const bySecondsNanos = (m: Message) => ({ seconds: m.seconds, nanos: m.nanos });
const wrapper = (name: string, schema: S.Schema<unknown, unknown>, gen: (r: Random) => unknown): WellKnownCase => ({
  type: `google.protobuf.${name}`,
  schema: field(S.optional(schema) as never, `google.protobuf.${name}`),
  es: (wkt as unknown as Record<string, DescMessage>)[`${name}Schema`]!,
  gen,
  toEs: (value) => ({ value }),
  fromEs: (m) => m.value,
});

const wellKnownCases: WellKnownCase[] = [
  {
    type: "google.protobuf.Value",
    schema: field(S.json as never),
    es: wkt.ValueSchema,
    gen: (r) => genJson(r, 0),
    toEs: (value) => fromJson(wkt.ValueSchema, value as never) as never,
    fromEs: (m) => toJson(wkt.ValueSchema, m as never),
  },
  {
    type: "google.protobuf.Struct",
    schema: field(S.record(S.json) as never, "google.protobuf.Struct"),
    es: wkt.StructSchema,
    gen: (r) => Object.fromEntries(Array.from({ length: int(r, 4) }, (_, i) => [`k${i}`, genJson(r, 1)])),
    toEs: (value) => fromJson(wkt.StructSchema, value as never) as never,
    fromEs: (m) => toJson(wkt.StructSchema, m as never),
  },
  {
    type: "google.protobuf.ListValue",
    schema: field(S.array(S.json) as never, "google.protobuf.ListValue"),
    es: wkt.ListValueSchema,
    gen: (r) => Array.from({ length: int(r, 4) }, () => genJson(r, 1)),
    toEs: (value) => fromJson(wkt.ListValueSchema, value as never) as never,
    fromEs: (m) => toJson(wkt.ListValueSchema, m as never),
  },
  {
    type: "google.protobuf.Timestamp as a Date",
    schema: field(S.date as never),
    es: wkt.TimestampSchema,
    gen: (r) => new Date(chance(r, 0.3) ? pick(r, [0, -1, 999, -1000, 253402300799999, -62135596800000]) : Math.round((r() - 0.3) * 1e13)),
    toEs: (value) => wkt.timestampFromDate(value as Date) as never,
    fromEs: (m) => wkt.timestampDate(m as never),
    key: (value) => (value as Date).getTime(),
  },
  {
    type: "google.protobuf.Timestamp",
    schema: field(secondsNanos, "google.protobuf.Timestamp"),
    es: wkt.TimestampSchema,
    gen: genSecondsNanos,
    toEs: (value) => value as Message,
    fromEs: bySecondsNanos,
  },
  {
    type: "google.protobuf.Duration",
    schema: field(secondsNanos, "google.protobuf.Duration"),
    es: wkt.DurationSchema,
    gen: genSecondsNanos,
    toEs: (value) => value as Message,
    fromEs: bySecondsNanos,
  },
  {
    type: "google.protobuf.FieldMask",
    schema: field(S.array(S.string) as never, "google.protobuf.FieldMask"),
    es: wkt.FieldMaskSchema,
    gen: (r) => Array.from({ length: int(r, 4) }, () => genScalar(r, "string")),
    toEs: (paths) => ({ paths }),
    fromEs: (m) => m.paths,
  },
  {
    type: "google.protobuf.Empty",
    schema: field(S.schema({}) as never, "google.protobuf.Empty"),
    es: wkt.EmptySchema,
    gen: () => ({}),
    toEs: () => ({}),
    fromEs: () => ({}),
  },
  wrapper("DoubleValue", S.number as never, (r) => genScalar(r, "double")),
  wrapper("FloatValue", S.number as never, (r) => genScalar(r, "float")),
  wrapper("Int64Value", S.bigint as never, (r) => genScalar(r, "int64")),
  wrapper("UInt64Value", S.bigint as never, (r) => genScalar(r, "uint64")),
  wrapper("Int32Value", S.int32 as never, (r) => genScalar(r, "int32")),
  wrapper("UInt32Value", S.integer as never, (r) => genScalar(r, "uint32")),
  wrapper("BoolValue", S.boolean as never, (r) => genScalar(r, "bool")),
  wrapper("StringValue", S.string as never, (r) => genScalar(r, "string")),
  wrapper("BytesValue", S.uint8Array as never, (r) => genScalar(r, "bytes")),
];

// Every well-known type against protobuf-es's own: the same bytes for the same
// value, and both sides reading those bytes back as it.
const wellKnownFindings = (r: Random, seed: number, count: number): Finding[] => {
  const findings: Finding[] = [];
  for (const c of wellKnownCases) {
    const encode = S.decodeOrThrow(c.schema, S.protobuf);
    const decode = S.decodeOrThrow(S.protobuf, c.schema);
    const key = c.key ?? ((value: unknown) => value);
    for (let i = 0; i < count; i++) {
      const value = c.gen(r);
      try {
        const bytes = encode({ v: value }).slice();
        const sury = payload(bytes);
        const es = toBinary(c.es, create(c.es, c.toEs(value) as never));
        if (bytesOf(sury).join() !== bytesOf(es).join()) {
          findings.push({ kind: `wkt: Sury writes a ${c.type} protobuf-es does not`, seed, detail: `value=${show(value)} sury=[${bytesOf(sury)}] es=[${bytesOf(es)}]` });
          continue;
        }
        if (!equalValue(key((decode(bytes) as { v: unknown }).v), key(value))) {
          findings.push({ kind: `wkt: Sury reads its ${c.type} back as another`, seed, detail: `value=${show(value)}` });
        }
        if (!equalValue(key(c.fromEs(fromBinary(c.es, sury) as never)), key(value))) {
          findings.push({ kind: `wkt: protobuf-es reads Sury's ${c.type} as another`, seed, detail: `value=${show(value)}` });
        }
      } catch (e) {
        findings.push({ kind: `wkt: ${c.type} throws (${(e as Error).message.replace(/\d+/g, "#")})`, seed, detail: `value=${show(value)}` });
      }
    }
  }
  return findings;
};

export type FuzzOptions = { seeds: number; values: number; mutants: number; from: number };

export const runFuzz = ({ seeds, values, mutants, from }: FuzzOptions): { findings: Finding[]; graphs: number; checks: number } => {
  const findings: Finding[] = [];
  let checks = 0;
  for (let seed = from; seed < from + seeds; seed++) {
    const { r, fields } = graphOf(seed);
    findings.push(...wellKnownFindings(rng(-seed), seed, values));
    checks += values * wellKnownCases.length;
    let decode: (b: Uint8Array) => unknown;
    let encode: (v: unknown) => Uint8Array;
    let esType: DescMessage;
    try {
      const schema = suryMessage(fields);
      decode = S.decodeOrThrow(S.protobuf, schema) as never;
      encode = S.decodeOrThrow(schema, S.protobuf) as never;
      esType = protobufEsType(fields);
    } catch (e) {
      findings.push({ kind: "compile", seed, detail: (e as Error).message });
      continue;
    }
    for (let v = 0; v < values; v++) {
      const value = genValue(r, fields, 0);
      checks++;
      const result = runRoundTrip(`fuzz/${seed}/${v}`, fields, value);
      if (result.status !== "pass") {
        findings.push({ kind: `roundtrip: ${result.detail!.replace(/[\d,]+/g, "#")}`, seed, detail: `${result.detail} value=${show(value)}` });
        continue;
      }
      const bytes = encode(value).slice();
      for (let m = 0; m < mutants; m++) {
        const mutant = mutate(r, bytes);
        checks++;
        const sury = suryRead(decode, mutant);
        const es = esDecode(esType, mutant);
        const suryOk = !(sury instanceof Error);
        const esOk = !(es instanceof Error);
        if (suryOk !== esOk) {
          const reason = (suryOk ? es : sury) as Error;
          findings.push({
            kind: `acceptance: ${suryOk ? "only sury" : "only protobuf-es"} accepts (${reason.message.replace(/[\d]+/g, "#").replace(/ at .*/, "")})`,
            seed,
            detail: `bytes=[${bytesOf(mutant)}]`,
          });
          continue;
        }
        if (!suryOk) continue;
        // What protobuf-es wrote back is read by Sury and compared as a value,
        // not as bytes: a required message absent from the wire is an empty
        // one to Sury, which it then writes, where protobuf-es keeps it absent.
        // Both are the same value to a reader of this schema.
        const again = suryRead(decode, es as Uint8Array);
        if (again instanceof Error || !equalValue(again.value, sury.value)) {
          findings.push({
            kind: "reencode: sury reads protobuf-es's re-encode as another value",
            seed,
            detail: `bytes=[${bytesOf(mutant)}] es=[${bytesOf(es as Uint8Array)}] sury=${show(sury.value).slice(0, 300)}`,
          });
        }
      }
    }
  }
  return { findings, graphs: seeds, checks };
};

export const reportFuzz = (findings: Finding[], full: boolean): { text: string; ok: boolean } => {
  const byKind = new Map<string, Finding[]>();
  for (const f of findings) byKind.set(f.kind, [...(byKind.get(f.kind) ?? []), f]);
  const lines: string[] = [];
  let ok = true;
  byKind.forEach((list, kind) => {
    const known = KNOWN[kind] ?? KNOWN_WIDE[kind];
    if (known === undefined) ok = false;
    lines.push(`${known === undefined ? "NEW  " : "known"} ${kind}  x${list.length}  (first: seed ${list[0]!.seed})`);
    if (known === undefined) for (const f of list.slice(0, 2)) lines.push(`        ${f.detail.slice(0, 600)}`);
  });
  for (const kind of Object.keys(KNOWN)) {
    if (full && !byKind.has(kind)) {
      ok = false;
      lines.push(`HOLDS ${kind} - listed as known, no longer found; remove it from KNOWN`);
    }
  }
  return { text: lines.join("\n"), ok };
};
