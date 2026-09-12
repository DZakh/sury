import { isDeepStrictEqual } from "node:util";
import * as S from "sury";
import {
  decodeOnlyCases,
  rejectCases,
  roundTripCases,
  skipped,
  suryMessage,
  type FieldDef,
} from "./cases";
import {
  decodeProtobufjs,
  encodeProtobufjs,
  printedProtobufEsType,
  printedProtobufjsType,
  protobufEsType,
  reencodeProtobufEs,
} from "./reference";

export type CaseResult = {
  id: string;
  status: "pass" | "fail" | "error";
  detail?: string;
};

export type SuiteResult = {
  passed: string[];
  failed: string[];
  errored: string[];
  skipped: string[];
  results: CaseResult[];
};

export type Golden = {
  $comment: string;
  reference: string;
  summary: {
    cases: number;
    passed: number;
    failed: number;
    errored: number;
    skipped: number;
    rate: string;
  };
  failed: string[];
  errored: string[];
  skipped: string[];
  protobufEsWrong: string[];
};

const bytesOf = (value: Uint8Array | number[]): number[] =>
  Array.from(value instanceof Uint8Array ? value : new Uint8Array(value));

const equalBytes = (a: Uint8Array, b: Uint8Array): boolean => {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false;
  return true;
};

const equalValue = (a: unknown, b: unknown): boolean => {
  if (Object.is(a, b)) return true;
  if (a instanceof Uint8Array && b instanceof Uint8Array) return equalBytes(a, b);
  if (Array.isArray(a) && Array.isArray(b)) {
    return a.length === b.length && a.every((item, i) => equalValue(item, b[i]));
  }
  if (typeof a === "object" && typeof b === "object" && a && b && !ArrayBuffer.isView(a) && !ArrayBuffer.isView(b)) {
    // An absent optional field and one set to `undefined` are the same value
    // to Sury, which emits the latter when it rebuilds an object.
    const definedKeys = (o: object) =>
      Object.keys(o).filter((key) => (o as Record<string, unknown>)[key] !== undefined).sort();
    const ak = definedKeys(a);
    const bk = definedKeys(b);
    if (ak.length !== bk.length) return false;
    return ak.every((key, i) => key === bk[i] && equalValue((a as Record<string, unknown>)[key], (b as Record<string, unknown>)[key]));
  }
  return isDeepStrictEqual(a, b);
};

const ops = (fields: FieldDef[]) => {
  const schema = suryMessage(fields);
  return {
    schema,
    encode: S.decodeOrThrow(schema, S.protobuf),
    decode: S.decodeOrThrow(S.protobuf, schema),
  };
};

const fail = (id: string, detail: string): CaseResult => ({ id, status: "fail", detail });

// `S.toProtoOrThrow` of the case's schema, parsed by protobufjs, must speak the same
// wire as the reference `.proto` written from the field table.
const printedAgrees = (
  schema: S.Schema<unknown, unknown>,
  fields: FieldDef[],
  bytes: Uint8Array,
  referenceDecoded: Record<string, unknown>,
  value?: Record<string, unknown>,
  referenceBytes?: Uint8Array,
): string | undefined => {
  const printed = printedProtobufjsType(schema);
  if (!equalValue(decodeProtobufjs(fields, bytes, printed), referenceDecoded)) {
    return "protobufjs decodes the printed .proto differently from the reference";
  }
  if (value && !equalBytes(encodeProtobufjs(fields, value, printed), referenceBytes!)) {
    return "protobufjs encodes the printed .proto differently from the reference";
  }
  return undefined;
};
// Two cases protobuf-es gets wrong, checked against protobufjs like every
// other and skipped only here. Both were confirmed against protobuf-es
// directly, not inferred from a disagreement:
//
//   - its string decoder strips a leading BOM, so U+FEFF as the whole of a
//     proto3 string decodes to "" and re-encodes to nothing. A BOM inside a
//     length-delimited string is data; protobufjs and Sury both keep it.
//   - it stores map entries on a plain object, so an entry keyed `__proto__`
//     is swallowed by [[SetPrototypeOf]] and never reaches the message.
const PROTOBUF_ES_WRONG: Record<string, string> = {
  "official/string-bom": "protobuf-es strips a leading BOM from a string field",
  "map/proto-key-is-own-property": "protobuf-es loses a map entry keyed __proto__",
};

// protobuf-es over the same bytes, through the reference `.proto` and again
// through the printed one. Re-encoding is the comparison (see reference.ts):
// a printer bug that names a wire type, a label or a packing the writer did
// not mean comes back as different bytes here even where protobufjs, which
// parsed both files, reads them alike.
const protobufEsAgrees = (
  id: string,
  schema: S.Schema<unknown, unknown>,
  fields: FieldDef[],
  bytes: Uint8Array,
): string | undefined => {
  if (id in PROTOBUF_ES_WRONG) return undefined;
  const viaReference = reencodeProtobufEs(bytes, protobufEsType(fields));
  if (!equalBytes(viaReference, bytes)) {
    return `protobuf-es re-encodes ${bytesOf(viaReference)} !== sury ${bytesOf(bytes)}`;
  }
  const viaPrinted = reencodeProtobufEs(bytes, printedProtobufEsType(schema));
  if (!equalBytes(viaPrinted, viaReference)) {
    return `protobuf-es reads the printed .proto as ${bytesOf(viaPrinted)}, the reference as ${bytesOf(viaReference)}`;
  }
  return undefined;
};

const pass = (id: string): CaseResult => ({ id, status: "pass" });
const error = (id: string, detail: string): CaseResult => ({ id, status: "error", detail });

const runRoundTrip = (id: string, fields: FieldDef[], value: Record<string, unknown>, wire?: number[]): CaseResult => {
  try {
    const { schema, encode, decode } = ops(fields);
    const suryBytes = encode(value);
    const pbjsBytes = encodeProtobufjs(fields, value);
    if (wire && !equalBytes(suryBytes, new Uint8Array(wire))) {
      return fail(id, `sury wire ${bytesOf(suryBytes)} !== official ${wire}`);
    }
    if (!equalBytes(suryBytes, pbjsBytes)) {
      return fail(id, `sury wire ${bytesOf(suryBytes)} !== protobufjs ${bytesOf(pbjsBytes)}`);
    }
    const suryBack = decode(suryBytes);
    if (!equalValue(suryBack, value)) {
      return fail(id, `sury roundtrip mismatch`);
    }
    const fromPbjs = decode(pbjsBytes);
    if (!equalValue(fromPbjs, value)) {
      return fail(id, `sury decode of protobufjs bytes mismatch`);
    }
    const pbjsBack = decodeProtobufjs(fields, suryBytes);
    if (!equalValue(pbjsBack, value)) {
      return fail(id, `protobufjs decode of sury bytes mismatch`);
    }
    const printed = printedAgrees(schema, fields, suryBytes, pbjsBack, value, pbjsBytes);
    if (printed) return fail(id, printed);
    const es = protobufEsAgrees(id, schema, fields, suryBytes);
    if (es) return fail(id, es);
    return pass(id);
  } catch (e) {
    return error(id, (e as Error).message);
  }
};

const show = (value: unknown): string =>
  JSON.stringify(value, (_, v) => (typeof v === "bigint" ? `${v}n` : v instanceof Uint8Array ? bytesOf(v) : v));

const runDecodeOnly = (
  id: string,
  fields: FieldDef[],
  wire: number[],
  value: Record<string, unknown>,
  reencoded?: number[],
): CaseResult => {
  try {
    const { schema, encode, decode } = ops(fields);
    const got = decode(new Uint8Array(wire));
    if (!equalValue(got, value)) return fail(id, `decoded ${show(got)} !== ${show(value)}`);
    if (reencoded) {
      const back = encode(got);
      if (!equalBytes(back, new Uint8Array(reencoded))) {
        return fail(id, `re-encoded ${bytesOf(back)} !== ${reencoded}`);
      }
    }
    const bytes = new Uint8Array(wire);
    const printed = printedAgrees(schema, fields, bytes, decodeProtobufjs(fields, bytes));
    if (printed) return fail(id, printed);
    const es = protobufEsAgrees(id, schema, fields, encode(got));
    if (es) return fail(id, es);
    return pass(id);
  } catch (e) {
    return error(id, (e as Error).message);
  }
};

const runReject = (id: string, fields: FieldDef[], wire: number[]): CaseResult => {
  try {
    const { decode } = ops(fields);
    decode(new Uint8Array(wire));
    return fail(id, "expected decode to throw");
  } catch (e) {
    if ((e as Error).message === "expected decode to throw") return fail(id, (e as Error).message);
    return pass(id);
  }
};

export const runSuite = (): SuiteResult => {
  const results: CaseResult[] = [];
  for (const c of roundTripCases) results.push(runRoundTrip(c.id, c.fields, c.value, c.wire));
  for (const c of decodeOnlyCases) results.push(runDecodeOnly(c.id, c.fields, c.wire, c.value, c.reencoded));
  for (const c of rejectCases) results.push(runReject(c.id, c.fields, c.wire));
  const passed = results.filter((r) => r.status === "pass").map((r) => r.id);
  const failed = results.filter((r) => r.status === "fail").map((r) => r.id);
  const errored = results.filter((r) => r.status === "error").map((r) => r.id);
  return { passed, failed, errored, skipped: [...skipped], results };
};

export const rate = (passed: number, total: number): string =>
  total === 0 ? "100%" : `${((100 * passed) / total).toFixed(1)}%`;

export const toGolden = (suite: SuiteResult): Golden => ({
  $comment: "Generated by `pnpm protobuf:compliance --update`. Do not edit by hand.",
  reference: "protobufjs + protobuf-es",
  summary: {
    cases: suite.results.length,
    passed: suite.passed.length,
    failed: suite.failed.length,
    errored: suite.errored.length,
    skipped: suite.skipped.length,
    rate: rate(suite.passed.length, suite.results.length),
  },
  failed: suite.failed,
  errored: suite.errored,
  skipped: suite.skipped,
  protobufEsWrong: Object.keys(PROTOBUF_ES_WRONG).sort(),
});

export const serializeGolden = (golden: Golden): string => `${JSON.stringify(golden, null, 2)}\n`;
