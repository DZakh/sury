import type { Sury } from "./types";

const RESCRIPT_BINDING =
  "ReScript binding surface, not the JS/TS public API";

export type FuzzExport =
  | { use: "schema"; schema: (S: Sury) => unknown }
  | { use: "wrap"; wrap: (S: Sury, inner: unknown) => unknown }
  | { use: "modify"; modify: (S: Sury, schema: any) => unknown; on: string[] }
  | { use: "build" }
  | { use: "skip"; reason: string };

const skip = (reason: string): FuzzExport => ({ use: "skip", reason });
const schema = (schema: (S: Sury) => unknown): FuzzExport => ({
  use: "schema",
  schema,
});
const wrap = (wrap: (S: Sury, inner: unknown) => unknown): FuzzExport => ({
  use: "wrap",
  wrap,
});
const modify = (
  on: string[],
  modify: (S: Sury, schema: any) => unknown,
): FuzzExport => ({ use: "modify", on, modify });
const build = (): FuzzExport => ({ use: "build" });

// Every live `Object.keys(S)` name must appear here. The catalog test fails
// when an export is added without saying how generation may use it.
export const FUZZ_EXPORTS: Record<string, FuzzExport> = {
  $Metadata_Id_make: skip(RESCRIPT_BINDING),
  $Metadata_get: skip(RESCRIPT_BINDING),
  $Metadata_set: skip(RESCRIPT_BINDING),
  $Option_getOr: skip(RESCRIPT_BINDING),
  $Option_getOrWith: skip(RESCRIPT_BINDING),
  $assertAsyncOrThrow: skip(RESCRIPT_BINDING),
  $nullAsOption: skip(RESCRIPT_BINDING),
  $nullAsUnit: skip(RESCRIPT_BINDING),
  $nullableAsOption: skip(RESCRIPT_BINDING),
  $option: skip(RESCRIPT_BINDING),
  $pathConcat: skip(RESCRIPT_BINDING),
  $pathFromArray: skip(RESCRIPT_BINDING),
  $pathFromLocation: skip(RESCRIPT_BINDING),
  $pathToArray: skip(RESCRIPT_BINDING),
  $schema: skip(RESCRIPT_BINDING),
  $setExnId: skip(RESCRIPT_BINDING),
  $unit: skip(RESCRIPT_BINDING),
  Error: skip("exception class, not a schema factory"),
  any: schema((S) => S.any),
  anyOf: build(),
  array: wrap((S, inner) => S.array(inner)),
  assertInput: skip("operation, not a schema factory"),
  assertOutput: skip("operation, not a schema factory"),
  asyncDecoder: skip("operation, not a schema factory"),
  asyncEncoder: skip("operation, not a schema factory"),
  asyncInputConstructor: skip("operation, not a schema factory"),
  asyncOutputConstructor: skip("operation, not a schema factory"),
  asyncParser: skip("operation, not a schema factory"),
  base64: schema((S) => S.base64),
  base64url: schema((S) => S.base64url),
  bigint: schema((S) => S.bigint),
  blob: schema((S) => S.blob),
  bool: schema((S) => S.bool),
  boolean: schema((S) => S.boolean),
  brand: skip("nominal metadata, does not change parse"),
  compactColumns: skip(
    "columnar array codec; not a union-member combinator the grammar calls",
  ),
  cuid: schema((S) => S.cuid),
  date: schema((S) => S.date),
  decoder: skip("operation, not a schema factory"),
  deepStrict: modify(["object"], (S, schema) => S.deepStrict(schema)),
  deepStrip: modify(["object"], (S, schema) => S.deepStrip(schema)),
  dict: wrap((S, inner) => S.dict(inner)),
  duration: schema((S) => S.duration),
  email: schema((S) => S.email),
  enableStandardJSONSchema: skip("mutates global JSON Schema converter"),
  encoder: skip("operation, not a schema factory"),
  enum: build(),
  extendJSONSchema: skip("JSON Schema document helper, not a schema factory"),
  file: schema((S) => S.file),
  float: schema((S) => S.float),
  fromJSONSchema: skip("JSON Schema import, not a generation primitive"),
  global: skip("mutates global config"),
  gt: modify(["number", "bigint"], (S, schema) =>
    schema.with(S.gt, schema.type === "bigint" ? 0n : 0),
  ),
  gte: modify(["number", "bigint"], (S, schema) =>
    schema.with(S.gte, schema.type === "bigint" ? 0n : 0),
  ),
  hostname: schema((S) => S.hostname),
  idnEmail: schema((S) => S.idnEmail),
  idnHostname: schema((S) => S.idnHostname),
  inputConstructor: skip("operation, not a schema factory"),
  inputExpression: skip("debug printer, not a schema factory"),
  inputJSONSchema: skip("JSON Schema export, not a schema factory"),
  inputValidator: skip("operation, not a schema factory"),
  instance: build(),
  int: schema((S) => S.int),
  int32: schema((S) => S.int32),
  integer: schema((S) => S.integer),
  ipv4: schema((S) => S.ipv4),
  ipv6: schema((S) => S.ipv6),
  iri: schema((S) => S.iri),
  iriReference: schema((S) => S.iriReference),
  isoDate: schema((S) => S.isoDate),
  isoDateTime: schema((S) => S.isoDateTime),
  isoTime: schema((S) => S.isoTime),
  json: schema((S) => S.json),
  jsonPointer: schema((S) => S.jsonPointer),
  jsonString: schema((S) => S.jsonString),
  jsonStringWithSpace: schema((S) => S.jsonStringWithSpace(0)),
  length: modify(["string", "array"], (S, schema) => schema.with(S.length, 1)),
  list: wrap((S, inner) => S.list(inner)),
  literal: build(),
  lt: modify(["number", "bigint"], (S, schema) =>
    schema.with(S.lt, schema.type === "bigint" ? 100n : 100),
  ),
  lte: modify(["number", "bigint"], (S, schema) =>
    schema.with(S.lte, schema.type === "bigint" ? 100n : 100),
  ),
  maxLength: modify(["string", "array"], (S, schema) =>
    schema.with(S.maxLength, 32),
  ),
  maxSize: modify(["instance"], (S, schema) => schema.with(S.maxSize, 1024)),
  merge: skip("object merge; tagged objects are built with S.schema"),
  meta: skip("documentation metadata, does not change parse"),
  minLength: modify(["string", "array"], (S, schema) =>
    schema.with(S.minLength, 0),
  ),
  minSize: modify(["instance"], (S, schema) => schema.with(S.minSize, 0)),
  multipleOf: modify(["number", "bigint"], (S, schema) =>
    schema.with(S.multipleOf, schema.type === "bigint" ? 1n : 1),
  ),
  nan: schema((S) => S.nan),
  never: schema((S) => S.never),
  noValidation: skip("strips checks; would hide dispatch bugs"),
  nonEmpty: modify(["string", "array"], (S, schema) => schema.with(S.nonEmpty)),
  nullable: wrap((S, inner) => S.nullable(inner)),
  nullish: wrap((S, inner) => S.nullish(inner)),
  number: schema((S) => S.number),
  object: build(),
  optional: wrap((S, inner) => S.optional(inner)),
  outputConstructor: skip("operation, not a schema factory"),
  outputExpression: skip("debug printer, not a schema factory"),
  outputJSONSchema: skip("JSON Schema export, not a schema factory"),
  outputValidator: skip("operation, not a schema factory"),
  parser: skip("operation, not a schema factory"),
  pattern: modify(["string"], (S, schema) => schema.with(S.pattern, /(?:)/)),
  port: schema((S) => S.port),
  record: wrap((S, inner) => S.record(inner)),
  recursive: skip("cyclic schemas; generation is acyclic"),
  refine: modify(["string", "number", "bigint", "boolean", "object", "array"], (S, schema) =>
    schema.with(S.refine, () => true),
  ),
  relativeJsonPointer: schema((S) => S.relativeJsonPointer),
  reverse: skip("operation, not a schema factory"),
  safe: skip("operation, not a schema factory"),
  safeAsync: skip("operation, not a schema factory"),
  schema: build(),
  shape: skip("output reshape; not a union-member combinator"),
  size: modify(["instance"], (S, schema) => schema.with(S.size, 1)),
  strict: modify(["object"], (S, schema) => S.strict(schema)),
  string: schema((S) => S.string),
  strip: modify(["object"], (S, schema) => S.strip(schema)),
  symbol: schema((S) => S.symbol),
  to: modify(["string"], (S, schema) =>
    schema.with(S.to, S.number, {
      decode: (v: string) => v.length,
      encode: "auto",
    }),
  ),
  trim: modify(["string"], (S, schema) => schema.with(S.trim)),
  tuple: build(),
  uint8Array: schema((S) => S.uint8Array),
  union: build(),
  unknown: schema((S) => S.unknown),
  uri: schema((S) => S.uri),
  uriReference: schema((S) => S.uriReference),
  uriTemplate: schema((S) => S.uriTemplate),
  url: schema((S) => S.url),
  uuid: schema((S) => S.uuid),
  void: schema((S) => S.void),
};

export const catalogNames = (): string[] => Object.keys(FUZZ_EXPORTS).sort();

export const missingCatalogNames = (S: Sury): string[] =>
  Object.keys(S)
    .filter((name) => !(name in FUZZ_EXPORTS))
    .sort();

export const staleCatalogNames = (S: Sury): string[] =>
  Object.keys(FUZZ_EXPORTS)
    .filter((name) => !(name in S))
    .sort();

export const schemaLeaves = (S: Sury): { name: string; schema: unknown }[] => {
  const leaves: { name: string; schema: unknown }[] = [];
  for (const [name, spec] of Object.entries(FUZZ_EXPORTS)) {
    if (spec.use !== "schema") continue;
    leaves.push({ name, schema: spec.schema(S) });
  }
  return leaves;
};

export const wraps = (): [string, Extract<FuzzExport, { use: "wrap" }>][] =>
  Object.entries(FUZZ_EXPORTS).filter(
    (entry): entry is [string, Extract<FuzzExport, { use: "wrap" }>] =>
      entry[1].use === "wrap",
  );

export const modifiers = (): [string, Extract<FuzzExport, { use: "modify" }>][] =>
  Object.entries(FUZZ_EXPORTS).filter(
    (entry): entry is [string, Extract<FuzzExport, { use: "modify" }>] =>
      entry[1].use === "modify",
  );
