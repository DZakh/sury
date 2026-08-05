// The single public entry for both surfaces:
//  - JS/TS consumers import the package root and get the public API under its
//    documented names (typed by the hand-written index.d.ts).
//  - The ReScript bindings module (S.res) binds to this same module with
//    `@module("sury") external` declarations, so both languages share one
//    runtime instance (one Exn identity, one set of schema singletons, one
//    seq counter).
//
// Built by scripts/pack.ts into index.mjs (the publish step additionally
// emits a CJS index.js into the artifact for the require condition). The extra
// ReScript-binding exports ($res_*-named) are invisible to TS users
// (index.d.ts is the curated surface) and tree-shake when unused like any
// other export.

// ── Schema singletons (shared by both surfaces) ──────────────────────────────
//
// Re-exports of module-level consts, each PURE-initialized at its declaration,
// so unused ones tree-shake out of consumer bundles.

export {
  string,
  bool as boolean,
  bool,
  int as int32,
  int,
  integer,
  float as number,
  float,
  bigint,
  symbol,
  nan,
  void_ as void,
  unit as $res_unit,
} from "./primitives";
export { never_ as never } from "./parse";
export { json, jsonString } from "./advanced/json";
export { uint8Array } from "./advanced/uint8Array";
export { date } from "./advanced/date";
export {
  isoDateTime,
  port,
  email,
  uuid,
  cuid,
  url,
} from "./refinements";
export { nullAsUnit as $res_nullAsUnit } from "./modifiers";
export {
  unknown,
  unknown as any,
  errorClass as Error,
  __setExnId as $res_setExnId,
} from "./base";

// ── Public JS/TS API (names match index.d.ts) ────────────────────────────────

export {
  js_optional as optional,
  js_nullable as nullable,
  js_union as union,
  js_parser as parser,
  js_asyncParser as asyncParser,
  js_asyncDecoder as asyncDecoder,
  js_encoder as encoder,
  js_asyncEncoder as asyncEncoder,
  js_assert as assert,
  js_is as is,
  js_merge as merge,
  js_to as to,
  js_asyncDecoderAssert as asyncDecoderAssert,
  js_refine as refine,
  global,
} from "./jsapi";
export { getDecoder as decoder, reverse, instance } from "./parse";
export { schemaFactory as schema, schemaFactory as literal, enum } from "./factory";
export {
  recursive,
} from "./advanced/recursive";
export {
  strict,
  deepStrict,
  strip,
  deepStrip,
  noValidation,
} from "./modifiers";
export {
  isAsync,
  js_safe as safe,
  js_safeAsync as safeAsync,
} from "./operations";
export { array } from "./composites";
// `nullish` accepts null | undefined (the 3-member union) — distinct from
// `nullable` (js_nullable) above, which handles null only.
export { nullable as nullish } from "./refinements";
export {
  compactColumns,
} from "./advanced/compactColumns";
export {
  dict,
  dict as record,
  object,
  shape,
  tuple,
  pattern,
  trim,
  gt,
  gte,
  lt,
  lte,
  multipleOf,
  minLength,
  maxLength,
  length,
  empty,
  nonEmpty,
} from "./refinements";
export {
  meta,
  brand,
} from "./modifiers";
export { jsonStringWithSpace } from "./advanced/json";
export { list } from "./advanced/list";
export {
  toJSONSchema,
  fromJSONSchema,
  extendJSONSchema,
  enableStandardJSONSchema,
} from "./jsonschema";
export { inputExpression } from "./base";
export { outputExpression } from "./parse";

// ── ReScript binding surface (extra names, not part of index.d.ts) ───────────
//
// Only APIs with no public-JS equivalent live here; everything else in S.res
// binds the public names directly (or wraps them in ReScript). `$res_` marks
// the exports as ReScript-binding internals — `~res_` would be clearer, but
// ReScript externals only accept valid JS identifiers as names.

export {
  pathToArray as $res_pathToArray,
  pathFromArray as $res_pathFromArray,
  pathFromLocation as $res_pathFromLocation,
  pathConcat as $res_pathConcat,
} from "./base";
export {
  // Async flavor of the public `assert` — no public JS equivalent
  // (`asyncDecoderAssert` is a different, callback-taking API).
  assertAsyncOrThrow as $res_assertAsyncOrThrow,
} from "./operations";
export {
  transform as $res_transform,
  Option_getOr as $res_Option_getOr,
  Option_getOrWith as $res_Option_getOrWith,
  Metadata_Id_make as $res_Metadata_Id_make,
  Metadata_get as $res_Metadata_get,
  Metadata_set as $res_Metadata_set,
} from "./modifiers";
export { option as $res_option } from "./composites";
export {
  nullAsOption as $res_nullAsOption,
  nullableAsOption as $res_nullableAsOption,
} from "./refinements";
// The ReScript-flavored schema factory (definer-callback ctx); the public JS
// `schema` takes a raw definition instead.
export { schemaDefiner as $res_schema } from "./factory";
