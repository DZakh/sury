// The single public entry for both surfaces:
//  - JS/TS consumers import the package root and get the public API under its
//    documented names (typed by the hand-written S.d.ts).
//  - The ReScript bindings module (S.res) binds to this same module with
//    `@module("sury") external` declarations, so both languages share one
//    runtime instance (one Exn identity, one schema cache, one seq counter).
//
// Built by scripts/pack.ts into src/S.mjs (the publish step additionally
// emits a CJS S.js into the artifact for the require condition). Every eager
// schema constant is PURE-annotated so unused ones tree-shake out of consumer
// bundles; the extra ReScript-binding exports ($res_*-named) are invisible
// to TS users (S.d.ts is the curated surface) and tree-shake when unused like
// any other export.

import {
  string as stringFactory,
  bool as boolFactory,
  int as intFactory,
  float as floatFactory,
  bigint as bigintFactory,
  symbol as symbolFactory,
  nan as nanFactory,
  unit as unitFactory,
  void_ as voidFactory,
} from "./primitives";
import { never_ } from "./parse";
import { nullAsUnit as nullAsUnitFactory } from "./operations";
import {
  json as jsonFactory,
  jsonString as jsonStringFactory,
  uint8Array as uint8ArrayFactory,
  date as dateFactory,
  isoDateTime as isoDateTimeFactory,
  port as portFactory,
  email as emailFactory,
  uuid as uuidFactory,
  cuid as cuidFactory,
  url as urlFactory,
} from "./formats";

// ── Eager schema constants (shared by both surfaces) ─────────────────────────

export const string = /* @__PURE__ */ stringFactory();
const _boolean = /* @__PURE__ */ boolFactory();
export { _boolean as boolean, _boolean as bool };
const _int32 = /* @__PURE__ */ intFactory();
export { _int32 as int32, _int32 as int };
const _number = /* @__PURE__ */ floatFactory();
export { _number as number, _number as float };
export const bigint = /* @__PURE__ */ bigintFactory();
export const symbol = /* @__PURE__ */ symbolFactory();
export const never = /* @__PURE__ */ never_();
export const nan = /* @__PURE__ */ nanFactory();
const _void = /* @__PURE__ */ voidFactory();
export { _void as void };
export const $res_unit = /* @__PURE__ */ unitFactory();
export const $res_nullAsUnit = /* @__PURE__ */ nullAsUnitFactory();
export const json = /* @__PURE__ */ jsonFactory();
export const jsonString = /* @__PURE__ */ jsonStringFactory();
export const uint8Array = /* @__PURE__ */ uint8ArrayFactory();
export const date = /* @__PURE__ */ dateFactory();
export const isoDateTime = /* @__PURE__ */ isoDateTimeFactory();
export const port = /* @__PURE__ */ portFactory();
export const email = /* @__PURE__ */ emailFactory();
export const uuid = /* @__PURE__ */ uuidFactory();
export const cuid = /* @__PURE__ */ cuidFactory();
export const url = /* @__PURE__ */ urlFactory();
export {
  unknown,
  unknown as any,
  errorClass as Error,
  __setExnId as $res_setExnId,
} from "./schema";

// ── Public JS/TS API (names match S.d.ts) ────────────────────────────────────

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
  strict,
  deepStrict,
  strip,
  deepStrip,
  noValidation,
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
  dict,
  dict as record,
  object,
  shape,
  tuple,
  pattern,
  trim,
} from "./refinements";
export { meta, brand, jsonStringWithSpace, list } from "./formats";
export {
  toJSONSchema,
  fromJSONSchema,
  extendJSONSchema,
  enableStandardJSONSchema,
  min,
  max,
  length,
} from "./jsonschema";
export { toExpression } from "./types";

// ── ReScript binding surface (extra names, not part of S.d.ts) ───────────────
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
} from "./path";
export {
  // Async flavor of the public `assert` — no public JS equivalent
  // (`asyncDecoderAssert` is a different, callback-taking API).
  assertAsyncOrThrow as $res_assertAsyncOrThrow,
  transform as $res_transform,
  Option_getOr as $res_Option_getOr,
  Option_getOrWith as $res_Option_getOrWith,
  Metadata_Id_make as $res_Metadata_Id_make,
  Metadata_get as $res_Metadata_get,
  Metadata_set as $res_Metadata_set,
} from "./operations";
export { option as $res_option } from "./composites";
export {
  nullAsOption as $res_nullAsOption,
  nullableAsOption as $res_nullableAsOption,
} from "./refinements";
// The ReScript-flavored schema factory (definer-callback ctx); the public JS
// `schema` takes a raw definition instead.
export { schemaDefiner as $res_schema } from "./factory";
