// The single public entry for both surfaces:
//  - JS/TS consumers import the package root and get the public API under its
//    documented names (typed by the hand-written S.d.ts).
//  - The ReScript bindings module (S.res) binds to this same module with
//    `@module("sury") external` declarations, so both languages share one
//    runtime instance (one Exn identity, one schema cache, one seq counter).
//
// Built by scripts/pack.ts into src/S.mjs (+ the ESM dev src/S.js twin; the
// publish step overwrites the artifact's S.js with a CJS build). Every eager
// schema constant is PURE-annotated so unused ones tree-shake out of consumer
// bundles; the extra ReScript-binding exports (res_*, Metadata_*, path*, …)
// are invisible to TS users (S.d.ts is the curated surface) and tree-shake
// when unused like any other export.

import {
  string as stringFactory,
  bool as boolFactory,
  int as intFactory,
  float as floatFactory,
  bigint as bigintFactory,
  symbol as symbolFactory,
  nan as nanFactory,
  unit as unitFactory,
} from "./core/primitives.ts";
import { never_ } from "./core/parse.ts";
import { nullAsUnit as nullAsUnitFactory } from "./core/operations.ts";
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
} from "./core/formats.ts";

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
const _never = /* @__PURE__ */ never_();
export { _never as never };
export const nan = /* @__PURE__ */ nanFactory();
const _void = /* @__PURE__ */ unitFactory();
export { _void as void, _void as unit };
export const nullAsUnit = /* @__PURE__ */ nullAsUnitFactory();
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
export { unknown, unknown as any, errorClass as Error, errorClass, __setExnId } from "./core/schema.ts";

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
} from "./core/jsapi.ts";
export { getDecoder as decoder, reverse, instance } from "./core/parse.ts";
export {
  js_schema as schema,
  // The ReScript-flavored schema factory (definer-callback ctx); the public
  // JS `schema` above takes a raw definition instead.
  schemaFactory as res_schema,
  literal,
  enum,
} from "./core/factory.ts";
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
} from "./core/operations.ts";
export { array } from "./core/composites.ts";
// `nullish` accepts null | undefined (the 3-member union) — distinct from
// `nullable` (js_nullable) above, which handles null only.
export { nullable as nullish } from "./core/refinements.ts";
// The ReScript-flavored plain `to` (no custom coders); the public JS `to`
// above is the options variant.
export { to as res_to } from "./core/formats.ts";
export {
  compactColumns,
  dict,
  dict as record,
  object,
  shape,
  tuple,
  pattern,
  trim,
} from "./core/refinements.ts";
export { meta, brand, jsonStringWithSpace, list } from "./core/formats.ts";
export {
  toJSONSchema,
  fromJSONSchema,
  extendJSONSchema,
  enableStandardJSONSchema,
  min,
  max,
  length,
} from "./core/jsonschema.ts";
export { toExpression } from "./core/types.ts";

// ── ReScript binding surface (extra names, not part of S.d.ts) ───────────────

export {
  pathToArray,
  pathFromArray,
  pathFromLocation,
  pathConcat,
} from "./core/types.ts";
export {
  parseOrThrow,
  parseAsyncOrThrow,
  assertOrThrow,
  assertAsyncOrThrow,
  decodeOrThrow,
  decodeAsyncOrThrow,
  decoder1,
  asyncDecoder1,
  // The ReScript-flavored decoder/asyncDecoder reverse `from` before
  // compiling (decode FROM a schema's output space); the public JS `decoder`
  // is the raw variadic getDecoder.
  decoder as res_decoder,
  asyncDecoder as res_asyncDecoder,
  transform,
  // The ReScript-flavored refine (labeled error/path args); the public JS
  // `refine` above is the options-object variant.
  refine as res_refine,
  Option_getOr,
  Option_getOrWith,
  Metadata_Id_make,
  Metadata_get,
  Metadata_set,
} from "./core/operations.ts";
export { option } from "./core/composites.ts";
export {
  null_,
  nullAsOption,
  nullableAsOption,
  tuple1,
  tuple2,
  tuple3,
  floatMin,
  floatMax,
} from "./core/refinements.ts";
