// The single public entry for both surfaces:
//  - JS/TS consumers import the package root and get the public API under its
//    documented names (typed by the hand-written index.d.ts).
//  - The ReScript bindings module (S.res) binds to this same module with
//    `@module("sury") external` declarations, so both languages share one
//    runtime instance (one Exn identity, one set of schema singletons, one
//    seq counter).
//
// Most of the surface is a re-export of the module that implements it. The
// exception is the adapter section below: a handful of public names exist only
// to give a core primitive the argument shape the public API documents
// (overload dispatch, an options object, a default value). Nothing but this
// entry ever calls them, so they are declared here rather than in a module of
// their own.
//
// Built by scripts/pack.ts into index.mjs (the publish step additionally
// emits a CJS index.jsonSchema into the artifact for the require condition). The extra
// ReScript-binding exports ($-prefixed) are invisible to TS users
// (index.d.ts is the curated surface) and tree-shake when unused like any
// other export.

import { schemaFactory } from "./factory";
import {
  baseSchema,
  type Builder,
  functionTag,
  globalConfig,
  type GlobalConfigOverride,
  initialDefaultFlag,
  initialOnAdditionalItems,
  inputExpression,
  isOptional,
  type Internal,
  objectTag,
  panic,
  type Path,
  stringify,
  stringTag,
  U,
  unknown,
  type Val
} from "./base";
import {
  B_contentDiffers,
  B_conversion,
  B_invalidOperation,
  B_neverSlot
} from "./builder";
import {
 definitionToSchema,
 objectDecoder
} from "./composites";
import {
  codecTo,
  linkTo,
  nullAsUnit,
  Option_getOr,
  Option_getOrWith,
  refine as refineCore,
} from "./modifiers";
import {
 getOutputSchema,
 reverse
} from "./parse";
import {
 nullLiteral,
 unit
} from "./primitives";
import {
 unionFactory
} from "./union";

// ── Schema singletons (shared by both surfaces) ──────────────────────────────
//
// Re-exports of module-level consts, each PURE-initialized at its declaration,
// so unused ones tree-shake out of consumer bundles.

export {
  string,
  bool as boolean,
  int as int32,
  integer,
  float as number,
  bigint,
  symbol,
  nan,
  void_ as void,
  unit as $unit,
} from "./primitives";
export { never_ as never } from "./parse";
export { json, jsonString } from "./advanced/json";
export { uint8Array } from "./advanced/uint8Array";
export { arrayBuffer } from "./advanced/arrayBuffer";
export { protobuf, toProtoOrThrow } from "./advanced/protobuf";
export { protobufField } from "./advanced/protobufField";
export { date } from "./advanced/date";
export { url } from "./advanced/url";
export { blob, file } from "./advanced/file";
export { formData } from "./advanced/formData";
export { env } from "./advanced/env";
export { urlSearchParams, queryString } from "./advanced/urlSearchParams";
export {
  isoDateTime,
  utcDateTime,
  port,
  email,
  uuid,
  uuidv4,
  uuidv6,
  uuidv7,
  cuid,
  cuid2,
  ulid,
  ksuid,
  xid,
  nanoid,
  e164,
  mac,
  hex,
  cidrv4,
  cidrv6,
  httpUrl,
  base64,
  base64url,
  uri,
  isoDate,
  isoTime,
  duration,
  hostname,
  idnHostname,
  ipv4,
  ipv6,
  uriReference,
  uriTemplate,
  iri,
  iriReference,
  idnEmail,
  jsonPointer,
  relativeJsonPointer,
} from "./refinements";
export { nullAsUnit as $nullAsUnit } from "./modifiers";
export {
  unknown,
  unknown as any,
  errorClass as Error,
  __setExnId as $setExnId,
} from "./base";

// ── Public JS/TS API (names match index.d.ts) ────────────────────────────────

export { reverse, instance } from "./parse";
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
  parseOrThrow,
  parseAsResult,
  parseAsPromiseOrReject,
  parseAsResultPromise,
  parseAsPromisableResult,
  decodeOrThrow,
  decodeAsResult,
  decodeAsPromiseOrReject,
  decodeAsResultPromise,
  decodeAsPromisableResult,
  encodeOrThrow,
  encodeAsResult,
  encodeAsPromiseOrReject,
  encodeAsResultPromise,
  encodeAsPromisableResult,
  makeInputOrThrow,
  makeInputAsResult,
  makeInputAsPromiseOrReject,
  makeInputAsResultPromise,
  makeInputAsPromisableResult,
  makeOutputOrThrow,
  makeOutputAsResult,
  makeOutputAsPromiseOrReject,
  makeOutputAsResultPromise,
  makeOutputAsPromisableResult,
  isInput,
  isOutput,
  isInputAsPromise,
  isOutputAsPromise,
  isEqualInput,
  isEqualOutput,
  assertInputOrThrow,
  assertOutputOrThrow,
  assertInputAsPromiseOrReject,
  assertOutputAsPromiseOrReject,
} from "./operations";
export { array, dict as record } from "./composites";
export { schemaObject as object, schemaShape as shape, schemaTuple as tuple } from "./factory";
// `nullish` accepts null | undefined (the 3-member union) - distinct from
// `nullable` below, which handles null only.
export { nullable as nullish } from "./refinements";
export {
  compactColumns,
} from "./advanced/compactColumns";
export {
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
  nonEmpty,
  minSize,
  maxSize,
  size,
} from "./refinements";
export {
  meta,
  brand,
} from "./modifiers";
export { jsonStringWithSpace } from "./advanced/json";
export { list } from "./advanced/list";
export {
  inputJSONSchema as toInputJSONSchemaOrThrow,
  outputJSONSchema as toOutputJSONSchemaOrThrow,
  fromJSONSchema as fromJSONSchemaOrThrow,
  extendJSONSchema,
  enableStandardJSONSchema,
} from "./jsonschema";
export { inputExpression as toInputExpression, pathToText } from "./base";
export { outputExpression as toOutputExpression } from "./parse";

// ── Public JS/TS API implemented here (argument-shape adapters) ──────────────

// Curried so the definition's type is inferred at the second call, which is
// what index.d.ts compares against the type argument. The type argument is
// erased, so the first call has nothing to do but hand back the factory.
// @__NO_SIDE_EFFECTS__
export const schemaOf = () => schemaFactory;

// @__NO_SIDE_EFFECTS__
export const union = (values: unknown[]) => unionFactory(values.map(definitionToSchema));
// The JSON Schema spelling of the same thing. A re-export rather than
// `const anyOf = union`, which would shed the purity annotation above.
export { union as anyOf };

// The decode-only shorthand leaves the encode direction undefined. It errors
// at operation creation, and unlike the never slot it stays a hard error
// inside a union too: skipping the variant silently would commit to a
// semantics the caller never chose.
const ambiguousEncode =
  (from: Internal, to: Internal): Builder =>
  (input: Val) =>
    B_invalidOperation(
      input,
      `Ambiguous encode for ${inputExpression(from)} -> ${inputExpression(to)}. Only decode is defined. Add encode to S.to as a function, "auto" or "never"`,
    );

// One codec slot resolved. `"auto"` (and an omitted argument)
// is `undefined`, which every caller reads as "no coder, use the built-in
// conversion". `junction` picks which seam the coder's result lands on (see
// B_conversion). An `{async}` object must carry that key alone: guessing past
// a typo would silently pick a different direction's semantics.
// `name` is the key the caller wrote, so the rejection names the direction
// they got wrong rather than the pair. `"pack"`/`"unpack"` are the odd pair out:
// they are not coders but a choice between a content link's two readings
// (CONTENT_CODEC_SPEC.md rule 1), so they resolve to a boolean that rides the
// link itself - `true` opens the direction's own source, `false` stores it.
const conversionBuilder = (
  name: string,
  slot: unknown,
  junction: boolean,
): Builder | boolean | undefined => {
  const async = (slot as { async?: unknown } | null)?.async;
  if (slot === "auto") {
    return U;
  } else if (slot === "never") {
    return B_neverSlot;
  } else if (slot === "unpack") {
    return true;
  } else if (slot === "pack") {
    return false;
  } else if (typeof slot === functionTag) {
    return B_conversion(slot as (value: unknown) => unknown, false, junction);
  } else if (typeof async === functionTag && Object.keys(slot as object).length === 1) {
    return B_conversion(async as (value: unknown) => Promise<unknown>, true, junction);
  } else {
    return panic(
      `Invalid ${name} ${stringify(slot)}. Expected a function, "auto", "never", "pack", "unpack" or {async: fn}`,
    );
  }
};

// @__NO_SIDE_EFFECTS__
export const to = (schema: Internal, target: Internal, custom?: unknown) => {
  // A misspelled export arrives as `undefined`, which used to link to nothing
  // and hand back the source unchanged - the conversion silently absent.
  if (!target) {
    return panic(`Expected a schema to convert to`);
  }
  let decode: Builder | boolean | undefined;
  let encode: Builder | boolean | undefined;
  let outputSeam = false;
  if (custom === "unpack" || custom === "pack") {
    decode = custom === "unpack";
    encode = custom === "pack";
  } else if (typeof custom === functionTag) {
    decode = B_conversion(custom as (value: unknown) => unknown, false, true);
    encode = ambiguousEncode(target, schema);
  } else if (custom) {
    const codecs = custom as Record<string, unknown>;
    // Two spellings, one per seam, never mixed: `{decode, encode}` is the
    // public TS surface, `{decodeToOutput, encodeFromOutput}` is what the
    // ReScript `~custom` adapter emits and is deliberately absent from
    // index.d.ts. The key count rejects a typo instead of reading it as a
    // missing direction.
    const toOutput = codecs["decodeToOutput"];
    const fromRescript = !!toOutput;
    const decodeSlot = fromRescript ? toOutput : codecs["decode"];
    const encodeSlot = fromRescript ? codecs["encodeFromOutput"] : codecs["encode"];
    if (!decodeSlot || !encodeSlot || Object.keys(codecs).length !== 2) {
      return panic(`Expected {decode, encode}. Use "auto" for the built-in conversion`);
    }
    // `S.any` is this very `unknown` schema under a second name, and its
    // ReScript type is `t<'any>` - a variable that unifies with whatever the
    // coder returns, so the seam against it carries nothing to trust. Same
    // carve-out B_conversion makes for a literal target, one level up: the
    // untrustworthy side can be either end of the pair, and only `to` sees
    // both.
    outputSeam = fromRescript && schema !== unknown && target !== unknown;
    decode = conversionBuilder("decode", decodeSlot, !outputSeam);
    encode = conversionBuilder("encode", encodeSlot, !outputSeam);
    // Each reading names what its direction does to its own source, so the two
    // directions can't both open (or both store) - there would be no side of
    // the link left holding the payload - and a reading opposite the built-in
    // conversion leaves that side still asking the question the reading just
    // answered. A coder opposite one is fine: it answers for itself.
  }
  if (typeof decode === "boolean" || typeof encode === "boolean") {
    if (decode === encode || decode === U || encode === U) {
      return panic(`Expected "pack" opposite "unpack"`);
    }
    const from = getOutputSchema(schema);
    if (
      from.content === U ||
      target.content === U ||
      !B_contentDiffers(from.content, target.content) ||
      from.isJson ||
      target.isJson
    ) {
      return panic(`Can't pick a reading for this link. Use {decode, encode} coders instead`);
    }
  }
  // Chaining a schema to itself would append a second copy of its own chain,
  // re-decoding the value it just produced. Resolving the slots first is what
  // makes the all-"auto" spelling behave exactly like the coder-less one - and
  // a reading is the same: there is nothing to pick between when both sides are
  // the same schema.
  if (schema === target && typeof decode !== functionTag && typeof encode !== functionTag) {
    return schema;
  }
  // An output-seam coder claims the target as its result, so a target that
  // still converts on its own would have that conversion skipped. The
  // junction seam feeds the target's chain instead, so it stays legal, as do
  // the slots that place no coder.
  // A reading is exempt with `B_neverSlot`: neither places a coder, so neither
  // claims the target's result - the very case a reading exists for is a target
  // that converts on its own.
  if (
    outputSeam &&
    target.to &&
    ((typeof decode === functionTag && decode !== B_neverSlot) ||
      (typeof encode === functionTag && encode !== B_neverSlot))
  ) {
    return panic(
      `The target already converts. Chain S.to instead of passing a custom codec`,
    );
  }
  // Interned when the third argument is bounded by construction - absent, or
  // one of the two reading strings. A coder is not: `{decode, encode}` and an
  // inline function are fresh objects, so keying on them would miss every time
  // and grow the list without bound.
  return custom === U || typeof custom === stringTag
    ? linkTo(schema, target, custom, decode as boolean, encode as boolean)
    : codecTo(schema, target, decode, encode);
};

// @__NO_SIDE_EFFECTS__
export const refine = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  refineOptions?: { error?: string; path?: Path },
) => refineCore(schema, refineCheck, refineOptions?.error, refineOptions?.path);

// @__NO_SIDE_EFFECTS__
export const optional = (definition: unknown, maybeOr: unknown): Internal => {
  const schema = unionFactory([definitionToSchema(definition), unit]);
  if (maybeOr !== U && typeof maybeOr === functionTag) {
    return Option_getOrWith(schema, maybeOr as () => unknown);
  } else if (maybeOr !== U) {
    return Option_getOr(schema, maybeOr);
  } else {
    return schema;
  }
};

// @__NO_SIDE_EFFECTS__
export const nullable = (definition: unknown, maybeOr: unknown): Internal => {
  const schema = definitionToSchema(definition);
  if (maybeOr !== U) {
    const schema2 = unionFactory([schema, nullAsUnit]);
    if (typeof maybeOr === functionTag) {
      return Option_getOrWith(schema2, maybeOr as () => unknown);
    } else {
      return Option_getOr(schema2, maybeOr);
    }
  } else {
    return unionFactory([schema, nullLiteral]);
  }
};

// A string additionalItems is what separates a plain object from an S.record,
// whose keys aren't known field-wise; `to` marks a transformed one, whose
// fields describe the output rather than what merging would produce.
const isMergeable = (s: Internal): boolean =>
  s.type === objectTag && typeof s.additionalItems === stringTag && !s.to;

// @__NO_SIDE_EFFECTS__
export const merge = (s1: Internal, s2: Internal): Internal => {
  if (!isMergeable(s1) || !isMergeable(s2)) {
    // Recomputed, not cached - this path throws, and the temp measured larger.
    const bad = isMergeable(s1) ? s2 : s1;
    return panic(`Can't merge ${bad.to ? "transformed " : ""}${inputExpression(bad)}`);
  }
  const properties = { ...s1.properties!, ...s2.properties! };

  const mut = baseSchema(objectTag, false, objectDecoder);
  mut.required = Object.keys(properties).filter((k) => !isOptional(properties[k]!));
  mut.properties = properties;
  mut.additionalItems = s1.additionalItems;
  return mut;
};

// A module export named `global` is legal; Node's `var global` is a different binding.
export const global = (override: GlobalConfigOverride): void => {
  globalConfig.a =
    override.defaultAdditionalItems !== U
      ? override.defaultAdditionalItems
      : initialOnAdditionalItems;
  globalConfig.f =
    override.disableNanNumberValidation === true
      ? 2
      : initialDefaultFlag;
};

// ── ReScript binding surface (extra names, not part of index.d.ts) ───────────
//
// Only APIs with no public-JS equivalent live here; everything else in S.res
// binds the public names directly (or wraps them in ReScript). The `$` prefix
// marks the exports as ReScript-binding internals while staying a valid JS
// identifier, which is all ReScript externals accept as names.

export {
  $parseAsResult,
  $parseAsResultPromise,
  $encodeAsResult,
  $encodeAsResultPromise,
  $makeAsResult,
  $makeAsResultPromise,
} from "./operations";
export {
  Option_getOr as $Option_getOr,
  Option_getOrWith as $Option_getOrWith,
  Metadata_Id_make as $Metadata_Id_make,
  Metadata_get as $Metadata_get,
  Metadata_set as $Metadata_set,
} from "./modifiers";
export { option as $option } from "./modifiers";
export {
  nullAsOption as $nullAsOption,
  nullableAsOption as $nullableAsOption,
} from "./refinements";
// The ReScript-flavored schema factory (definer-callback ctx); the public JS
// `schema` takes a raw definition instead.
export { schemaDefiner as $schema } from "./factory";
