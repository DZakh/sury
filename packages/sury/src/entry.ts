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
// emits a CJS index.js into the artifact for the require condition). The extra
// ReScript-binding exports ($-prefixed) are invisible to TS users
// (index.d.ts is the curated surface) and tree-shake when unused like any
// other export.

import {
  baseSchema,
  type Builder,
  type Check,
  functionTag,
  getOrRethrow,
  globalConfig,
  type GlobalConfigOverride,
  initialDefaultFlag,
  initialOnAdditionalItems,
  inputExpression,
  type Internal,
  jsonName,
  isSchemaObject,
  objectTag,
  panic,
  pathEmpty,
  pathFromArray,
  stringify,
  stringTag,
  U,
  unknown,
  type Val
} from "./base";
import {
  B_contentDiffers,
  B_conversion,
  B_embed,
  B_invalidInputBuilder,
  B_invalidOperation,
  B_neverSlot
} from "./builder";
import {
 definitionToSchema,
 objectDecoder
} from "./composites";
import {
  codecTo,
  internalRefine,
  nullAsUnit,
  Option_getOr,
  Option_getOrWith
} from "./modifiers";
import {
 assertResult
} from "./operations";
import {
 getDecoder,
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
  unit as $unit,
} from "./primitives";
export { never_ as never } from "./parse";
export { json, jsonString } from "./advanced/json";
export { uint8Array } from "./advanced/uint8Array";
export { date } from "./advanced/date";
export { url } from "./advanced/url";
export { blob, file } from "./advanced/file";
export {
  isoDateTime,
  port,
  email,
  uuid,
  cuid,
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
  safe,
  safeAsync,
} from "./operations";
export { array, dict, dict as record } from "./composites";
export { schemaObject as object, schemaShape as shape, schemaTuple as tuple } from "./factory";
// `nullish` accepts null | undefined (the 3-member union) — distinct from
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
  toJSONSchema,
  fromJSONSchema,
  extendJSONSchema,
  enableStandardJSONSchema,
} from "./jsonschema";
export { inputExpression } from "./base";
export { outputExpression } from "./parse";

// ── Public JS/TS API implemented here (argument-shape adapters) ──────────────

// Spreading the own rest param straight through (`getDecoder(unknown,
// ...args)`) is a shape engines already optimize — an arity fast path here
// measured nothing, so these stay generic.
// @__NO_SIDE_EFFECTS__
export const parser = (...args: unknown[]) => getDecoder(unknown, ...args);

// @__NO_SIDE_EFFECTS__
export const asyncParser = (...args: unknown[]) => getDecoder(unknown, ...args, 1);

// @__NO_SIDE_EFFECTS__
export const asyncDecoder = (...args: unknown[]) => getDecoder(...args, 1);

// The 1-schema branch dodges a per-call allocation: `.map` builds a fresh
// array every call, which spreading a rest param does not. Chained (2+)
// schemas keep the generic map.
// @__NO_SIDE_EFFECTS__
export const encoder = (a: unknown, ...rest: unknown[]) =>
  rest.length
    ? getDecoder(...([a, ...rest] as Internal[]).map(reverse))
    : getDecoder(reverse(a as Internal));

// @__NO_SIDE_EFFECTS__
export const asyncEncoder = (a: unknown, ...rest: unknown[]) =>
  rest.length
    ? getDecoder(...([a, ...rest] as Internal[]).map(reverse), 1)
    : getDecoder(reverse(a as Internal), 1);

// `assert` and `is` accept both `(schema, data)` and `(data, schema)`, told
// apart by the Standard Schema marker. The truthiness guard keeps falsy data
// from throwing on the marker access, routing it to the data slot so
// validation fails with a proper Sury error.
export const assert = (a: unknown, b: unknown): unknown => {
  const aIsSchema = !!a && isSchemaObject(a);
  const schema = (aIsSchema ? a : b) as Internal;
  const data = aIsSchema ? b : a;
  return getDecoder(unknown, schema, assertResult)(data);
};

export const is = (a: unknown, b: unknown): boolean => {
  const aIsSchema = !!a && isSchemaObject(a);
  // Compiled outside the try: a conversion rejected at operation creation
  // means the schema can't check any value, so it throws rather than reading
  // as `false` — the same split `~standard.validate` makes.
  const operation = getDecoder(unknown, (aIsSchema ? a : b) as Internal, assertResult);
  try {
    operation(aIsSchema ? b : a);
    return true;
  } catch (exn) {
    // Rethrow anything that isn't a Sury validation failure.
    getOrRethrow(exn);
    return false;
  }
};

// @__NO_SIDE_EFFECTS__
export const union = (values: unknown[]) => unionFactory(values.map(definitionToSchema));
// The JSON Schema spelling of the same thing. A re-export rather than
// `const anyOf = union`, which would shed the purity annotation above.
export { union as anyOf };

// The decode-only shorthand leaves the encode direction undefined. It errors
// at operation creation, and unlike the never slot it stays a hard error
// inside a union too: skipping the variant silently would commit to a
// semantics the caller never chose.
const ambiguousEncode: Builder = (input: Val) =>
  B_invalidOperation(
    input,
    "Encoding is ambiguous when only a decode function is provided. Use S.to(target, {decode, encode})",
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
// link itself — `true` opens the direction's own source, `false` stores it.
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
  // and hand back the source unchanged — the conversion silently absent.
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
    encode = ambiguousEncode;
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
    // ReScript type is `t<'any>` — a variable that unifies with whatever the
    // coder returns, so the seam against it carries nothing to trust. Same
    // carve-out B_conversion makes for a literal target, one level up: the
    // untrustworthy side can be either end of the pair, and only `to` sees
    // both.
    outputSeam = fromRescript && schema !== unknown && target !== unknown;
    decode = conversionBuilder("decode", decodeSlot, !outputSeam);
    encode = conversionBuilder("encode", encodeSlot, !outputSeam);
    // Each reading names what its direction does to its own source, so the two
    // directions can't both open (or both store) — there would be no side of
    // the link left holding the payload — and a reading opposite the built-in
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
      from.name === jsonName ||
      target.name === jsonName
    ) {
      return panic(`Can't pick a reading for this link. Use {decode, encode} coders instead`);
    }
  }
  // Chaining a schema to itself would append a second copy of its own chain,
  // re-decoding the value it just produced. Resolving the slots first is what
  // makes the all-"auto" spelling behave exactly like the coder-less one — and
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
  // claims the target's result — the very case a reading exists for is a target
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
  return codecTo(schema, target, decode, encode);
};

// @__NO_SIDE_EFFECTS__
export const refine = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  refineOptions?: { error?: string; path?: string[] },
) => {
  const message = refineOptions?.error ?? "Refinement failed";
  const extraPath =
    refineOptions?.path !== U ? pathFromArray(refineOptions.path) : pathEmpty;
  return internalRefine(schema, (_: Internal) => (input: Val): Check[] => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar: string) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(U, extraPath, message),
      },
    ];
  });
};

// @__NO_SIDE_EFFECTS__
export const optional = (definition: unknown, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
  const schema = unionFactory([unit, definitionToSchema(definition)]);
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
  // TODO: maybeOr should be part of the unit schema
  if (maybeOr !== U) {
    const schema2 = unionFactory([nullAsUnit, schema]);
    if (typeof maybeOr === functionTag) {
      return Option_getOrWith(schema2, maybeOr as () => unknown);
    } else {
      return Option_getOr(schema2, maybeOr);
    }
  } else {
    return unionFactory([nullLiteral, schema]);
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
    // Recomputed, not cached — this path throws, and the temp measured larger.
    const bad = isMergeable(s1) ? s2 : s1;
    // TODO: Can theoretically support the transformed case
    return panic(`Can't merge ${bad.to ? "transformed " : ""}${inputExpression(bad)}`);
  }
  const properties = { ...s1.properties!, ...s2.properties! };

  const mut = baseSchema(objectTag, false, objectDecoder);

  // TODO: Merge to required fields
  mut.required = Object.keys(properties);
  mut.properties = properties;
  mut.additionalItems = s1.additionalItems;
  return mut;
};

// PORT-NOTE: kept the source's `global` name — legal as a module-scoped
// export even though Node types declare a `global` var.
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
  pathToArray as $pathToArray,
  pathFromArray as $pathFromArray,
  pathFromLocation as $pathFromLocation,
  pathConcat as $pathConcat,
} from "./base";
export {
  // Async flavor of the public `assert`, which has no public JS equivalent.
  assertAsyncOrThrow as $assertAsyncOrThrow,
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
