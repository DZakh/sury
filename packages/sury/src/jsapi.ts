import {
  baseSchema,
  type Builder,
  type Check,
  flagDisableNanNumberValidation,
  functionTag,
  getOrRethrow,
  globalConfig,
  type GlobalConfigOverride,
  initialDefaultFlag,
  initialOnAdditionalItems,
  type Internal,
  isSchemaObject,
  objectTag,
  panic,
  pathEmpty,
  pathFromArray,
  stringify,
  stringTag,
  U,
  unknown,
  type Val,
} from "./base";
import {
  B_conversion,
  B_embed,
  B_invalidInputBuilder,
  B_invalidOperation,
  B_neverSlot,
  B_refine,
} from "./builder";
import { objectDecoder } from "./composites";
import { definitionToSchema } from "./factory";
import {
  codecTo,
  internalRefine,
  nullAsUnit,
  Option_getOr,
  Option_getOrWith,
} from "./modifiers";
import { assertResult } from "./operations";
import { getDecoder, reverse } from "./parse";
import { nullLiteral, unit } from "./primitives";
import { unionFactory } from "./union";

// @__NO_SIDE_EFFECTS__
export const js_parser = (...args: unknown[]) => getDecoder(unknown, ...args);

// @__NO_SIDE_EFFECTS__
export const js_asyncParser = (...args: unknown[]) => getDecoder(unknown, ...args, 1);

// @__NO_SIDE_EFFECTS__
export const js_asyncDecoder = (...args: unknown[]) => getDecoder(...args, 1);

// @__NO_SIDE_EFFECTS__
export const js_encoder = (...args: unknown[]) => getDecoder(...(args as Internal[]).map(reverse));

// @__NO_SIDE_EFFECTS__
export const js_asyncEncoder = (...args: unknown[]) =>
  getDecoder(...(args as Internal[]).map(reverse), 1);

// Accepts both `(schema, data)` and `(data, schema)` arg orders. We tell them
// apart by the Standard Schema marker on a schema object. The truthiness guard
// keeps `null`/`undefined` data from throwing on the marker access, routing it
// to the data slot so validation fails with a proper Sury error.
export const js_assert = (a: unknown, b: unknown): unknown => {
  const aIsSchema = !!a && isSchemaObject(a);
  const schema = (aIsSchema ? a : b) as Internal;
  const data = aIsSchema ? b : a;
  return getDecoder(unknown, schema, assertResult)(data);
};

export const js_is = (a: unknown, b: unknown): boolean => {
  try {
    js_assert(a, b);
    return true;
  } catch (exn) {
    // Rethrow anything that isn't a Sury validation failure.
    getOrRethrow(exn);
    return false;
  }
};

// @__NO_SIDE_EFFECTS__
export const js_union = (values: unknown[]) => unionFactory(values.map(definitionToSchema));

// Rule 3: the decode shorthand's encode direction is a hard error at
// operation creation, even inside a union — silently skipping would commit to
// a semantics the user never chose.
const ambiguousEncode: Builder = (input: Val) =>
  B_invalidOperation(
    input,
    "Encoding is ambiguous when only a decode function is provided. Use S.to(target, {decode, encode})",
  );

// One codec slot resolved to its Builder: `"auto"` (and an omitted argument)
// is the built-in conversion, `"never"` the unreachable direction, a function
// a sync coder, `{async}` an async one. `junction` picks the seam the coder's
// result lands on (see B_conversion).
const conversionBuilder = (slot: unknown, junction?: boolean): Builder | undefined => {
  if (slot === "auto") {
    return U;
  } else if (slot === "never") {
    return B_neverSlot;
  } else if (typeof slot === functionTag) {
    return B_conversion(slot as (value: unknown) => unknown, false, junction);
  } else if (
    slot !== null &&
    typeof slot === objectTag &&
    typeof (slot as { async?: unknown }).async === functionTag &&
    Object.keys(slot as object).length === 1
  ) {
    return B_conversion(
      (slot as { async: (value: unknown) => Promise<unknown> }).async,
      true,
      junction,
    );
  } else {
    return panic(
      `Unknown conversion ${stringify(slot)} — expected a function, "auto", "never" or {async}`,
    );
  }
};

// @__NO_SIDE_EFFECTS__
export const js_to = (schema: Internal, target: Internal, custom?: unknown) => {
  let decode: Builder | undefined;
  let encode: Builder | undefined;
  let outputSeamCoder = false;
  if (typeof custom === functionTag) {
    decode = B_conversion(custom as (value: unknown) => unknown, false, true);
    encode = ambiguousEncode;
  } else if (custom !== U) {
    // The public TS surface is the junction seam ({decode, encode}); the
    // decodeToOutput/encodeFromOutput spellings are the output seam, kept out
    // of S.d.ts — they exist for the ReScript `~custom` adapter, whose record
    // is only typeable there. One spelling per direction, nothing else.
    const codecs = custom as Record<string, unknown>;
    const decodeJunction = codecs["decode"] !== U;
    const encodeJunction = codecs["encode"] !== U;
    if (
      !(decodeJunction || codecs["decodeToOutput"] !== U) ||
      !(encodeJunction || codecs["encodeFromOutput"] !== U)
    ) {
      return panic(
        `Both decode and encode are required for custom codecs — use "auto" for the built-in conversion`,
      );
    }
    if (Object.keys(codecs).length !== 2) {
      return panic(`Custom codecs take exactly one conversion per direction`);
    }
    decode = conversionBuilder(
      decodeJunction ? codecs["decode"] : codecs["decodeToOutput"],
      decodeJunction,
    );
    encode = conversionBuilder(
      encodeJunction ? codecs["encode"] : codecs["encodeFromOutput"],
      encodeJunction,
    );
    outputSeamCoder =
      (!decodeJunction && decode !== U && decode !== B_neverSlot) ||
      (!encodeJunction && encode !== U && encode !== B_neverSlot);
  }
  // Chaining a schema to itself would append a second copy of its own chain,
  // re-decoding the value it just produced. Custom coders still get a real
  // conversion step — the check runs after slot resolution so the all-"auto"
  // spelling behaves exactly like the coder-less one.
  if (schema === target && decode === U && encode === U) {
    return schema;
  }
  // Rule 4's guard: only the output seam is ambiguous on a target with its
  // own `.to` chain. A junction coder's result enters the chain's head, so
  // it stays legal — as do the "never"/"auto" slots, which place no coder.
  if (outputSeamCoder && target.to !== U) {
    return panic(`The target carries its own conversion — chain S.to explicitly`);
  }
  return codecTo(schema, target, decode, encode);
};

// @__NO_SIDE_EFFECTS__
export const js_refine = (
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

// An assert doesn't change the value, so the encode direction claims the
// reversed continuation outright instead of compiling a try/catch around an
// identity call.
const passthroughSlot: Builder = (input: Val) =>
  B_refine(input, input.e.to!, U, input.e.to!);

// @__NO_SIDE_EFFECTS__
export const js_asyncDecoderAssert = (
  schema: Internal,
  assertFn: (value: unknown) => Promise<unknown>,
) => {
  return codecTo(
    schema,
    unknown,
    B_conversion((v: unknown) => assertFn(v).then(() => v), true),
    passthroughSlot,
  );
};

// @__NO_SIDE_EFFECTS__
export const js_optional = (schema: Internal, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
  schema = unionFactory([schema, unit]);
  if (maybeOr !== U && typeof maybeOr === functionTag) {
    return Option_getOrWith(schema, maybeOr as () => unknown);
  } else if (maybeOr !== U) {
    return Option_getOr(schema, maybeOr);
  } else {
    return schema;
  }
};

// @__NO_SIDE_EFFECTS__
export const js_nullable = (schema: Internal, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
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

// @__NO_SIDE_EFFECTS__
export const js_merge = (s1: Internal, s2: Internal): Internal => {
  // PORT-NOTE: the source matches on the public `Object({...})` variants —
  // at runtime that's a `type === "object"` check plus field reads, ported
  // as explicit conditions below.
  let result: Internal | undefined;
  if (
    s1.type === objectTag &&
    s2.type === objectTag &&
    // Filter out S.record schemas
    typeof s1.additionalItems === stringTag &&
    typeof s2.additionalItems === stringTag &&
    !s1.to &&
    !s2.to
  ) {
    const properties = { ...s1.properties!, ...s2.properties! };

    const mut = baseSchema(objectTag, false);

    // TODO: Merge to required fields
    mut.required = Object.keys(properties);
    mut.properties = properties;
    mut.additionalItems = s1.additionalItems;
    mut.decoder = objectDecoder;
    result = mut;
  }
  if (result !== U) {
    return result;
  } else {
    return panic(
      "The merge supports only structured object schemas without transformations",
    );
  }
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
      ? flagDisableNanNumberValidation
      : initialDefaultFlag;
};
