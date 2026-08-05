import {
  baseSchema,
  type Builder,
  type Check,
  copySchema,
  flagDisableNanNumberValidation,
  flagUnionTransformContext,
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
  stringTag,
  U,
  unknown,
  updateOutput,
  type Val,
} from "./base";
import {
  _var,
  B_embed,
  B_failWithArg,
  B_invalidInputBuilder,
  B_makeInvalidConversionDetails,
  B_next,
  B_varWithoutAllocation,
} from "./builder";
import { objectDecoder } from "./composites";
import { definitionToSchema } from "./factory";
import {
  internalRefine,
  nullAsUnit,
  Option_getOr,
  Option_getOrWith,
  transform,
} from "./modifiers";
import { assertResult } from "./operations";
import { getDecoder, reverse } from "./parse";
import { nullLiteral, unit } from "./primitives";
import { unionFactory } from "./union";

// Spreading the own rest param straight through (`getDecoder(unknown,
// ...args)`) is a shape engines already optimize — an arity fast path here
// measured nothing, so these stay generic.
// @__NO_SIDE_EFFECTS__
export const js_parser = (...args: unknown[]) => getDecoder(unknown, ...args);

// @__NO_SIDE_EFFECTS__
export const js_asyncParser = (...args: unknown[]) => getDecoder(unknown, ...args, 1);

// @__NO_SIDE_EFFECTS__
export const js_asyncDecoder = (...args: unknown[]) => getDecoder(...args, 1);

// The 1-schema branch dodges a per-call allocation: `.map` builds a fresh
// array every call, which spreading a rest param does not. Chained (2+)
// schemas keep the generic map.
// @__NO_SIDE_EFFECTS__
export const js_encoder = (a: unknown, ...rest: unknown[]) =>
  rest.length
    ? getDecoder(...([a, ...rest] as Internal[]).map(reverse))
    : getDecoder(reverse(a as Internal));

// @__NO_SIDE_EFFECTS__
export const js_asyncEncoder = (a: unknown, ...rest: unknown[]) =>
  rest.length
    ? getDecoder(...([a, ...rest] as Internal[]).map(reverse), 1)
    : getDecoder(reverse(a as Internal), 1);

// `assert` and `is` accept both `(schema, data)` and `(data, schema)`, told
// apart by the Standard Schema marker. The truthiness guard keeps falsy data
// from throwing on the marker access, routing it to the data slot so
// validation fails with a proper Sury error.
export const js_assert = (a: unknown, b: unknown): unknown => {
  const aIsSchema = !!a && isSchemaObject(a);
  const schema = (aIsSchema ? a : b) as Internal;
  const data = aIsSchema ? b : a;
  return getDecoder(unknown, schema, assertResult)(data);
};

export const js_is = (a: unknown, b: unknown): boolean => {
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
export const js_union = (values: unknown[]) => unionFactory(values.map(definitionToSchema));

// FIXME: Test how it'll work if we have async var as input
// FIXME: Might not work well with object targets
const customBuilder = (fn: (value: unknown) => unknown): Builder => {
  return (input: Val): Val => {
    const target = input.e.to!;
    const outputVar = B_varWithoutAllocation(input.g);
    const output = B_next(input, outputVar, target, target);
    output.v = _var;
    output.cp = `let ${outputVar};try{${output.i}=${B_embed(
      input,
      fn,
    )}(${input.i})}catch(x){${
      input.g.o & flagUnionTransformContext
        ? `${B_embed(input, getOrRethrow)}(x);`
        : ""
    }${B_failWithArg(
      output,
      (e: unknown) => B_makeInvalidConversionDetails(input, target, e),
      `x`,
    )}}`;
    return output;
  };
};

// @__NO_SIDE_EFFECTS__
export const js_to = (
  schema: Internal,
  target: Internal,
  maybeDecoder?: (value: unknown) => unknown,
  maybeEncoder?: (target: unknown) => unknown,
) => {
  // Chaining a schema to itself would append a second copy of its own chain,
  // re-decoding the value it just produced. Custom coders still get a real
  // conversion step — only the coder-less spelling is a no-op.
  if (schema === target && !maybeDecoder && !maybeEncoder) {
    return schema;
  }
  return updateOutput(schema, (mut) => {
    if (maybeEncoder) {
      const targetMut = copySchema(target);
      targetMut.serializer = customBuilder(maybeEncoder);
      mut.to = targetMut;
    } else {
      mut.to = target;
    }
    if (maybeDecoder) {
      mut.parser = customBuilder(maybeDecoder);
    }
  });
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

const noop = <T>(a: T): T => a;
// @__NO_SIDE_EFFECTS__
export const js_asyncDecoderAssert = (
  schema: Internal,
  assertFn: (value: unknown) => Promise<unknown>,
) => {
  return transform(schema, () => {
    return {
      a: (v: unknown) => assertFn(v).then(() => v),
      s: noop,
    };
  });
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
