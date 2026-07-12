import { nullLiteral, unit } from "./primitives";
import { GlobalConfigOverride, baseSchema, copySchema, getOrRethrow, globalConfig, initialDefaultFlag, initialOnAdditionalItems, panic, unknown, updateOutput } from "./schema";
import { B_embed, B_failWithArg, B_invalidInputBuilder, B_makeInvalidConversionDetails, B_next, B_varWithoutAllocation, EffectCtx, _var } from "./builder";
import { definitionToSchema } from "./factory";
import { objectDecoder, unionFactory } from "./composites";
import { Option_getOr, Option_getOrWith, getAssertResult, internalRefine, nullAsUnit, transform } from "./operations";
import { Check, Internal, Val, isSchemaObject } from "./types";
import { Builder } from "./builder";
import { flagDisableNanNumberValidation } from "./flags";
import { functionTag, objectTag, stringTag } from "./tags";
import { pathEmpty, pathFromArray } from "./path";
import { getDecoder, reverse } from "./parse";

export const js_parser = (...args: unknown[]) => getDecoder(unknown, ...args);

export const js_asyncParser = (...args: unknown[]) => getDecoder(unknown, ...args, 1);

export const js_asyncDecoder = (...args: unknown[]) => getDecoder(...args, 1);

export const js_encoder = (...args: unknown[]) => getDecoder(...(args as Internal[]).map(reverse));

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
  return getDecoder(unknown, schema, getAssertResult())(data);
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

export const js_union = (values: unknown[]) => unionFactory(values.map(definitionToSchema));

export const js_to = /* @__PURE__ */ (() => {
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
      )}(${input.i})}catch(x){${B_failWithArg(
        output,
        (e: unknown) => B_makeInvalidConversionDetails(input, target, e),
        `x`,
      )}}`;
      return output;
    };
  };

  return (
    schema: Internal,
    target: Internal,
    maybeDecoder?: (value: unknown) => unknown,
    maybeEncoder?: (target: unknown) => unknown,
  ) => {
    return updateOutput(schema, (mut) => {
      if (maybeEncoder !== undefined) {
        const targetMut = copySchema(target);
        targetMut.serializer = customBuilder(maybeEncoder);
        mut.to = targetMut;
      } else {
        mut.to = target;
      }
      if (maybeDecoder !== undefined) {
        mut.parser = customBuilder(maybeDecoder);
      }
    });
  };
})();

export const js_refine = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  refineOptions?: { error?: string; path?: string[] },
) => {
  const message = refineOptions?.error ?? "Refinement failed";
  const extraPath =
    refineOptions?.path !== undefined ? pathFromArray(refineOptions.path) : pathEmpty;
  return internalRefine(schema, (_: Internal) => (input: Val): Check[] => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar: string) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(undefined, extraPath, message),
      },
    ];
  });
};

const noop = <A>(a: A): A => a;
export const js_asyncDecoderAssert = (
  schema: Internal,
  assertFn: (value: unknown) => Promise<unknown>,
) => {
  return transform(schema, (_: EffectCtx) => {
    return {
      a: (v: unknown) => assertFn(v).then(() => v),
      s: noop,
    };
  });
};

export const js_optional = (schema: Internal, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
  schema = unionFactory([schema, unit()]);
  if (maybeOr !== undefined && typeof maybeOr === functionTag) {
    return Option_getOrWith(schema, maybeOr as () => unknown);
  } else if (maybeOr !== undefined) {
    return Option_getOr(schema, maybeOr);
  } else {
    return schema;
  }
};

export const js_nullable = (schema: Internal, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
  if (maybeOr !== undefined) {
    const schema2 = unionFactory([schema, nullAsUnit()]);
    if (typeof maybeOr === functionTag) {
      return Option_getOrWith(schema2, maybeOr as () => unknown);
    } else {
      return Option_getOr(schema2, maybeOr);
    }
  } else {
    return unionFactory([schema, nullLiteral()]);
  }
};

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
  if (result !== undefined) {
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
    override.defaultAdditionalItems !== undefined
      ? override.defaultAdditionalItems
      : initialOnAdditionalItems;
  globalConfig.f =
    override.disableNanNumberValidation === true
      ? flagDisableNanNumberValidation
      : initialDefaultFlag;
};
