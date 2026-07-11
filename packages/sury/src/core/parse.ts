import { instanceofCond, isArrayCond, nanCond, objectTagCond, setHas, typeofCond } from "./primitives.ts";
import { baseSchema, cached, copySchema, getOrRethrow, globalConfig, panic, reversedKey, unknown, updateOutput, valKey, valueOptions } from "./schema.ts";
import { B_Val_scope, B_embedInvalidInput, B_inlineConst, B_markOutput, B_merge, B_next, B_operationArg, B_refine, B_unsupportedDecode, failInvalidType, noopOperation, operationArgVar } from "./builder.ts";
import { Builder, Encoder, Flag, Internal, Tag, Val, flagAsync, flagDisableNanNumberValidation, flagUnsafeHas, instanceTag, isLiteral, neverTag, numberTag, objectTag, pathConcat, pathDynamic, pathEmpty, s, tagFlagArray, tagFlagBigint, tagFlagBoolean, tagFlagInstance, tagFlagNaN, tagFlagNull, tagFlagNumber, tagFlagObject, tagFlagString, tagFlagSymbol, tagFlagUndefined, tagFlagUnknown, tagFlags, unknownTag, valFlagAsync } from "./types.ts";
// Section: Sury.res lines 2256-2708
// parse / parseDynamic / isAsyncInternal / compileDecoder / getOutputSchema /
// reverse / getDecoder / nestedLoc / itemCode / neverBuilderFn / never_ /
// nestedOptionParser / instanceDecoder / instance / typeCheckCond
//
// TODO(integration): expects from earlier sections:
//   - `B` (Builder.B): B_Val_scope, B_next, B_refine, B_merge, B_markOutput,
//     B_operationArg, B_operationArgVar, B_unsupportedDecode,
//     B_embedInvalidInput, B_inlineConst, failInvalidType
//   - `Builder` const: Builder.make (identity cast), Builder.noopOperation
//   - `setHas` (Sury.res ~2137)
//   - cond atoms: `typeofCond`, `nanCond`, `isArrayCond`, `objectTagCond`,
//     `instanceofCond` (Sury.res ~1911-1915)
// Note on getDecoder2/getDecoder3: they are `@val external` self-references to
// `getDecoder` (it reads `arguments`). Call sites elsewhere become plain
// `getDecoder(s1, s2, flag?)` / `getDecoder(s1, s2, s3, flag?)` calls — no
// separate bindings are emitted here.
// =============================================================================

export const parse = (input: Val): Val => {
  let valRef: Val = input;
  let appliedEncoderRef: Encoder | undefined = undefined;
  let loopCount = 0;
  while (!valRef.io || (valRef.e.to as unknown as boolean)) {
    const appliedEncoder: Encoder | undefined = appliedEncoderRef;
    appliedEncoderRef = undefined;
    const loopInput = valRef;

    loopCount = loopCount + 1;

    // Console.log(loopInput)
    if (loopCount > 50) {
      const error = new Error("Loop count exceeded 100");
      throw error;
    }

    if (loopInput.e["$defs"] as unknown as boolean) {
      if (loopInput.g.d as unknown as boolean) {
        Object.assign(loopInput.g.d!, loopInput.e["$defs"]!);
      } else {
        loopInput.g.d = loopInput.e["$defs"];
      }
    }

    if (
      flagUnsafeHas(
        loopInput.f,
        valFlagAsync,
      ) /* FIXME: why was it needed? && step.contents !== #convert */
    ) {
      const operationInputVar = loopInput.v();

      const operationInput = B_Val_scope(loopInput);
      const operationOutput = parse(operationInput);
      const operationCode = B_merge(operationOutput);
      if (operationInput.i !== operationOutput.i || operationCode !== "") {
        valRef = B_next(
          loopInput,
          `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
          operationOutput.s,
          operationOutput.e,
        );
      } else {
        valRef = B_refine(loopInput, operationOutput.s, undefined, operationOutput.e);
      }
      valRef.f = (valRef.f | valFlagAsync);
      valRef.io = true;
    } else if (loopInput.io) {
      // It's guaranteed that to is not None, because it's checked in the while condition
      const to = loopInput.e.to!;
      if (loopInput.e.parser !== undefined) {
        valRef = loopInput.e.parser(loopInput);
      } else {
        valRef = B_refine(valRef, undefined, undefined, to);
      }
    } else {
      const maybeEncoder = loopInput.s.encoder;
      if (
        (maybeEncoder as unknown as boolean) &&
        maybeEncoder !== appliedEncoder &&
        loopInput.s !== loopInput.e &&
        loopInput.e.type !== unknownTag
      ) {
        valRef = maybeEncoder!(loopInput, loopInput.e);
      }

      // If encoder didn't change the value, we can decode it,
      // otherwise let's start the loop from the beginning
      if (loopInput !== valRef) {
        appliedEncoderRef = maybeEncoder!;
      } else {
        valRef = loopInput.e.decoder(loopInput);

        // Primitive decoder (no internal transforms): apply refiners here.
        // Advanced decoders set isOutput themselves and own refiner application.
        if (!valRef.io) {
          valRef = B_markOutput(valRef, valRef);
        }
      }
    }
  }

  return valRef;
}
export const parseDynamic = (input: Val): Val => {
  try {
    return parse(input);
  } catch (exn) {
    const error = getOrRethrow(exn);
    (error as unknown as Record<string, unknown>)["path"] =
      // For the case parent must always be present
      pathConcat(
        input.p !== undefined ? input.p.path : pathEmpty,
        pathConcat(pathConcat(input.path, pathDynamic), error.path),
      );

    throw error;
  }
}

export const isAsyncInternal = (
  schema: Internal,
  defs: Record<string, Internal> | undefined
): boolean => {
  try {
    const input = B_operationArg(unknown, schema, flagAsync, defs);
    const output = parse(input);
    const isAsync = flagUnsafeHas(output.f, valFlagAsync);
    schema.isAsync = isAsync;
    return isAsync;
  } catch (exn) {
    getOrRethrow(exn);
    return false;
  }
}
export const compileDecoder = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): (input: unknown) => unknown => {
  const input = B_operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);

  const output = parse(input);
  const code = B_merge(output);

  const isAsync = flagUnsafeHas(output.f, valFlagAsync);
  expected.isAsync = isAsync;
  const hasTransform = output.t === true;
  expected.hasTransform = hasTransform;

  if (
    code === "" &&
    (output === input || output.i === input.i) &&
    !flagUnsafeHas(flag, flagAsync)
  ) {
    return noopOperation;
  } else {
    let inlinedOutput = output.i;
    if (flagUnsafeHas(flag, flagAsync) && !isAsync && !(defs as unknown as boolean)) {
      inlinedOutput = `Promise.resolve(${inlinedOutput})`;
    }

    const inlinedFunction = `${operationArgVar}=>{${code}return ${inlinedOutput}}`;

    // Console.log(inlinedFunction)

    const fn = new Function("e", "s", `return ${inlinedFunction}`)(input.g.e, s);
    (fn as unknown as Record<string, unknown>)["embedded"] = input.g.e;
    return fn;
  }
}
export const getOutputSchema = (schema: Internal): Internal => {
  if (schema.to !== undefined) {
    return getOutputSchema(schema.to);
  } else {
    return schema;
  }
}
// FIXME: Define it as a schema property
export const reverse = (schema: Internal): Internal => {
  if (reversedKey in (schema as unknown as Record<string, unknown>)) {
    return (schema as unknown as Record<string, unknown>)[reversedKey] as Internal;
  } else {
    let reversedHead: Internal | undefined = undefined;
    let current: Internal | undefined = schema;

    while (current as unknown as boolean) {
      const mut = copySchema(current!);
      const next = mut.to;
      if (reversedHead === undefined) {
        delete mut.to;
      } else {
        mut.to = reversedHead;
      }
      const parser = mut.parser;
      if (mut.serializer !== undefined) {
        mut.parser = mut.serializer;
      } else {
        delete mut.parser;
      }
      if (parser !== undefined) {
        mut.serializer = parser;
      } else {
        delete mut.serializer;
      }
      // Swap inputRefiner and refiner
      const refiner = mut.refiner;
      if (mut.inputRefiner !== undefined) {
        mut.refiner = mut.inputRefiner;
      } else {
        delete mut.refiner;
      }
      if (refiner !== undefined) {
        mut.inputRefiner = refiner;
      } else {
        delete mut.inputRefiner;
      }
      const fromDefault = mut.fromDefault;
      if (mut.default !== undefined) {
        mut.fromDefault = mut.default;
      } else {
        delete mut.fromDefault;
      }
      if (fromDefault !== undefined) {
        mut.default = fromDefault;
      } else {
        delete mut.default;
      }
      if (mut.items !== undefined) {
        mut.items = mut.items.map(reverse);
      }
      if (mut.properties !== undefined) {
        const properties = mut.properties;
        const newProperties: Record<string, Internal> = {};
        const keys = Object.keys(properties);
        for (let idx = 0; idx <= keys.length - 1; idx++) {
          const key = keys[idx]!;
          newProperties[key] = reverse(properties[key]!);
        }
        mut.properties = newProperties;
      }
      // Skip tuple
      if ((typeof mut.additionalItems as Tag) === objectTag) {
        mut.additionalItems = reverse(mut.additionalItems as unknown as Internal);
      }
      if (mut.anyOf !== undefined) {
        const anyOf = mut.anyOf;
        const has: Record<string, boolean> = {};
        const newAnyOf: Internal[] = [];
        for (let idx = 0; idx <= anyOf.length - 1; idx++) {
          const s = anyOf[idx]!;
          const reversed = reverse(s);
          newAnyOf.push(reversed);
          setHas(has, reversed.type);
        }
        mut.has = has;
        mut.anyOf = newAnyOf;
      }
      if (mut["$defs"] !== undefined) {
        const defs = mut["$defs"];
        const reversedDefs: Record<string, Internal> = {};
        for (let idx = 0; idx <= Object.keys(defs).length - 1; idx++) {
          const key = Object.keys(defs)[idx]!;
          reversedDefs[key] = reverse(defs[key]!);
        }
        mut["$defs"] = reversedDefs;
      }
      reversedHead = mut;
      current = next;
    }

    // Use defineProperty even though it's slower
    // but it improves logging experience a lot
    // for some reason Wallaby still shows the property
    const r = reversedHead!;
    valueOptions[valKey] = r;
    Object.defineProperty(schema, reversedKey, valueOptions as PropertyDescriptor);
    valueOptions[valKey] = schema;
    Object.defineProperty(r, reversedKey, valueOptions as PropertyDescriptor);
    return r;
  }
}

// PORT-NOTE: The ReScript signature `(~s1 as _, ~flag as _=?)` discards its
// labeled args and the body reads `arguments` directly — so this is a plain
// (non-arrow, to keep `arguments`) function with dummy params for arity.
// getDecoder2/getDecoder3 call sites become getDecoder(s1, s2[, s3][, flag]).
export function getDecoder(
  _s1?: unknown,
  _s2?: unknown,
  _s3?: unknown,
  _flag?: unknown
): (from: unknown) => unknown {
  const args = arguments as unknown as unknown[];
  let idx = 0;
  let flag: Flag | undefined = undefined;
  let keyRef = "";
  let maxSeq = 0;
  let cacheTarget: Internal | undefined = undefined;

  while (flag === undefined) {
    const arg = args[idx];
    if (!(arg as unknown as boolean)) {
      const f = globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else if ((typeof arg as Tag) === numberTag) {
      const f = (arg as unknown as Flag) | globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else {
      const schema: Internal = arg as unknown as Internal;
      const seq: number = schema.seq as unknown as number;
      if (seq > maxSeq) {
        maxSeq = seq;
        cacheTarget = schema;
      }
      keyRef = keyRef + (seq as unknown as string) + "-";
      idx = idx + 1;
    }
  }

  if (cacheTarget === undefined) {
    return panic("No schema provided for decoder.");
  } else {
    const key = keyRef;
    if (key in (cacheTarget as unknown as Record<string, unknown>)) {
      return (cacheTarget as unknown as Record<string, unknown>)[key] as (
        from: unknown
      ) => unknown;
    } else {
      let schema: Internal = args[idx - 1] as unknown as Internal;
      for (let i = idx - 2; i >= 0; i--) {
        const to = schema;
        schema = updateOutput(args[i] as unknown as Internal, (mut) => {
          mut.to = to;
        });
      }
      const f = compileDecoder(
        schema,
        schema,
        flag!,
        0 as unknown as Record<string, Internal> | undefined
      );
      // Reusing the same object makes it a little bit faster
      valueOptions[valKey] = f;
      // Use defineProperty, so the cache keys are not enumerable
      Object.defineProperty(cacheTarget, key, valueOptions as PropertyDescriptor);
      return f as (from: unknown) => unknown;
    }
  }
}

export const nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE";

// @unboxed — runtime value is the string or the array itself.
export type ItemCode = string | string[];

export const neverBuilderFn = (input: Val): Val => {
  const output = B_refine(input, undefined, undefined, never_());
  output.cp = B_embedInvalidInput(input) + ";";
  return output;
}
export const never_ = (): Internal => {
  return cached(neverTag as string, neverTag, (s) => {
    s.decoder = neverBuilderFn;
  });
}

export const nestedOptionParser: Builder = ((input: Val) => {
  const nextSchema = input.e.to!;
  return B_next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties![nestedLoc]!.const as unknown as string}}`,
    nextSchema,
    nextSchema
  );
});

export const instanceDecoder: Builder = ((input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refine(input, input.e, [
      {
        c: instanceofCond(input, input.e.class),
        f: failInvalidType,
      },
    ]);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === input.e.class) {
    return input;
  } else {
    return B_unsupportedDecode(input, input.s, input.e);
  }
});

export const instance = (class_: unknown): Internal => {
  const mut = baseSchema(instanceTag, true);
  mut.class = class_;
  mut.decoder = instanceDecoder;
  return mut;
}

// Type-narrow condition for a union variant, built from the shared atoms with no
// per-type factory reference — so unused type decoders tree-shake.
export const typeCheckCond = (input: Val, schema: Internal, inputVar: string): string => {
  const tagFlag = tagFlags[schema.type]!;
  if (flagUnsafeHas(tagFlag, tagFlagObject)) {
    return `${objectTagCond(inputVar)}&&!${isArrayCond(inputVar)}`;
  } else if (flagUnsafeHas(tagFlag, tagFlagArray)) {
    return isArrayCond(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagInstance)) {
    return instanceofCond(input, schema.class)(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagNumber)) {
    const typeofCheck = typeofCond(numberTag)(inputVar);
    if (flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
      return typeofCheck;
    } else {
      return `${typeofCheck}&&!${nanCond(inputVar)}`;
    }
  } else if (flagUnsafeHas(tagFlag, tagFlagNaN)) {
    return nanCond(inputVar);
  } else if (flagUnsafeHas(tagFlag, (tagFlagUndefined | tagFlagNull))) {
    // null/undefined reuse literalDecoder's inline-const form (=== null / void 0)
    return `${inputVar}===${B_inlineConst(input, schema)}`;
  } else if (
    flagUnsafeHas(
      tagFlag,
      tagFlagString | tagFlagBoolean | tagFlagBigint | tagFlagSymbol
    )
  ) {
    // literals reuse this typeof check; their per-const check stays in the case body
    return typeofCond(schema.type)(inputVar);
  } else {
    // Unreachable: catch-all tags use the `unknown` narrow, never this path.
    return "";
  }
}
// =============================================================================
// Section 05: object/tuple/array/dict/union decoders & encoders
