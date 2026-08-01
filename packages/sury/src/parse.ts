import {
  baseSchema,
  type Builder,
  copySchema,
  type Encoder,
  type Flag,
  flagAsync,
  flagDisableNanNumberValidation,
  flagUnsafeHas,
  getOrRethrow,
  globalConfig,
  initSchema,
  instanceTag,
  type Internal,
  isLiteral,
  neverTag,
  numberTag,
  objectTag,
  panic,
  pathConcat,
  pathDynamic,
  pathEmpty,
  reversedKey,
  s,
  setHas,
  tagFlagArray,
  tagFlagBigint,
  tagFlagBoolean,
  tagFlagInstance,
  tagFlagNaN,
  tagFlagNull,
  tagFlagNumber,
  tagFlagObject,
  tagFlags,
  tagFlagString,
  tagFlagSymbol,
  tagFlagUndefined,
  tagFlagUnknown,
  U,
  unknown,
  unknownTag,
  updateOutput,
  type Val,
  valFlagAsync,
  valKey,
  valueOptions,
} from "./base";
import {
  B_embedInvalidInput,
  B_inlineConst,
  B_markOutput,
  B_merge,
  B_next,
  B_operationArg,
  B_refine,
  B_scope,
  B_unsupportedDecode,
  failInvalidType,
  noopOperation,
  operationArgVar,
} from "./builder";
import {
  instanceofCond,
  isArrayCond,
  nanCond,
  objectTagCond,
  typeofCond,
} from "./primitives";

export const parse = (input: Val): Val => {
  let result: Val = input;
  let appliedEncoderRef: Encoder | undefined = U;
  let loopCount = 0;
  while (!result.io || result.e.to) {
    const appliedEncoder: Encoder | undefined = appliedEncoderRef;
    appliedEncoderRef = U;
    const loopInput = result;

    loopCount = loopCount + 1;

    if (loopCount > 50) {
      const error = new Error("Loop count exceeded 50");
      throw error;
    }

    if (loopInput.e["$defs"]) {
      if (loopInput.g.d) {
        Object.assign(loopInput.g.d!, loopInput.e["$defs"]!);
      } else {
        loopInput.g.d = loopInput.e["$defs"];
      }
    }

    if (
      flagUnsafeHas(
        loopInput.f,
        valFlagAsync,
      ) // FIXME: is the `valFlagAsync` check alone sufficient here, or was
        // there originally a second condition (dropped during the ReScript
        // port) that this branch also needs? Unconfirmed — see PR discussion.
    ) {
      const operationInputVar = loopInput.v();

      const operationInput = B_scope(loopInput);
      const operationOutput = parse(operationInput);
      const operationCode = B_merge(operationOutput);
      if (operationInput.i !== operationOutput.i || operationCode !== "") {
        result = B_next(
          loopInput,
          `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
          operationOutput.s,
          operationOutput.e,
        );
      } else {
        result = B_refine(loopInput, operationOutput.s, U, operationOutput.e);
      }
      result.f |= valFlagAsync;
      result.io = true;
    } else if (loopInput.io) {
      // It's guaranteed that to is not undefined, because it's checked in the while condition
      const to = loopInput.e.to!;
      if (loopInput.e.parser !== U) {
        result = loopInput.e.parser(loopInput);
      } else {
        result = B_refine(result, U, U, to);
      }
    } else {
      const maybeEncoder = loopInput.s.encoder;
      if (
        maybeEncoder &&
        maybeEncoder !== appliedEncoder &&
        loopInput.s !== loopInput.e &&
        loopInput.e.type !== unknownTag &&
        // A `noValidation` target (S.assert's result sentinel) throws the value
        // away, so there is nothing for an encoder to re-represent.
        !loopInput.e.noValidation
      ) {
        result = maybeEncoder!(loopInput, loopInput.e);
      }

      // If encoder didn't change the value, we can decode it,
      // otherwise let's start the loop from the beginning
      if (loopInput !== result) {
        appliedEncoderRef = maybeEncoder!;
      } else {
        result = loopInput.e.decoder(loopInput);

        // Primitive decoder (no internal transforms): apply refiners here.
        // Advanced decoders set isOutput themselves and own refiner application.
        if (!result.io) {
          result = B_markOutput(result, result);
        }
      }
    }
  }

  return result;
}
export const parseDynamic = (input: Val): Val => {
  try {
    return parse(input);
  } catch (exn) {
    const error = getOrRethrow(exn);
    // For the case parent must always be present
    error.path = pathConcat(
      input.p !== U ? input.p.path : pathEmpty,
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
    if (flagUnsafeHas(flag, flagAsync) && !isAsync && !defs) {
      inlinedOutput = `Promise.resolve(${inlinedOutput})`;
    }

    const inlinedFunction = `${operationArgVar}=>{${code}return ${inlinedOutput}}`;

    const fn = new Function("e", "s", `return ${inlinedFunction}`)(input.g.e, s);
    fn.embedded = input.g.e;
    return fn;
  }
}
export const getOutputSchema = (schema: Internal): Internal => {
  if (schema.to !== U) {
    return getOutputSchema(schema.to);
  } else {
    return schema;
  }
}
// @__NO_SIDE_EFFECTS__
export const reverse = (schema: Internal): Internal => {
  const schemaRecord = schema as unknown as Record<string, Internal>;
  if (reversedKey in schemaRecord) {
    return schemaRecord[reversedKey]!;
  } else {
    let reversedHead: Internal | undefined = U;
    let current: Internal | undefined = schema;

    while (current) {
      const mut = copySchema(current!);
      const next = mut.to;
      if (reversedHead === U) {
        delete mut.to;
      } else {
        mut.to = reversedHead;
      }
      const parser = mut.parser;
      if (mut.serializer !== U) {
        mut.parser = mut.serializer;
      } else {
        delete mut.parser;
      }
      if (parser !== U) {
        mut.serializer = parser;
      } else {
        delete mut.serializer;
      }
      // Swap inputRefiner and refiner
      const refiner = mut.refiner;
      if (mut.inputRefiner !== U) {
        mut.refiner = mut.inputRefiner;
      } else {
        delete mut.refiner;
      }
      if (refiner !== U) {
        mut.inputRefiner = refiner;
      } else {
        delete mut.inputRefiner;
      }
      const fromDefault = mut.fromDefault;
      if (mut.default !== U) {
        mut.fromDefault = mut.default;
      } else {
        delete mut.fromDefault;
      }
      if (fromDefault !== U) {
        mut.default = fromDefault;
      } else {
        delete mut.default;
      }
      if (mut.items !== U) {
        mut.items = mut.items.map(reverse);
      }
      if (mut.properties !== U) {
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
      if (typeof mut.additionalItems === objectTag) {
        mut.additionalItems = reverse(mut.additionalItems as Internal);
      }
      if (mut.anyOf !== U) {
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
      if (mut["$defs"] !== U) {
        const defs = mut["$defs"];
        const reversedDefs: Record<string, Internal> = {};
        const defsKeys = Object.keys(defs);
        for (let idx = 0; idx <= defsKeys.length - 1; idx++) {
          const key = defsKeys[idx]!;
          reversedDefs[key] = reverse(defs[key]!);
        }
        mut["$defs"] = reversedDefs;
      }
      reversedHead = mut;
      current = next;
    }

    // Use defineProperty even though it's slower
    // but it improves logging experience a lot
    const r = reversedHead!;
    valueOptions[valKey] = r;
    Object.defineProperty(schema, reversedKey, valueOptions as PropertyDescriptor);
    valueOptions[valKey] = schema;
    Object.defineProperty(r, reversedKey, valueOptions as PropertyDescriptor);
    return r;
  }
}

// A plain (non-arrow, to keep `arguments`) function so call sites can pass
// getDecoder(s1, s2[, s3][, flag]) with any number of schemas plus an
// optional trailing flag — the body reads `arguments` directly; the declared
// rest param (unused, hence `_`) exists only to make that call shape typecheck.
// @__NO_SIDE_EFFECTS__
export function getDecoder(..._args: unknown[]): (from: unknown) => unknown {
  const args = arguments as unknown as unknown[];
  let idx = 0;
  let flag: Flag | undefined = U;
  let keyRef = "";
  let maxSeq = 0;
  let cacheTarget: Internal | undefined = U;

  while (flag === U) {
    const arg = args[idx];
    if (!arg) {
      const f = globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else if (typeof arg === numberTag) {
      const f = (arg as Flag) | globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else {
      const schema: Internal = arg as Internal;
      const seq = schema.seq!;
      if (seq > maxSeq) {
        maxSeq = seq;
        cacheTarget = schema;
      }
      keyRef = keyRef + seq + "-";
      idx = idx + 1;
    }
  }

  if (cacheTarget === U) {
    return panic("No schema provided for decoder.");
  } else {
    const key = keyRef;
    const cacheTargetRecord = cacheTarget as unknown as Record<string, (from: unknown) => unknown>;
    if (key in cacheTargetRecord) {
      return cacheTargetRecord[key]!;
    } else {
      let schema: Internal = args[idx - 1] as Internal;
      for (let i = idx - 2; i >= 0; i--) {
        const to = schema;
        schema = updateOutput(args[i] as Internal, (mut) => {
          mut.to = to;
        });
      }
      const f = compileDecoder(schema, schema, flag!, U);
      // Reusing the same object makes it a little bit faster
      valueOptions[valKey] = f;
      // Use defineProperty, so the cache keys are not enumerable
      Object.defineProperty(cacheTarget, key, valueOptions as PropertyDescriptor);
      return f as (from: unknown) => unknown;
    }
  }
}

export const nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE";

const neverBuilderFn = (input: Val): Val => {
  // Carry `never` as the val's own schema, not the input's: nothing gets past
  // this branch, so a union built from its cases' output schemas must not list
  // the input type as something the union can produce.
  const output = B_refine(input, never_, U, never_);
  output.cp = B_embedInvalidInput(input) + ";";
  return output;
}
export const never_: Internal = /* @__PURE__ */ initSchema(neverTag, (schema) => {
  schema.decoder = neverBuilderFn;
});

export const nestedOptionParser: Builder = (input: Val) => {
  const nextSchema = input.e.to!;
  return B_next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties![nestedLoc]!.const as string}}`,
    nextSchema,
    nextSchema
  );
};

export const instanceDecoder: Builder = (input: Val) => {
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
};

// @__NO_SIDE_EFFECTS__
export const instance = (class_: unknown): Internal => {
  const mut = baseSchema(instanceTag, true);
  mut.class = class_;
  mut.decoder = instanceDecoder;
  return mut;
}

// Type-narrow condition for a union variant, built from the shared atoms with no
// per-type factory reference — so unused type decoders tree-shake.
//
// Cross-module contract: a decoder's own type narrow must be exactly what this
// returns for its tag. A union group's shared narrow stands in for its members'
// type checks, so a decoder that narrowed more loosely — an object mode dropping
// `!Array.isArray` because it rebuilds the value anyway — would widen what the
// case accepts past what its acceptance mask claims, and arrays would dispatch
// to an object member.
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
