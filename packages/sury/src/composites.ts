import { Literal_parse, isArrayCond, jsonName, objectTagCond, setHas, unit } from "./primitives";
import { baseSchema, getOrRethrow, panic, unknown, updateOutput } from "./schema";
import { getOutputSchema, nestedLoc, nestedOptionParser, never_, parse, parseDynamic, typeCheckCond } from "./parse";
import { B_addObjectField, B_addKey, B_scope, B_asyncVal, B_dynamicScope, B_embed, B_failWithArg, B_hoistChildChecks, B_hoistDecl, B_inlineConst, B_inlineLocation, B_isHoistable, B_makeInvalidInputDetails, B_markOutput, B_merge, B_mergeWithPathPrepend, B_next, B_nextConst, B_pushCheck, B_refine, B_throw, B_unsupportedDecode, B_varWithoutAllocation, Builder, _notVar, _notVarAtParent, _var, failInvalidType } from "./builder";
import { AdditionalItems, Check, ErrorDetails, Internal, SuryErrorRecord, Val, immutableEmptyArray, immutableEmptyObject, isLiteral, isOptional } from "./types";
import { flagUnsafeHas, valFlagAsync, valFlagNone } from "./flags";
import { pathConcat, pathFromInlinedLocation } from "./path";
import { Tag, arrayTag, nullTag, numberTag, objectTag, tagFlagArray, tagFlagFunction, tagFlagInstance, tagFlagNaN, tagFlagNever, tagFlagNull, tagFlagObject, tagFlagRef, tagFlagUndefined, tagFlagUnion, tagFlagUnknown, tagFlags, undefinedTag, unionTag, unknownTag } from "./tags";

// An object/array val (`makeObjectVal`'s result) reuses the plain `Val`
// shape — there's no separate "object val" type.

// Narrows the dict-value-schema-or-mode union down to the schema case.
const isItemSchema = (x: AdditionalItems | undefined): x is Internal =>
  x !== undefined && typeof x !== "string";

type CheckCache = { contents: Check[] | undefined };

export const makeObjectVal = (prev: Val, schema: Internal): Val => {
  return {
    prev,
    v: _notVar,
    i: "",
    f: valFlagNone,
    s: (schema.type === arrayTag
      ? {
          type: arrayTag,
          items: [],
          additionalItems: "strict",
          decoder: arrayDecoder,
        }
      : {
          type: objectTag,
          required: [],
          properties: Object.create(null),
          additionalItems: "strict",
          decoder: objectDecoder,
        }) as Internal,
    e: prev.e,
    d: Object.create(null),
    t: true,
    cp: "",
    hd: "",
    path: prev.path,
    g: prev.g,
  };
}
export const completeObjectVal = (objectVal: Val): Val => {
  const isArray = objectVal.s.type === arrayTag;
  let inline = "";
  let promiseAllContent = "";
  let optionalSettingCode: ((objectVar: string) => string) | undefined = undefined;

  const keys = Object.keys(objectVal.d!);

  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const val = objectVal.d![key]!;
    if (flagUnsafeHas(val.f, valFlagAsync)) {
      promiseAllContent = promiseAllContent + val.i + ",";
    }
    if (val.o) {
      const existingFn = optionalSettingCode as ((objectVar: string) => string) | undefined;
      optionalSettingCode = (objectVar: string) => {
        return (
          (existingFn === undefined ? "" : existingFn(objectVar)) +
          `if(${val.v()}!==void 0){${objectVar}[${B_inlineLocation(objectVal.g, key)}]=${val.i}}`
        );
      };
    } else {
      inline =
        inline +
        (isArray ? `${val.i}` : `${B_inlineLocation(objectVal.g, key)}:${val.i}`) +
        ",";
    }
  }

  objectVal.i = isArray ? "[" + inline + "]" : "{" + inline + "}";

  // FIXME: Test whether re-asserting `additionalItems = "strict"` here is
  // needed, now that the object's properties are already fully assembled.
  const valWithRequired = objectVal;

  if (promiseAllContent) {
    // FIXME: Test how this interacts with optional fields and fix if broken.
    const operationInput = B_scope(valWithRequired);
    operationInput.io = true;
    const operationOutput = parse(operationInput);
    const operationCode = B_merge(operationOutput);

    if (operationCode === "" && promiseAllContent === `${operationOutput.i},`) {
      valWithRequired.i = operationOutput.i;
    } else {
      valWithRequired.i = `Promise.all([${promiseAllContent}]).then(([${promiseAllContent}])=>{${operationCode}return ${operationOutput.i}})`;
    }
    valWithRequired.f |= valFlagAsync;
    valWithRequired.s = operationOutput.s;
    valWithRequired.e = operationOutput.e;
    valWithRequired.io = true;
    return valWithRequired;
  } else {
    if (optionalSettingCode === undefined) {
      return valWithRequired;
    } else {
      const code = optionalSettingCode(valWithRequired.v());
      const output = B_refine(valWithRequired);
      output.cp = output.cp + code;
      return output;
    }
  }
}
export const array = (item: Internal): Internal => {
  const itemInternal = item;
  const mut = baseSchema(arrayTag, itemInternal.r === itemInternal);
  mut.additionalItems = itemInternal;
  mut.items = immutableEmptyArray as Internal[];
  mut.decoder = arrayDecoder;
  return mut;
}
export const arrayDecoder = (unknownInput: Val): Val => {
  const isUnion = unknownInput.u!;
  const expectedSchema = unknownInput.e;
  const unknownInputTagFlag = tagFlags[unknownInput.s.type]!;
  const expectedItems = expectedSchema.items!;
  const expectedLength = expectedItems.length;

  let input: Val;
  if (flagUnsafeHas(unknownInputTagFlag, (tagFlagUnknown | tagFlagArray))) {
    const isArrayInput = flagUnsafeHas(unknownInputTagFlag, tagFlagArray);
    let schema: Internal;
    if (!isArrayInput) {
      schema = array(unknown);
    } else {
      schema = unknownInput.s;
    }
    const checks: Check[] = [];
    if (!isArrayInput) {
      checks.push({
        c: isArrayCond,
        f: failInvalidType,
      });
    }

    const schemaAdditionalItems = schema.additionalItems;
    const isExactSize = isItemSchema(schemaAdditionalItems)
      ? false
      : schema.items!.length === expectedLength;

    if (!isExactSize) {
      const expectedAdditionalItems = expectedSchema.additionalItems;
      if (expectedAdditionalItems === "strict") {
        checks.push({
          c: (inputVar) => `${inputVar}.length===${expectedLength}`,
          f: failInvalidType,
        });
      } else if (expectedAdditionalItems === "strip") {
        checks.push({
          c: (inputVar) => `${inputVar}.length>=${expectedLength}`,
          f: failInvalidType,
        });
      }
    }

    // Apply refine also when there are no checks,
    // so literals for union cases don't mutate input
    // FIXME: This should be removed and validation attached to output instead
    if (checks.length > 0) {
      input = B_refine(unknownInput, schema, checks);
    } else {
      input = B_refine(unknownInput, schema);
    }
  } else {
    input = B_unsupportedDecode(unknownInput, unknownInput.s, expectedSchema);
  }

  let output: Val;
  const expectedAdditionalItems = expectedSchema.additionalItems;
  if (isItemSchema(expectedAdditionalItems)) {
    const itemSchema = expectedAdditionalItems;
    if (itemSchema === unknown) {
      output = input;
    } else {
      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);

      const itemInput = B_dynamicScope(input, iteratorVar);
      const itemOutput = parseDynamic(itemInput);
      const hasTransform = itemOutput.t!;
      const output2 = hasTransform
        ? // The next `.to` segment decodes from this schema — item-output, not expectedSchema (#284)
          B_next(input, `new Array(${inputVar}.length)`, array(itemOutput.s))
        : B_refine(input, expectedSchema);

      const itemCode = B_mergeWithPathPrepend(
        itemOutput,
        input,
        iteratorVar,
        hasTransform ? () => B_addKey(output2, iteratorVar, itemOutput) : undefined,
      );

      if (hasTransform || itemCode !== "") {
        output2.cp =
          output2.cp +
          `for(let ${iteratorVar}=${expectedLength};${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`;
      }

      if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
        output = B_asyncVal(output2, `Promise.all(${output2.i})`);
      } else {
        output = output2;
      }
    }
  } else {
    const objectVal = makeObjectVal(input, expectedSchema);
    let shouldRecreateInput: boolean;
    {
      const ai = expectedSchema.additionalItems;
      // Since we have a check validating the exact properties existence
      if (ai === "strict") {
        shouldRecreateInput = false;
      } else if (ai === "strip") {
        const inputAi = input.s.additionalItems;
        shouldRecreateInput = isItemSchema(inputAi) ? true : input.s.items!.length !== expectedLength;
      } else {
        shouldRecreateInput = true;
      }
    }

    for (let idx = 0; idx < expectedLength; idx++) {
      const schema = expectedItems[idx]!;
      const key = String(idx);
      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion; // We want to control validation on the decoder side
      const itemOutput = parse(itemInput);

      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }

      B_addObjectField(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t!;
      }
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      // Same stale-schema class as #284/#252: carry expectedSchema, not
      // input.schema (which may be a minimal union dispatch narrow), so a
      // pending `.to(json)` conversion routes through the fixed-items path
      const o = B_refine(input, expectedSchema);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
}
export const objectDecoder = (unknownInput: Val): Val => {
  const isUnion = unknownInput.u!;
  const expectedSchema = unknownInput.e;

  const unknownInputTagFlag = tagFlags[unknownInput.s.type]!;

  let input: Val;
  if (flagUnsafeHas(unknownInputTagFlag, (tagFlagUnknown | tagFlagObject))) {
    const isObjectInput = flagUnsafeHas(unknownInputTagFlag, tagFlagObject);
    let schema: Internal;
    if (!isObjectInput) {
      // TODO: Use dictFactory here
      const mut = baseSchema(objectTag, false);
      mut.properties = immutableEmptyObject as Record<string, Internal>;
      mut.additionalItems = unknown;
      schema = mut;
    } else {
      schema = unknownInput.s;
    }
    const checks: Check[] = [];
    if (!isObjectInput) {
      checks.push({
        c: objectTagCond,
        f: failInvalidType,
      });
      if (expectedSchema.additionalItems !== "strip") {
        // For strip case we recreate the value
        // For other cases we might optimize it,
        // this is why the check is a must have
        checks.push({
          c: (inputVar) => `!${isArrayCond(inputVar)}`,
          f: failInvalidType,
        });
      }
    }

    // Apply refine also when there are no checks,
    // so literals for union cases don't mutate input
    if (checks.length > 0) {
      input = B_refine(unknownInput, schema, checks);
    } else {
      input = B_refine(unknownInput, schema);
    }
  } else {
    input = B_unsupportedDecode(unknownInput, unknownInput.s, expectedSchema);
  }

  // The target's value schema when it's a dict (additionalProperties), else None
  // for a fixed-property object target.
  const expectedAdditionalItems = expectedSchema.additionalItems;
  const dictItem: Internal | undefined = isItemSchema(expectedAdditionalItems)
    ? expectedAdditionalItems
    : undefined;
  // Only a dict source can be iterated dynamically (`for..in`). A fixed-property
  // object source coerced into a dict target reuses the static object-literal
  // construction below, driven by the source's known keys.
  const inputAdditionalItems = input.s.additionalItems;
  const sourceIsDict = isItemSchema(inputAdditionalItems);

  let output: Val;
  // dict<unknown> target: any object/dict is already a valid value, pass through.
  if (dictItem !== undefined && dictItem === unknown) {
    output = input;
  } else if (dictItem !== undefined && sourceIsDict) {
    const inputVar = input.v();
    const keyVar = B_varWithoutAllocation(input.g);
    const itemInput = B_dynamicScope(input, keyVar);
    const itemOutput = parseDynamic(itemInput);

    const hasTransform = itemOutput.t!;
    const output2 = hasTransform
      ? // The next `.to` segment decodes from this schema — item-output, not expectedSchema (#284)
        B_next(input, "{}", dictFactory(itemOutput.s))
      : B_refine(input, expectedSchema);

    const itemCode = B_mergeWithPathPrepend(
      itemOutput,
      input,
      keyVar,
      hasTransform ? () => B_addKey(output2, keyVar, itemOutput) : undefined,
    );

    if (hasTransform || itemCode !== "") {
      output2.cp = output2.cp + `for(let ${keyVar} in ${inputVar}){${itemCode}}`;
    }

    if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
      const resolveVar = B_varWithoutAllocation(output2.g);
      const rejectVar = B_varWithoutAllocation(output2.g);
      const asyncParseResultVar = B_varWithoutAllocation(output2.g);
      const counterVar = B_varWithoutAllocation(output2.g);
      const outputVar = output2.v();
      output = B_asyncVal(
        output2,
        `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`,
      );
    } else {
      output = output2;
    }
  } else if (dictItem !== undefined) {
    const itemSchema = dictItem;
    // Encode a fixed-property object into a dict: build an object literal from
    // the SOURCE's keys, coercing every value to the dict's value schema.
    // `completeObjectVal` drops a field that is still optional after coercion.
    // (A dict source took the dynamic branch above, so the source is an object.)
    const objectVal = makeObjectVal(input, expectedSchema);
    const keys = Object.keys(input.s.properties!);
    for (let idx = 0; idx < keys.length; idx++) {
      const key = keys[idx]!;
      const itemInput = valGet(input, key);
      itemInput.e = itemSchema;
      itemInput.io = false;
      itemInput.u = isUnion;
      B_addObjectField(objectVal, key, parse(itemInput));
    }
    output = completeObjectVal(objectVal);
  } else {
    // Build a fixed-property object target (from a dict or object source).
    const properties = expectedSchema.properties!;
    const keys = Object.keys(properties);
    const keysCount = keys.length;

    const objectVal = makeObjectVal(input, expectedSchema);
    let shouldRecreateInput: boolean;
    {
      const ai = expectedSchema.additionalItems;
      // Since we have a check validating the exact properties existence
      if (ai === "strict") {
        shouldRecreateInput = false;
      } else if (ai === "strip") {
        shouldRecreateInput =
          sourceIsDict || Object.keys(input.s.properties!).length !== keysCount;
      } else {
        shouldRecreateInput = true;
      }
    }

    // FIXME: hack — detect "JSON-sourced object" via additionalItems=json
    // (set by jsonEncoderFn) and patch the field read inline to coalesce
    // `??null`. The proper fix is for the JSON pipeline to treat missing
    // object keys as the option's empty sentinel, instead of leaving
    // objectDecoder to sniff the source and rewrite codegen by hand:
    //   - jsonEncoderFn rewrites the option arm from `v===void 0` to
    //     `v===null` because JSON has no undefined,
    //   - but `i[key]` for a missing key returns undefined, so the
    //     rewritten arm rejects `{}` for `{foo: option<...>}`.
    // Detection is fragile (string-compares the schema name) and only
    // covers the union-with-undefined shape; fold this into a shared
    // JSON option representation post-release.
    const isJsonParent = isItemSchema(inputAdditionalItems)
      ? inputAdditionalItems.name === jsonName
      : false;

    for (let idx = 0; idx < keysCount; idx++) {
      const key = keys[idx]!;
      const schema = properties[key]!;

      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion; // We want to control validation on the decoder side
      if (isJsonParent && schema.type === unionTag && schema.has![undefinedTag]) {
        itemInput.i = `(${itemInput.i}??null)`;
      }

      const itemOutput = parse(itemInput);

      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }

      B_addObjectField(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t!;
      }
    }

    if (expectedSchema.additionalItems === "strict" && isItemSchema(inputAdditionalItems)) {
      const keyVar = B_varWithoutAllocation(objectVal.g);
      B_hoistDecl(input, keyVar);
      objectVal.cp = objectVal.cp + `for(${keyVar} in ${input.v()}){if(`;
      if (keys.length === 0) {
        objectVal.cp = objectVal.cp + "true";
      } else {
        for (let idx = 0; idx < keys.length; idx++) {
          const key = keys[idx]!;
          if (idx !== 0) {
            objectVal.cp = objectVal.cp + "&&";
          }
          objectVal.cp = objectVal.cp + `${keyVar}!==${B_inlineLocation(input.g, key)}`;
        }
      }
      objectVal.cp =
        objectVal.cp +
        `){${B_failWithArg(
          input,
          (excessFieldName: string) =>
            ({
              code: "unrecognized_keys",
              path: objectVal.path,
              reason: `Unrecognized key "${excessFieldName}"`,
              keys: [excessFieldName],
            }) as ErrorDetails,
          keyVar,
        )}}}`;
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      // The value was just validated against expectedSchema — carry it as
      // the val's schema instead of input.schema, which may be a minimal
      // union dispatch narrow ({properties:{}, additionalItems: unknown}).
      // Keeping the narrow mis-routed a pending `.to(json)` conversion
      // into the dict path, which rejects undefined optional fields (#252)
      const o = B_refine(input, expectedSchema);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
}

export const dictFactory = (item: Internal): Internal => {
  const mut = baseSchema(objectTag, item.r === item);
  mut.properties = immutableEmptyObject as Record<string, Internal>;
  mut.additionalItems = item;
  mut.decoder = objectDecoder;
  return mut;
}

export const unionToKey = (schema: Internal): string => {
  return flagUnsafeHas(tagFlags[schema.type]!, tagFlagInstance)
    ? (schema.class as { name: string })["name"]
    : schema.type;
}

export const unionIsPriority = (tagFlag: number, byKey: Record<string, unknown[]>): boolean => {
  return (
    (flagUnsafeHas(tagFlag, (tagFlagArray | tagFlagInstance)) &&
      objectTag in byKey) ||
    (flagUnsafeHas(tagFlag, tagFlagNaN) && numberTag in byKey)
  );
}

// Whether decoding a value already known to be of the schema type
// is a noop — no transformation anywhere in the schema tree.
// Recursive refs are conservatively treated as transforming
export const unionIsSelfDecodeNoop = (schema: Internal): boolean => {
  const additionalItems = schema.additionalItems;
  return (
    schema.to === undefined &&
    schema.parser === undefined &&
    !flagUnsafeHas(tagFlags[schema.type]!, tagFlagRef) &&
    (schema.anyOf !== undefined ? schema.anyOf.every(unionIsSelfDecodeNoop) : true) &&
    (schema.items !== undefined ? schema.items.every(unionIsSelfDecodeNoop) : true) &&
    (schema.properties !== undefined
      ? Object.values(schema.properties).every(unionIsSelfDecodeNoop)
      : true) &&
    (additionalItems !== undefined && typeof additionalItems !== "string"
      ? unionIsSelfDecodeNoop(additionalItems)
      : true)
  );
}

export const unionIsWiderSchema = (schemaAnyOf: Internal[], inputAnyOf: Internal[]): boolean => {
  return inputAnyOf.every((inputSchema, idx) => {
    const schema = schemaAnyOf[idx];
    if (schema !== undefined) {
      return (
        !flagUnsafeHas(
          tagFlags[inputSchema.type]!,
          tagFlagArray | tagFlagInstance | tagFlagRef | tagFlagUnion | tagFlagObject,
        ) &&
        inputSchema.type === schema.type &&
        inputSchema.const === schema.const &&
        inputSchema.to === undefined
      );
    } else {
      return false;
    }
  });
}

// The union's own `.to` chain which is applied per case during decoding.
// None when the union has a custom parser owning the `.to` conversion
export const unionGetToPerCase = (schema: Internal): Internal | undefined => {
  return schema.parser === undefined && schema.to !== undefined ? schema.to : undefined;
}

// Whether a union-typed input can be decoded by dispatching
// over its variants with `.to(target)` appended to each
export const unionCanDispatchPerVariant = (inputAnyOf: Internal[], target: Internal): boolean => {
  return (
    // S.json and recursive targets keep their dedicated union-input handling
    !flagUnsafeHas(tagFlags[getOutputSchema(target).type]!, tagFlagRef) &&
    !(
      target.type === unionTag &&
      target.anyOf!.some((v) => flagUnsafeHas(tagFlags[v.type]!, tagFlagRef))
    ) &&
    // Variants with transformations or recursive refs (option machinery,
    // transformed unions) aren't supported per-variant yet
    !inputAnyOf.some(
      (v) =>
        v.to !== undefined ||
        v.parser !== undefined ||
        flagUnsafeHas(tagFlags[v.type]!, tagFlagRef),
    )
  );
}

// Re-drives the source union with `.to(target)` appended, so its decoder
// dispatches per variant and each variant converts to the target
// independently (the documented per-source-variant algorithm)
export const unionPerVariantVal = (input: Val, target: Internal): Val => {
  return B_refine(
    input,
    unknown,
    undefined,
    updateOutput<Internal>(input.s, (mut) => {
      mut.to = target;
    }),
  );
}

// Applied by the parse loop when a union-typed val
// meets a different expected schema
export const unionEncoder = (input: Val, target: Internal): Val => {
  const inputAnyOf = input.s.anyOf!;
  if (
    target.type === unionTag &&
    unionGetToPerCase(target) === undefined &&
    unionIsWiderSchema(target.anyOf!, inputAnyOf)
  ) {
    // The target union decoder passes a narrower union input through as-is
    return input;
  } else if (unionCanDispatchPerVariant(inputAnyOf, target)) {
    return unionPerVariantVal(input, target);
  } else {
    return input;
  }
}

export const unionDecoder: Builder = (input: Val) => {
  const selfSchema = input.e;
  let schemas = selfSchema.anyOf!;
  const initialInputTagFlag = tagFlags[input.s.type]!;

  const toPerCase = unionGetToPerCase(selfSchema);

  if (
    // The input val is already of the union type (trusted self-decode).
    // Only allowed when no variant transforms the value
    (input.s === selfSchema &&
      toPerCase === undefined &&
      schemas.every(unionIsSelfDecodeNoop)) ||
    (flagUnsafeHas(initialInputTagFlag, tagFlagUnion) &&
      unionIsWiderSchema(schemas, input.s.anyOf!) &&
      toPerCase === undefined) ||
    (input.io! && input.e === input.s)
  ) {
    return input;
  } else {
    if (
      flagUnsafeHas(initialInputTagFlag, tagFlagUnion) ||
      (input.s.encoder === undefined && flagUnsafeHas(initialInputTagFlag, tagFlagRef))
    ) {
      input.s = unknown;
    }

    let activeKeyRef = "";
    if (
      !flagUnsafeHas(
        initialInputTagFlag,
        ((tagFlagUnion | tagFlagRef) | tagFlagUnknown),
      )
    ) {
      const sourceKey = unionToKey(input.s);
      let hasNull = false;
      let hasUndefined = false;
      const len = schemas.length;
      let i = 0;
      while (activeKeyRef === "" && i < len) {
        const s = schemas[i]!;
        if (unionToKey(s) === sourceKey) {
          activeKeyRef = sourceKey;
        } else if (s.type === nullTag) {
          hasNull = true;
        } else if (s.type === undefinedTag) {
          hasUndefined = true;
        }
        i = i + 1;
      }
      if (activeKeyRef === "") {
        if (flagUnsafeHas(initialInputTagFlag, tagFlagUndefined) && hasNull) {
          activeKeyRef = nullTag;
        } else if (flagUnsafeHas(initialInputTagFlag, tagFlagNull) && hasUndefined) {
          activeKeyRef = undefinedTag;
        }
      }
    }
    const activeKey = activeKeyRef;

    const initialInline = input.i;

    const fail = (caught: string) => {
      return `${B_embed(
        input,
        // Reads `arguments`, so this must stay a `function` expression, not an arrow.
        function () {
          const args = arguments;
          B_throw(
            B_makeInvalidInputDetails(
              selfSchema,
              unknown,
              input.path,
              args[0],
              true,
              args.length > 1 ? (Array.from(args).slice(1) as SuryErrorRecord[]) : undefined,
            ),
          );
        },
      )}(${input.v()}${caught})`;
    };

    // Create a copy of the input val, so we can mutate it
    // It's still the same value though, until mutated
    const output = B_refine(input);
    const outputAnyOf: Internal[] = [];

    // Set when a single-case block fails at codegen time, so the caller
    // can drop the block and pass the embedded error along instead of
    // emitting a guaranteed runtime throw
    let staticBlockFailure = "";

    const getArrItemsCode = (arr: unknown[], isDeopt: boolean): string => {
      const typeValidationInput = arr[0] as Val;
      const typeValidationOutput = arr[1] as Val;

      let itemStart = "";
      let itemEnd = "";
      let itemNextElse = false;
      let itemNoop = "";
      let caught = "";

      // Accumulate schemas code by refinement (discriminant)
      // so if we have two schemas with the same discriminant
      // We can generate a single switch statement
      // with try/catch blocks for each item
      // If we come across an item without a discriminant
      // we need to dump all accumulated schemas in try block
      // and have the item without discriminant as catch all
      // If we come across an item without a discriminant
      // and without any code, it means that this item is always valid
      // and we should exit early
      // Each entry is either a single item's code, or an array of codes once
      // a second item shares the same discriminant — discriminated with
      // Array.isArray at the call site below.
      let byDiscriminant: Record<string, string | string[]> = {};

      const preItems = 2;
      let itemIdx = preItems;
      const lastIdx = arr.length - 1;
      while (itemIdx <= lastIdx) {
        // Copy it one more time, since every case decoder
        // might mutate the input
        const input = B_scope(typeValidationOutput);
        input.u = true;
        input.t = typeValidationOutput.t;
        input.io = false;
        input.e = arr[itemIdx] as Internal;

        const isLast = itemIdx === lastIdx;
        const isFirst = itemIdx === preItems;
        const isOnlyCase = isFirst && isLast;
        let withExhaustiveCheck = !isOnlyCase;

        let itemSkipped = false;
        let itemCodeRef = "";
        const itemCondRef = { contents: "" };
        try {
          const itemOutput = parse(input);
          outputAnyOf.push(itemOutput.s);

          itemCodeRef = B_merge(itemOutput, itemCondRef);

          if (itemOutput.t!) {
            output.t = true;
            if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
              output.f |= valFlagAsync;
            }
            const itemVar = typeValidationInput.v();
            if (itemOutput.i !== itemVar) {
              itemCodeRef =
                itemCodeRef +
                // Need to allocate a var here, so we don't mutate the input object field
                `${itemVar}=${itemOutput.i}`;
            }
          }
        } catch (exn) {
          const errorVar = B_embed(input, getOrRethrow(exn));
          caught = `${caught},${errorVar}`;
          if (isDeopt && isOnlyCase) {
            staticBlockFailure = errorVar;
            itemSkipped = true;
          } else if (isLast) {
            withExhaustiveCheck = false;
            itemCodeRef = isDeopt ? "throw " + errorVar : fail(caught);
          } else {
            // The case is guaranteed to fail at runtime, so skip its code
            // and keep the embedded error for the exhaustive failure args
            itemSkipped = true;
          }
        }
        const itemCond = itemCondRef.contents;
        const itemCode = itemCodeRef;

        // Accumulate item parser when it has a discriminant
        if (!itemSkipped && itemCond) {
          if (itemCode) {
            const existing = byDiscriminant[itemCond];
            if (existing !== undefined) {
              if (Array.isArray(existing)) {
                existing.push(itemCode);
              } else {
                byDiscriminant[itemCond] = [existing, itemCode];
              }
            } else {
              byDiscriminant[itemCond] = itemCode;
            }
          } else {
            // We have a condition but without additional parsing logic
            // So we accumulate it in case it's needed for a refinement later
            itemNoop = itemNoop ? `${itemNoop}||${itemCond}` : itemCond;
          }
        }

        // Allocate all accumulated discriminants
        // If we have an item without a discriminant
        // and need to deopt. Or we are at the last item
        if (!itemSkipped && (!itemCond || isLast)) {
          const accedDiscriminants = Object.keys(byDiscriminant);
          for (let idx = 0; idx < accedDiscriminants.length; idx++) {
            const discrim = accedDiscriminants[idx]!;
            const if_ = itemNextElse ? "else if" : "if";
            itemStart = itemStart + if_ + `(${discrim}){`;
            const entry = byDiscriminant[discrim]!;
            if (!Array.isArray(entry)) {
              itemStart = itemStart + entry + "}";
            } else {
              let caught = "";
              for (let idx = 0; idx < entry.length; idx++) {
                const code = entry[idx]!;
                const errorVar = `e` + idx;
                itemStart = itemStart + `try{${code}}catch(${errorVar}){`;
                caught = `${caught},${errorVar}`;
              }
              itemStart = itemStart + fail(caught) + "}".repeat(entry.length) + "}";
            }
            itemNextElse = true;
          }
          byDiscriminant = {};
        }

        if (!itemSkipped && !itemCond) {
          if (!itemCode) {
            // If we don't have a condition (discriminant)
            // and additional parsing logic,
            // it means that this item is always passes
            // so we can remove preceding accumulated refinements
            // and exit early even if there are other items
            itemNoop = "";
            itemIdx = lastIdx;
            withExhaustiveCheck = false;
          } else {
            // The item without refinement should switch to deopt mode
            // Since there might be validation in the body
            if (itemNoop) {
              const if_ = itemNextElse ? "else if" : "if";
              itemStart = itemStart + if_ + `(!(${itemNoop})){`;
              itemEnd = "}" + itemEnd;
              itemNoop = "";
              itemNextElse = false;
            }
            if (isLast && (isDeopt || !withExhaustiveCheck || isFirst)) {
              // For the last item don't add try/catch
              itemStart = itemStart + `${itemNextElse ? "else{" : ""}${itemCode}`;
              itemEnd = (itemNextElse ? "}" : "") + itemEnd;
            } else {
              const errorVar = `e` + (itemIdx - preItems);
              itemStart =
                itemStart + `${itemNextElse ? "else{" : ""}try{${itemCode}}catch(${errorVar}){`;
              itemEnd = (itemNextElse ? "}" : "") + "}" + itemEnd;
              caught = `${caught},${errorVar}`;
              itemNextElse = false;
            }
          }
        }
        if (isLast) {
          if (itemNoop) {
            if (
              itemStart ||
              // Skipped cases have their errors embedded,
              // which the hoisted check below can't reference
              caught
            ) {
              const if_ = itemNextElse ? "else if" : "if";
              itemStart = itemStart + if_ + `(!(${itemNoop})){${fail(caught)}}`;
            } else {
              B_pushCheck(typeValidationOutput, {
                c: (_inputVar) => `(${itemNoop})`,
                f: failInvalidType,
              });
            }
          } else if (withExhaustiveCheck) {
            const errorCode = fail(caught);
            itemStart = itemStart + (itemNextElse ? `else{${errorCode}}` : errorCode);
          }
        }

        itemIdx = itemIdx + 1;
      }

      return itemStart + itemEnd;
    };

    let start = "";
    let end = "";
    let caught = "";
    // If we got a case which always passes,
    // we can exit early
    let exit = false;

    const lastIdx = schemas.length - 1;
    let byKey: Record<string, unknown[]> = {};
    let keys: string[] = [];

    // FIXME: minimal fix — applies the union's refiner/inputRefiner per
    // surviving case (previously dropped when the union has `.to`). The
    // emit shape isn't ideal; fold this into the shared refiner pipeline
    // post-release.
    const appendUnionRefiners = (() => {
      const unionRefiner = selfSchema.refiner;
      const unionInputRefiner = selfSchema.inputRefiner;
      // Call each source refiner at most once so its predicate is embedded
      // in `input.global.embeded` once and every case references the same
      // `e[N]`. `B_embed` is append-only, so a per-case call would duplicate.
      const cachedRefinerChecks: CheckCache = { contents: undefined };
      const cachedInputRefinerChecks: CheckCache = { contents: undefined };
      const attach = (
        current: ((input: Val) => Check[]) | undefined,
        source: ((input: Val) => Check[]) | undefined,
        cache: CheckCache,
      ): ((input: Val) => Check[]) | undefined => {
        if (source === undefined) {
          return current;
        } else {
          const fn = source;
          const getCached = (input: Val): Check[] => {
            if (cache.contents !== undefined) {
              return cache.contents;
            } else {
              const checks = fn(input);
              cache.contents = checks;
              return checks;
            }
          };
          if (current === undefined) {
            return getCached;
          } else {
            const existing = current;
            return (input: Val) => {
              const arr = existing(input);
              const next = getCached(input);
              for (let i = 0; i < next.length; i++) {
                arr.push(next[i]!);
              }
              return arr;
            };
          }
        }
      };
      return (mut: Internal) => {
        const r = attach(mut.refiner, unionRefiner, cachedRefinerChecks);
        if (r !== undefined) {
          mut.refiner = r;
        }
        const ir = attach(mut.inputRefiner, unionInputRefiner, cachedInputRefinerChecks);
        if (ir !== undefined) {
          mut.inputRefiner = ir;
        }
      };
    })();

    // Tier 1: for a typed const input, variants with a matching const are
    // tried before catch-all and differently-const'ed variants
    if (isLiteral(input.s)) {
      const matching: Internal[] = [];
      const rest: Internal[] = [];
      for (let idx = 0; idx <= lastIdx; idx++) {
        const schema = schemas[idx]!;
        if (isLiteral(schema) && schema.const === input.s.const) {
          matching.push(schema);
        } else {
          rest.push(schema);
        }
      }
      schemas = matching.concat(rest);
    }

    for (let idx = 0; idx <= lastIdx; idx++) {
      const schema =
        toPerCase !== undefined
          ? updateOutput<Internal>(schemas[idx]!, (mut) => {
              appendUnionRefiners(mut);
              mut.to = toPerCase;
            })
          : schemas[idx]!;
      const tag = schema.type;
      const tagFlag = tagFlags[tag]!;
      const key = unionToKey(schema);

      if (activeKey !== "" && activeKey !== key) {
        // not in active tier — skip
      } else if (
        flagUnsafeHas(tagFlag, tagFlagUndefined) &&
        "fromDefault" in selfSchema
      ) {
        // skip it
      } else {
        const initialArr = byKey[key];
        if (initialArr !== undefined) {
          const arr = initialArr;
          if (
            flagUnsafeHas(tagFlag, tagFlagObject) &&
            nestedLoc in schema.properties!
          ) {
            // This is a special case for https://github.com/DZakh/sury/issues/150
            // When nested option goes together with an empty object schema
            // Since we put None case check second, we need to change priority here.
            arr.splice(arr.length - 1, 0, schema as unknown);
          } else if (
            // TODO: Is this check needed?
            // There can only be one valid. Dedupe
            !flagUnsafeHas(
              tagFlag,
              ((tagFlagUndefined | tagFlagNull) | tagFlagNaN),
            )
          ) {
            arr.push(schema as unknown);
          }
        } else {
          // Recreate input val for every schema
          // since we will mutate it
          const typeValidationInput = B_scope(input);
          // Tree-shaking: build the narrow without a per-type factory. A
          // `string()`/`instance()`/… reference would pin every type decoder into
          // any union-using bundle — and `S.optional`/`S.nullable` are unions.
          if (
            flagUnsafeHas(
              tagFlag,
              tagFlagUnknown | tagFlagUnion | tagFlagRef | tagFlagFunction | tagFlagNever,
            )
          ) {
            // unknown / union / ref / json / function / never have no `typeof`
            // discriminant — the deopt (try-each) path handles them, so no
            // narrow is needed.
            typeValidationInput.e = unknown;
          } else {
            // A minimal narrow standing in as the variant's runtime schema,
            // carrying the member's encoder so a pending `.to` reverse reaches it.
            const narrow = baseSchema(schema.type, false);
            narrow.encoder = schema.encoder;
            if (flagUnsafeHas(tagFlag, tagFlagInstance)) {
              narrow.class = schema.class;
            } else if (flagUnsafeHas(tagFlag, tagFlagObject)) {
              narrow.properties = immutableEmptyObject as Record<string, Internal>;
              narrow.additionalItems = unknown;
            } else if (flagUnsafeHas(tagFlag, tagFlagArray)) {
              narrow.additionalItems = unknown;
              narrow.items = immutableEmptyArray as Internal[];
            } else if (
              flagUnsafeHas(
                tagFlag,
                ((tagFlagNull | tagFlagUndefined) | tagFlagNaN),
              )
            ) {
              // null/undefined/nan stay literals so the case body passes through.
              narrow.const = schema.const;
            }
            // Per-invocation, not hoisted: this narrow is re-decoded during `.to`
            // per-variant conversion — with the union's `unknown` input (emit the
            // discriminant) or a concrete coerced value (delegate to schema.decoder).
            narrow.decoder = (input: Val) => {
              if (flagUnsafeHas(tagFlags[input.s.type]!, tagFlagUnknown)) {
                return B_refine(input, input.e, [
                  {
                    c: (inputVar) => typeCheckCond(input, schema, inputVar),
                    f: failInvalidType,
                  },
                ]);
              } else {
                return schema.decoder(input);
              }
            };
            typeValidationInput.e = narrow;
          }

          let typeValidationOutput: Val;
          try {
            typeValidationOutput = parse(typeValidationInput);
          } catch (_) {
            // Discard any checks parse managed to push before throwing,
            // so the deopt path doesn't see leftover partial state.
            typeValidationInput.vc = undefined;
            typeValidationOutput = typeValidationInput;
          }

          if (unionIsPriority(tagFlag, byKey)) {
            // Not the fastest way, but it's the simplest way
            // to make sure NaN is checked before number
            // And instance and array checked before object
            keys.unshift(key);
          } else {
            keys.push(key);
          }
          byKey[key] = [
            typeValidationInput as unknown,
            typeValidationOutput as unknown,
            schema as unknown,
          ];

          let shouldDeopt = true;
          let valRef: Val | undefined = typeValidationOutput;
          while (valRef !== undefined && shouldDeopt) {
            const v: Val = valRef;
            valRef = v.prev;
            // Deopt to a try/catch block unless every level's checks are
            // hoistable into the dispatch condition (same rule as merge).
            shouldDeopt = !(v.vc && B_isHoistable(v));
          }

          if (shouldDeopt) {
            for (let keyIdx = 0; keyIdx < keys.length; keyIdx++) {
              const key = keys[keyIdx]!;
              if (!exit) {
                const arr = byKey[key]!;
                const typeValidationOutput = arr[1] as Val;
                const itemsCode = getArrItemsCode(arr, true);
                const blockCode = B_merge(typeValidationOutput) + itemsCode;

                const embeddedError = staticBlockFailure;
                if (embeddedError) {
                  staticBlockFailure = "";
                  if (blockCode) {
                    // Type validation code is still relevant — restore the throw
                    const errorVar = `e` + (idx + keyIdx);
                    start =
                      start + `try{${blockCode}throw ${embeddedError}}catch(${errorVar}){`;
                    end = "}" + end;
                    caught = `${caught},${errorVar}`;
                  } else {
                    // The block always fails — drop it
                    // and pass the embedded error along
                    caught = `${caught},${embeddedError}`;
                  }
                } else if (blockCode) {
                  const errorVar = `e` + (idx + keyIdx);
                  start = start + `try{${blockCode}}catch(${errorVar}){`;
                  end = "}" + end;
                  caught = `${caught},${errorVar}`;
                } else {
                  exit = true;
                }
              }
            }

            byKey = {};
            keys = [];
          }
        }
      }
    }

    if (!exit) {
      let nextElse = false;
      let noop = "";

      for (let idx = 0; idx < keys.length; idx++) {
        const arr = byKey[keys[idx]!]!;
        const typeValidationOutput = arr[1] as Val;
        const firstSchema = arr[2] as Internal;

        const itemsCode = getArrItemsCode(arr, false);

        const blockCondRef = { contents: "" };
        const blockCode = B_merge(typeValidationOutput, blockCondRef) + itemsCode;
        const blockCond = blockCondRef.contents;

        if (blockCode || unionIsPriority(tagFlags[firstSchema.type]!, byKey)) {
          const if_ = nextElse ? "else if" : "if";
          start = start + if_ + `(${blockCond}){${blockCode}}`;
          nextElse = true;
        } else {
          noop = noop ? `${noop}||${blockCond}` : blockCond;
        }
      }

      const errorCode = fail(caught);
      start =
        start +
        (noop
          ? (nextElse ? "else if" : "if") + `(!(${noop})){${errorCode}}`
          : nextElse
            ? `else{${errorCode}}`
            : end === ""
              ? // The bare fail call might be followed by more code, eg `return`
                errorCode + ";"
              : errorCode);
    }

    output.cp = output.cp + start + end;

    // In case if input.var was called, but output.var wasn't
    if (input.i !== output.i) {
      output.i = input.i;
    }

    let o: Val;
    if (flagUnsafeHas(output.f, valFlagAsync)) {
      output.i = `Promise.resolve(${output.i})`;
      output.v = _notVar;
      o = output;
    } else if (output.v === _var) {
      // TODO: Think how to make it more robust
      // Recreate to not break the logic to determine
      // whether the output is changed

      // Use output.b instead of b because of B_mergeWithCatch
      // Should refactor B_mergeWithCatch to make it simpler
      // All of this is a hack to make B_mergeWithCatch think that there are no changes. eg S.array(S.option(item))
      if (input.cp === "" && output.cp === "" && initialInline === "i") {
        // FIXME: Might not be needed
        input.hd = "";
        input.v = _notVar;
        input.i = initialInline;
        o = input;
      } else {
        o = output;
      }
    } else {
      o = output;
    }

    // Build the output schema from collected case output schemas. Variants
    // coercing to the same `.to` target now produce structurally-identical (but
    // not identity-equal) outputs; `toJSONSchema` collapses the duplicate.
    o.s = outputAnyOf.length ? unionFactory(outputAnyOf) : never_();
    if (toPerCase !== undefined) {
      o.io = true;
      o.e = getOutputSchema(toPerCase);
    } else {
      o.e = selfSchema;
    }

    return o;
  }
}
export const unionFactory = (schemas: Internal[]): Internal => {
  // TODO:
  // 1. Filter out items without parser
  // 2. Remove duplicate schemas
  // 3. Spread Union and JSON if they are not transformed
  // 4. Provide correct `has` value for Union and JSON
  if (schemas.length === 0) {
    return panic("S.union requires at least one item");
  } else if (schemas.length === 1) {
    return schemas[0]!;
  } else {
    const has: Partial<Record<Tag, boolean>> = {};
    const anyOf = new Set<Internal>();

    schemas.forEach((schema) => {
      // Check if the union is not transformed
      if (schema.type === unionTag && schema.to === undefined) {
        schema.anyOf!.forEach((item) => {
          anyOf.add(item);
        });
        Object.assign(has, schema.has!);
      } else {
        anyOf.add(schema);
        setHas(has, schema.type);
      }
    });
    const mut = baseSchema(unionTag, false);
    mut.anyOf = Array.from(anyOf);
    mut.decoder = unionDecoder;
    mut.encoder = unionEncoder;
    mut.has = has;
    return mut;
  }
}

export const nestedNone = (): Internal => {
  const itemSchema = Literal_parse(0);
  // FIXME: dict{}
  const properties: Record<string, Internal> = {};
  properties[nestedLoc] = itemSchema;
  return {
    type: objectTag,
    required: [nestedLoc],
    properties,
    additionalItems: "strip",
    decoder: objectDecoder,
    // TODO: Support this as a default coercion
    serializer: (input: Val) => {
      const nextSchema = input.e.to!;
      return B_nextConst(input, nextSchema, nextSchema);
      // FIXME: Need to set isOutput?
    },
  } as Internal;
}

export const nestedOption = (item: Internal): Internal => {
  return updateOutput<Internal>(item, (mut) => {
    mut.to = nestedNone();
    mut.parser = nestedOptionParser;
  });
}

// PORT-NOTE: the `~unit` labeled arg is renamed to `unitSchema` so the
// default expression can still reference the module-level `unit` factory.
export const optionFactory = (item: Internal, unitSchema: Internal = unit()): Internal => {
  const out = getOutputSchema(item);
  if (out.type === undefinedTag) {
    return unionFactory([unitSchema, nestedOption(item)]);
  } else if (out.type === unionTag) {
    const anyOf = out.anyOf;
    const has = out.has;
    return updateOutput<Internal>(item, (mut) => {
      const schemas = anyOf!;
      const mutHas = { ...has! };

      const newAnyOf: Internal[] = [];
      for (let idx = 0; idx < schemas.length; idx++) {
        const schema = schemas[idx]!;
        let toPush: Internal;
        const schemaOut = getOutputSchema(schema);
        if (schemaOut.type === undefinedTag) {
          mutHas[unitSchema.type] = true;
          newAnyOf.push(unitSchema);
          toPush = nestedOption(schema);
        } else if (schemaOut.properties !== undefined) {
          const properties = schemaOut.properties;
          const nestedSchema = properties[nestedLoc];
          if (nestedSchema !== undefined) {
            toPush = updateOutput<Internal>(schema, (mut) => {
              // FIXME: dict{}
              const properties: Record<string, Internal> = {};
              properties[nestedLoc] = {
                ...nestedSchema,
                const: (nestedSchema.const as number) + 1,
              } as Internal;
              mut.properties = properties;
            });
          } else {
            toPush = schema;
          }
        } else {
          toPush = schema;
        }
        newAnyOf.push(toPush);
      }

      if (newAnyOf.length === schemas.length) {
        mutHas[unitSchema.type] = true;
        newAnyOf.push(unitSchema);
      }

      mut.anyOf = newAnyOf;
      mut.has = mutHas;
    });
  } else {
    return unionFactory([item, unitSchema]);
  }
}

export const option = (item: Internal): Internal => {
  return optionFactory(item, unit());
}

export const valGet = (parent: Val, location: string): Val => {
  let vals: Record<string, Val>;
  if (parent.d !== undefined) {
    vals = parent.d;
  } else {
    const d: Record<string, Val> = Object.create(null);
    parent.d = d;
    vals = d;
  }

  const existing = vals[location];
  if (existing !== undefined) {
    return B_scope(existing);
  } else {
    let locationSchema: Internal | undefined;
    if (parent.s.type === objectTag) {
      locationSchema = parent.s.properties![location];
    } else {
      locationSchema = parent.s.items![Number(location)];
    }
    let schema: Internal;
    if (locationSchema !== undefined) {
      schema = locationSchema;
    } else {
      const additionalItems = parent.s.additionalItems;
      if (isItemSchema(additionalItems)) {
        const s = additionalItems;
        // A `dict<V>` read by a fixed key may be absent (dicts have no required
        // keys), so model it as `option<V>` and let the union coercion handle a
        // missing key uniformly. Scoped to dict parents (objectTag) with a
        // concrete value type — array->tuple rest reads (arrayTag) and
        // json/unknown values read as-is. `option` is reachable directly because
        // valGet is defined alongside the decoders it's mutually recursive with.
        if (
          parent.s.type === objectTag &&
          s.type !== unknownTag &&
          !flagUnsafeHas(tagFlags[s.type]!, tagFlagRef) &&
          !isOptional(s)
        ) {
          schema = option(s);
        } else {
          schema = s;
        }
      } else {
        schema = B_unsupportedDecode(parent, parent.s, parent.e);
      }
    }

    const pathAppend = pathFromInlinedLocation(B_inlineLocation(parent.g, location));

    const item: Val = {
      v: _notVarAtParent,
      i: isLiteral(schema) ? B_inlineConst(parent, schema) : `${parent.v()}${pathAppend}`,
      f: valFlagNone,
      s: schema,
      e: schema,
      cp: "",
      hd: "",
      path: pathConcat(parent.path, pathAppend),
      g: parent.g,
      p: parent,
    };
    vals[location] = item;
    return item;
  }
}
