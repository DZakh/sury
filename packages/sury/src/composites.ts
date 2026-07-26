import { Literal_parse, isArrayCond, jsonName, objectTagCond, setHas, unit } from "./primitives";
import { baseSchema, copySchema, getOrRethrow, panic, unknown, updateOutput } from "./schema";
import { getOutputSchema, nestedLoc, nestedOptionParser, never_, parse, parseDynamic, typeCheckCond } from "./parse";
import { B_addObjectField, B_addKey, B_scope, B_asyncVal, B_dynamicScope, B_embed, B_failWithArg, B_hoistChildChecks, B_hoistDecl, B_inlineConst, B_invalidOperation, B_isHoistable, B_makeInvalidInputDetails, B_markOutput, B_merge, B_mergeWithPathPrepend, B_next, B_nextConst, B_pushCheck, B_refine, B_throw, B_unsupportedDecode, B_varWithoutAllocation, Builder, _notVar, _notVarAtParent, _var, failInvalidType } from "./builder";
import { AdditionalItems, Check, ErrorDetails, Internal, SuryErrorRecord, U, Val, immutableEmptyArray, immutableEmptyObject, isLiteral, isOptional, toExpression } from "./types";
import { flagUnsafeHas, valFlagAsync, valFlagNone } from "./flags";
import { inlinedValueFromString, pathConcat, pathFromInlinedLocation } from "./path";
import { Tag, arrayTag, neverTag, nullTag, numberTag, objectTag, tagFlagArray, tagFlagFunction, tagFlagInstance, tagFlagNaN, tagFlagNever, tagFlagNull, tagFlagObject, tagFlagRef, tagFlagUndefined, tagFlagUnion, tagFlagUnknown, tagFlags, undefinedTag, unionTag, unknownTag } from "./tags";

// An object/array val (`makeObjectVal`'s result) reuses the plain `Val`
// shape — there's no separate "object val" type.

// Narrows the dict-value-schema-or-mode union down to the schema case.
const isItemSchema = (x: AdditionalItems | undefined): x is Internal =>
  x !== U && typeof x !== "string";

type CheckCache = { contents: Check[] | undefined };

export const makeObjectVal = (prev: Val, schema: Internal): Val => {
  // Canonical Val field order (see B_operationArg in builder.ts).
  return {
    b: U,
    p: U,
    v: _notVar,
    i: "",
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
    io: U,
    e: prev.e,
    prev,
    f: valFlagNone,
    d: Object.create(null),
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: true,
    path: prev.path,
    g: prev.g,
    o: U,
  };
}
export const completeObjectVal = (objectVal: Val): Val => {
  const isArray = objectVal.s.type === arrayTag;
  let inline = "";
  let promiseAllContent = "";
  let optionalSettingCode: ((objectVar: string) => string) | undefined = U;

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
          (existingFn === U ? "" : existingFn(objectVar)) +
          `if(${val.v()}!==void 0){${objectVar}[${inlinedValueFromString(key)}]=${val.i}}`
        );
      };
    } else {
      inline =
        inline +
        (isArray ? `${val.i}` : `${inlinedValueFromString(key)}:${val.i}`) +
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
    if (optionalSettingCode === U) {
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
        hasTransform ? () => B_addKey(output2, iteratorVar, itemOutput) : U,
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
    : U;
  // Only a dict source can be iterated dynamically (`for..in`). A fixed-property
  // object source coerced into a dict target reuses the static object-literal
  // construction below, driven by the source's known keys.
  const inputAdditionalItems = input.s.additionalItems;
  const sourceIsDict = isItemSchema(inputAdditionalItems);

  let output: Val;
  // dict<unknown> target: any object/dict is already a valid value, pass through.
  if (dictItem !== U && dictItem === unknown) {
    output = input;
  } else if (dictItem !== U && sourceIsDict) {
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
      hasTransform ? () => B_addKey(output2, keyVar, itemOutput) : U,
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
  } else if (dictItem !== U) {
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
          objectVal.cp = objectVal.cp + `${keyVar}!==${inlinedValueFromString(key)}`;
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

export const unionIsPriority = (tagFlag: number, byTypeKey: Record<string, boolean>): boolean => {
  return (
    (flagUnsafeHas(tagFlag, (tagFlagArray | tagFlagInstance)) &&
      objectTag in byTypeKey) ||
    (flagUnsafeHas(tagFlag, tagFlagNaN) && numberTag in byTypeKey)
  );
}

// What a compiled union case dispatches on:
// - `k` its dispatch signature, the runtime types its type checks narrow to.
//   Two cases sharing it compete for the same values, so they may only be
//   emitted under one shared check when neither carries a body of its own.
// - `p` whether the case is a pure narrow (no body, no transformation).
// An empty signature means the case has no hoistable discriminant at all —
// there is nothing to branch on, so it needs a try/catch slot.
export const unionCaseDispatch = (val: Val): { k: string; p: boolean } => {
  let key = "";
  let pure = val.t !== true;
  let current: Val | undefined = val;
  while (current !== U) {
    const v: Val = current;
    current = v.prev;
    if (v.cp !== "" || v.hd !== "") {
      pure = false;
    }
    if (v.vc) {
      if (current === U || !B_isHoistable(v)) {
        pure = false;
      } else {
        const checks = v.vc;
        for (let i = 0; i < checks.length; i++) {
          if (checks[i]!.f !== failInvalidType) {
            pure = false;
          }
        }
        key = unionToKey(v.s) + "|" + key;
      }
    }
  }
  return { k: key, p: pure };
}

// Whether decoding a value already known to be of the schema type
// is a noop — no transformation anywhere in the schema tree.
// Recursive refs are conservatively treated as transforming
export const unionIsSelfDecodeNoop = (schema: Internal): boolean => {
  const additionalItems = schema.additionalItems;
  return (
    schema.to === U &&
    schema.parser === U &&
    !flagUnsafeHas(tagFlags[schema.type]!, tagFlagRef) &&
    (schema.anyOf !== U ? schema.anyOf.every(unionIsSelfDecodeNoop) : true) &&
    (schema.items !== U ? schema.items.every(unionIsSelfDecodeNoop) : true) &&
    (schema.properties !== U
      ? Object.values(schema.properties).every(unionIsSelfDecodeNoop)
      : true) &&
    (additionalItems !== U && typeof additionalItems !== "string"
      ? unionIsSelfDecodeNoop(additionalItems)
      : true)
  );
}

// Pass-through shortcut for a union-typed val meeting a union expectation that
// covers it position by position. Only reachable when per-variant dispatch is
// unavailable (a recursive variant) — a real conversion goes through the rules.
export const unionIsWiderSchema = (schemaAnyOf: Internal[], inputAnyOf: Internal[]): boolean => {
  return inputAnyOf.every((inputSchema, idx) => {
    const schema = schemaAnyOf[idx];
    if (schema !== U) {
      return (
        !flagUnsafeHas(
          tagFlags[inputSchema.type]!,
          tagFlagArray | tagFlagInstance | tagFlagRef | tagFlagUnion | tagFlagObject,
        ) &&
        inputSchema.type === schema.type &&
        inputSchema.const === schema.const &&
        inputSchema.to === U &&
        // A paired variant with its own `.to` still transforms the value,
        // so passing the input through would skip the conversion
        schema.to === U
      );
    } else {
      return false;
    }
  });
}

// The union's own `.to` chain which is applied per case during decoding.
// None when the union has a custom parser owning the `.to` conversion
export const unionGetToPerCase = (schema: Internal): Internal | undefined => {
  return schema.parser === U && schema.to !== U ? schema.to : U;
}

// Two schemas have the same type when their type tags match — the class for
// instances and the format for formatted primitives included, so `S.int32` and
// `S.number` are different types. A schema with no tag of its own to compare —
// a recursive schema, or a union taking part as a normal schema — has the same
// type as another only when the two are strictly equal. `unknown` has no type
// to compare at all: it matches nothing, so an unknown source always reaches
// every variant's decoder (which is what plain union validation is).
export const sameType = (a: Internal, b: Internal): boolean => {
  const flags = tagFlags[a.type]! | tagFlags[b.type]!;
  if (flagUnsafeHas(flags, tagFlagUnknown)) {
    return false;
  }
  if (a === b) {
    return true;
  }
  return (
    !flagUnsafeHas(flags, (tagFlagRef | tagFlagUnion)) &&
    a.type === b.type &&
    a.class === b.class &&
    a.format === b.format &&
    // Two literals of the same tag are still different types when they hold
    // different values — otherwise rule 4 would pair `"a"` with `"b"`. A
    // literal and the plain type it belongs to do match: `S.literal("a")` is
    // one of the values `S.string` produces.
    (a.const === b.const || !isLiteral(a) || !isLiteral(b))
  );
}

// A union carrying its own format, transformation or refinement is not
// flattened and takes part in a conversion as a normal schema — otherwise the
// thing it carries would be dropped together with the wrapper.
export const isPlainUnion = (schema: Internal): boolean => {
  return (
    schema.type === unionTag &&
    schema.to === U &&
    schema.parser === U &&
    schema.refiner === U &&
    schema.inputRefiner === U &&
    schema.format === U
  );
}

// `S.never` marks a deliberately unreachable path, so a variant matching by a
// `never` type is skipped by the rules below instead of failing them.
const isUnreachable = (schema: Internal): boolean => schema.type === neverTag;

const conversionFailure = (input: Val, from: Internal, to: Internal, detail: string): never => {
  return B_invalidOperation(
    input,
    `Can't decode ${toExpression(from)} to ${toExpression(to)}: ${detail}`
  );
}

// Rules 2 and 3 — a conversion between a union and a schema with a type of
// its own applies the built-in decoder per variant. When the single side has
// the same type as some but not all variants, there is no telling a widening
// from a decoding intent, so the operation is rejected. A variant is seen by
// what the conversion reads from it: its input when the union is the target,
// its output when the union is the source.
export const unionCheckAmbiguous = (
  input: Val,
  from: Internal,
  to: Internal,
  variants: Internal[],
  isSource: boolean
): void => {
  const single = isSource ? to : from;
  let matched: Internal | undefined = U;
  let unmatched: Internal | undefined = U;
  for (let idx = 0; idx < variants.length; idx++) {
    const variant = isSource ? getOutputSchema(variants[idx]!) : variants[idx]!;
    if (isUnreachable(variant)) {
      continue;
    }
    if (sameType(single, variant)) {
      matched = matched !== U ? matched : variant;
    } else {
      unmatched = unmatched !== U ? unmatched : variant;
    }
  }
  if (matched !== U && unmatched !== U) {
    conversionFailure(
      input,
      from,
      to,
      `${toExpression(matched)} matches it, ${toExpression(unmatched)} doesn't` + conversionHint
    );
  }
}

const conversionHint = ". Use S.to on it, or S.never to make it unreachable";

const noMatchDetail = (schema: Internal): string =>
  `${toExpression(schema)} has no variant of the same type on the other side` + conversionHint;

const nullishOpposite = (schema: Internal): Tag | undefined => {
  const tag = schema.type;
  return tag === nullTag ? undefinedTag : tag === undefinedTag ? nullTag : U;
}

// Rule 4 (union → union): no coercion — every value passes through to the
// same-type target variant. Returns the target variant each source variant
// converts into (None for an unreachable variant), and rejects the operation
// unless the two unions cover each other. A `null`/`undefined` variant left
// unmatched may bridge to the opposite nullish variant on the other side — at
// runtime the same-type target wins, the bridge only fills a gap.
export const unionPairVariants = (
  input: Val,
  source: Internal,
  target: Internal,
  sourceVariants: Internal[],
  targetVariants: Internal[]
): (Internal | undefined)[] => {
  const outputs = sourceVariants.map(getOutputSchema);
  const covered: boolean[] = [];
  const find = (out: Internal, byIdentity: boolean): Internal | undefined => {
    for (let j = 0; j < targetVariants.length; j++) {
      const variant = targetVariants[j]!;
      if (
        byIdentity
          ? variant === out
          : !isUnreachable(variant) &&
            (sameType(out, variant) || variant.type === nullishOpposite(out))
      ) {
        covered[j] = true;
        return variant;
      }
    }
    return U;
  };

  // Same-type matching alone can't tell two object variants apart (their tag
  // is all they have), so the variant that *is* the target wins first — a
  // reordered union of objects pairs each variant with itself instead of with
  // whichever object the target happens to list first.
  const pairs = outputs.map((out) => (isUnreachable(out) ? U : find(out, true)));
  outputs.forEach((out, idx) => {
    if (pairs[idx] === U && !isUnreachable(out)) {
      const pair = find(out, false);
      if (pair === U) {
        conversionFailure(input, source, target, noMatchDetail(out));
      }
      pairs[idx] = pair;
    }
  });

  targetVariants.forEach((variant, j) => {
    const opposite = nullishOpposite(variant);
    let bridged = covered[j] === true || isUnreachable(variant);
    for (let idx = 0; opposite !== U && !bridged && idx < outputs.length; idx++) {
      bridged = outputs[idx]!.type === opposite;
    }
    if (!bridged) {
      conversionFailure(input, source, target, noMatchDetail(variant));
    }
  });

  return pairs;
}

// Resolves what each source variant converts into, applying rule 4 when both
// sides are plain unions and rule 3 otherwise.
export const unionResolveTargets = (
  input: Val,
  source: Internal,
  variants: Internal[],
  target: Internal
): (Internal | undefined)[] => {
  let resolved = target;
  if (source.implicit) {
    // An implicit union is the library's own model of a possibly-absent
    // property read, not something the author wrote, so there is no authoring
    // ambiguity for the rules to reject: every variant keeps attempting the
    // whole target and a variant with no decoder stays a runtime failure. The
    // mark travels onto the target union so nested conversions stay exempt.
    if (target.type === unionTag && !target.implicit) {
      resolved = copySchema(target);
      resolved.implicit = true;
    }
  } else if (isPlainUnion(target)) {
    return unionPairVariants(input, source, target, variants, target.anyOf!);
  } else {
    unionCheckAmbiguous(input, source, target, variants, true);
  }
  return variants.map(() => resolved);
}

// Re-drives the source union with `.to(target)` appended, so its decoder
// dispatches per variant and each variant converts to the target
// independently (the documented per-source-variant algorithm)
export const unionPerVariantVal = (input: Val, target: Internal): Val => {
  return B_refine(
    input,
    unknown,
    U,
    updateOutput<Internal>(input.s, (mut) => {
      mut.to = target;
    }),
  );
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
        v.to !== U ||
        v.parser !== U ||
        flagUnsafeHas(tagFlags[v.type]!, tagFlagRef),
    )
  );
}

// Applied by the parse loop when a union-typed val meets a different expected
// schema. An author-written union conversion never lands here — `S.to` puts
// the target on the union itself, which `unionDecoder` resolves through the
// rules — so this is a val flowing into a differently-spelled expectation.
export const unionEncoder = (input: Val, target: Internal): Val => {
  const inputAnyOf = input.s.anyOf!;
  if (
    target.type === unionTag &&
    unionGetToPerCase(target) === U &&
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
  const schemas = selfSchema.anyOf!;
  const initialInputTagFlag = tagFlags[input.s.type]!;

  const toPerCase = unionGetToPerCase(selfSchema);

  if (
    // The input val is already of the union type (trusted self-decode).
    // Only allowed when no variant transforms the value
    (input.s === selfSchema &&
      toPerCase === U &&
      schemas.every(unionIsSelfDecodeNoop)) ||
    (flagUnsafeHas(initialInputTagFlag, tagFlagUnion) &&
      unionIsWiderSchema(schemas, input.s.anyOf!) &&
      toPerCase === U) ||
    (input.io! && input.e === input.s)
  ) {
    return input;
  } else {
    // Rule 2 — a conversion into this union from a schema that has a type of
    // its own. Runs before the input schema is widened below, and never for a
    // union/ref/unknown source: those have no type to disambiguate against
    // (an `unknown` source is plain union validation). A union carrying
    // something of its own — or one the library synthesized — takes part as a
    // normal schema, so a conversion into it is rule 1 and its variants aren't
    // the author's to disambiguate.
    if (
      isPlainUnion(selfSchema) &&
      !selfSchema.implicit &&
      !flagUnsafeHas(
        initialInputTagFlag,
        (((tagFlagUnion | tagFlagRef) | tagFlagUnknown) | tagFlagNever),
      )
    ) {
      unionCheckAmbiguous(input, input.s, selfSchema, schemas, false);
    }

    // Rules 3 and 4 — this union is the source of a conversion, so each
    // variant is paired with what it converts into.
    const perCaseTargets =
      toPerCase !== U
        ? unionResolveTargets(input, selfSchema, schemas, toPerCase)
        : U;

    if (
      flagUnsafeHas(initialInputTagFlag, tagFlagUnion) ||
      (input.s.encoder === U && flagUnsafeHas(initialInputTagFlag, tagFlagRef))
    ) {
      input.s = unknown;
    }

    const initialInline = input.i;
    // No conversion to blame for a case that fails to build: nothing narrows
    // the input, and `.to(S.unknown)` only widens (it's what an operation
    // appends to run a schema's own chain).
    const isValidation =
      (toPerCase === U || getOutputSchema(toPerCase).type === unknownTag) &&
      flagUnsafeHas(tagFlags[input.s.type]!, tagFlagUnknown);
    // Whether a case that fails to build takes the whole operation down.
    // Plain union validation still aggregates a member that can't run in this
    // direction (an `S.transform` with only a serializer) into a runtime
    // error, but a pair with no built-in decoder belongs to the conversion
    // that asked for it — wherever that conversion is written.
    const caseFailureIsFatal = (exn: unknown): boolean =>
      selfSchema.implicit !== true &&
      (!isValidation ||
        (exn as { code?: string }).code === "unsupported_decode");

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
              args.length > 1 ? (Array.from(args).slice(1) as SuryErrorRecord[]) : U,
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
          // A case the decoder can't be built for rejects the operation — the
          // error belongs where the conversion is written, not to every value
          // that reaches this branch. Only an implicit union still degrades it
          // to a runtime failure (see `unionResolveTargets`).
          if (caseFailureIsFatal(exn)) {
            throw exn;
          }
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
            if (existing !== U) {
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
    // Whether a group's shared type check is a pure narrow — only then may a
    // later case with the same signature join it.
    let byPure: Record<string, boolean> = {};
    // Type tags of the open groups, for the NaN-before-number and
    // instance/array-before-object dispatch ordering.
    let byTypeKey: Record<string, boolean> = {};
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
      const cachedRefinerChecks: CheckCache = { contents: U };
      const cachedInputRefinerChecks: CheckCache = { contents: U };
      const attach = (
        current: ((input: Val) => Check[]) | undefined,
        source: ((input: Val) => Check[]) | undefined,
        cache: CheckCache,
      ): ((input: Val) => Check[]) | undefined => {
        if (source === U) {
          return current;
        } else {
          const fn = source;
          const getCached = (input: Val): Check[] => {
            if (cache.contents !== U) {
              return cache.contents;
            } else {
              const checks = fn(input);
              cache.contents = checks;
              return checks;
            }
          };
          if (current === U) {
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
        if (r !== U) {
          mut.refiner = r;
        }
        const ir = attach(mut.inputRefiner, unionInputRefiner, cachedInputRefinerChecks);
        if (ir !== U) {
          mut.inputRefiner = ir;
        }
      };
    })();

    // Closes every accumulated group into a try/catch chain, so a case that
    // fails at runtime falls through to the next one instead of being cut off
    // by an `else`. Used whenever the if/else-if shape would be wrong: a case
    // with no hoistable discriminant, or one repeating an earlier group's
    // dispatch signature (both compete for the same values).
    const flushGroups = (idx: number) => {
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
              start = start + `try{${blockCode}throw ${embeddedError}}catch(${errorVar}){`;
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
      byPure = {};
      byTypeKey = {};
      keys = [];
    };

    for (let idx = 0; idx <= lastIdx; idx++) {
      const source = schemas[idx]!;
      // Rule 4 pairs each variant with what it converts into; rules 2 and 3
      // give every variant the same target. None means the variant is
      // unreachable, or already produces the paired type.
      const pairTarget = perCaseTargets !== U ? perCaseTargets[idx] : U;
      const needsTo = pairTarget !== U && getOutputSchema(source) !== pairTarget;
      const schema =
        toPerCase !== U
          ? updateOutput<Internal>(source, (mut) => {
              appendUnionRefiners(mut);
              if (needsTo) {
                mut.to = pairTarget;
              }
            })
          : source;
      const tag = schema.type;
      const tagFlag = tagFlags[tag]!;
      const typeKey = unionToKey(schema);

      if (
        // `S.never` marks a deliberately unreachable path: no value can reach
        // this case, so it contributes no dispatch branch. (A variant that
        // decodes *into* never keeps its branch — its input is reachable, and
        // rejecting it at runtime is the point.)
        flagUnsafeHas(tagFlag, tagFlagNever) ||
        (flagUnsafeHas(tagFlag, tagFlagUndefined) && "fromDefault" in selfSchema)
      ) {
        // skip it
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

        // A variant the decoder can't be built for rejects the whole
        // operation — being one case of a union changes nothing about that.
        let typeValidationOutput: Val;
        try {
          typeValidationOutput = parse(typeValidationInput);
        } catch (exn) {
          if (caseFailureIsFatal(exn)) {
            throw exn;
          }
          // Discard any checks parse managed to push before throwing,
          // so the deopt path doesn't see leftover partial state.
          typeValidationInput.vc = U;
          typeValidationOutput = typeValidationInput;
        }
        const dispatch = unionCaseDispatch(typeValidationOutput);
        const key = dispatch.k;

        const initialArr = byKey[key];
        if (initialArr !== U && dispatch.p && byPure[key]!) {
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
          if (initialArr !== U) {
            // Same dispatch signature as an open group we can't join — an
            // `else if` would make this case unreachable, so chain instead.
            flushGroups(idx);
          }

          if (unionIsPriority(tagFlag, byTypeKey)) {
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
          byPure[key] = dispatch.p;
          byTypeKey[typeKey] = true;

          if (key === "") {
            flushGroups(idx);
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

        if (blockCode || unionIsPriority(tagFlags[firstSchema.type]!, byTypeKey)) {
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
    if (toPerCase !== U) {
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
      // A nested union flattens into this one, unless it carries something of
      // its own (a refinement, transformation or format) that inlining its
      // variants would drop.
      if (isPlainUnion(schema)) {
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
        } else if (schemaOut.properties !== U) {
          const properties = schemaOut.properties;
          const nestedSchema = properties[nestedLoc];
          if (nestedSchema !== U) {
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
  if (parent.d !== U) {
    vals = parent.d;
  } else {
    const d: Record<string, Val> = Object.create(null);
    parent.d = d;
    vals = d;
  }

  const existing = vals[location];
  if (existing !== U) {
    return B_scope(existing);
  } else {
    let locationSchema: Internal | undefined;
    if (parent.s.type === objectTag) {
      locationSchema = parent.s.properties![location];
    } else {
      locationSchema = parent.s.items![Number(location)];
    }
    let schema: Internal;
    if (locationSchema !== U) {
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
          // Not an authored union — the conversion rules skip it (see
          // `unionResolveTargets`), so `dict<string>` still coerces into a
          // structured object whose fields need their own decoders.
          schema.implicit = true;
        } else {
          schema = s;
        }
      } else {
        schema = B_unsupportedDecode(parent, parent.s, parent.e);
      }
    }

    const pathAppend = pathFromInlinedLocation(inlinedValueFromString(location));

    // Canonical Val field order (see B_operationArg in builder.ts).
    const item: Val = {
      b: U,
      p: parent,
      v: _notVarAtParent,
      i: isLiteral(schema) ? B_inlineConst(parent, schema) : `${parent.v()}${pathAppend}`,
      s: schema,
      io: U,
      e: schema,
      prev: U,
      f: valFlagNone,
      d: U,
      fv: U,
      cp: "",
      hd: "",
      fz: U,
      vc: U,
      u: U,
      t: U,
      path: pathConcat(parent.path, pathAppend),
      g: parent.g,
      o: U,
    };
    vals[location] = item;
    return item;
  }
}
