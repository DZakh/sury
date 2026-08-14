// An object/array val (`makeObjectVal`'s result) reuses the plain `Val`
// shape — there's no separate "object val" type.

import {
  anyOfTag,
  type AdditionalItems,
  arrayTag,
  baseSchema,
  copySchema,
  type Check,
  type ErrorDetails,
  flagUnsafeHas,
  globalConfig,
  immutableEmptyArray,
  immutableEmptyObject,
  inlinedValueFromString,
  instanceTag,
  type Internal,
  inputExpression,
  isLiteral,
  isOptional,
  isSchemaObject,
  jsonName,
  objectTag,
  panic,
  pathConcat,
  pathFromInlinedLocation,
  flagAsync,
  tagFlagArray,
  tagFlagBoolean,
  tagFlagNull,
  tagFlagObject,
  tagFlagRef,
  tagFlags,
  tagFlagString,
  tagFlagUnknown,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  updateOutput,
  type Val,
  valFlagAsync,
  valFlagNone,
} from "./base";
import {
  _notVar,
  _notVarAtParent,
  B_addKey,
  B_addObjectField,
  B_asyncVal,
  B_dynamicScope,
  B_failWithArg,
  B_hoistChildChecks,
  B_hoistDecl,
  B_inlineConst,
  B_markOutput,
  B_merge,
  B_mergeWithPathPrepend,
  B_next,
  B_nextConst,
  B_refine,
  B_scope,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType,
} from "./builder";
import {
  getOutputSchema,
  nestedLoc,
  nestedOptionParser,
  parse,
  parseDynamic,
} from "./parse";
import { isArrayCond, Literal_parse, literalDecoder, objectTagCond, unit } from "./primitives";
import { unionFactory } from "./union";

// Narrows the dict-value-schema-or-mode union down to the schema case.
const isItemSchema = (x: AdditionalItems | undefined): x is Internal =>
  x !== U && typeof x !== "string";

// A `.to` continuation into non-pretty jsonString serializes dynamic items in
// its own loop (jsonStringAggregate in advanced/json.ts) and re-parses each
// item from unknown when the incoming val carries `uv` — so the validation
// loop here would walk the container a second time (and rebuild transformed
// items) for nothing. Skip it and hand the container over unvalidated. Value
// types the aggregate serializes via native JSON.stringify (its fallback:
// bare strings/booleans/null) must stay validated here — the aggregate
// mirrors that by never taking the fallback on a `uv` val.
const B_fuseIntoJsonString = (
  input: Val,
  expectedSchema: Internal,
  item: Internal,
  isArr: boolean,
): Val | undefined => {
  const to = expectedSchema.to;
  if (
    // Only an unknown-typed source has validation pending — a typed source
    // (decode direction) has nothing to fuse, and marking it would make the
    // aggregate re-validate trusted input.
    input.s.additionalItems === unknown &&
    to !== U &&
    to.format === "json" &&
    !to.space &&
    !flagUnsafeHas(input.g.o, flagAsync) &&
    (isArr ||
      !(
        item.to === U &&
        flagUnsafeHas(
          tagFlags[item.type]!,
          (tagFlagString | tagFlagBoolean) | tagFlagNull,
        )
      ))
  ) {
    const marked = copySchema(expectedSchema);
    marked.uv = true;
    return B_refine(input, marked);
  }
  return U;
};

// The wire form of a nested bare json-format string is an escaped string
// value, not raw JSON text (see fieldPiece in advanced/json.ts). So a
// JSON-sourced item (a JSON.parse result typed `json`) converting to one must
// validate the string and pass it through — narrowing the source to `unknown`
// routes it to jsonString's own decoder instead of json's serialize encoder,
// which would re-stringify and double-wrap on encode.
const B_narrowJsonSourcedJsonString = (itemInput: Val): void => {
  if (
    itemInput.s.name === jsonName &&
    itemInput.e.format === "json" &&
    itemInput.e.to === U
  ) {
    itemInput.s = unknown;
  }
};

// An `s.rest(...)` reshape re-reads the source value (`collectRest` in
// factory.ts), so the decoder has to leave that value in place — a rebuilt one
// carries the declared part only. The rebuild is dead weight there anyway: the
// reshape assembles its own value out of the decoded field vals. The returned
// schema keeps the SOURCE's `additionalItems`, so the reshape still sees the
// rest as unvalidated and checks it exactly once instead of never.
const restSourceSchema = (expectedSchema: Internal, input: Val): Internal => {
  const mut = copySchema(expectedSchema);
  mut.additionalItems = input.s.additionalItems;
  return mut;
}

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
    sp: U,
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
          (key === "__proto__"
            ? `if(${val.v()}!==void 0){${objectVar}={...${objectVar},["__proto__"]:${val.i}}}`
            : `if(${val.v()}!==void 0){${objectVar}[${inlinedValueFromString(key)}]=${val.i}}`)
        );
      };
    } else {
      inline =
        inline +
        (isArray
          ? `${val.i}`
          : `${key === "__proto__" ? '["__proto__"]' : inlinedValueFromString(key)}:${val.i}`) +
        ",";
    }
  }

  // Where the spread goes is the whole difference between the two containers: a
  // tuple's declared items own indices 0..n-1 so the rest follows them, while an
  // object's declared keys have to win a name collision so the rest precedes
  // them — which is the precedence the decode side gets for free, its rest loop
  // skipping every declared key.
  const spread = objectVal.sp !== U ? "..." + objectVal.sp.i : "";
  objectVal.i = isArray
    ? "[" + inline + spread + "]"
    : "{" + (spread === "" ? "" : spread + ",") + inline + "}";

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
// `S.json` builds its members before operations.ts installs the `~standard`
// marker, so `array` would misread them as instance literals — init-time and
// codegen callers take this one.
export const arrayFactory = (item: Internal): Internal => {
  const mut = baseSchema(arrayTag, !!item.sr, arrayDecoder);
  mut.additionalItems = item;
  mut.items = immutableEmptyArray as Internal[];
  return mut;
}
// @__NO_SIDE_EFFECTS__
export const array = (item: unknown): Internal => arrayFactory(definitionToSchema(item));
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
      schema = arrayFactory(unknown);
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
      } else if (expectedAdditionalItems === "strip" || expectedLength !== 0) {
        // `strip`, or a rest schema over a fixed prefix: the prefix items are
        // read by index below, so they have to be there. A rest schema with no
        // prefix is a plain array and accepts any length.
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
  const restItem: Internal | undefined = isItemSchema(expectedAdditionalItems)
    ? expectedAdditionalItems
    : U;
  // A rest schema over a fixed prefix (`[string, ...number[]]`) is not a plain
  // array: the prefix has its own item schemas, decoded by index in the path
  // below, and only the tail is iterated.
  if (restItem !== U && expectedLength === 0) {
    const itemSchema = restItem;
    if (itemSchema === unknown) {
      output = input;
    } else {
      // Plain-array fusion only — hence this branch, not the prefixed one:
      // fixed slots are read by the aggregate outside its dynamic loop, so they
      // must stay validated.
      const fused = B_fuseIntoJsonString(input, expectedSchema, itemSchema, true);
      if (fused !== U) {
        return B_markOutput(fused, input);
      }
      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);

      const raiseCountBefore = input.g.t;
      const itemInput = B_dynamicScope(input, iteratorVar);
      B_narrowJsonSourcedJsonString(itemInput);
      const itemOutput = parseDynamic(itemInput);
      const hasTransform = itemOutput.t!;
      const output2 = hasTransform
        ? // The next `.to` segment decodes from this schema — item-output, not expectedSchema (#284)
          B_next(input, `new Array(${inputVar}.length)`, arrayFactory(itemOutput.s))
        : B_refine(input, expectedSchema);

      const itemCode = B_mergeWithPathPrepend(
        itemOutput,
        input,
        iteratorVar,
        hasTransform ? () => B_addKey(output2, iteratorVar, itemOutput) : U,
        hasTransform ? U : raiseCountBefore,
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
        // A rest schema keeps every item it validates, so the input still
        // describes the output — only a transform forces a rebuild.
        shouldRecreateInput = false;
      }
    }

    for (let idx = 0; idx < expectedLength; idx++) {
      const schema = expectedItems[idx]!;
      const key = String(idx);
      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion; // We want to control validation on the decoder side
      B_narrowJsonSourcedJsonString(itemInput);
      const itemOutput = parse(itemInput);

      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }

      B_addObjectField(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t!;
      }
    }

    // `items` + a rest schema: the prefix was decoded by index above, and every
    // item past it goes through `restItem`.
    let restOutput: Val | undefined = U;
    let restIteratorVar = "";
    let restRaiseCountBefore = 0;
    // Same hand-off as the object rest loop.
    if (restItem !== U && expectedSchema.fromRest === U) {
      restIteratorVar = B_varWithoutAllocation(objectVal.g);
      restRaiseCountBefore = input.g.t;
      const itemInput = B_dynamicScope(input, restIteratorVar);
      B_narrowJsonSourcedJsonString(itemInput);
      restOutput = parseDynamic(itemInput);
      panicOnAsyncRest(restOutput, expectedSchema);
      if (restOutput.t) {
        shouldRecreateInput = true;
      }
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput && expectedSchema.fromRest === U) {
      output = completeObjectVal(objectVal);
    } else {
      // Same stale-schema class as #284/#252: carry expectedSchema, not
      // input.schema (which may be a minimal union dispatch narrow), so a
      // pending `.to(json)` conversion routes through the fixed-items path
      const o = B_refine(
        input,
        expectedSchema.fromRest !== U ? restSourceSchema(expectedSchema, input) : expectedSchema,
      );
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }

    if (restOutput !== U) {
      // Same as the object rest loop: a rebuilt output holds the fixed items
      // only, so the tail has to be copied into it even when the rest schema
      // doesn't transform.
      const itemCode = B_mergeWithPathPrepend(
        restOutput,
        input,
        restIteratorVar,
        shouldRecreateInput ? () => B_addKey(output, restIteratorVar, restOutput!) : U,
        restOutput.t ? U : restRaiseCountBefore,
      );
      if (shouldRecreateInput || itemCode !== "") {
        const inputVar = input.v();
        output.cp =
          output.cp +
          `for(let ${restIteratorVar}=${expectedLength};${restIteratorVar}<${inputVar}.length;++${restIteratorVar}){${itemCode}}`;
      }
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
      const mut = baseSchema(objectTag, false, objectDecoder);
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
      // An array is not an object, whatever the mode. `strip` could skip this
      // and still produce a sound value — it rebuilds from known properties, so
      // an array decodes to `{}` — but that's silent acceptance of the wrong
      // type, and it made the narrow weaker than `typeCheckCond`, which a union
      // dispatch reads as the case's acceptance mask.
      checks.push({
        c: (inputVar) => `!${isArrayCond(inputVar)}`,
        f: failInvalidType,
      });
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

  // The target's value schema for whatever the declared properties don't cover.
  const expectedAdditionalItems = expectedSchema.additionalItems;
  const restItem: Internal | undefined = isItemSchema(expectedAdditionalItems)
    ? expectedAdditionalItems
    : U;
  const expectedKeys = Object.keys(expectedSchema.properties!);
  // A rest schema with no declared properties is a dict — the whole value is
  // iterated. With declared properties it's `properties` + `additionalProperties`,
  // which the fixed-property path below handles: declared keys are decoded by
  // name, and the rest loop covers what's left.
  const dictItem: Internal | undefined = expectedKeys.length === 0 ? restItem : U;
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
    const fused = B_fuseIntoJsonString(input, expectedSchema, dictItem, false);
    if (fused !== U) {
      return B_markOutput(fused, input);
    }
    const inputVar = input.v();
    const keyVar = B_varWithoutAllocation(input.g);
    const raiseCountBefore = input.g.t;
    const itemInput = B_dynamicScope(input, keyVar);
    B_narrowJsonSourcedJsonString(itemInput);
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
      hasTransform ? U : raiseCountBefore,
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
      B_narrowJsonSourcedJsonString(itemInput);
      B_addObjectField(objectVal, key, parse(itemInput));
    }
    output = completeObjectVal(objectVal);
  } else {
    // Build a fixed-property object target (from a dict or object source).
    const properties = expectedSchema.properties!;
    const keys = expectedKeys;
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
        // A rest schema keeps every key it validates, so the input still
        // describes the output — only a transform forces a rebuild.
        shouldRecreateInput = false;
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
      if (isJsonParent && schema.type === anyOfTag && schema.has![undefinedTag]) {
        itemInput.i = `(${itemInput.i}??null)`;
      }
      B_narrowJsonSourcedJsonString(itemInput);

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
      objectVal.cp =
        objectVal.cp +
        `for(${keyVar} in ${input.v()}){if(${undeclaredKeyCond(keys, keyVar)}){${B_failWithArg(
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

    // `properties` + a rest schema: the declared keys were decoded by name
    // above, and every other key of the source goes through `restItem`. Only a
    // dict-shaped source can carry keys the properties don't name — a
    // fixed-property source has none to iterate.
    let restOutput: Val | undefined = U;
    let restKeyVar = "";
    let restRaiseCountBefore = 0;
    // `fromRest` means an `s.rest(...)` reshape collects and decodes these keys
    // itself (`collectRest` in factory.ts) — validating them here too would run
    // every check twice.
    if (restItem !== U && sourceIsDict && expectedSchema.fromRest === U) {
      restKeyVar = B_varWithoutAllocation(objectVal.g);
      restRaiseCountBefore = input.g.t;
      const itemInput = B_dynamicScope(input, restKeyVar);
      B_narrowJsonSourcedJsonString(itemInput);
      restOutput = parseDynamic(itemInput);
      panicOnAsyncRest(restOutput, expectedSchema);
      if (restOutput.t) {
        shouldRecreateInput = true;
      }
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput && expectedSchema.fromRest === U) {
      output = completeObjectVal(objectVal);
    } else {
      // The value was just validated against expectedSchema — carry it as
      // the val's schema instead of input.schema, which may be a minimal
      // union dispatch narrow ({properties:{}, additionalItems: unknown}).
      // Keeping the narrow mis-routed a pending `.to(json)` conversion
      // into the dict path, which rejects undefined optional fields (#252)
      const o = B_refine(
        input,
        expectedSchema.fromRest !== U ? restSourceSchema(expectedSchema, input) : expectedSchema,
      );
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }

    if (restOutput !== U) {
      // A rebuilt output holds the declared fields only, so the rest keys have
      // to be copied into it even when the rest schema itself doesn't transform
      // — otherwise a transform on any declared field silently drops every key
      // the rest just validated.
      const itemCode = B_mergeWithPathPrepend(
        restOutput,
        input,
        restKeyVar,
        shouldRecreateInput ? () => B_addKey(output, restKeyVar, restOutput!) : U,
        restOutput.t ? U : restRaiseCountBefore,
      );
      if (shouldRecreateInput || itemCode !== "") {
        output.cp =
          output.cp +
          `for(let ${restKeyVar} in ${input.v()}){if(${undeclaredKeyCond(
            keys,
            restKeyVar,
          )}){${itemCode}}}`;
      }
    }
  }
  return B_markOutput(output, input);
}

// The declared part and the rest settle through two different mechanisms —
// `completeObjectVal`'s Promise.all over a fixed list, versus a counter over a
// dynamic one — and nothing joins them, so an async rest would compile to an
// object of pending promises.
const panicOnAsyncRest = (restOutput: Val, schema: Internal): void => {
  if (flagUnsafeHas(restOutput.f, valFlagAsync)) {
    panic(`Async rest is not supported for ${inputExpression(schema)}`);
  }
}

// The `for..in` guard that skips the keys `properties` already covers — shared
// by the strict excess-key check, the rest loop, and `collectRest` in
// factory.ts, which differ only in what they do with a key that gets through.
export const undeclaredKeyCond = (keys: string[], keyVar: string): string => {
  let cond = "";
  for (let idx = 0; idx < keys.length; idx++) {
    cond = cond + (idx === 0 ? "" : "&&") + `${keyVar}!==${inlinedValueFromString(keys[idx]!)}`;
  }
  return cond === "" ? "true" : cond;
}

// Same init-order constraint as arrayFactory.
export const dictFactory = (item: Internal): Internal => {
  const mut = baseSchema(objectTag, !!item.sr, objectDecoder);
  mut.properties = immutableEmptyObject as Record<string, Internal>;
  mut.additionalItems = item;
  return mut;
}
// @__NO_SIDE_EFFECTS__
export const dict = (item: unknown): Internal => dictFactory(definitionToSchema(item));

// An already-built schema is the overwhelmingly common argument, so it exits
// here instead of paying traverseDefinition's typeof/null checks on top of the
// ones isSchemaObject just made.
export const definitionToSchema = (definition: unknown): Internal =>
  isSchemaObject(definition)
    ? (definition as Internal)
    : traverseDefinition(definition, (node) =>
        isSchemaObject(node) ? (node as Internal) : U
      );

export const traverseDefinition = (
  definition: unknown,
  onNode: (node: unknown) => Internal | undefined
): Internal => {
  if (typeof definition === objectTag && definition !== null) {
    const s = onNode(definition);
    if (s !== U) {
      return s;
    } else {
      if (Array.isArray(definition)) {
        const node = definition as unknown[];
        for (let idx = 0; idx < node.length; idx++) {
          node[idx] = traverseDefinition(node[idx], onNode);
        }
        const items = node as Internal[];

        const mut = baseSchema(arrayTag, false, arrayDecoder);
        mut.items = items;
        mut.additionalItems = "strict";
        return mut;
      } else {
        // A prototype other than Object.prototype (or null, e.g. Object.create(null))
        // means `definition` is a genuine class instance (Date, RegExp, a user
        // class, ...) to match as a literal — not a plain-record description.
        // Checking definition["constructor"] instead would misclassify any plain
        // record that happens to declare an own field named "constructor".
        const proto = Object.getPrototypeOf(definition);
        if (proto !== null && proto !== Object.prototype) {
          const mut = baseSchema(instanceTag, true, literalDecoder);
          mut.class = (definition as Record<string, unknown>)["constructor"];
          mut.const = definition;
          return mut;
        } else {
          const node = definition as Record<string, unknown>;
          const fieldNames = Object.keys(node);
          const length = fieldNames.length;
          for (let idx = 0; idx < length; idx++) {
            const location = fieldNames[idx]!;
            node[location] = traverseDefinition(node[location], onNode);
          }
          const mut = baseSchema(objectTag, false, objectDecoder);
          mut.required = fieldNames;
          mut.properties = node as Record<string, Internal>;
          mut.additionalItems = globalConfig.a;
          return mut;
        }
      }
    }
  } else {
    return Literal_parse(definition);
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
// default expression can still reference the module-level `unit` singleton.
export const optionFactory = (item: Internal, unitSchema: Internal = unit): Internal => {
  const out = getOutputSchema(item);
  if (out.type === undefinedTag) {
    return unionFactory([unitSchema, nestedOption(item)]);
  } else if (out.type === anyOfTag) {
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
              // copySchema, not a spread: a spread keeps the original's seq,
              // and two schemas sharing a seq can collide in the seq-keyed
              // operation caches.
              const bumped = copySchema(nestedSchema);
              bumped.const = (nestedSchema.const as number) + 1;
              // FIXME: dict{}
              const properties: Record<string, Internal> = {};
              properties[nestedLoc] = bumped;
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

// @__NO_SIDE_EFFECTS__
export const option = (item: Internal): Internal => {
  return optionFactory(item, unit);
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
          schema.perVariant = true;
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
      i: isLiteral(schema)
        ? B_inlineConst(parent, schema)
        : parent.s.type === objectTag && location in Object.prototype
          ? `(Object.hasOwn(${parent.v()},${inlinedValueFromString(location)})?${parent.v()}${pathAppend}:void 0)`
          : `${parent.v()}${pathAppend}`,
      s: schema,
      io: U,
      e: schema,
      prev: U,
      f: valFlagNone,
      d: U,
      fv: U,
    sp: U,
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
