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
  immutableEmptyArray,
  immutableEmptyObject,
  inlinedValueFromString,
  type Internal,
  isLiteral,
  isOptional,
  jsonName,
  objectTag,
  pathConcat,
  pathFromInlinedLocation,
  tagFlagArray,
  tagFlagObject,
  tagFlagRef,
  tagFlags,
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
import { isArrayCond, Literal_parse, objectTagCond, unit } from "./primitives";
import { unionFactory } from "./union";

// Narrows the dict-value-schema-or-mode union down to the schema case.
const isItemSchema = (x: AdditionalItems | undefined): x is Internal =>
  x !== U && typeof x !== "string";

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
// @__NO_SIDE_EFFECTS__
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
      if (isJsonParent && schema.type === anyOfTag && schema.has![undefinedTag]) {
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

// @__NO_SIDE_EFFECTS__
export const dictFactory = (item: Internal): Internal => {
  const mut = baseSchema(objectTag, item.r === item);
  mut.properties = immutableEmptyObject as Record<string, Internal>;
  mut.additionalItems = item;
  mut.decoder = objectDecoder;
  return mut;
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
