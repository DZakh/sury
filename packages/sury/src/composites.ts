// An object/array val (`makeObjectVal`'s result) reuses the plain `Val`
// shape — there's no separate "object val" type.

import {
  anyOfTag,
  type AdditionalItems,
  arrayTag,
  baseSchema,
  copySchema,
  type Check,
  type Encoder,
  type ErrorDetails,
  globalConfig,
  immutableEmptyArray,
  immutableEmptyObject,
  inlinedObjectKey,
  inlinedValueFromString,
  instanceTag,
  type Internal,
  isLiteral,
  isOptional,
  isSchemaObject,
  jsonName,
  noopDecoder,
  objectTag,
  pathConcat,
  setHas,
  tagFlags,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  type Val
} from "./base";
import {
  _notVar,
  _notVarAtParent,
  _var,
  B_addKey,
  B_addObjectField,
  B_asyncVal,
  B_dynamicScope,
  B_embedInvalidInput,
  B_failWithArg,
  B_hoistChildChecks,
  B_hoistDecl,
  B_inlineConst,
  B_markOutput,
  B_merge,
  B_mergeWithPathPrepend,
  B_next,
  B_refine,
  B_scope,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType
} from "./builder";
import {
  getOutputSchema,
  parse,
  parseDynamic,
} from "./parse";
import {
 isArrayCond,
 Literal_parse,
 literalDecoder,
 objectTagCond,
 unit
} from "./primitives";

// Narrows the dict-value-schema-or-mode union down to the schema case.
const isItemSchema = (x: AdditionalItems | undefined): x is Internal =>
  x !== U && typeof x !== "string";

// A `.to` target that builds its document piecewise (jsonString) can take a
// container raw: its `fz` hook (installed in advanced/json.ts) hands back the
// container schema marked `uv` when validation can be left to the aggregate,
// which does it inside the same pass that renders. For a dynamic container
// (`item` given) that is the whole item loop; for a fixed one every field is
// left raw except a union member's literals, whose discriminant has to be
// hoisted from here. Only the target knows when, so this side just asks, and
// a bundle without jsonString ships no decision.
const B_fused = (input: Val, expectedSchema: Internal, item?: Internal): Internal | undefined => {
  const to = expectedSchema.to;
  return to !== U && to.fz !== U ? to.fz(input, expectedSchema, item) : U;
};

// The wire form of a nested json-format string is an escaped string value, not
// raw JSON text (see fieldPiece in advanced/json.ts). So a JSON-sourced item (a
// JSON.parse result typed `json`) converting to one holds the document itself —
// narrowing the source to `unknown` routes it to jsonString's own decoder
// instead of json's serialize encoder, which would re-stringify and double-wrap
// on encode, and would hand a declared payload (CONTENT_CODEC_SPEC.md rule 3)
// the text it had just escaped instead of parsing it.
const B_narrowJsonSourcedJsonString = (itemInput: Val): void => {
  if (itemInput.s.name === jsonName && itemInput.e.format === "json") {
    itemInput.s = unknown;
  }
};

const B_makeContainerVal = (prev: Val, schema: Internal): Val => ({
  b: U,
  p: U,
  v: _notVar,
  i: "",
  s: schema,
  io: U,
  e: prev.e,
  prev,
  f: 0,
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
});

export const makeObjectVal = (prev: Val, _schema?: Internal): Val =>
  B_makeContainerVal(prev, {
    type: objectTag,
    required: [],
    properties: Object.create(null),
    additionalItems: "strict",
    decoder: objectDecoder,
  } as Internal);

export const makeArrayVal = (prev: Val, _schema?: Internal): Val =>
  B_makeContainerVal(prev, {
    type: arrayTag,
    items: [],
    additionalItems: "strict",
    decoder: arrayDecoder,
  } as Internal);
export const completeObjectVal = (objectVal: Val): Val => {
  const isArray = objectVal.s.type === arrayTag;
  let inline = "";
  let promiseAllContent = "";
  let optionalSettingCode: ((objectVar: string) => string) | undefined = U;

  const keys = Object.keys(objectVal.d!);

  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const val = objectVal.d![key]!;
    if ((val.f & 1)) {
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
        (isArray ? `${val.i}` : `${inlinedObjectKey(key)}:${val.i}`) +
        ",";
    }
  }

  objectVal.i = isArray ? "[" + inline.slice(0, -1) + "]" : "{" + inline.slice(0, -1) + "}";

  // FIXME: Test whether re-asserting `additionalItems = "strict"` here is
  // needed, now that the object's properties are already fully assembled.
  const valWithRequired = objectVal;

  if (promiseAllContent) {
    promiseAllContent = promiseAllContent.slice(0, -1);
    const operationInput = B_scope(valWithRequired);
    operationInput.io = true;
    const operationOutput = parse(operationInput);
    let operationCode = B_merge(operationOutput);
    let result = operationOutput.i;

    // Inside the `.then`, where the fields the optional ones read are bound:
    // the sync branch below appends the same code after the literal, and
    // leaving it off here dropped every optional field of an object that had
    // any async one.
    if (optionalSettingCode !== U) {
      const objectVar = B_varWithoutAllocation(objectVal.g);
      operationCode =
        operationCode + `let ${objectVar}=${result};` + optionalSettingCode(objectVar);
      result = objectVar;
    }

    if (operationCode === "" && promiseAllContent === result) {
      valWithRequired.i = result;
    } else {
      valWithRequired.i = `Promise.all([${promiseAllContent}]).then(([${promiseAllContent}])=>{${operationCode}return ${result}})`;
    }
    valWithRequired.f |= 1;
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
  if ((unknownInputTagFlag & (1 | 128))) {
    const isArrayInput = (unknownInputTagFlag & 128);
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
      if (expectedLength === 0) {
        // Plain-array fusion only: fixed tuple slots are read by the aggregate
        // outside its dynamic loop, so they must stay validated here.
        const fused = B_fused(input, expectedSchema, itemSchema);
        if (fused !== U) {
          return B_markOutput(B_refine(input, fused), input);
        }
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

      if ((itemOutput.f & 1)) {
        output = B_asyncVal(output2, `Promise.all(${output2.i})`);
      } else {
        output = output2;
      }
    }
  } else {
    const objectVal = makeArrayVal(input, expectedSchema);
    const fused = B_fused(input, expectedSchema);
    const ai = expectedSchema.additionalItems;
    // A fused tuple is read slot by slot off this val, so a rebuilt array
    // would go unread; strict has a check validating the exact length.
    let shouldRecreateInput =
      fused === U &&
      ai !== "strict" &&
      (ai !== "strip" ||
        isItemSchema(input.s.additionalItems) ||
        input.s.items!.length !== expectedLength);

    for (let idx = 0; idx < expectedLength; idx++) {
      const schema = expectedItems[idx]!;
      const key = String(idx);
      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion; // We want to control validation on the decoder side
      if (fused !== U && !(isUnion && isLiteral(schema))) {
        B_addObjectField(objectVal, key, itemInput);
        continue;
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

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      // Same stale-schema class as #284/#252: carry expectedSchema, not
      // input.schema (which may be a minimal union dispatch narrow), so a
      // pending `.to(json)` conversion routes through the fixed-items path
      const o = B_refine(input, fused || expectedSchema);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
}
// Shared, immutable: B_refine wraps it in a fresh array. Must match
// typeCheckCond's object tag — strip could skip `!Array.isArray` (it
// rebuilds, so an array would decode to `{}`) but that widens the union
// acceptance mask.
const objectTypeCheck: Check = {
  c: (inputVar) => `${objectTagCond(inputVar)}&&!${isArrayCond(inputVar)}`,
  f: failInvalidType,
};

export const objectDecoder = (unknownInput: Val): Val => {
  const isUnion = unknownInput.u!;
  const expectedSchema = unknownInput.e;

  const unknownInputTagFlag = tagFlags[unknownInput.s.type]!;

  let input: Val;
  if ((unknownInputTagFlag & (1 | 64))) {
    const isObjectInput = (unknownInputTagFlag & 64);
    let schema: Internal;
    if (!isObjectInput) {
      // Not dictFactory(unknown): unknown.sr is true; this input schema must not be.
      const mut = baseSchema(objectTag, false, objectDecoder);
      mut.properties = immutableEmptyObject as Record<string, Internal>;
      mut.additionalItems = unknown;
      schema = mut;
    } else {
      schema = unknownInput.s;
    }
    // Refine even with no checks so literals for union cases don't mutate input.
    input = isObjectInput
      ? B_refine(unknownInput, schema)
      : B_refine(unknownInput, schema, [objectTypeCheck]);
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
    const fused = B_fused(input, expectedSchema, dictItem);
    if (fused !== U) {
      return B_markOutput(B_refine(input, fused), input);
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

    if ((itemOutput.f & 1)) {
      const resolveVar = B_varWithoutAllocation(output2.g);
      const rejectVar = B_varWithoutAllocation(output2.g);
      const asyncParseResultVar = B_varWithoutAllocation(output2.g);
      const counterVar = B_varWithoutAllocation(output2.g);
      const outputVar = output2.v();
      output = B_asyncVal(
        output2,
        // `if(!counter)` first: with no keys the loop never runs, so nothing
        // would ever resolve the promise.
        `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;if(!${counterVar}){${resolveVar}(${outputVar})}for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`,
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
    const keys = Object.keys(properties);
    const keysCount = keys.length;

    const objectVal = makeObjectVal(input, expectedSchema);
    const ai = expectedSchema.additionalItems;
    const fused = B_fused(input, expectedSchema);
    let shouldRecreateInput =
      fused === U &&
      ai !== "strict" &&
      (ai !== "strip" || sourceIsDict || Object.keys(input.s.properties!).length !== keysCount);

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
    const isJsonParent = isItemSchema(inputAdditionalItems) && inputAdditionalItems.name === jsonName;

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
      if (fused !== U && !(isUnion && isLiteral(schema))) {
        B_addObjectField(objectVal, key, itemInput);
        continue;
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

    if (ai === "strict" && isItemSchema(inputAdditionalItems)) {
      const keyVar = B_varWithoutAllocation(objectVal.g);
      B_hoistDecl(input, keyVar);
      const fail = B_failWithArg(
        input,
        (excessFieldName: string) =>
          ({
            code: "unrecognized_keys",
            path: objectVal.path,
            reason: `Unrecognized key "${excessFieldName}"`,
            keys: [excessFieldName],
          }) as ErrorDetails,
        keyVar,
      );
      let cond = "";
      for (let idx = 0; idx < keysCount; idx++) {
        if (idx) cond += "&&";
        cond += `${keyVar}!==${inlinedValueFromString(keys[idx]!)}`;
      }
      objectVal.cp +=
        `for(${keyVar} in ${input.v()})` + (cond ? `if(${cond})` : "") + fail + ";";
    }

    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      // The value was just validated against expectedSchema — carry it as
      // the val's schema instead of input.schema, which may be a minimal
      // union dispatch narrow ({properties:{}, additionalItems: unknown}).
      // Keeping the narrow mis-routed a pending `.to(json)` conversion
      // into the dict path, which rejects undefined optional fields (#252)
      const o = B_refine(input, fused || expectedSchema);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
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

// Dict-missing-key as `T | undefined` without unionFactory, so valGet does
// not put the union compiler on the objectDecoder/arrayDecoder SCC. A missing
// key against an optional target stays absent (None); against a required
// target it fails — not the string `"undefined"`.
const missingKeyEncoder: Encoder = (input, target) => {
  const item = input.s.anyOf![0]!;
  const v = input.v();

  const presentIn = B_scope(input);
  presentIn.io = false;
  presentIn.s = item;
  presentIn.e = target;
  presentIn.u = true;
  const presentOut = parse(presentIn);
  const presentCode = B_merge(presentOut);
  const presentAssign = presentOut.i === v ? "" : `${v}=${presentOut.i};`;

  // Optional field: leave `undefined` as-is (None). Required field: reject.
  const absentCode = isOptional(target) ? "" : B_embedInvalidInput(input, target);

  const output = B_next(input, v, getOutputSchema(target), target);
  output.v = _var;
  output.io = true;
  const presentBody = presentCode + presentAssign;
  output.cp =
    presentBody === ""
      ? absentCode === ""
        ? ""
        : `${v}!==void 0||${absentCode};`
      : absentCode === ""
        ? `if(${v}!==void 0){${presentBody}}`
        : `if(${v}!==void 0){${presentBody}}else{${absentCode}}`;
  return output;
};

const wrapDictMissingKeyLight = (s: Internal): Internal => {
  const mut = baseSchema(anyOfTag, false, noopDecoder);
  mut.anyOf = [s, unit];
  mut.has = { [undefinedTag]: true };
  setHas(mut.has, s.type);
  mut.encoder = missingKeyEncoder;
  mut.perVariant = true;
  return mut;
};

const wrapMissingDictKey = wrapDictMissingKeyLight;

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
        // json/unknown values read as-is. Light T|undefined wrap (not
        // optionFactory) so this decoder SCC does not statically retain union.
        if (
          parent.s.type === objectTag &&
          s.type !== unknownTag &&
          !(tagFlags[s.type]! & 512) &&
          !isOptional(s)
        ) {
          schema = wrapMissingDictKey(s);
        } else {
          schema = s;
        }
      } else {
        schema = B_unsupportedDecode(parent, parent.s, parent.e);
      }
    }

    const accessor = `[${inlinedValueFromString(location)}]`;

    // Canonical Val field order (see B_operationArg in builder.ts).
    const item: Val = {
      b: U,
      p: parent,
      v: _notVarAtParent,
      i: isLiteral(schema)
        ? B_inlineConst(parent, schema)
        : parent.s.type === objectTag && location in Object.prototype
          ? `(Object.hasOwn(${parent.v()},${inlinedValueFromString(location)})?${parent.v()}${accessor}:void 0)`
          : `${parent.v()}${accessor}`,
      s: schema,
      io: U,
      e: schema,
      prev: U,
      f: 0,
      d: U,
      fv: U,
      cp: "",
      hd: "",
      fz: U,
      vc: U,
      u: U,
      t: U,
      path: pathConcat(parent.path, [location]),
      g: parent.g,
      o: U,
    };
    vals[location] = item;
    return item;
  }
}
