// `S.json` and its string form. A recursive union of the JSON types, plus the
// encoder/decoder that represent an arbitrary schema as JSON — the only schema
// that rewrites another schema's shape rather than just validating it.

import {
  anyOfTag,
  arrayTag,
  baseSchema,
  type Builder,
  copySchema,
  defsPath,
  type Encoder,
  flagAsync,
  flagUnsafeHas,
  initSchema,
  inlinedValueFromString,
  type Internal,
  isLiteral,
  jsonName,
  refTag,
  stringTag,
  type Tag,
  tagFlagArray,
  tagFlagBigint,
  tagFlagBoolean,
  tagFlagNaN,
  tagFlagNull,
  tagFlagNumber,
  tagFlagObject,
  tagFlagRef,
  tagFlags,
  tagFlagString,
  tagFlagUndefined,
  tagFlagUnion,
  tagFlagUnknown,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  updateOutput,
  type Val,
} from "../base";
import {
  _var,
  B_addObjectField,
  B_dynamicScope,
  B_embedPure,
  B_embedInvalidInput,
  B_merge,
  B_mergeWithPathPrepend,
  B_next,
  B_nextConst,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
} from "../builder";
import {
  array,
  arrayDecoder,
  completeObjectVal,
  dictFactory,
  makeObjectVal,
  valGet,
} from "../composites";
import { getOutputSchema, parse, parseDynamic } from "../parse";
import {
  bool,
  float,
  inputToString,
  literalDecoder,
  nullLiteral,
  string,
  stringDecoderFn,
} from "../primitives";
import { unionDecoder, unionFactory, unionRewriteTo } from "../union";
import { recursiveDecoder } from "./recursive";

export const jsonEncoderFn = (input: Val, target: Internal): Val => {
  // A json-formatted string target means "serialize", not "coerce to string":
  // without this branch the string case below would re-validate the JSON value
  // as being a string, making S.json -> S.jsonString reject every non-string.
  if (target.format === "json") {
    return B_next(
      input,
      `JSON.stringify(${input.i}${target.space ? `,null,${target.space}` : ""})`,
      target,
      target,
    );
  }
  const toTagFlag = tagFlags[target.type]!;

  if (
    flagUnsafeHas(
      toTagFlag,
      tagFlagString | tagFlagBoolean | tagFlagNumber | tagFlagNull,
    )
  ) {
    return parse(B_refine(input, unknown, U, target));
  } else if (flagUnsafeHas(toTagFlag, (tagFlagUndefined | tagFlagNaN))) {
    const jsonExpected = copySchema(nullLiteral);
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  } else if (flagUnsafeHas(toTagFlag, tagFlagArray)) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    const jsonExpected = array(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json;
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, tagFlagObject)) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    const jsonExpected = dictFactory(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json;
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, (tagFlagUnion | tagFlagRef))) {
    return input;
  } else {
    // For non-JSON types (bigint, instance, etc.), decode through string
    const jsonExpected = copySchema(string);
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  }
}

export const isJsonable = (schema: Internal): boolean => {
  const tagFlag = tagFlags[schema.type]!;
  return (
    flagUnsafeHas(
      tagFlag,
      tagFlagString | tagFlagNumber | tagFlagBoolean | tagFlagNull,
    ) ||
    schema["$ref"] === json["$ref"] ||
    (flagUnsafeHas(tagFlag, tagFlagUnion) && schema.anyOf!.every(isJsonable)) ||
    (flagUnsafeHas(tagFlag, tagFlagArray) &&
      (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) &&
      schema.items!.every(isJsonable)) ||
    (flagUnsafeHas(tagFlag, tagFlagObject) &&
      (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) &&
      Object.values(schema.properties!).every(isJsonable))
  );
}

export const jsonDecoderFn = (input: Val): Val => {
  const inputTagFlag = tagFlags[input.s.type]!;

  if (isJsonable(input.s)) {
    return input;
  } else if (flagUnsafeHas(inputTagFlag, (tagFlagUndefined | tagFlagNaN))) {
    return B_nextConst(input, nullLiteral);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagArray)) {
    const expected = baseSchema(arrayTag, false);
    expected.items = input.s.items!.map((_) => json);
    expected.decoder = arrayDecoder;
    expected.additionalItems =
      typeof input.s.additionalItems === "object"
        ? json
        : input.s.additionalItems;
    expected.to = input.e.to;
    return parse(B_refine(input, U, U, expected));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagObject)) {
    if (typeof input.s.additionalItems === "object") {
      const expected = dictFactory(json);
      expected.to = input.e.to;
      return parse(B_refine(input, U, U, expected));
    } else {
      const jsonVal = makeObjectVal(input, input.s);
      jsonVal.e = json;
      if (input.e.to) {
        jsonVal.e = copySchema(jsonVal.e);
        jsonVal.e.to = input.e.to;
      }

      const keys = Object.keys(input.s.properties!);
      for (let idx = 0; idx <= keys.length - 1; idx++) {
        const key = keys[idx]!;
        const itemVal = valGet(input, key);
        itemVal.io = false;

        if (itemVal.s.type === anyOfTag && itemVal.s.has![undefinedTag]) {
          // Per-variant conversion instead of a generic `undefined | JSON`
          // check: an undefined variant stays undefined so the object
          // rebuild omits the field, while non-jsonable variants get
          // `.to(json)` appended and keep converting recursively (#311)
          const mapped = unionFactory(
            itemVal.s.anyOf!.map((variant) => {
              const variantOutput = getOutputSchema(variant);
              return variantOutput.type === undefinedTag || isJsonable(variantOutput)
                ? variant
                : updateOutput<Internal>(variant, (mut) => {
                    mut.to = json;
                  });
            })
          );
          // Already resolved variant by variant, so the union encoder pairs them
          // by position instead of re-matching them by type.
          mapped.perVariant = true;
          itemVal.e = mapped;
          const itemOutput = parse(itemVal);
          itemOutput.o = true;
          B_addObjectField(jsonVal, key, itemOutput);
        } else {
          itemVal.e = json;
          B_addObjectField(jsonVal, key, parse(itemVal));
        }
      }

      return completeObjectVal(jsonVal);
    }
  } else if (flagUnsafeHas(inputTagFlag, tagFlagRef)) {
    // FIXME: Should be a unified solution for ref inputs
    return recursiveDecoder(input);
  } else if (
    flagUnsafeHas(inputTagFlag, tagFlagUnion) &&
    // Union-tagged schemas always carry `anyOf` and `has`
    // (set by unionFactory, reverse and the S.json def).
    // Unions with an undefined variant are not supported,
    // since undefined is not representable in JSON
    !(undefinedTag in input.s.has!)
  ) {
    // Decode each union variant to JSON separately
    return parse(unionRewriteTo(input, input.e));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    const to = input.e.to!;
    // Whether we can optimize encoding during decoding. Encoding into a
    // concrete type validates implicitly — except a json-format target, whose
    // JSON.stringify accepts (or silently drops) anything, so it still needs
    // the JSON validation here.
    // FIXME: should this also check !input.e.refiner, like jsonStringDecoder's preEncode does?
    const preEncode: boolean = !!to && to.format !== "json" && !input.e.parser;
    if (preEncode) {
      input.s = json;
      return jsonEncoderFn(input, input.e);
    } else if (input.e.noValidation!) {
      input.s = json;
      return input;
    } else {
      return recursiveDecoder(input);
    }
  } else {
    try {
      const expected = copySchema(string);
      expected.to = input.e;
      input.e = expected;
      return parse(input);
    } catch {
      return B_unsupportedDecode(input, input.s, json);
    }
  }
}

export const json: Internal = /* @__PURE__ */ initSchema(refTag, (s) => {
  const jsonRef = baseSchema(refTag, true);
  jsonRef["$ref"] = `${defsPath}${jsonName}`;
  jsonRef.name = jsonName;

  jsonRef.decoder = jsonDecoderFn;
  jsonRef.encoder = jsonEncoderFn;

  s["$ref"] = jsonRef["$ref"];
  s.name = jsonName;
  s.decoder = jsonDecoderFn;
  s.encoder = jsonEncoderFn;

  const anyOf = [
    string,
    bool,
    float,
    nullLiteral,
    dictFactory(jsonRef),
    array(jsonRef),
  ];
  const has: Partial<Record<Tag, boolean>> = {};
  anyOf.forEach((schema) => {
    has[schema.type] = true;
  });

  const jsonDef = baseSchema(anyOfTag, true);
  jsonDef.anyOf = anyOf;
  jsonDef.has = has;
  jsonDef.decoder = unionDecoder;
  jsonDef.name = jsonName;
  jsonDef.type = anyOfTag;

  const defs: Record<string, Internal> = {};
  defs[jsonName] = jsonDef;
  s["$defs"] = defs;
});

// Runtime helper embedded into generated jsonString code: the JSON text of a
// string value. The fast path skips JSON.stringify's escape handling when a
// regex scan proves no character needs it; thresholds follow
// fast-json-stringify (the scan + concat loses to JSON.stringify on long
// strings).
const strEscapeRe = /[\u0000-\u001f"\\\ud800-\udfff]/;
const asJsonString = (value: string): string =>
  value.length < 5000 && !strEscapeRe.test(value) ? `"${value}"` : JSON.stringify(value);

// The raw JSON text of a literal schema's value, or undefined for a const with
// no JSON representation. JSON.stringify (not inlinedValueFromString) so string
// escapes and non-finite numbers (-> null) are correct JSON.
const constJsonText = (schema: Internal): string | undefined => {
  const tagFlag = tagFlags[schema.type]!;
  if (flagUnsafeHas(tagFlag, ((tagFlagUndefined | tagFlagNull) | tagFlagNaN))) {
    return "null";
  } else if (
    flagUnsafeHas(tagFlag, (tagFlagString | tagFlagNumber) | tagFlagBoolean)
  ) {
    return JSON.stringify(schema.const)!;
  } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
    return `"${schema.const}"`;
  } else {
    return U;
  }
};

export const jsonString = /* @__PURE__ */ (() => {
  const inlineJsonString = (input: Val, schema: Internal): string => {
    const text = constJsonText(schema);
    return text !== U
      ? inlinedValueFromString(text)
      : B_unsupportedDecode(input, schema, input.e);
  };

  const constSchemaToJsonStringConst = (input: Val, target: Internal): string => {
    const text = constJsonText(target);
    return text !== U ? text : B_unsupportedDecode(input, input.s, target);
  };

  const jsonStringEncoder: Encoder = (input, target) => {
    if (target.format !== "json") {
      if (isLiteral(target)) {
        const jsonStringConstSchema = baseSchema(stringTag, true);
        jsonStringConstSchema.const = constSchemaToJsonStringConst(input, target);
        jsonStringConstSchema.to = target;
        jsonStringConstSchema.decoder = literalDecoder;
        return B_refine(input, U, U, jsonStringConstSchema);
      } else {
        const outputVar = B_varWithoutAllocation(input.g);

        const nextSchema = copySchema(json);
        nextSchema.to = target;

        const output = B_next(input, outputVar, nextSchema, nextSchema);
        output.io = true;
        output.v = _var;

        const inputVar = input.v();
        output.cp = `let ${outputVar};try{${outputVar}=JSON.parse(${inputVar})}catch(t){${B_embedInvalidInput(
          input,
          input.s,
        )}}`;

        return output;
      }
    } else {
      return input;
    }
  };

  // Retarget a resolved val at jsonString. Values jsonString itself can't
  // represent (unknown, recursive refs) validate through `json` first and
  // stringify at runtime — the same coverage the old whole-value `json` +
  // JSON.stringify path had, but scoped to the one subtree that needs it.
  const toPiece = (val: Val): Val => {
    const tagFlag = tagFlags[val.s.type]!;
    if (
      flagUnsafeHas(tagFlag, tagFlagUnknown) ||
      (flagUnsafeHas(tagFlag, tagFlagRef) && val.s["$ref"] !== json["$ref"])
    ) {
      const jsonVal = parse(B_refine(val, U, U, json));
      return B_next(jsonVal, `JSON.stringify(${jsonVal.i})`, jsonString, jsonString);
    }
    return parse(B_refine(val, U, U, jsonString));
  };

  // `""+x` folds away when the piece lands after an already-string part of a
  // concatenation, which is where every piece lands.
  const foldStringCoercion = (piece: string): string =>
    piece.startsWith(`""+`) ? piece.slice(3) : piece;

  // A field/dict-value piece: `p` produces the JSON text, `g` (when set) is the
  // var to test against void 0 — an undefined-able value renders by omission,
  // matching JSON.stringify. The two-variant `X | undefined` shape skips the
  // union dispatch entirely when X stays a pure expression: the `!== void 0`
  // guard IS the dispatch. Restricted to primitive X so the piece can't own
  // statements that would then run unguarded.
  const fieldPiece = (itemVal: Val): { p: Val; g: string | undefined } => {
    const cur = itemVal.s;
    if (cur.type === anyOfTag && cur.has![undefinedTag]) {
      const variants = cur.anyOf!;
      if (variants.length === 2) {
        const undefinedFirst = getOutputSchema(variants[0]!).type === undefinedTag;
        const single = variants[undefinedFirst ? 1 : 0]!;
        if (
          getOutputSchema(variants[undefinedFirst ? 0 : 1]!).type === undefinedTag &&
          getOutputSchema(single).type !== undefinedTag &&
          single.to === U &&
          flagUnsafeHas(
            tagFlags[single.type]!,
            (((tagFlagString | tagFlagNumber) | (tagFlagBoolean | tagFlagBigint)) |
              (tagFlagNull | tagFlagNaN)),
          )
        ) {
          const guard = itemVal.v();
          return { p: parse(B_refine(itemVal, single, U, jsonString)), g: guard };
        }
      }
      // Per-variant conversion, mirroring the `json` object-field mapping
      // above: undefined variants stay undefined for the omission guard.
      const mapped = unionFactory(
        variants.map((variant) =>
          getOutputSchema(variant).type === undefinedTag
            ? variant
            : updateOutput<Internal>(variant, (mut) => {
                mut.to = jsonString;
              }),
        ),
      );
      mapped.perVariant = true;
      const p = parse(B_refine(itemVal, U, U, mapped));
      return { p, g: p.v() };
    }
    return { p: toPiece(itemVal), g: U };
  };

  const jsonStringAggregate = (input: Val, expectedSchema: Internal): Val => {
    const schema = input.s;
    const isArr = schema.type === arrayTag;
    const additionalItems = schema.additionalItems;
    const dynamicItem =
      additionalItems !== U && typeof additionalItems === "object"
        ? additionalItems
        : U;

    // A dict serializes only its dynamic values (objectDecoder's dict shape).
    const keys = isArr ? U : dynamicItem !== U ? [] : Object.keys(schema.properties!);
    const items = isArr ? schema.items! : U;
    const fixedLen = isArr ? items!.length : keys!.length;

    let code = "";
    const entryTexts: (string | undefined)[] = [];
    const entryVals: (Val | undefined)[] = [];
    const entryGuards: (string | undefined)[] = [];
    let hasOpt = false;

    for (let idx = 0; idx < fixedLen; idx++) {
      const location = isArr ? "" + idx : keys![idx]!;
      const fieldSchema = isArr ? items![idx]! : schema.properties![location]!;
      if (isLiteral(fieldSchema) && fieldSchema.to === U) {
        const text = constJsonText(fieldSchema);
        if (text !== U) {
          entryTexts.push(text);
          entryVals.push(U);
          entryGuards.push(U);
          continue;
        }
      }
      const itemVal = valGet(input, location);
      // Tuple items render undefined as null (like JSON.stringify), so they
      // convert as a whole; only object fields get the omission guard.
      const { p, g } = isArr ? { p: toPiece(itemVal), g: U } : fieldPiece(itemVal);
      if (g !== U) {
        hasOpt = true;
      }
      code = code + B_merge(p);
      entryTexts.push(U);
      entryVals.push(p);
      entryGuards.push(g);
    }

    // JS-expression accumulator: alternating raw JSON text chunks and pieces.
    let expr = "";
    let chunk = "";
    const flush = (): void => {
      if (chunk !== "") {
        expr = expr + (expr === "" ? "" : "+") + inlinedValueFromString(chunk);
        chunk = "";
      }
    };
    const push = (piece: string): void => {
      flush();
      expr =
        expr === "" ? piece : expr + "+" + foldStringCoercion(piece);
    };
    const keyText = (idx: number): string =>
      isArr ? "" : JSON.stringify(keys![idx]) + ":";

    if (dynamicItem !== U) {
      const inputVar = input.v();
      const iterVar = B_varWithoutAllocation(input.g);
      const accVar = B_varWithoutAllocation(input.g);
      const keyEmbed = isArr ? "" : B_embedPure(input, asJsonString);
      const raiseCountBefore = input.g.t;
      const itemInput = B_dynamicScope(input, iterVar);
      itemInput.e = itemInput.s;
      const resolved = parseDynamic(itemInput);
      const { p, g } = isArr ? { p: toPiece(resolved), g: U } : fieldPiece(resolved);
      const appendCode = isArr
        ? `${accVar}+=${
            fixedLen ? `","` : `(${iterVar}?",":"")`
          }+${foldStringCoercion(p.i)}`
        : `${accVar}+=(${accVar}?",":"")+${keyEmbed}(${iterVar})+":"+${foldStringCoercion(p.i)}`;
      const itemCode = B_mergeWithPathPrepend(
        p,
        input,
        iterVar,
        () => (g !== U ? `if(${g}!==void 0){${appendCode}}` : appendCode),
        raiseCountBefore,
      );
      const loopCode = isArr
        ? `let ${accVar}="";for(let ${iterVar}=${fixedLen};${iterVar}<${inputVar}.length;++${iterVar}){${itemCode}}`
        : `let ${accVar}="";for(let ${iterVar} in ${inputVar}){${itemCode}}`;

      chunk = isArr ? "[" : "{";
      for (let idx = 0; idx < fixedLen; idx++) {
        chunk = chunk + (idx === 0 ? "" : ",");
        const text = entryTexts[idx];
        if (text !== U) {
          chunk = chunk + text;
        } else {
          push(entryVals[idx]!.i);
        }
      }
      push(accVar);
      chunk = isArr ? "]" : "}";
      flush();
      const output = B_next(input, expr, expectedSchema, expectedSchema);
      output.cp = code + loopCode;
      return output;
    }

    if (!hasOpt) {
      chunk = isArr ? "[" : "{";
      for (let idx = 0; idx < fixedLen; idx++) {
        chunk = chunk + (idx === 0 ? "" : ",") + keyText(idx);
        const text = entryTexts[idx];
        if (text !== U) {
          chunk = chunk + text;
        } else {
          push(entryVals[idx]!.i);
        }
      }
      chunk = chunk + (isArr ? "]" : "}");
      flush();
      const output = B_next(input, expr, expectedSchema, expectedSchema);
      output.cp = code;
      return output;
    }

    // Optional fields present: build into an accumulator var. Static comma for
    // a field that follows an always-present one, a runtime probe otherwise.
    const accVar = B_varWithoutAllocation(input.g);
    let stmts = "";
    const flushRun = (): void => {
      flush();
      if (expr !== "") {
        stmts = stmts + `${accVar}+=${expr};`;
        expr = "";
      }
    };
    let hasDefiniteBefore = false;
    for (let idx = 0; idx < fixedLen; idx++) {
      const guard = entryGuards[idx];
      if (guard === U) {
        if (idx !== 0 && !hasDefiniteBefore) {
          flushRun();
          push(`(${accVar}?",":"")`);
        } else if (idx !== 0) {
          chunk = chunk + ",";
        }
        chunk = chunk + keyText(idx);
        const text = entryTexts[idx];
        if (text !== U) {
          chunk = chunk + text;
        } else {
          push(entryVals[idx]!.i);
        }
        hasDefiniteBefore = true;
      } else {
        flushRun();
        stmts =
          stmts +
          `if(${guard}!==void 0){${accVar}+=${
            idx !== 0 && !hasDefiniteBefore ? `(${accVar}?",":"")+` : ""
          }${inlinedValueFromString(
            (idx !== 0 && hasDefiniteBefore ? "," : "") + keyText(idx),
          )}+${foldStringCoercion(entryVals[idx]!.i)}}`;
      }
    }
    flushRun();
    const output = B_next(input, `"{"+${accVar}+"}"`, expectedSchema, expectedSchema);
    output.cp = code + `let ${accVar}="";` + stmts;
    return output;
  };

  const jsonStringDecoder: Builder = (input) => {
    const inputTagFlag = tagFlags[input.s.type]!;
    const expectedSchema = input.e;

    if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      const to = expectedSchema.to!;
      // Whether we can optimize encoding during decoding
      const preEncode: boolean =
        !!to && to.type !== unknownTag && !expectedSchema.parser && !expectedSchema.refiner;

      const stringVal = stringDecoderFn(input);
      stringVal.s = expectedSchema;
      stringVal.e = expectedSchema;

      if (preEncode) {
        return jsonStringEncoder(stringVal, to);
      } else {
        const stringVar = stringVal.v();
        const output = B_refine(stringVal, expectedSchema);
        output.cp = `try{JSON.parse(${stringVar})}catch(t){${B_embedInvalidInput(
          stringVal,
        )}}`;
        return output;
      }
    } else if (input.s.format === "json") {
      return input;
    } else if (isLiteral(input.s)) {
      return B_next(input, inlineJsonString(input, input.s), expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return B_next(
        input,
        `${B_embedPure(input, asJsonString)}(${input.i})`,
        expectedSchema,
      );
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagNumber | tagFlagBoolean))) {
      const output = inputToString(input);
      output.s = expectedSchema;
      return output;
    } else if (flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
      return B_next(input, `"\\""+${input.i}+"\\""`, expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagObject | tagFlagArray))) {
      // Pretty-printing and async fields keep the whole-value JSON.stringify
      // path — inlined aggregation supports neither indentation nor promises.
      if (
        (expectedSchema.space !== U && expectedSchema.space !== 0) ||
        flagUnsafeHas(input.g.o, flagAsync)
      ) {
        const jsonVal = parse(B_refine(input, U, U, json));
        return B_next(
          jsonVal,
          `JSON.stringify(${jsonVal.i}${
            expectedSchema.space === 0 || expectedSchema.space === U
              ? ""
              : `,null,${expectedSchema.space}`
          })`,
          expectedSchema,
          expectedSchema,
        );
      }
      return jsonStringAggregate(input, expectedSchema);
    } else {
      // Same fallback `json` uses: decode to string first (covers instances
      // with a string representation, e.g. Date), then serialize that.
      try {
        const expected = copySchema(string);
        expected.to = expectedSchema;
        input.e = expected;
        return parse(input);
      } catch {
        return B_unsupportedDecode(input, input.s, expectedSchema);
      }
    }
  };

  return initSchema(stringTag, (s) => {
    s.format = "json";
    s.name = `${jsonName} string`;
    s.encoder = jsonStringEncoder;
    s.decoder = jsonStringDecoder;
  });
})();

// @__NO_SIDE_EFFECTS__
export const jsonStringWithSpace = (space: number): Internal => {
  const mut = copySchema(jsonString);
  mut.space = space;
  return mut;
}
