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
  failInvalidType,
} from "../builder";
import { internalRefine } from "../modifiers";
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

// The one JSON.stringify call shape: space 0/undefined omits the indent
// argument. Both jsonEncoderFn and the pretty-print fallback go through it so
// the space convention can't diverge.
const B_stringifyCall = (i: string, space: number | undefined): string =>
  `JSON.stringify(${i}${space ? `,null,${space}` : ""})`;

export const jsonEncoderFn = (input: Val, target: Internal): Val => {
  // A json-formatted string target means "serialize", not "coerce to string":
  // without this branch the string case below would re-validate the JSON value
  // as being a string, making S.json -> S.jsonString reject every non-string.
  if (target.format === "json") {
    return B_next(input, B_stringifyCall(input.i, target.space), target, target);
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

// Per-variant conversion instead of a generic `undefined | X` check: an
// undefined variant stays undefined so the object rebuild omits the field,
// while the rest get `.to(target)` appended and keep converting recursively
// (#311). `keep` names the variant outputs that already convert as-is.
const perVariantTo = (
  variants: Internal[],
  target: Internal,
  keep: (variantOutput: Internal) => boolean,
): Internal => {
  const mapped = unionFactory(
    variants.map((variant) => {
      const variantOutput = getOutputSchema(variant);
      return variantOutput.type === undefinedTag || keep(variantOutput)
        ? variant
        : updateOutput<Internal>(variant, (mut) => {
            mut.to = target;
          });
    }),
  );
  // Already resolved variant by variant, so the union encoder pairs them
  // by position instead of re-matching them by type.
  mapped.perVariant = true;
  return mapped;
};

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
          itemVal.e = perVariantTo(itemVal.s.anyOf!, json, isJsonable);
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
    // JSON has no non-finite numbers: bare `float` admits Infinity, which
    // JSON.stringify silently demotes to null and the jsonString aggregator
    // would splice as invalid text — raise at validation instead, matching
    // the number -> jsonString piece.
    internalRefine(float, () => () => [
      { c: (inputVar) => `Number.isFinite(${inputVar})`, f: failInvalidType },
    ]),
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

// An operation embeds the helper once, however many string pieces it has.
const B_embedJsonStr = (b: Val): string => {
  const idx = b.g.e.indexOf(asJsonString);
  return idx === -1 ? B_embedPure(b, asJsonString) : `e[${idx}]`;
};

// The raw JSON text of a literal schema's value, or undefined for a const with
// no JSON representation. JSON.stringify (not inlinedValueFromString) so string
// escapes and non-finite numbers (-> null) are correct JSON.
const B_constJsonText = (schema: Internal): string | undefined => {
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
    const text = B_constJsonText(schema);
    return text !== U
      ? inlinedValueFromString(text)
      : B_unsupportedDecode(input, schema, input.e);
  };

  const constSchemaToJsonStringConst = (input: Val, target: Internal): string => {
    const text = B_constJsonText(target);
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

  // `""+x` folds away when the piece lands after an already-string part of a
  // concatenation, which is where every piece lands.
  const foldStringCoercion = (piece: string): string =>
    piece.startsWith(`""+`) ? piece.slice(3) : piece;

  // Compile-time merge of adjacent string-literal operands: `"a"+"b"` → `"ab"`.
  // String concat is left-associative, so folding a literal pair joined by `+`
  // preserves semantics whenever the operator before the first literal binds no
  // tighter than `+`. Contents splice verbatim (only the quotes between two
  // merged literals are dropped), so escape semantics can't change.
  const mergeStrLits = (code: string): string => {
    const litEnd = (from: number): number => {
      let j = from + 1;
      while (j < code.length && code[j] !== '"') {
        j += code[j] === "\\" ? 2 : 1;
      }
      return j + 1;
    };
    let out = "";
    let from = 0;
    let i = code.indexOf('"');
    while (i !== -1) {
      let j = litEnd(i);
      out = out + code.slice(from, i);
      let lit = code.slice(i, j);
      const prev = out[out.length - 1];
      if (prev !== "-" && prev !== "*" && prev !== "/" && prev !== "%") {
        while (code[j] === "+" && code[j + 1] === '"') {
          const k = litEnd(j + 1);
          lit = lit.slice(0, -1) + code.slice(j + 2, k);
          j = k;
        }
      }
      out = out + lit;
      from = j;
      i = code.indexOf('"', j);
    }
    return out + code.slice(from);
  };

  // A serialization piece: `p` produces the JSON text, `g` (when set) is the
  // var to test against void 0 — an undefined-able value renders by omission,
  // matching JSON.stringify. Tuple items (`isArr`) render undefined as null
  // instead (also matching JSON.stringify), so they convert as a whole and
  // never guard.
  const fieldPiece = (itemVal: Val, isArr: boolean): { p: Val; g: string | undefined } => {
    const cur = itemVal.s;
    // Values jsonString itself can't decode piecewise (unknown, refs) validate
    // through `json` and stringify at runtime — the coverage the old
    // whole-value `json` + JSON.stringify path had, scoped to the one subtree
    // that needs it. JSON.stringify can still yield undefined (a toJSON
    // returning it), which whole-value stringify rendered as an omitted field
    // or a null item — the guard/`??"null"` keeps that contract.
    if (flagUnsafeHas(tagFlags[cur.type]!, tagFlagUnknown | tagFlagRef)) {
      const jsonVal = parse(B_refine(itemVal, U, U, json));
      if (isArr) {
        const p = B_next(
          jsonVal,
          `(JSON.stringify(${jsonVal.i})??"null")`,
          jsonString,
          jsonString,
        );
        return { p, g: U };
      }
      const outputVar = B_varWithoutAllocation(itemVal.g);
      const p = B_next(jsonVal, outputVar, jsonString, jsonString);
      p.v = _var;
      p.cp = `let ${outputVar}=JSON.stringify(${jsonVal.i});`;
      return { p, g: outputVar };
    }
    if (!isArr && cur.type === anyOfTag && cur.has![undefinedTag]) {
      const variants = cur.anyOf!;
      if (variants.length === 2) {
        // The two-variant `X | undefined` shape skips the union dispatch
        // entirely when X stays a pure expression: the `!== void 0` guard IS
        // the dispatch. Restricted to primitive X so the piece can't own
        // statements that would then run unguarded.
        const uIdx = getOutputSchema(variants[0]!).type === undefinedTag ? 0 : 1;
        const single = variants[1 - uIdx]!;
        if (
          getOutputSchema(variants[uIdx]!).type === undefinedTag &&
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
      const p = parse(
        B_refine(itemVal, U, U, perVariantTo(variants, jsonString, () => false)),
      );
      return { p, g: p.v() };
    }
    return { p: parse(B_refine(itemVal, U, U, jsonString)), g: U };
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
    const entries: { t?: string; p?: Val; g?: string }[] = [];
    let hasOpt = false;

    for (let idx = 0; idx < fixedLen; idx++) {
      const location = isArr ? "" + idx : keys![idx]!;
      const fieldSchema = isArr ? items![idx]! : schema.properties![location]!;
      if (isLiteral(fieldSchema) && fieldSchema.to === U) {
        const text = B_constJsonText(fieldSchema);
        if (text !== U) {
          entries.push({ t: text });
          continue;
        }
      }
      const { p, g } = fieldPiece(valGet(input, location), isArr);
      if (g !== U) {
        hasOpt = true;
      }
      code = code + B_merge(p);
      entries.push({ p, g });
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
    const emitEntry = (idx: number, comma: string): void => {
      chunk = chunk + comma + keyText(idx);
      const entry = entries[idx]!;
      if (entry.t !== U) {
        chunk = chunk + entry.t;
      } else {
        push(entry.p!.i);
      }
    };

    if (dynamicItem !== U || !hasOpt) {
      let loopCode = "";
      let dynAcc = "";
      if (dynamicItem !== U) {
        const inputVar = input.v();
        const iterVar = B_varWithoutAllocation(input.g);
        dynAcc = B_varWithoutAllocation(input.g);
        const keyEmbed = isArr ? "" : B_embedJsonStr(input);
        const raiseCountBefore = input.g.t;
        const itemInput = B_dynamicScope(input, iterVar);
        itemInput.e = itemInput.s;
        const resolved = parseDynamic(itemInput);
        const { p, g } = fieldPiece(resolved, isArr);
        const appendCode = isArr
          ? `${dynAcc}+=${
              fixedLen ? `","` : `(${iterVar}?",":"")`
            }+${foldStringCoercion(p.i)}`
          : `${dynAcc}+=(${dynAcc}?",":"")+${keyEmbed}(${iterVar})+":"+${foldStringCoercion(p.i)}`;
        const itemCode = B_mergeWithPathPrepend(
          p,
          input,
          iterVar,
          () => (g !== U ? `if(${g}!==void 0){${appendCode}}` : appendCode),
          raiseCountBefore,
        );
        loopCode = `let ${dynAcc}="";for(let ${iterVar}${
          isArr ? `=${fixedLen};${iterVar}<${inputVar}.length;++${iterVar}` : ` in ${inputVar}`
        }){${itemCode}}`;
      }

      chunk = isArr ? "[" : "{";
      for (let idx = 0; idx < fixedLen; idx++) {
        emitEntry(idx, idx === 0 ? "" : ",");
      }
      if (dynamicItem !== U) {
        push(dynAcc);
      }
      chunk = chunk + (isArr ? "]" : "}");
      flush();
      const output = B_next(input, mergeStrLits(expr), expectedSchema, expectedSchema);
      output.cp = code + mergeStrLits(loopCode);
      return output;
    }

    // Optional fields present: build into an accumulator var. Static comma for
    // a field that follows an always-present one, a runtime probe otherwise.
    // The first unconditional run seeds the accumulator's declaration.
    const accVar = B_varWithoutAllocation(input.g);
    let stmts = "";
    let accInit: string | undefined = U;
    const flushRun = (): void => {
      flush();
      if (expr !== "") {
        if (stmts === "" && accInit === U) {
          accInit = expr;
        } else {
          stmts = stmts + `${accVar}+=${expr};`;
        }
        expr = "";
      }
    };
    let hasDefiniteBefore = false;
    for (let idx = 0; idx < fixedLen; idx++) {
      const guard = entries[idx]!.g;
      if (guard === U) {
        if (idx !== 0 && !hasDefiniteBefore) {
          flushRun();
          push(`(${accVar}?",":"")`);
          emitEntry(idx, "");
        } else {
          emitEntry(idx, idx === 0 ? "" : ",");
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
          )}+${foldStringCoercion(entries[idx]!.p!.i)}}`;
      }
    }
    flushRun();
    const output = B_next(input, `"{"+${accVar}+"}"`, expectedSchema, expectedSchema);
    output.cp =
      code + mergeStrLits(`let ${accVar}=${accInit !== U ? accInit : `""`};` + stmts);
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
        `${B_embedJsonStr(input)}(${input.i})`,
        expectedSchema,
      );
    } else if (flagUnsafeHas(inputTagFlag, tagFlagBoolean)) {
      const output = inputToString(input);
      output.s = expectedSchema;
      return output;
    } else if (flagUnsafeHas(inputTagFlag, tagFlagNumber)) {
      // JSON has no non-finite numbers: number validation admits Infinity and
      // typed inputs skip it entirely, so an unchecked `""+x` would splice
      // invalid `Infinity`/`NaN` text. Raise instead of JSON.stringify's
      // silent null. An expression (not a statement) so a `!== void 0`
      // omission guard around the piece keeps guarding the check too.
      const inputVar = input.v();
      return B_next(
        input,
        `(Number.isFinite(${inputVar})?""+${inputVar}:${B_embedInvalidInput(input)})`,
        expectedSchema,
      );
    } else if (flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
      return B_next(input, `"\\""+${input.i}+"\\""`, expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagObject | tagFlagArray))) {
      const additionalItems = input.s.additionalItems;
      // Pretty-printing and async fields keep the whole-value JSON.stringify
      // path — inlined aggregation supports neither indentation nor promises.
      // So does a dict whose values JSON.stringify already serializes
      // byte-identically (plain strings, booleans, null): a dynamic-key loop
      // built from JS string concat can't beat the native call. Number values
      // stay compiled — the aggregate raises on non-finite where
      // JSON.stringify demotes to null — and json-format strings stay
      // compiled because they embed as raw JSON text, not quoted strings.
      if (
        (expectedSchema.space !== U && expectedSchema.space !== 0) ||
        flagUnsafeHas(input.g.o, flagAsync) ||
        (input.s.type !== arrayTag &&
          typeof additionalItems === "object" &&
          additionalItems.to === U &&
          additionalItems.format !== "json" &&
          flagUnsafeHas(
            tagFlags[additionalItems.type]!,
            (tagFlagString | tagFlagBoolean) | tagFlagNull,
          ))
      ) {
        const jsonVal = parse(B_refine(input, U, U, json));
        return B_next(
          jsonVal,
          B_stringifyCall(jsonVal.i, expectedSchema.space),
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
