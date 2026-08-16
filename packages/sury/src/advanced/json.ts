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
import {
  arrayDecoder,
  arrayFactory,
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
import { internalRefine } from "../modifiers";
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
    const jsonExpected = arrayFactory(unknown);
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
    const expected = baseSchema(arrayTag, false, arrayDecoder);
    expected.items = input.s.items!.map((_) => json);
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
  } else if (flagUnsafeHas(inputTagFlag, tagFlagUnion)) {
    // Each variant decodes to JSON separately, and an `undefined` one becomes
    // `null` through the branch above — the nullish bridge (CODEC_SPEC.md),
    // which a union reaching the target as a whole already applied. Refusing it
    // here only made a bridgeable variant unreachable one level down.
    // Only an object property can express "absent" rather than `null`, and it
    // never arrives here: the object branch below resolves its own optional
    // properties through `perVariantTo` before recursing.
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

export const json: Internal = /* @__PURE__ */ initSchema(refTag, jsonDecoderFn, (s) => {
  const jsonRef = baseSchema(refTag, true, jsonDecoderFn);
  jsonRef["$ref"] = `${defsPath}${jsonName}`;
  jsonRef.name = jsonName;

  jsonRef.encoder = jsonEncoderFn;

  s["$ref"] = jsonRef["$ref"];
  s.name = jsonName;
  s.encoder = jsonEncoderFn;

  const anyOf = [
    string,
    bool,
    // JSON has no non-finite numbers: bare `float` admits Infinity, which
    // JSON.stringify silently demotes to null and the jsonString aggregator
    // would splice as invalid text — raise at validation instead, matching
    // the number -> jsonString piece. A refiner, not a custom decoder: union
    // dispatch derives each variant's type narrow itself (unionNarrowSchema)
    // and only appends refiner checks, so a decoder-emitted check would be
    // silently dropped from the compiled union.
    internalRefine(float, () => () => [
      { c: (inputVar) => `Number.isFinite(${inputVar})`, f: failInvalidType },
    ]),
    nullLiteral,
    dictFactory(jsonRef),
    arrayFactory(jsonRef),
  ];
  const has: Partial<Record<Tag, boolean>> = {};
  anyOf.forEach((schema) => {
    has[schema.type] = true;
  });

  const jsonDef = baseSchema(anyOfTag, true, unionDecoder);
  jsonDef.anyOf = anyOf;
  jsonDef.has = has;
  jsonDef.name = jsonName;
  jsonDef.type = anyOfTag;

  const defs: Record<string, Internal> = {};
  defs[jsonName] = jsonDef;
  s["$defs"] = defs;
});

// Formats whose every accepted value is ASCII carrying no JSON escape
// character, so the value splices between bare quotes and skips the escape
// helper. Two kinds qualify:
//
//   - manufactured by the code we emit — date.ts's `toISOString()` is
//     "date-time", url.ts's `urlToUri` is "uri" — where the producer's range
//     is the proof, and no caller can reach around it;
//   - carried by a value the format's own anchored pattern already checked,
//     which is what `B_isEscFree` insists on: the check is the guarantee, so
//     `noValidation` (which drops it) must fall back to the helper or the
//     encoder emits `{"id":"a"b"}` for a lying caller.
//
// `pnpm --filter=sury fuzz:escfree` proves the second kind — that no value a
// listed pattern accepts needs escaping. The first kind it can only observe
// through a format, since `dateTimeString`/`uriString` are module-private;
// their proof is the producer, restated at each one.
//
// Deliberately partial. `cuid` is `/^c[^\s-]{8,}$/i`, which accepts a quote,
// and the IDN/IRI family admits arbitrary non-ASCII including lone
// surrogates. Widening this list without running the fuzzer emits broken JSON
// rather than merely over-escaped JSON, so add a format only once the fuzzer
// clears it — and re-run it when a pattern here changes.
// Anchored, so an absent format stringifies to "undefined" and misses.
const escFreeFormatRe =
  /^(date(-time)?|duration|uuid|email|hostname|ipv[46]|uri(-reference)?)$/;

// An identifier, property access, index or no-arg call — nothing that could
// hold an operator. Everything else has to be parenthesized before it can sit
// between two `+`: `+` binds tighter than `?:`, so the bare ternary a `.to`
// chain with a default hands over (`i===void 0?e[2]:i.toISOString()`)
// reassociates into `("\""+i)===void 0?…` and loses the opening quote on every
// input.
const accessorRe = /^[\w$]+(\.[\w$]+|\[[^\[\]]*\]|\(\))*$/;


// Runtime helper embedded into generated jsonString code: the JSON text of a
// string value. The fast path skips JSON.stringify's escape handling when a
// regex scan proves no character needs it; thresholds follow
// fast-json-stringify (the scan + concat loses to JSON.stringify on long
// strings).
const strEscapeRe = /[\u0000-\u001f"\\\ud800-\udfff]/;
// A non-string (noValidation, a violated decode contract) falls through to
// JSON.stringify instead of crashing on `.length`; undefined renders as null
// so the piece still splices valid JSON text.
const asJsonString = (value: unknown): string =>
  typeof value === "string" && value.length < 5000 && !strEscapeRe.test(value)
    ? `"${value}"`
    : JSON.stringify(value) ?? "null";

// An operation embeds the helper once, however many string pieces it has.
const B_embedJsonStr = (b: Val): string =>
  b.g.js || (b.g.js = B_embedPure(b, asJsonString));

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
  }
  // An instance literal (a Date, a class instance) has no JSON text of its
  // own — an object/array `const` never reaches here, since it builds a
  // structural schema whose fields are literals (primitiveToSchema in
  // jsonschema.ts) and serializes field by field.
  return U;
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
        const jsonStringConstSchema = baseSchema(stringTag, true, literalDecoder);
        jsonStringConstSchema.const = constSchemaToJsonStringConst(input, target);
        jsonStringConstSchema.to = target;
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
  // concatenation, which is where every piece lands. The number piece nests
  // its coercion inside a ternary, still redundant in a concat position (both
  // branches feed the same string `+`) — but load-bearing in the bare
  // top-level piece, which never passes through here.
  const foldStringCoercion = (piece: string): string =>
    piece.startsWith(`""+`)
      ? piece.slice(3)
      : piece.startsWith(`(Number.isFinite(`)
        ? piece.replace(`?""+`, "?")
        : piece;

  // Compile-time merge of adjacent string-literal operands: `"a"+"b"` → `"ab"`.
  // String concat is left-associative, so folding a literal pair joined by `+`
  // preserves semantics whenever the operator before the first literal binds no
  // tighter than `+`. Contents splice verbatim (only the quotes between two
  // merged literals are dropped), so escape semantics can't change.
  // Single-quoted literals (the builder's path-prepend code emits them, with
  // stray `"` inside) are opaque: skipped whole, never merged into or across.
  const mergeStrLits = (code: string): string => {
    const litEnd = (from: number): number => {
      const quote = code[from];
      let j = from + 1;
      while (j < code.length && code[j] !== quote) {
        j += code[j] === "\\" ? 2 : 1;
      }
      return j + 1;
    };
    let out = "";
    let from = 0;
    let i = 0;
    while (i < code.length) {
      const c = code[i];
      if (c === "'") {
        i = litEnd(i);
        continue;
      }
      if (c !== '"') {
        i = i + 1;
        continue;
      }
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
      i = j;
    }
    return out + code.slice(from);
  };

  // A union dispatch assigns each case's output back through its input var.
  // A nested field's var can resolve to the source property access itself
  // (finalized parent — see _notVarAtParent), where that write would mutate
  // the caller's object and break idempotence. Copy into a local first; a
  // val already backed by a plain identifier passes through untouched.
  const B_unionWritable = (itemVal: Val): Val => {
    const inputVar = itemVal.v();
    if (/^[\w$]+$/.test(inputVar)) {
      return itemVal;
    }
    const localVar = B_varWithoutAllocation(itemVal.g);
    const local = B_next(itemVal, localVar, itemVal.s, itemVal.e);
    local.v = _var;
    local.cp = `let ${localVar}=${inputVar};`;
    return local;
  };

  // A serialization piece: `p` produces the JSON text, `g` (when set) is the
  // var to test against void 0 — an undefined-able value renders by omission,
  // matching JSON.stringify. Tuple items (`isArr`) render undefined as null
  // instead (also matching JSON.stringify), so they convert as a whole and
  // never guard.
  const fieldPiece = (itemVal: Val, isArr: boolean): { p: Val; g: string | undefined } => {
    const cur = itemVal.s;
    // A nested json-format string stays an escaped string value inside the
    // outer document — matching JSON.stringify of the same object. Only the
    // top-level jsonString -> jsonString conversion is the identity
    // (jsonStringDecoder's format branch), so bypass it here: raw-splicing
    // the field's text would emit it as a JSON value, and the encode
    // direction would hand back a parsed object where a string went in.
    if (
      flagUnsafeHas(tagFlags[cur.type]!, tagFlagString) &&
      cur.format === "json" &&
      cur.to === U
    ) {
      const p = B_next(
        itemVal,
        `${B_embedJsonStr(itemVal)}(${itemVal.i})`,
        jsonString,
        jsonString,
      );
      return { p, g: U };
    }
    // Values jsonString itself can't decode piecewise (unknown, refs) validate
    // through `json` and stringify at runtime — the coverage the old
    // whole-value `json` + JSON.stringify path had, scoped to the one subtree
    // that needs it. An undefined value renders by omission/null instead of
    // failing JSON validation (unknown admits undefined, and whole-value
    // JSON.stringify omitted it), so the validation sits behind the same
    // `!== void 0` guard as the append — compiled on a chain detached from
    // the field val, landing guarded inside the piece's own code.
    // JSON.stringify can still yield undefined on a guarded value (a toJSON
    // returning it); the outputVar guard/`??"null"` keeps that contract too.
    const guardedJsonPiece = (): { p: Val; g: string | undefined } => {
      const inputVar = itemVal.v();
      const detached = B_next(itemVal, inputVar, unknown, json);
      detached.v = _var;
      detached.prev = U;
      const jsonVal = parse(B_refine(detached, U, U, json));
      const validation = B_merge(jsonVal);
      const outputVar = B_varWithoutAllocation(itemVal.g);
      const p = B_next(itemVal, outputVar, jsonString, jsonString);
      p.v = _var;
      p.cp = isArr
        ? `let ${outputVar}="null";if(${inputVar}!==void 0){${validation}${outputVar}=JSON.stringify(${jsonVal.i})??"null"}`
        : `let ${outputVar};if(${inputVar}!==void 0){${validation}${outputVar}=JSON.stringify(${jsonVal.i})}`;
      return { p, g: isArr ? U : outputVar };
    };
    if (flagUnsafeHas(tagFlags[cur.type]!, tagFlagUnknown)) {
      return guardedJsonPiece();
    }
    // A declared ref (`S.json`, recursive) requires a value — undefined is
    // not JSON — so its validation stays unguarded.
    if (flagUnsafeHas(tagFlags[cur.type]!, tagFlagRef)) {
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
    if (cur.type === anyOfTag && cur.has![undefinedTag]) {
      const variants = cur.anyOf!;
      // unknown/ref variants can't serialize piecewise (jsonStringDecoder's
      // unknown branch treats its input as the JSON text) — take the guarded
      // validate-and-stringify path: the union admits undefined, which the
      // guard renders by omission/null.
      if (
        variants.some((variant) =>
          flagUnsafeHas(
            tagFlags[getOutputSchema(variant).type]!,
            tagFlagUnknown | tagFlagRef,
          ),
        )
      ) {
        return guardedJsonPiece();
      }
      if (!isArr) {
        if (variants.length === 2) {
          // The two-variant `X | undefined` shape skips the union dispatch
          // entirely when X stays a pure expression: the `!== void 0` guard IS
          // the dispatch. Restricted to primitive X so the piece can't own
          // statements that would then run unguarded.
          const u0 = getOutputSchema(variants[0]!).type === undefinedTag;
          const u1 = getOutputSchema(variants[1]!).type === undefinedTag;
          const single = variants[u0 ? 1 : 0]!;
          if (
            u0 !== u1 &&
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
          B_refine(
            B_unionWritable(itemVal),
            U,
            U,
            perVariantTo(variants, jsonString, () => false),
          ),
        );
        return { p, g: p.v() };
      }
    }
    return {
      p: parse(
        B_refine(
          cur.type === anyOfTag ? B_unionWritable(itemVal) : itemVal,
          U,
          U,
          jsonString,
        ),
      ),
      g: U,
    };
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

    // A dynamic item implies no optional fixed pieces: a dict has no fixed
    // keys, and tuple items never guard (undefined renders as null).
    if (!hasOpt) {
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
        // A fused container (see B_fuseIntoJsonString in composites.ts)
        // skipped its validation loop — re-parse each item from unknown so
        // the checks land inside this loop instead of a second walk.
        let piece: { p: Val; g: string | undefined } | undefined = U;
        if (schema.uv) {
          const item = itemInput.s;
          itemInput.s = unknown;
          if (
            item.type === anyOfTag &&
            !item.has![undefinedTag] &&
            item.to === U &&
            !item.anyOf!.some((variant) =>
              flagUnsafeHas(
                tagFlags[getOutputSchema(variant).type]!,
                tagFlagUnknown | tagFlagRef,
              ),
            )
          ) {
            // One dispatch, not two: parsing straight to `union -> jsonString`
            // makes each case validate its fields and emit text in the same
            // branch, where resolving the union first would rebuild the item
            // and then re-dispatch on it to serialize.
            itemInput.e = updateOutput<Internal>(item, (mut) => {
              mut.to = jsonString;
            });
            piece = { p: parseDynamic(itemInput), g: U };
          }
        }
        const { p, g } = piece !== U ? piece : fieldPiece(parseDynamic(itemInput), isArr);
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
        // `Object.keys`, not `for...in`: the latter walks the prototype chain,
        // so an inherited enumerable key would be serialized where
        // JSON.stringify (and the whole-value path this replaced) emits own
        // keys only.
        loopCode = `let ${dynAcc}="";for(let ${iterVar}${
          isArr
            ? `=${fixedLen};${iterVar}<${inputVar}.length;++${iterVar}`
            : ` of Object.keys(${inputVar})`
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
    // A definite first field lets the opening brace fold into the seed: no
    // `(acc?",":"")` probe is ever emitted then, so the seeded prefix can't
    // fake a preceding field.
    const braceSeeded = entries[0]!.g === U;
    if (braceSeeded) {
      chunk = "{";
    }
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
    const output = B_next(
      input,
      braceSeeded ? `${accVar}+"}"` : `"{"+${accVar}+"}"`,
      expectedSchema,
      expectedSchema,
    );
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
      // `accessorRe` here is double duty, and the second job is the
      // load-bearing one: it is also the only cheap evidence that the
      // expression really is the string the format vouches for. A `.to` chain
      // carrying a default emits `i===void 0?e[2]:i.toISOString()`, whose
      // default branch is the default value itself — a `Date`, not its ISO
      // text — so the format proves nothing about it. The helper stringifies
      // that correctly (JSON.stringify of a Date is its ISO text) where a
      // splice would emit `Mon Jan 01 2024 …`. And `noValidation` drops the
      // format check the splice relies on. Either way: keep the helper.
      return B_next(
        input,
        !input.s.noValidation &&
          escFreeFormatRe.test(input.s.format!) &&
          accessorRe.test(input.i)
          ? `"\\""+${input.i}+"\\""`
          : `${B_embedJsonStr(input)}(${input.i})`,
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
      // Blamed on `json`, not on the jsonString target: what the value fails
      // to be is a JSON value, and `S.parser(S.json)` rejects it with that
      // same wording.
      const inputVar = input.v();
      return B_next(
        input,
        `(Number.isFinite(${inputVar})?""+${inputVar}:${B_embedInvalidInput(input, json)})`,
        expectedSchema,
      );
    } else if (flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
      // Parenthesized unless a bare accessor: same reassociation hazard as the
      // splice above, and bigint has no helper to fall back to.
      return B_next(
        input,
        `"\\""+${accessorRe.test(input.i) ? input.i : `(${input.i})`}+"\\""`,
        expectedSchema,
      );
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagObject | tagFlagArray))) {
      const additionalItems = input.s.additionalItems;
      // Pretty-printing and async fields keep the whole-value JSON.stringify
      // path — inlined aggregation supports neither indentation nor promises.
      // So does a dict or array whose dynamic values JSON.stringify already
      // serializes byte-identically (strings — nested json-format ones escape
      // as strings too — booleans, null): a per-item loop built from JS string
      // concat can't beat the native call. Number values stay compiled — the
      // aggregate raises on non-finite where JSON.stringify demotes to null.
      // `!items.length`: a tuple prefix serializes under its own item schemas,
      // which the whole-value call would ignore.
      if (
        (expectedSchema.space !== U && expectedSchema.space !== 0) ||
        flagUnsafeHas(input.g.o, flagAsync) ||
        // `!uv`: a fused container skipped upstream validation, and the
        // whole-value paths don't validate — only the aggregate loop does.
        (!input.s.uv &&
          !input.s.items?.length &&
          typeof additionalItems === "object" &&
          additionalItems.to === U &&
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
      const stringTarget = copySchema(string);
      stringTarget.to = expectedSchema;
      try {
        input.e = stringTarget;
        return parse(input);
      } catch {
        // A schema that converts to string only when it is itself the target
        // (S.uint8Array reads `e.to` to decide, so a bare `string` target
        // leaves it out of the chain) needs the conversion asked of it
        // directly: keep the input's own schema and hang the string target
        // off its `.to`.
        try {
          const viaSelf = copySchema(input.s);
          viaSelf.to = stringTarget;
          input.e = viaSelf;
          return parse(input);
        } catch {
          return B_unsupportedDecode(input, input.s, expectedSchema);
        }
      }
    }
  };

  return initSchema(stringTag, jsonStringDecoder, (s) => {
    s.format = "json";
    s.name = `${jsonName} string`;
    s.encoder = jsonStringEncoder;
  });
})();

// @__NO_SIDE_EFFECTS__
export const jsonStringWithSpace = (space: number): Internal => {
  const mut = copySchema(jsonString);
  mut.space = space;
  return mut;
}
