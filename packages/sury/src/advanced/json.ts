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
  initSchema,
  inlinedValueFromString,
  type Internal,
  isLiteral,
  isOptional,
  jsonName,
  objectTag,
  refTag,
  setContent,
  stringTag,
  type Tag,
  tagFlags,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  updateOutput,
  type Val
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
  B_readsPayload,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType
} from "../builder";
import {
  arrayDecoder,
  arrayFactory,
  completeObjectVal,
  dictFactory,
  makeObjectVal,
  valGet
} from "../composites";
import {
 getOutputSchema,
 parse,
 parseDynamic
} from "../parse";
import {
  bool,
  float,
  inputToString,
  literalDecoder,
  nullLiteral,
  string,
  stringDecoderFn
} from "../primitives";
import {
 internalRefine
} from "../modifiers";
import {
 unionDecoder,
 unionFactory,
 unionRewriteTo
} from "../union";
import {
 recursiveDecoder
} from "./recursive";

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
    (toTagFlag & (2 | 8 | 4 | 32))
  ) {
    return parse(B_refine(input, unknown, U, target));
  } else if ((toTagFlag & (16 | 2048))) {
    const jsonExpected = copySchema(nullLiteral);
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  } else if ((toTagFlag & 128)) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    const jsonExpected = arrayFactory(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json;
    output.e = target;
    output.io = false;
    return output;
  } else if ((toTagFlag & 64)) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    const jsonExpected = dictFactory(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json;
    output.e = target;
    output.io = false;
    return output;
  } else if ((toTagFlag & (256 | 512))) {
    // A variant that stores a payload is read out of a document exactly like a
    // lone one is (CONTENT_CODEC_SPEC.md rule 2) — but the dispatch works from
    // the target's own variants, so the hop through the stored form has to be
    // spelled into them. `perVariantTo` does the same on the way out.
    const anyOf = target.anyOf;
    // The variant's own head, exactly as the `else` branch below reads the
    // target's: an arm that already says how it is stored (`S.string.with(S.to,
    // S.uint8Array)` is text, not base64) keeps saying it. An arm already shaped
    // like its stored form (`S.base64`, and anything derived from it) needs no
    // hop — one would re-run the format's own checks and drop whatever the
    // caller put on the arm — and one storing a JSON value is
    // left alone too — a document nested in a document is an escaped string,
    // which `B_narrowJsonSourcedJsonString` already routes, and standing
    // `S.json` in front of it would match every value and swallow the dispatch.
    const storedApart = (variant: Internal): Internal | undefined => {
      const content = variant.content;
      return content !== U && content !== json && content.type !== variant.type ? content : U;
    };
    if (anyOf !== U && anyOf.some((variant) => storedApart(variant) !== U)) {
      const stored = unionFactory(
        anyOf.map((variant) => {
          // `null` for an undefined arm, for the reason the branch above gives:
          // JSON has no undefined, and objectDecoder has already coalesced the
          // absent key into one.
          const from = storedApart(variant) ?? (isOptional(variant) ? nullLiteral : U);
          if (from === U) {
            return variant;
          }
          // A bare `.to`, not `codecTo`: the pair is this module's own — a
          // schema and the very content marker it names — so there is no
          // reading for the content rules to be asked about.
          const stored = copySchema(from);
          stored.to = variant;
          return stored;
        })
      );
      stored.perVariant = true;
      return parse(B_refine(input, unknown, U, stored));
    }
    return input;
  } else {
    // For non-JSON types (bigint, instance, etc.), decode through the schema
    // the target is stored as — a plain string, unless it carries a payload of
    // its own and names how a document holds it (bytes as base64).
    const jsonExpected = copySchema(target.content !== U ? target.content : string);
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  }
}

export const isJsonable = (schema: Internal): boolean => {
  const tagFlag = tagFlags[schema.type]!;
  return (
    (tagFlag & (2 | 4 | 8 | 32)) !== 0 ||
    schema["$ref"] === json["$ref"] ||
    ((tagFlag & 256) !== 0 && schema.anyOf!.every(isJsonable)) ||
    ((tagFlag & 128) !== 0 &&
      (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) &&
      schema.items!.every(isJsonable)) ||
    ((tagFlag & 64) !== 0 &&
      (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) &&
      Object.values(schema.properties!).every(isJsonable))
  );
}

// Per-variant conversion instead of a generic `undefined | X` check: the
// variants `keep` names stay as they are (an undefined one, so the object
// rebuild omits the field — #311), the rest get `.to(target)` appended and
// keep converting recursively.
const perVariantTo = (
  variants: Internal[],
  target: Internal,
  keep: (variantOutput: Internal) => boolean,
): Internal => {
  const mapped = unionFactory(
    variants.map((variant) =>
      keep(getOutputSchema(variant))
        ? variant
        : updateOutput<Internal>(variant, (mut) => {
            mut.to = target;
          })
    )
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
  } else if ((inputTagFlag & (16 | 2048))) {
    return B_nextConst(input, nullLiteral);
  } else if ((inputTagFlag & 128)) {
    const expected = baseSchema(arrayTag, false, arrayDecoder);
    expected.items = input.s.items!.map((_) => json);
    expected.additionalItems =
      typeof input.s.additionalItems === "object"
        ? json
        : input.s.additionalItems;
    expected.to = input.e.to;
    return parse(B_refine(input, U, U, expected));
  } else if ((inputTagFlag & 64)) {
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
          itemVal.e = perVariantTo(
            itemVal.s.anyOf!,
            json,
            (variantOutput) => variantOutput.type === undefinedTag || isJsonable(variantOutput),
          );
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
  } else if ((inputTagFlag & 512)) {
    // FIXME: Should be a unified solution for ref inputs
    return recursiveDecoder(input);
  } else if ((inputTagFlag & 256)) {
    // Each variant decodes to JSON separately, and an `undefined` one becomes
    // `null` through the branch above — the nullish bridge (CODEC_SPEC.md),
    // which a union reaching the target as a whole already applied. Refusing it
    // here only made a bridgeable variant unreachable one level down.
    // Only an object property can express "absent" rather than `null`, and it
    // never arrives here: the object branch below resolves its own optional
    // properties through `perVariantTo` before recursing.
    return parse(unionRewriteTo(input, input.e));
  } else if ((inputTagFlag & 1)) {
    const to = input.e.to!;
    // Whether we can optimize encoding during decoding. Encoding into a
    // concrete type validates implicitly — except a json-format target, whose
    // JSON.stringify accepts (or silently drops) anything, so it still needs
    // the JSON validation here.
    // FIXME: should this also check !input.e.refiner, like `carriedJsonString`'s caller does?
    // The `undefined` sentinel `S.assertInput` targets is `noValidation` and reads
    // nothing, so encoding into it asserts nothing either — `S.inputValidator(S.json)(x)`
    // answered true for a function. A `noValidation` document is a different
    // thing: it still holds the value, and the encode is how it is described.
    const preEncode: boolean =
      !!to &&
      to.format !== "json" &&
      !(to.noValidation && to.type === undefinedTag) &&
      !input.e.parser;
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
  setContent(s, s);

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

// Anything but a bare accessor needs parenthesizing before it can sit between
// two `+`: `+` binds tighter than `?:`, so the ternary a `.to` chain with a
// default hands over reassociates into `("\""+i)===void 0?…` and drops the
// opening quote on every input.
//
// A call *with arguments* stays out, even though it binds no looser than a bare
// one: `escapeFree` is a property of the values a schema admits, and what a
// conversion hands over is the value its source was never checked to be — so a
// packed carrier goes through the helper unless it materialized a var of its
// own. The zero-argument form is grandfathered, `.toISOString()` among it; see
// `jsonstring-novalidation-date`, which is what that costs.
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
  if ((tagFlag & ((16 | 32) | 2048))) {
    return "null";
  } else if (
    (tagFlag & ((2 | 4) | 8))
  ) {
    return JSON.stringify(schema.const)!;
  } else if ((tagFlag & 1024)) {
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
      if (target.content !== U && target.content !== json && !B_readsPayload(target)) {
        // The target stores this document rather than being another rendering
        // of it, so it takes the text as it stands.
        return input;
      }
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

  const initJsonString = (s: Internal): void => {
    s.format = "json";
    s.name = `${jsonName} string`;
    s.encoder = jsonStringEncoder;
    setContent(s, json);
    // Only an unknown-typed source has validation pending — a typed source
    // (decode direction) has nothing to fuse, and marking it would make the
    // aggregate re-validate trusted input. A pretty-printed or async document
    // goes through JSON.stringify whole. Dynamic items JSON.stringify already
    // serializes byte-identically (strings, booleans, null) stay on the
    // whole-value path, where a per-item loop can't beat the native call. A
    // fixed container is left to the aggregate unless it carries a refiner
    // (it would read unvalidated fields) or a tuple's rest item, whose fixed
    // slots and loop the aggregate validates separately.
    s.fz = (input, container, item) => {
      if (
        input.s.additionalItems === unknown &&
        !s.space &&
        !(input.g.o & 1) &&
        (item !== U
          ? !(item.to === U && (tagFlags[item.type]! & ((2 | 8) | 32)))
          : container.refiner === U &&
            container.inputRefiner === U &&
            typeof container.additionalItems !== objectTag)
      ) {
        const marked = copySchema(container);
        marked.uv = true;
        return marked;
      }
      return U;
    };
  };

  // The target every piece of an aggregated document renders into. It is
  // `jsonString` in every reading but one: a json-format string source is a
  // nested document, which sits inside the outer one as an escaped string
  // value — matching JSON.stringify of the same object — where the top-level
  // conversion is the identity. Unions reach it per variant (`perVariantTo`
  // appends it by position), so a jsonString variant next to a number is
  // never the "same type as the target" ambiguity a top-level conversion
  // would be.
  const jsonPiece: Internal = initSchema(
    stringTag,
    (input) =>
      input.s.format === "json"
        ? B_next(input, `${B_embedJsonStr(input)}(${input.i})`, jsonPiece, jsonPiece)
        : jsonStringDecoder(input),
    initJsonString,
  );

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
  // val already backed by a plain identifier passes through untouched. A raw
  // fused field takes the same local so its checks and its splice read the
  // property once.
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

  // An enum — string literals none of which needs escaping — renders as the
  // validated value between bare quotes (the string branch's escape-free
  // splice, with `bareString` standing in for the union), instead of a
  // dispatch that maps each literal to its own quoted text. An undefined
  // variant is fine where the piece is guarded (an object field) and not
  // where it must become null. Fixed fields only: in a dynamic loop the
  // two quote concats per item cost more than the dispatch's constants.
  const isBareEnum = (variants: Internal[], guarded: boolean): boolean =>
    variants.every((variant) => {
      const variantOutput = getOutputSchema(variant);
      const c = variantOutput.const;
      return c === U
        ? guarded && variantOutput.type === undefinedTag
        : typeof c === stringTag && JSON.stringify(c) === `"${c}"`;
    });
  const bareString = copySchema(string);
  bareString.escapeFree = true;

  // A serialization piece: `p` produces the JSON text, `g` (when set) is the
  // var to test against void 0 — an undefined-able value renders by omission,
  // matching JSON.stringify. Tuple items (`isArr`) render undefined as null
  // instead (also matching JSON.stringify), so they convert as a whole and
  // never guard.
  // `declared` is the field's schema when the container was fused
  // (`fz`, installed above) and the value arrives unvalidated: a
  // dispatching shape validates inside the same pass that renders it, and a
  // shape rendered off the validated value validates first. `loop` marks a
  // dynamic item, where the bare enum splice loses to the dispatch.
  const fieldPiece = (
    itemVal: Val,
    isArr: boolean,
    declared?: Internal,
    loop?: boolean,
  ): { p: Val; g: string | undefined } => {
    const cur = declared || itemVal.s;
    // `noValidation` is the one declared shape that reads the field once.
    if (declared !== U && !declared.noValidation) itemVal = B_unionWritable(itemVal);
    const validated = (): Val =>
      declared !== U ? parse(B_refine(itemVal, U, U, declared)) : itemVal;
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
    const guardedJsonPiece = (itemVal: Val): { p: Val; g: string | undefined } => {
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
    if ((tagFlags[cur.type]! & 1)) {
      return guardedJsonPiece(itemVal);
    }
    // A declared ref (`S.json`, recursive) requires a value — undefined is
    // not JSON — so its validation stays unguarded.
    if ((tagFlags[cur.type]! & 512)) {
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
    if (cur.type === anyOfTag && cur.to === U) {
      const variants = cur.anyOf!;
      // unknown/ref variants can't serialize piecewise (jsonStringDecoder's
      // unknown branch treats its input as the JSON text) — take the guarded
      // validate-and-stringify path, which renders an undefined value by
      // omission/null.
      if (
        variants.some((variant) =>
          (tagFlags[getOutputSchema(variant).type]! & (1 | 512))
        )
      ) {
        return guardedJsonPiece(validated());
      }
      const optional = !isArr && !!cur.has![undefinedTag];
      if (!loop && isBareEnum(variants, optional)) {
        const v = validated();
        const guard = optional ? v.v() : U;
        return { p: parse(B_refine(v, bareString, U, jsonPiece)), g: guard };
      }
      if (optional && variants.length === 2) {
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
          (tagFlags[single.type]! & (((2 | 4) | (8 | 1024)) |
              (32 | 2048)))
        ) {
          const v = validated();
          const guard = v.v();
          return { p: parse(B_refine(v, single, U, jsonPiece)), g: guard };
        }
      }
      // A field keeps its undefined variants (omission); a tuple item converts
      // them too, since jsonPiece renders undefined as null.
      const p = parse(
        B_refine(
          B_unionWritable(itemVal),
          U,
          U,
          perVariantTo(variants, jsonPiece, (variantOutput) =>
            !isArr && variantOutput.type === undefinedTag
          ),
        )
      );
      return { p, g: optional ? p.v() : U };
    }
    return {
      p: parse(
        B_refine(
          itemVal,
          U,
          U,
          declared !== U
            ? updateOutput<Internal>(declared, (mut) => {
                mut.to = jsonPiece;
              })
            : jsonPiece,
        )
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
      const itemVal = valGet(input, location);
      // A fused container's field is raw unless its decoder validated it (a
      // union member's literal) — told apart by the val's type, not the
      // schema's, so the decoder may keep any subset.
      const declared = schema.uv && (tagFlags[itemVal.s.type]! & 1) ? fieldSchema : U;
      if (isLiteral(fieldSchema) && fieldSchema.to === U) {
        const text = B_constJsonText(fieldSchema);
        if (text !== U) {
          if (declared !== U) code = code + B_merge(parse(B_refine(itemVal, U, U, declared)));
          entries.push({ t: text });
          continue;
        }
      }
      const { p, g } = fieldPiece(itemVal, isArr, declared);
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
        // A fused container (see `fz` in initJsonString and base.ts)
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
              (tagFlags[getOutputSchema(variant).type]! & (1 | 512))
            )
          ) {
            // One dispatch, not two: parsing straight to `union -> jsonString`
            // makes each case validate its fields and emit text in the same
            // branch, where resolving the union first would rebuild the item
            // and then re-dispatch on it to serialize.
            itemInput.e = perVariantTo(item.anyOf!, jsonPiece, () => false);
            piece = { p: parseDynamic(itemInput), g: U };
          }
        }
        const { p, g } = piece !== U ? piece : fieldPiece(parseDynamic(itemInput), isArr, U, true);
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
            (idx !== 0 && hasDefiniteBefore ? "," : "") + keyText(idx)
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

  // A string that already IS the document, rather than a value to be escaped
  // into one. The declared payload is decoded straight out of it where nothing
  // in between needs the intermediate string — encoding into a concrete type
  // validates the JSON implicitly, so the parse doubles as the check.
  const carriedJsonString = (input: Val, expectedSchema: Internal): Val => {
    const to = expectedSchema.to;
    const stringVal = stringDecoderFn(input);
    stringVal.s = expectedSchema;
    stringVal.e = expectedSchema;

    // `S.assertInput`'s `undefined` result sentinel alongside `unknown`: neither
    // reads the text, so neither can stand in for the parse below.
    if (
      to !== U &&
      to.type !== unknownTag &&
      !(to.noValidation && to.type === undefinedTag) &&
      !expectedSchema.parser &&
      !expectedSchema.refiner
    ) {
      const encoded = jsonStringEncoder(stringVal, to);
      // Unless the target only stores the text: then nothing downstream reads it
      // as JSON, so the check below is the only thing asserting it is. A
      // document target that goes on to read its own payload does, and adding
      // the check would parse the same text twice — one that stops there (a
      // bare jsonString, or a jsonPiece about to escape it) reads nothing.
      if (encoded !== stringVal || (to.format === "json" && B_readsPayload(to))) {
        return encoded;
      }
    }
    const stringVar = stringVal.v();
    const output = B_refine(stringVal, expectedSchema);
    output.cp = `try{JSON.parse(${stringVar})}catch(t){${B_embedInvalidInput(stringVal)}}`;
    return output;
  };

  const jsonStringDecoder: Builder = (input) => {
    const inputTagFlag = tagFlags[input.s.type]!;
    const expectedSchema = input.e;

    if ((inputTagFlag & 1)) {
      return carriedJsonString(input, expectedSchema);
    } else if (input.s.format === "json") {
      return input;
    } else if (isLiteral(input.s)) {
      return B_next(input, inlineJsonString(input, input.s), expectedSchema);
    } else if ((inputTagFlag & 2)) {
      // A carrier opened into this format handed over its document (rule 3), so
      // it is parsed rather than escaped — and checked here, since nothing has
      // read it yet. A source already claiming this payload (a union narrow) is
      // the same unverified text. Every other string is a value, and stays one.
      if (
        input.s.content === json ||
        (input.s.content !== U && B_readsPayload(expectedSchema))
      ) {
        return carriedJsonString(input, expectedSchema);
      }
      // Two ways `escapeFree`'s proof is void here: `noValidation` drops the
      // pattern check it rests on, and a `.to` chain carrying a default hands
      // over `i===void 0?e[2]:i.toISOString()`, whose default branch is the
      // raw default — a `Date`, not its ISO text. The helper handles both.
      return B_next(
        input,
        input.s.escapeFree && !input.s.noValidation && accessorRe.test(input.i)
          ? `"\\""+${input.i}+"\\""`
          : `${B_embedJsonStr(input)}(${input.i})`,
        expectedSchema,
      );
    } else if ((inputTagFlag & 8)) {
      const output = inputToString(input);
      output.s = expectedSchema;
      return output;
    } else if ((inputTagFlag & 4)) {
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
    } else if ((inputTagFlag & 1024)) {
      // Same reassociation hazard, with no helper to fall back to.
      return B_next(
        input,
        `"\\""+${accessorRe.test(input.i) ? input.i : `(${input.i})`}+"\\""`,
        expectedSchema,
      );
    } else if ((inputTagFlag & (64 | 128))) {
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
        (input.g.o & 1) ||
        // `!uv`: a fused container skipped upstream validation, and the
        // whole-value paths don't validate — only the aggregate loop does.
        (!input.s.uv &&
          !input.s.items?.length &&
          typeof additionalItems === "object" &&
          additionalItems.to === U &&
          (tagFlags[additionalItems.type]! & ((2 | 8) | 32)))
      ) {
        const jsonVal = parse(B_refine(input, U, U, json));
        // An async field leaves a promise here, and `JSON.stringify` of one is
        // `{}` — the serialization is what waits, not the caller.
        if ((jsonVal.f & 1)) {
          const resolvedVar = B_varWithoutAllocation(input.g);
          const output = B_next(
            jsonVal,
            `${jsonVal.v()}.then(${resolvedVar}=>${B_stringifyCall(
              resolvedVar,
              expectedSchema.space,
            )})`,
            expectedSchema,
            expectedSchema,
          );
          output.f |= 1;
          return output;
        }
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
        // A schema with no string form of its own still has one when it is
        // asked directly — `S.never` is the reachable case, an unreachable item
        // whose branch compiles away rather than converting anything. Keep its
        // own schema and hang the string target off its `.to`.
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

  return initSchema(stringTag, jsonStringDecoder, initJsonString);
})();

// @__NO_SIDE_EFFECTS__
export const jsonStringWithSpace = (space: number): Internal => {
  const mut = copySchema(jsonString);
  mut.space = space;
  return mut;
}
