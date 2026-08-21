import {
  baseSchema,
  copySchema,
  bigintTag,
  booleanTag,
  type Builder,
  type Check,
  flagDisableNanNumberValidation,
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  isLiteral,
  nanTag,
  nullTag,
  numberTag,
  objectTag,
  setContent,
  stringTag,
  symbolTag,
  type Tag,
  tagFlagBigint,
  tagFlagBoolean,
  tagFlagNaN,
  tagFlagNull,
  tagFlagNumber,
  tagFlags,
  tagFlagString,
  tagFlagSymbol,
  tagFlagUndefined,
  tagFlagUnknown,
  U,
  undefinedTag,
  type Val,
} from "./base";
import {
  _var,
  B_embed,
  B_embedInvalidInput,
  B_inlineConst,
  B_next,
  B_nextConst,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType,
} from "./builder";

export const int32FormatValidation = (inputVar: string) => {
  return `${inputVar}<=2147483647&&${inputVar}>=-2147483648&&${inputVar}%1===0`;
};

// `%1===0` is NaN (falsy) for NaN and ±Infinity, so one check covers "is a
// finite mathematical integer" with no separate NaN validation.
export const integerFormatValidation = (inputVar: string) => {
  return `${inputVar}%1===0`;
};

// Atomic type-narrow conditions, shared by the type decoders and the union
// dispatch (`typeCheckCond`) so the two can't drift. Memoized per tag: the
// returned closure depends only on `tag`, and this is called all over the
// primitive decoders and union dispatch, so caching stops a fresh closure
// being allocated on every decode (a large share of codegen GC — see the
// GC-dominated compile profile).
const typeofCondCache: Record<string, (inputVar: string) => string> = {};
export const typeofCond = (tag: Tag): ((inputVar: string) => string) =>
  typeofCondCache[tag] ||
  (typeofCondCache[tag] = (inputVar) => `typeof ${inputVar}==="${tag}"`);
export const nanCond = (inputVar: string): string => `Number.isNaN(${inputVar})`;
export const isArrayCond = (inputVar: string): string => `Array.isArray(${inputVar})`;
export const objectTagCond = (inputVar: string): string =>
  `${typeofCond(objectTag)(inputVar)}&&${inputVar}`;
// `class` is a reserved word in TS, so the parameter is named `class_`.
export const instanceofCond = (b: Val, class_: unknown) => (inputVar: string): string =>
  `${inputVar} instanceof ${B_embed(b, class_)}`;

// Shared, immutable per-tag type-narrow Check objects. A Check's c/f are only
// ever called, never reassigned, and callers always wrap it in a fresh array
// before it becomes a val's `.vc` (which may then be pushed to / cleared), so
// reusing one object per tag drops a Check allocation on every primitive
// decode without any aliasing hazard.
const typeofCheckCache: Record<string, Check> = {};
const typeofCheck = (tag: Tag): Check =>
  typeofCheckCache[tag] || (typeofCheckCache[tag] = { c: typeofCond(tag), f: failInvalidType });
const notNanCheck: Check = { c: (inputVar) => `!${nanCond(inputVar)}`, f: failInvalidType };
const int32Check: Check = { c: int32FormatValidation, f: failInvalidType };
const integerCheck: Check = { c: integerFormatValidation, f: failInvalidType };
// For a source that already carries a number format — integer-valued by the
// NumberFormat invariant — only int32's range is left to check.
const int32RangeCheck: Check = {
  c: (inputVar) => `${inputVar}<=2147483647&&${inputVar}>=-2147483648`,
  f: failInvalidType,
};
const nanCheck: Check = { c: nanCond, f: failInvalidType };

// Reject anything but `tag` when the input is still `unknown` — shared by
// every primitive decoder's unknown-input branch.
const B_refineTypeofUnknown = (input: Val, tag: Tag): Val => {
  return B_refine(input, input.e, [typeofCheck(tag)]);
}

// Allocate a fresh var and start a new Val from it — shared by every
// primitive decoder that coerces its input into a differently-typed output.
const B_nextVar = (input: Val, expected: Internal): Val => {
  const output = B_next(input, B_varWithoutAllocation(input.g), expected);
  output.v = _var;
  return output;
}

export const numberDecoder: Builder = (input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;
  const expectedFormat = input.e.format;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    const checks: Check[] = [typeofCheck(numberTag)];
    if (expectedFormat === "int32") {
      checks.push(int32Check);
    } else if (expectedFormat === "integer") {
      checks.push(integerCheck);
    } else {
      if (!flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
        checks.push(notNanCheck);
      }
    }
    return B_refine(input, input.e, checks);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
    const output = B_nextVar(input, input.e);
    // Own the `+input` coercion (decl included) in codeFromPrev so it's
    // non-hoistable: feeding a union dispatch (e.g. str->to(option(int))) can't
    // lift the type-narrow check below above its `let v0=+i`.
    output.cp = `let ${output.i}=+${input.v()};`;

    output.vc = [
      {
        c: (_inputVar) =>
          expectedFormat === "int32"
            ? int32FormatValidation(output.i)
            : expectedFormat === "integer"
              ? integerFormatValidation(output.i)
              : `!${nanCond(output.i)}`,
        f: failInvalidType,
      },
    ];
    return output;
  } else if (
    flagUnsafeHas(inputTagFlag, tagFlagNaN) &&
    expectedFormat !== "int32" &&
    expectedFormat !== "integer" &&
    flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)
  ) {
    return B_refine(input, input.e);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagNumber)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else if (input.s.format !== expectedFormat && expectedFormat === "int32") {
    return B_refine(input, input.e, [input.s.format === U ? int32Check : int32RangeCheck]);
  } else if (expectedFormat === "integer" && input.s.format === U) {
    // Any formatted number source is already integer-valued (the NumberFormat
    // invariant), so only a bare number still needs the check.
    return B_refine(input, input.e, [integerCheck]);
  } else {
    return input;
  }
};

export const float: Internal = /* @__PURE__ */ initSchema(numberTag, numberDecoder);

export const int: Internal = /* @__PURE__ */ initSchema(numberTag, numberDecoder, (s) => {
  s.format = "int32";
  // The format's range as real bound fields, not just something the JSON
  // Schema emit knows: S.gte/S.lte compare against them, so a bound outside
  // int32 is caught as a contradiction instead of silently building.
  s.minimum = -2147483648;
  s.maximum = 2147483647;
});

// JSON Schema's unbounded `integer`: any number with no fractional part, with
// none of int32's range. Carries no bound fields — there is no range to
// advertise or for a user bound to contradict.
export const integer: Internal = /* @__PURE__ */ initSchema(numberTag, numberDecoder, (s) => {
  s.format = "integer";
});

// inputToString/stringDecoderFn/string are mutually recursive (stringDecoderFn
// falls back to inputToString, which builds its output schema via `string`)
// and so are kept together.
export const inputToString = (input: Val): Val => {
  return B_next(input, `""+${input.i}`, string);
}
export const stringDecoderFn = (input: Val): Val => {
  const inputTagFlag = tagFlags[input.s.type]!;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, stringTag);
  } else if (
    flagUnsafeHas(
      inputTagFlag,
      tagFlagBoolean | tagFlagNumber | tagFlagBigint | tagFlagUndefined | tagFlagNull | tagFlagNaN,
    ) && isLiteral(input.s)
  ) {
    const const_ = "" + (input.s.const as string);
    // The stringified literal is still a literal, so it wants `literalDecoder`
    // — taken off the source rather than imported, the way unionNarrowSchema
    // avoids naming a decoder. `isLiteral(input.s)` above is what guarantees
    // this is that decoder, and reaching this branch at all requires a literal
    // schema in the bundle: naming it statically would instead ship it to every
    // `S.string` consumer (+264 gz on that export, +4 on total).
    const schema = baseSchema(stringTag, false, input.s.decoder);
    schema.const = const_;
    return B_next(input, `"${const_}"`, schema);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagBoolean | tagFlagNumber | tagFlagBigint)) {
    return inputToString(input);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagString)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
}
export const string: Internal = /* @__PURE__ */ initSchema(stringTag, stringDecoderFn);

// The text a carrier hands over when it is opened (CONTENT_CODEC_SPEC.md rule
// 3), carrying what document it claims to be. That marker is what lets the
// format parse the text instead of escaping it, without every other string
// being read as a document too — and the text still gets checked, because
// nothing has looked at it yet.
// @__NO_SIDE_EFFECTS__
export const openedText = (format: Internal): Internal => {
  const opened = copySchema(string);
  setContent(opened, format.content!);
  return opened;
};

export const booleanDecoder: Builder = (input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, booleanTag);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
    const output = B_nextVar(input, input.e);
    const inputVar = input.v();
    output.cp = `let ${output.i};(${output.i}=${inputVar}==="true")||${inputVar}==="false"||${B_embedInvalidInput(
      input,
    )};`;
    return output;
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagBoolean)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};

export const bool: Internal = /* @__PURE__ */ initSchema(booleanTag, booleanDecoder);

export const bigintDecoder: Builder = (input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;

  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, bigintTag);
  } // TODO: Skip formats which 100% don't match
  else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
    const output = B_nextVar(input, input.e);
    output.cp = `let ${output.i};try{${output.i}=BigInt(${input.v()})}catch(_){${B_embedInvalidInput(
      input,
    )}}`;
    return output;
  } else if (flagUnsafeHas(inputTagFlag, tagFlagNumber)) {
    return B_next(input, `BigInt(${input.i})`, input.e);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};

export const bigint: Internal = /* @__PURE__ */ initSchema(bigintTag, bigintDecoder);

export const symbolDecoder: Builder = (input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, symbolTag);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagSymbol)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};

export const symbol: Internal = /* @__PURE__ */ initSchema(symbolTag, symbolDecoder);

export const literalDecoder: Builder = (input: Val) => {
  const expectedSchema = input.e;
  if (expectedSchema.noValidation && !input.u) {
    return B_nextConst(input, expectedSchema);
  } else if (isLiteral(input.s)) {
    if (input.s.const === expectedSchema.const) {
      return input;
    } else {
      return B_nextConst(input, expectedSchema);
    }
  } else {
    const schemaTagFlag = tagFlags[expectedSchema.type]!;

    if (
      flagUnsafeHas(tagFlags[input.s.type]!, tagFlagString) &&
      flagUnsafeHas(
        schemaTagFlag,
        tagFlagBoolean | tagFlagNumber | tagFlagBigint | tagFlagUndefined | tagFlagNull | tagFlagNaN,
      )
    ) {
      const stringConstSchema = baseSchema(stringTag, false, literalDecoder);
      stringConstSchema.const = "" + (expectedSchema.const as string);

      const stringConstVal = B_nextConst(input, stringConstSchema, stringConstSchema);

      stringConstVal.vc = [
        {
          c: (inputVar) => `${inputVar}==="${stringConstSchema.const as string}"`,
          f: failInvalidType,
        },
      ];

      return B_nextConst(stringConstVal, expectedSchema, expectedSchema);
    } else if (flagUnsafeHas(schemaTagFlag, tagFlagNaN)) {
      return B_refine(input, expectedSchema, [nanCheck]);
    } else {
      return B_refine(input, expectedSchema, [
        {
          c: (inputVar) => `${inputVar}===${B_inlineConst(input, expectedSchema)}`,
          f: failInvalidType,
        },
      ]);
    }
  }
};

export const unit: Internal = /* @__PURE__ */ initSchema(undefinedTag, literalDecoder, (s) => {
  s.const = U;
});

export const void_: Internal = /* @__PURE__ */ initSchema(undefinedTag, literalDecoder, (s) => {
  s.const = U;
  s.name = "void";
});

export const nullLiteral: Internal = /* @__PURE__ */ initSchema(nullTag, literalDecoder, (s) => {
  s.const = null;
});

export const nan: Internal = /* @__PURE__ */ initSchema(nanTag, literalDecoder, (s) => {
  s.const = NaN;
});

export const Literal_parse = (value: unknown): Internal => {
  if (value === null) {
    return nullLiteral;
  } else {
    const tag = typeof value;
    if (tag === undefinedTag) {
      return unit;
    } else if (tag === numberTag && Number.isNaN(value as number)) {
      return nan;
    } else if (tag === objectTag) {
      const s = baseSchema(instanceTag, true, literalDecoder);
      s.class = (value as Record<string, unknown>)["constructor"];
      s.const = value;
      return s;
    } else {
      const s = baseSchema(tag, true, literalDecoder);
      s.const = value;
      return s;
    }
  }
}
