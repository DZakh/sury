import { baseSchema, cached } from "./schema";
import { B_embed, B_embedInvalidInput, B_inlineConst, B_next, B_nextConst, B_refine, B_unsupportedDecode, B_varWithoutAllocation, _var, failInvalidType } from "./builder";
import { Check, Internal, Val, isLiteral } from "./types";
import { Builder } from "./builder";
import { flagDisableNanNumberValidation, flagUnsafeHas } from "./flags";
import { Tag, bigintTag, booleanTag, instanceTag, nanTag, nullTag, numberTag, objectTag, stringTag, symbolTag, tagFlagBigint, tagFlagBoolean, tagFlagNaN, tagFlagNull, tagFlagNumber, tagFlagRef, tagFlagString, tagFlagSymbol, tagFlagUndefined, tagFlagUnion, tagFlagUnknown, tagFlags, undefinedTag, unknownTag } from "./tags";

export const int32FormatValidation = (inputVar: string) => {
  return `${inputVar}<=2147483647&&${inputVar}>=-2147483648&&${inputVar}%1===0`;
};

// Atomic type-narrow conditions, shared by the type decoders and the union
// dispatch (`typeCheckCond`) so the two can't drift.
export const typeofCond = (tag: Tag) => (inputVar: string): string =>
  `typeof ${inputVar}==="${tag}"`;
export const nanCond = (inputVar: string): string => `Number.isNaN(${inputVar})`;
export const isArrayCond = (inputVar: string): string => `Array.isArray(${inputVar})`;
export const objectTagCond = (inputVar: string): string =>
  `${typeofCond(objectTag)(inputVar)}&&${inputVar}`;
// `class` is a reserved word in TS, so the parameter is named `class_`.
export const instanceofCond = (b: Val, class_: unknown) => (inputVar: string): string =>
  `${inputVar} instanceof ${B_embed(b, class_)}`;

// Reject anything but `tag` when the input is still `unknown` — shared by
// every primitive decoder's unknown-input branch.
const B_refineTypeofUnknown = (input: Val, tag: Tag): Val => {
  return B_refine(input, input.e, [
    {
      c: typeofCond(tag),
      f: failInvalidType,
    },
  ]);
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
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    const checks: Check[] = [
      {
        c: typeofCond(numberTag),
        f: failInvalidType,
      },
    ];
    if (input.e.format === "int32") {
      checks.push({
        c: int32FormatValidation,
        f: failInvalidType,
      });
    } else {
      if (!flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
        checks.push({
          c: (inputVar) => `!${nanCond(inputVar)}`,
          f: failInvalidType,
        });
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
          input.e.format === "int32"
            ? int32FormatValidation(output.i)
            : `!${nanCond(output.i)}`,
        f: failInvalidType,
      },
    ];
    return output;
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagNumber)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else if (input.s.format !== input.e.format && input.e.format === "int32") {
    return B_refine(input, input.e, [
      {
        c: int32FormatValidation,
        f: failInvalidType,
      },
    ]);
  } else {
    return input;
  }
};

export const float = () =>
  cached(numberTag, numberTag, (s) => {
    s.decoder = numberDecoder;
  });

export const int = () =>
  cached("i", numberTag, (s) => {
    s.format = "int32";
    s.decoder = numberDecoder;
  });

// inputToString/stringDecoderFn/string are mutually recursive (stringDecoderFn
// falls back to inputToString, which builds its output schema via string())
// and so are kept together.
export const inputToString = (input: Val): Val => {
  return B_next(input, `""+${input.i}`, string());
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
    const schema = baseSchema(stringTag, false);
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
export const string = (): Internal => {
  return cached(stringTag, stringTag, (s) => {
    s.decoder = stringDecoderFn;
  });
}

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

export const bool = () =>
  cached(booleanTag, booleanTag, (s) => {
    s.decoder = booleanDecoder;
  });

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

export const bigint = () =>
  cached(bigintTag, bigintTag, (s) => {
    s.decoder = bigintDecoder;
  });

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

export const symbol = () =>
  cached(symbolTag, symbolTag, (s) => {
    s.decoder = symbolDecoder;
  });

export const setHas = (has: Partial<Record<Tag, boolean>>, tag: Tag): void => {
  has[flagUnsafeHas(tagFlags[tag]!, tagFlagUnion | tagFlagRef) ? unknownTag : tag] = true;
}

export const jsonName = `JSON`;

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
      const stringConstSchema = baseSchema(stringTag, false);
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
      return B_refine(input, expectedSchema, [
        {
          c: nanCond,
          f: failInvalidType,
        },
      ]);
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

export const unit = () =>
  cached(undefinedTag, undefinedTag, (s) => {
    s.const = void 0;
    s.decoder = literalDecoder;
  });

export const void_ = () =>
  cached("void", undefinedTag, (s) => {
    s.const = void 0;
    s.name = "void";
    s.decoder = literalDecoder;
  });

export const nullLiteral = () =>
  cached(nullTag, nullTag, (s) => {
    s.const = null;
    s.decoder = literalDecoder;
  });

export const nan = () =>
  cached(nanTag, nanTag, (s) => {
    s.const = NaN;
    s.decoder = literalDecoder;
  });

export const Literal_parse = (value: unknown): Internal => {
  if (value === null) {
    return nullLiteral();
  } else {
    const tag = typeof value;
    if (tag === undefinedTag) {
      return unit();
    } else if (tag === numberTag && Number.isNaN(value as number)) {
      return nan();
    } else if (tag === objectTag) {
      const s = baseSchema(instanceTag, true);
      s.class = (value as Record<string, unknown>)["constructor"];
      s.const = value;
      s.decoder = literalDecoder;
      return s;
    } else {
      const s = baseSchema(tag, true);
      s.const = value;
      s.decoder = literalDecoder;
      return s;
    }
  }
}
