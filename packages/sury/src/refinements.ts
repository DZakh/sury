// Refinements — checks layered onto an existing schema — and the string
// formats, which are the same idea with a canned predicate.

import {
  bigintTag,
  cached,
  type Internal,
  numberTag,
  pathEmpty,
  stringify,
  stringTag,
  SuryError,
  type Val,
} from "./base";
import { B_embed, B_failWithErrorMessage } from "./builder";
import { optionFactory } from "./composites";
import { getMutErrorMessage, internalRefine, nullAsUnit, transform } from "./modifiers";
import { nullLiteral, numberDecoder, stringDecoderFn, unit } from "./primitives";
import { unionFactory } from "./union";

// Re-exports, not `const object = schemaObject` aliases: an alias makes the
// public name a variable that merely holds the function, and a bundler honors
// `@__NO_SIDE_EFFECTS__` only on the declaration that IS the function — so an
// alias silently drops the annotation, and every `S.object(…)` a consumer
// never uses stays in their bundle.
export { schemaObject as object, schemaShape as shape, schemaTuple as tuple } from "./factory";
export { dictFactory as dict } from "./composites";
export { unionFactory as union } from "./union";
// @__NO_SIDE_EFFECTS__
export const nullAsOption = (item: Internal): Internal =>
  optionFactory(item, nullAsUnit());
// `null` is a reserved word in JS/TS binding position, so this is exported
// as `null_`.
export const null_ = (item: Internal): Internal =>
  unionFactory([item, nullLiteral()]);

// =============
// Built-in refinements
// =============

// Load-bearing beyond the error message: every bound below is interpolated
// straight into generated source rather than embedded as `e[n]`, so this
// typeof is the only thing standing between a caller-supplied value and
// arbitrary code in a compiled operation. Anything that reaches the template
// must have passed through here first. `String()` of a number is always a
// valid numeric literal — including Infinity, -0 and 1e+21 — so no escaping
// is needed once the type holds; NaN is rejected because a bound it can never
// satisfy is a caller mistake, not because it would be unsafe to print.
export const assertNumber = (fnName: string, n: unknown): void => {
  if (typeof n !== numberTag || Number.isNaN(n)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected number, received ${stringify(n)}`,
    });
  }
};

export const intMin = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("gte", minValue);
  const message = maybeMessage ?? `Number must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${minValue - 1}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export const intMax = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  assertNumber("lte", maxValue);
  const message = maybeMessage ?? `Number must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${maxValue + 1}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export const floatMin = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("gte", minValue);
  const message = maybeMessage ?? `Number must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>=${minValue}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export const floatMax = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  assertNumber("lte", maxValue);
  const message = maybeMessage ?? `Number must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<=${maxValue}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export const intGreater = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("gt", minValue);
  const message = maybeMessage ?? `Number must be greater than ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMinimum = minValue;
    getMutErrorMessage(mut)["exclusiveMinimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${minValue}`,
          f: B_failWithErrorMessage("exclusiveMinimum", message),
        },
      ];
    };
  });
}

export const intLess = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  assertNumber("lt", maxValue);
  const message = maybeMessage ?? `Number must be lower than ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMaximum = maxValue;
    getMutErrorMessage(mut)["exclusiveMaximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${maxValue}`,
          f: B_failWithErrorMessage("exclusiveMaximum", message),
        },
      ];
    };
  });
}

export const floatGreater = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("gt", minValue);
  const message = maybeMessage ?? `Number must be greater than ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMinimum = minValue;
    getMutErrorMessage(mut)["exclusiveMinimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${minValue}`,
          f: B_failWithErrorMessage("exclusiveMinimum", message),
        },
      ];
    };
  });
}

export const floatLess = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  assertNumber("lt", maxValue);
  const message = maybeMessage ?? `Number must be lower than ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMaximum = maxValue;
    getMutErrorMessage(mut)["exclusiveMaximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${maxValue}`,
          f: B_failWithErrorMessage("exclusiveMaximum", message),
        },
      ];
    };
  });
}

// Guards interpolation into generated source, exactly as assertNumber does.
// `String()` of a bigint is digits with an optional leading `-`, so the bound
// is printed with an `n` suffix to stay a bigint literal — without it the
// comparison would silently become a mixed bigint/number one.
export const assertBigint = (fnName: string, n: unknown): void => {
  if (typeof n !== bigintTag) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected bigint, received ${stringify(n)}`,
    });
  }
};

export const bigintMin = (schema: Internal, minValue: bigint, maybeMessage?: string): Internal => {
  assertBigint("gte", minValue);
  const message = maybeMessage ?? `BigInt must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>=${minValue}n`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export const bigintMax = (schema: Internal, maxValue: bigint, maybeMessage?: string): Internal => {
  assertBigint("lte", maxValue);
  const message = maybeMessage ?? `BigInt must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<=${maxValue}n`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export const bigintGreater = (schema: Internal, minValue: bigint, maybeMessage?: string): Internal => {
  assertBigint("gt", minValue);
  const message = maybeMessage ?? `BigInt must be greater than ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMinimum = minValue;
    getMutErrorMessage(mut)["exclusiveMinimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${minValue}n`,
          f: B_failWithErrorMessage("exclusiveMinimum", message),
        },
      ];
    };
  });
}

export const bigintLess = (schema: Internal, maxValue: bigint, maybeMessage?: string): Internal => {
  assertBigint("lt", maxValue);
  const message = maybeMessage ?? `BigInt must be lower than ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMaximum = maxValue;
    getMutErrorMessage(mut)["exclusiveMaximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${maxValue}n`,
          f: B_failWithErrorMessage("exclusiveMaximum", message),
        },
      ];
    };
  });
}

export const arrayMinLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("minLength", length);
  const message = maybeMessage ?? `Array must be ${length} or more items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minItems = length;
    getMutErrorMessage(mut)["minItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage("minItems", message),
        },
      ];
    };
  });
}

export const arrayMaxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("maxLength", length);
  const message = maybeMessage ?? `Array must be ${length} or fewer items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maxItems = length;
    getMutErrorMessage(mut)["maxItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage("maxItems", message),
        },
      ];
    };
  });
}

export const arrayLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("length", length);
  const message = maybeMessage ?? `Array must be exactly ${length} items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minItems = length;
    mut.maxItems = length;
    const em = getMutErrorMessage(mut);
    em["minItems"] = message;
    em["maxItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage("minItems", message),
        },
      ];
    };
  });
}

export const stringMinLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("minLength", length);
  const message = maybeMessage ?? `String must be ${length} or more characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minLength = length;
    getMutErrorMessage(mut)["minLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage("minLength", message),
        },
      ];
    };
  });
}

export const stringMaxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("maxLength", length);
  const message = maybeMessage ?? `String must be ${length} or fewer characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maxLength = length;
    getMutErrorMessage(mut)["maxLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage("maxLength", message),
        },
      ];
    };
  });
}

export const stringLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("length", length);
  const message = maybeMessage ?? `String must be exactly ${length} characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minLength = length;
    mut.maxLength = length;
    const em = getMutErrorMessage(mut);
    em["minLength"] = message;
    em["maxLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage("minLength", message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const pattern = (schema: Internal, re: RegExp, message: string = `Invalid pattern`): Internal => {
  return internalRefine(schema, (mut: Internal) => {
    mut.pattern = re;
    getMutErrorMessage(mut)["pattern"] = message;
    return (input: Val) => {
      const embededRe = B_embed(input, re);
      return [
        {
          c: (inputVar: string) =>
            re.global
              ? `(${embededRe}.lastIndex=0,${embededRe}.test(${inputVar}))`
              : `${embededRe}.test(${inputVar})`,
          f: B_failWithErrorMessage("pattern", message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const trim = (schema: Internal): Internal => {
  const transformer = (string: unknown) => (string as string).trim();
  return transform(schema, (_: unknown) => ({
    p: transformer,
    s: transformer,
  }));
}

// @__NO_SIDE_EFFECTS__
export const nullable = (schema: Internal): Internal => {
  return unionFactory([schema, unit(), nullLiteral()]);
}

// @__NO_SIDE_EFFECTS__
export const nullableAsOption = (schema: Internal): Internal => {
  return unionFactory([schema, unit(), nullAsUnit()]);
}

export const isoDateTime = (): Internal => {
  return cached("date-time", stringTag, (s) => {
    const datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/;
    s.decoder = stringDecoderFn;
    s.format = "date-time";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, datetimeRe)}.test(${inputVar})`,
          f: B_failWithErrorMessage(
            "format",
            "Invalid datetime string! Expected UTC",
          ),
        },
      ];
    };
  });
}

export const port = (): Internal => {
  return cached("port", numberTag, (s) => {
    s.decoder = numberDecoder;
    s.format = "port";
    s.refiner = (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const email = (): Internal => {
  return cached("email", stringTag, (s) => {
    const emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i;
    s.decoder = stringDecoderFn;
    s.format = "email";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, emailRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const uuid = (): Internal => {
  return cached("uuid", stringTag, (s) => {
    const uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i;
    s.decoder = stringDecoderFn;
    s.format = "uuid";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, uuidRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const cuid = (): Internal => {
  return cached("cuid", stringTag, (s) => {
    const cuidRegex = /^c[^\s-]{8,}$/i;
    s.decoder = stringDecoderFn;
    s.format = "cuid";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, cuidRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const url = (): Internal => {
  return cached("url", stringTag, (s) => {
    const urlValidator = (s: string) => {
      try {
        new URL(s);
        return true;
      } catch {
        return false;
      }
    };
    s.decoder = stringDecoderFn;
    s.format = "url";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, urlValidator)}(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}
