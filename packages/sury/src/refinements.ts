// Refinements — checks layered onto an existing schema — and the string
// formats, which are the same idea with a canned predicate.

import {
  arrayTag,
  bigintTag,
  cached,
  type Internal,
  numberTag,
  panic,
  pathEmpty,
  stringify,
  stringTag,
  SuryError,
  toExpression,
  U,
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

// Every bound below is interpolated straight into generated source rather than
// embedded as `e[n]`, so these asserts are the only thing standing between a
// caller-supplied value and arbitrary code in a compiled operation. Nothing
// reaches a template without passing one first. `String()` of a number is
// always a valid numeric literal — Infinity, -0 and 1e+21 included — and of a
// bigint always digits, so no escaping is needed once the type holds.
export const assertNumber = (fnName: string, n: unknown): void => {
  if (typeof n !== numberTag || Number.isNaN(n)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected number, received ${stringify(n)}`,
    });
  }
};

// The schema decides which numeric type the bound must be: a bigint schema
// takes a bigint and nothing else, since JS refuses to compare the two.
const assertBound = (fnName: string, schema: Internal, value: unknown): void => {
  const tag = schema.type;
  if (tag !== numberTag && tag !== bigintTag) {
    panic(
      `S.${fnName} is not supported for ${toExpression(schema)} schema. Coerce the schema to number or bigint using S.to first.`
    );
  }
  if (tag === bigintTag ? typeof value !== bigintTag : typeof value !== numberTag || Number.isNaN(value)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected ${tag}, received ${stringify(value)}`,
    });
  }
};

const assertSized = (fnName: string, schema: Internal): void => {
  if (schema.type !== stringTag && schema.type !== arrayTag) {
    panic(
      `S.${fnName} is not supported for ${toExpression(schema)} schema. Coerce the schema to string or array using S.to first.`
    );
  }
};

// A bigint prints as bare digits, so the suffix goes back on to keep it a
// bigint literal — without it the comparison silently becomes a mixed
// bigint/number one.
const lit = (value: any): string => (typeof value === bigintTag ? `${value}n` : `${value}`);

const numNoun = (schema: Internal): string => (schema.type === bigintTag ? "BigInt" : "Number");

// A string bounds minLength/maxLength where an array bounds minItems/maxItems.
// Same generated check either way, so the tag picks the keyword and the noun
// rather than there being two of each function.
const sizeKey = (schema: Internal, upper: boolean): "minLength" | "maxLength" | "minItems" | "maxItems" =>
  schema.type === arrayTag ? (upper ? "maxItems" : "minItems") : upper ? "maxLength" : "minLength";

const sizeNoun = (schema: Internal): string[] =>
  schema.type === arrayTag ? ["Array", "items"] : ["String", "characters"];

// A bound only sticks if it actually narrows what the schema already accepts.
// The looser one is dropped rather than kept alongside, so a schema can never
// advertise a bound weaker than the checks it runs — and at most one of
// minimum/exclusiveMinimum survives per side, which the JSON Schema emit
// relies on when deciding whether a format's own range still says anything.
const narrowsLower = (schema: Internal, value: any, exclusive: boolean): boolean => {
  const inclusive = schema.minimum;
  const strict = schema.exclusiveMinimum;
  return (
    (inclusive === U || (exclusive ? value >= inclusive : value > inclusive)) &&
    (strict === U || value > strict)
  );
};

const narrowsUpper = (schema: Internal, value: any, exclusive: boolean): boolean => {
  const inclusive = schema.maximum;
  const strict = schema.exclusiveMaximum;
  return (
    (inclusive === U || (exclusive ? value <= inclusive : value < inclusive)) &&
    (strict === U || value < strict)
  );
};

const narrowsSize = (current: number | undefined, value: number, upper: boolean): boolean =>
  current === U || (upper ? value < current : value > current);

// An empty range is always a caller bug: the schema compiles, emits, and then
// rejects every possible value, which only shows up in production. Reported
// where it's written instead, naming both bounds. `>5` and `<=5` have no
// overlap either, so the boundary cases are contradictions too — hence the
// comparison flipping on whether the incoming bound is exclusive.
const conflict = (fnName: string, value: any, other: string | undefined): void => {
  if (other !== U) {
    panic(`S.${fnName}(${value}) contradicts S.${other} — no value satisfies both`);
  }
};

const assertLower = (fnName: string, schema: Internal, value: any, exclusive: boolean): void => {
  const inclusive = schema.maximum;
  const strict = schema.exclusiveMaximum;
  conflict(
    fnName,
    value,
    inclusive !== U && (exclusive ? value >= inclusive : value > inclusive)
      ? `lte(${inclusive})`
      : strict !== U && value >= strict
        ? `lt(${strict})`
        : U
  );
};

const assertUpper = (fnName: string, schema: Internal, value: any, exclusive: boolean): void => {
  const inclusive = schema.minimum;
  const strict = schema.exclusiveMinimum;
  conflict(
    fnName,
    value,
    inclusive !== U && (exclusive ? value <= inclusive : value < inclusive)
      ? `gte(${inclusive})`
      : strict !== U && value <= strict
        ? `gt(${strict})`
        : U
  );
};

const assertSize = (fnName: string, schema: Internal, value: number, upper: boolean): void => {
  const other = schema[sizeKey(schema, !upper)];
  conflict(
    fnName,
    value,
    other !== U && (upper ? value < other : value > other)
      ? `${upper ? "minLength" : "maxLength"}(${other})`
      : U
  );
};

// @__NO_SIDE_EFFECTS__
export const gte = (schema: Internal, minValue: any, maybeMessage?: string): Internal => {
  assertBound("gte", schema, minValue);
  assertLower("gte", schema, minValue, false);
  if (!narrowsLower(schema, minValue, false)) return schema;
  const message = maybeMessage ?? `${numNoun(schema)} must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    mut.exclusiveMinimum = U;
    getMutErrorMessage(mut)["minimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>=${lit(minValue)}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const lte = (schema: Internal, maxValue: any, maybeMessage?: string): Internal => {
  assertBound("lte", schema, maxValue);
  assertUpper("lte", schema, maxValue, false);
  if (!narrowsUpper(schema, maxValue, false)) return schema;
  const message = maybeMessage ?? `${numNoun(schema)} must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    mut.exclusiveMaximum = U;
    getMutErrorMessage(mut)["maximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<=${lit(maxValue)}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const gt = (schema: Internal, minValue: any, maybeMessage?: string): Internal => {
  assertBound("gt", schema, minValue);
  assertLower("gt", schema, minValue, true);
  if (!narrowsLower(schema, minValue, true)) return schema;
  const message = maybeMessage ?? `${numNoun(schema)} must be greater than ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMinimum = minValue;
    mut.minimum = U;
    getMutErrorMessage(mut)["exclusiveMinimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${lit(minValue)}`,
          f: B_failWithErrorMessage("exclusiveMinimum", message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const lt = (schema: Internal, maxValue: any, maybeMessage?: string): Internal => {
  assertBound("lt", schema, maxValue);
  assertUpper("lt", schema, maxValue, true);
  if (!narrowsUpper(schema, maxValue, true)) return schema;
  const message = maybeMessage ?? `${numNoun(schema)} must be lower than ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.exclusiveMaximum = maxValue;
    mut.maximum = U;
    getMutErrorMessage(mut)["exclusiveMaximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${lit(maxValue)}`,
          f: B_failWithErrorMessage("exclusiveMaximum", message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const minLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertSized("minLength", schema);
  assertNumber("minLength", length);
  assertSize("minLength", schema, length, false);
  const key = sizeKey(schema, false);
  if (!narrowsSize(schema[key], length, false)) return schema;
  const [subject, unit] = sizeNoun(schema);
  const message = maybeMessage ?? `${subject} must be ${length} or more ${unit} long`;
  return internalRefine(schema, (mut: Internal) => {
    mut[key] = length;
    getMutErrorMessage(mut)[key] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage(key, message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const maxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertSized("maxLength", schema);
  assertNumber("maxLength", length);
  assertSize("maxLength", schema, length, true);
  const key = sizeKey(schema, true);
  if (!narrowsSize(schema[key], length, true)) return schema;
  const [subject, unit] = sizeNoun(schema);
  const message = maybeMessage ?? `${subject} must be ${length} or fewer ${unit} long`;
  return internalRefine(schema, (mut: Internal) => {
    mut[key] = length;
    getMutErrorMessage(mut)[key] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage(key, message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const length = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertSized("length", schema);
  assertNumber("length", length);
  assertSize("length", schema, length, false);
  assertSize("length", schema, length, true);
  const minKey = sizeKey(schema, false);
  const maxKey = sizeKey(schema, true);
  const [subject, unit] = sizeNoun(schema);
  const message = maybeMessage ?? `${subject} must be exactly ${length} ${unit} long`;
  return internalRefine(schema, (mut: Internal) => {
    mut[minKey] = length;
    mut[maxKey] = length;
    const em = getMutErrorMessage(mut);
    em[minKey] = message;
    em[maxKey] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage(minKey, message),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const empty = (schema: Internal, maybeMessage?: string): Internal =>
  length(schema, 0, maybeMessage);

// @__NO_SIDE_EFFECTS__
export const nonEmpty = (schema: Internal, maybeMessage?: string): Internal =>
  minLength(schema, 1, maybeMessage);

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
