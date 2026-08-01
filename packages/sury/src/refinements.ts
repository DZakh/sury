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

// One shape for every way a bound can be called wrong, so the reader always
// gets the same three facts: which call, what it takes, what it got. The
// schema is named only when it adds something — not when it *is* the problem
// (it's already the `got`), and not when it reads the same as `accepted`.
const expects = (fnName: string, accepted: string, schema: Internal | undefined, got: string): string => {
  const on = schema !== U ? toExpression(schema) : accepted;
  return `S.${fnName} expects ${accepted}${on === accepted ? "" : ` for ${on}`}, got ${got}`;
};

// Every bound is interpolated straight into generated source rather than
// embedded as `e[n]`, so these asserts are the only thing standing between a
// caller-supplied value and arbitrary code in a compiled operation. Nothing
// reaches a template without passing one first. `String()` of a number is
// always a valid numeric literal — Infinity, -0 and 1e+21 included — and of a
// bigint always digits, so no escaping is needed once the type holds.
//
// A misused schema panics where a bad value raises a SuryError: fromJSONSchema
// reads the panic as `never` (a document may legally describe an empty range)
// and lets the SuryError through (a document with `minimum: "5"` is malformed).
const assertBound = (fnName: string, schema: Internal, value: unknown): void => {
  const tag = schema.type;
  if (tag !== numberTag && tag !== bigintTag) {
    panic(expects(fnName, "number | bigint", U, toExpression(schema)));
  }
  if (tag === bigintTag ? typeof value !== bigintTag : typeof value !== numberTag || Number.isNaN(value)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects(fnName, tag, schema, stringify(value)),
    });
  }
};

// A length is a count, so a negative, fractional or infinite one describes a
// schema nothing can satisfy — caught here rather than compiling to a check
// like `i.length>Infinity` that silently rejects everything.
const assertSized = (fnName: string, schema: Internal, value: unknown): void => {
  if (schema.type !== stringTag && schema.type !== arrayTag) {
    panic(expects(fnName, "string | array", U, toExpression(schema)));
  }
  if (typeof value !== numberTag || !Number.isSafeInteger(value) || (value as number) < 0) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects(fnName, "integer >= 0", schema, stringify(value)),
    });
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
// where it's written instead, as the two expressions that can't both hold.
// `>5` and `<=5` have no overlap either, so the boundary cases are
// contradictions too — hence the comparison flipping on whether the incoming
// bound is exclusive.
//
// Both sides render through toExpression, so they read in the same syntax the
// schema does — `string.length == 2 contradicts string.length >= 3`, not a
// pair of constructor names the caller may not have written.
const conflict = (incoming: Internal, existing: Internal): void => {
  panic(`${toExpression(incoming)} contradicts ${toExpression(existing)}`);
};

// One bound of `schema`, rendered alone. Copies the schema so toExpression
// still sees its type and items, but sets `bounds` to just this bit, so every
// other bound stays invisible.
const asBound = (schema: Internal, key: string, bit: number, value: any): Internal => {
  const mut = { ...schema, bounds: bit } as unknown as Record<string, unknown>;
  mut[key] = value;
  return mut as unknown as Internal;
};

const assertLower = (schema: Internal, value: any, exclusive: boolean): void => {
  const inclusive = schema.maximum;
  const strict = schema.exclusiveMaximum;
  const incoming = asBound(schema, exclusive ? "exclusiveMinimum" : "minimum", exclusive ? 4 : 1, value);
  if (inclusive !== U && (exclusive ? value >= inclusive : value > inclusive)) {
    conflict(incoming, asBound(schema, "maximum", 2, inclusive));
  }
  if (strict !== U && value >= strict) {
    conflict(incoming, asBound(schema, "exclusiveMaximum", 8, strict));
  }
};

const assertUpper = (schema: Internal, value: any, exclusive: boolean): void => {
  const inclusive = schema.minimum;
  const strict = schema.exclusiveMinimum;
  const incoming = asBound(schema, exclusive ? "exclusiveMaximum" : "maximum", exclusive ? 8 : 2, value);
  if (inclusive !== U && (exclusive ? value <= inclusive : value < inclusive)) {
    conflict(incoming, asBound(schema, "minimum", 1, inclusive));
  }
  if (strict !== U && value <= strict) {
    conflict(incoming, asBound(schema, "exclusiveMinimum", 4, strict));
  }
};

const assertSize = (schema: Internal, value: number, upper: boolean): void => {
  const otherKey = sizeKey(schema, !upper);
  const other = schema[otherKey];
  if (other !== U && (upper ? value < other : value > other)) {
    conflict(
      asBound(schema, sizeKey(schema, upper), upper ? 2 : 1, value),
      asBound(schema, otherKey, upper ? 1 : 2, other)
    );
  }
};

// @__NO_SIDE_EFFECTS__
export const gte = (schema: Internal, minValue: any, maybeMessage?: string): Internal => {
  assertBound("gte", schema, minValue);
  assertLower(schema, minValue, false);
  if (!narrowsLower(schema, minValue, false)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) & ~4 | 1;
    mut.minimum = minValue;
    mut.exclusiveMinimum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["minimum"] = maybeMessage;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>=${lit(minValue)}`,
          f: B_failWithErrorMessage("minimum"),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const lte = (schema: Internal, maxValue: any, maybeMessage?: string): Internal => {
  assertBound("lte", schema, maxValue);
  assertUpper(schema, maxValue, false);
  if (!narrowsUpper(schema, maxValue, false)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) & ~8 | 2;
    mut.maximum = maxValue;
    mut.exclusiveMaximum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["maximum"] = maybeMessage;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<=${lit(maxValue)}`,
          f: B_failWithErrorMessage("maximum"),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const gt = (schema: Internal, minValue: any, maybeMessage?: string): Internal => {
  assertBound("gt", schema, minValue);
  assertLower(schema, minValue, true);
  if (!narrowsLower(schema, minValue, true)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) & ~1 | 4;
    mut.exclusiveMinimum = minValue;
    mut.minimum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["exclusiveMinimum"] = maybeMessage;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${lit(minValue)}`,
          f: B_failWithErrorMessage("exclusiveMinimum"),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const lt = (schema: Internal, maxValue: any, maybeMessage?: string): Internal => {
  assertBound("lt", schema, maxValue);
  assertUpper(schema, maxValue, true);
  if (!narrowsUpper(schema, maxValue, true)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) & ~2 | 8;
    mut.exclusiveMaximum = maxValue;
    mut.maximum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["exclusiveMaximum"] = maybeMessage;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${lit(maxValue)}`,
          f: B_failWithErrorMessage("exclusiveMaximum"),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const minLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertSized("minLength", schema, length);
  assertSize(schema, length, false);
  const key = sizeKey(schema, false);
  if (!narrowsSize(schema[key], length, false)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) | 1;
    mut[key] = length;
    if (maybeMessage !== U) getMutErrorMessage(mut)[key] = maybeMessage;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage(key),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const maxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertSized("maxLength", schema, length);
  assertSize(schema, length, true);
  const key = sizeKey(schema, true);
  if (!narrowsSize(schema[key], length, true)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) | 2;
    mut[key] = length;
    if (maybeMessage !== U) getMutErrorMessage(mut)[key] = maybeMessage;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage(key),
        },
      ];
    };
  });
}

// @__NO_SIDE_EFFECTS__
export const length = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertSized("length", schema, length);
  assertSize(schema, length, false);
  assertSize(schema, length, true);
  const minKey = sizeKey(schema, false);
  const maxKey = sizeKey(schema, true);
  return internalRefine(schema, (mut: Internal) => {
    mut.bounds = (schema.bounds ?? 0) | 3;
    mut[minKey] = length;
    mut[maxKey] = length;
    if (maybeMessage !== U) {
      const em = getMutErrorMessage(mut);
      em[minKey] = maybeMessage;
      em[maxKey] = maybeMessage;
    }
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage(minKey),
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
    s.minimum = 0;
    s.maximum = 65535;
    s.refiner = (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}>=0&&${inputVar}<65536&&${inputVar}%1===0`,
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
