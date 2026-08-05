// Refinements — checks layered onto an existing schema — and the string
// formats, which are the same idea with a canned predicate.

import {
  arrayTag,
  bigintTag,
  initSchema,
  inputExpression,
  type Internal,
  numberTag,
  panic,
  pathEmpty,
  stringify,
  stringTag,
  SuryError,
  U,
  type Val,
} from "./base";
import { B_embed, B_failWithErrorMessage } from "./builder";
import { definitionToSchema, optionFactory } from "./composites";
import { getMutErrorMessage, internalRefine, nullAsUnit, transform } from "./modifiers";
import { nullLiteral, numberDecoder, stringDecoderFn, unit } from "./primitives";
import { unionFactory } from "./union";

// Re-exports, not `const object = schemaObject` aliases: an alias makes the
// public name a variable that merely holds the function, and a bundler honors
// `@__NO_SIDE_EFFECTS__` only on the declaration that IS the function — so an
// alias silently drops the annotation, and every `S.object(…)` a consumer
// never uses stays in their bundle.
export { schemaObject as object, schemaShape as shape, schemaTuple as tuple } from "./factory";
export { dict } from "./composites";
export { unionFactory as union } from "./union";
// @__NO_SIDE_EFFECTS__
export const nullAsOption = (item: Internal): Internal =>
  optionFactory(item, nullAsUnit);
// `null` is a reserved word in JS/TS binding position, so this is exported
// as `null_`.
export const null_ = (item: Internal): Internal =>
  unionFactory([item, nullLiteral]);

// =============
// Built-in refinements
// =============

// One shape for every way a bound can be called wrong: which call, what it
// wanted, what it got. What it wanted differs by which half is wrong — a bad
// bound value is measured against the schema it is being applied to, a bad
// schema against the set of schemas the bound accepts, which the word
// "schema" marks so the two can't be misread for each other.
const expects = (fnName: string, expected: string, got: string): string =>
  `S.${fnName} expects ${expected}, got ${got}`;

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
const assertNumericBound = (fnName: string, schema: Internal, value: unknown): void => {
  const tag = schema.type;
  if (tag !== numberTag && tag !== bigintTag) {
    panic(expects(fnName, "number | bigint schema", inputExpression(schema)));
  }
  if (tag === bigintTag ? typeof value !== bigintTag : typeof value !== numberTag || Number.isNaN(value)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects(fnName, inputExpression(schema), stringify(value)),
    });
  }
};

// A length is a count, so a negative, fractional or infinite one describes a
// schema nothing can satisfy — caught here rather than compiling to a check
// like `i.length>Infinity` that silently rejects everything.
const assertLengthBound = (fnName: string, schema: Internal, value: unknown): void => {
  if (schema.type !== stringTag && schema.type !== arrayTag) {
    panic(expects(fnName, "string | array schema", inputExpression(schema)));
  }
  if (typeof value !== numberTag || !Number.isSafeInteger(value) || (value as number) < 0) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects(fnName, "integer >= 0", stringify(value)),
    });
  }
};

// A bigint prints as bare digits, so the suffix goes back on to keep it a
// bigint literal — without it the generated comparison silently becomes a mixed
// bigint/number one, and a rendered bound reads as the number it isn't while
// `received` next to it prints `4n`.
const lit = (value: number | bigint): string => (typeof value === bigintTag ? `${value}n` : `${value}`);

// A string bounds minLength/maxLength where an array bounds minItems/maxItems.
// Same generated check either way, so the tag picks the keyword rather than
// there being two of each function.
const sizeKey = (schema: Internal, upper: boolean): "minLength" | "maxLength" | "minItems" | "maxItems" =>
  schema.type === arrayTag ? (upper ? "maxItems" : "minItems") : upper ? "maxLength" : "minLength";

// Bounds wrap the expression they constrain, in ArkType's double-bounded
// spelling — `0 < number < 10` rather than a clause per side. A string or
// array bounds its `.length`, which is named so the comparison can't be read
// against the value: `string.length >= 3` against a received `"hi"`.
//
// This lives here rather than in inputExpression so that a consumer who never
// writes a bound doesn't carry it: reaching it costs ~400 bytes that base.ts
// could never shake, where `expression` is the hook base.ts offers for a
// rendering another module owns.
const withBounds = (schema: Internal, base: string): string => {
  const written = schema.bounds!;
  const isArray = schema.type === arrayTag;
  const sized = isArray || schema.type === stringTag;
  const minKey = isArray ? "minItems" : sized ? "minLength" : "minimum";
  const maxKey = isArray ? "maxItems" : sized ? "maxLength" : "maximum";
  // No JSON Schema keyword bounds a length exclusively, so only a value bound
  // can be strict.
  const exMin = written & 4 ? schema.exclusiveMinimum : U;
  const exMax = written & 8 ? schema.exclusiveMaximum : U;
  const low = exMin !== U ? exMin : written & 1 ? schema[minKey] : U;
  const high = exMax !== U ? exMax : written & 2 ? schema[maxKey] : U;
  const subject = sized ? `${base}.length` : base;
  if (low === U) {
    return `${subject} ${exMax !== U ? "<" : "<="} ${lit(high!)}`;
  }
  if (high === U) {
    return `${subject} ${exMin !== U ? ">" : ">="} ${lit(low)}`;
  }
  return exMin === U && exMax === U && low === high
    ? `${subject} == ${lit(low)}`
    : `${lit(low)} ${exMin !== U ? "<" : "<="} ${subject} ${exMax !== U ? "<" : "<="} ${lit(high)}`;
};

// Only the first bound on a schema captures the rendering it wraps — a later
// one inherits this override through the copy and must reuse the same base, or
// the wrapping nests into `1 <= (1 <= number <= 9) <= 9`. `skipOverride` is
// what stops the base rendering from re-entering this.
const setBoundExpression = (mut: Internal, schema: Internal): void => {
  if (schema.bounds === U) {
    const base = schema.expression;
    mut.expression = (s: Internal) =>
      withBounds(s, base !== U ? base(s) : inputExpression(s, true));
  }
};

// Every comparison below casts the bound to `number`: JS compares a number
// against a bigint without complaint where TS refuses, and assertNumericBound
// has already established that the bound matches the schema's numeric type —
// so the cast is safe and stays at the comparison rather than widening four
// signatures to `any`.
//
// A bound only sticks if it actually narrows what the schema already accepts.
// The looser one is dropped rather than kept alongside, so a schema can never
// advertise a bound weaker than the checks it runs — and at most one of
// minimum/exclusiveMinimum survives per side, which the JSON Schema emit
// relies on when deciding whether a format's own range still says anything.
const narrowsLower = (schema: Internal, value: number | bigint, exclusive: boolean): boolean => {
  const bound = value as number;
  const inclusive = schema.minimum as number | undefined;
  const strict = schema.exclusiveMinimum as number | undefined;
  return (
    (inclusive === U || (exclusive ? bound >= inclusive : bound > inclusive)) &&
    (strict === U || bound > strict)
  );
};

const narrowsUpper = (schema: Internal, value: number | bigint, exclusive: boolean): boolean => {
  const bound = value as number;
  const inclusive = schema.maximum as number | undefined;
  const strict = schema.exclusiveMaximum as number | undefined;
  return (
    (inclusive === U || (exclusive ? bound <= inclusive : bound < inclusive)) &&
    (strict === U || bound < strict)
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
// Both sides render through inputExpression, so they read in the same syntax the
// schema does — `string.length == 2 contradicts string.length >= 3`, not a
// pair of constructor names the caller may not have written.
const conflict = (incoming: Internal, existing: Internal): void => {
  panic(`${inputExpression(incoming)} contradicts ${inputExpression(existing)}`);
};

// One bound of `schema`, rendered alone: a copy so inputExpression still sees the
// type and items, with `bounds` set to just this bit so every other bound
// stays invisible. Only ever called from a failing branch — building a message
// must not cost an allocation on every bound that turns out to be fine.
const asBound = (schema: Internal, key: string, bit: number, value: unknown): Internal => {
  const mut = { ...schema, bounds: bit } as unknown as Record<string, unknown>;
  mut[key] = value;
  // The first bound on a schema is reported before one was ever applied, so
  // the copy has no override to inherit and renders bare without this.
  setBoundExpression(mut as unknown as Internal, schema);
  return mut as unknown as Internal;
};

const assertLower = (schema: Internal, value: number | bigint, exclusive: boolean): void => {
  const key = exclusive ? "exclusiveMinimum" : "minimum";
  const bit = exclusive ? 4 : 1;
  const bound = value as number;
  const inclusive = schema.maximum as number | undefined;
  const strict = schema.exclusiveMaximum as number | undefined;
  if (inclusive !== U && (exclusive ? bound >= inclusive : bound > inclusive)) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "maximum", 2, inclusive));
  }
  if (strict !== U && bound >= strict) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "exclusiveMaximum", 8, strict));
  }
};

const assertUpper = (schema: Internal, value: number | bigint, exclusive: boolean): void => {
  const key = exclusive ? "exclusiveMaximum" : "maximum";
  const bit = exclusive ? 8 : 2;
  const bound = value as number;
  const inclusive = schema.minimum as number | undefined;
  const strict = schema.exclusiveMinimum as number | undefined;
  if (inclusive !== U && (exclusive ? bound <= inclusive : bound < inclusive)) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "minimum", 1, inclusive));
  }
  if (strict !== U && bound <= strict) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "exclusiveMinimum", 4, strict));
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
export const gte = (schema: Internal, minValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("gte", schema, minValue);
  assertLower(schema, minValue, false);
  if (!narrowsLower(schema, minValue, false)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~4) | 1;
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
export const lte = (schema: Internal, maxValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("lte", schema, maxValue);
  assertUpper(schema, maxValue, false);
  if (!narrowsUpper(schema, maxValue, false)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~8) | 2;
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
export const gt = (schema: Internal, minValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("gt", schema, minValue);
  assertLower(schema, minValue, true);
  if (!narrowsLower(schema, minValue, true)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~1) | 4;
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
export const lt = (schema: Internal, maxValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("lt", schema, maxValue);
  assertUpper(schema, maxValue, true);
  if (!narrowsUpper(schema, maxValue, true)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~2) | 8;
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
  assertLengthBound("minLength", schema, length);
  assertSize(schema, length, false);
  const key = sizeKey(schema, false);
  if (!narrowsSize(schema[key], length, false)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
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
  assertLengthBound("maxLength", schema, length);
  assertSize(schema, length, true);
  const key = sizeKey(schema, true);
  if (!narrowsSize(schema[key], length, true)) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
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
  assertLengthBound("length", schema, length);
  assertSize(schema, length, false);
  assertSize(schema, length, true);
  const minKey = sizeKey(schema, false);
  const maxKey = sizeKey(schema, true);
  // Both sides already pinned here: `=== length` is exactly what runs, so a
  // second copy of it is the one case this adds nothing, the same way a
  // non-narrowing bound is for the others.
  if (schema[minKey] === length && schema[maxKey] === length) return schema;
  return internalRefine(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
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
  return transform(schema, () => ({
    p: transformer,
    s: transformer,
  }));
}

// @__NO_SIDE_EFFECTS__
export const nullable = (definition: unknown): Internal => {
  return unionFactory([definitionToSchema(definition), unit, nullLiteral]);
}

// @__NO_SIDE_EFFECTS__
export const nullableAsOption = (schema: Internal): Internal => {
  return unionFactory([schema, unit, nullAsUnit]);
}

export const isoDateTime: Internal = /* @__PURE__ */ initSchema(stringTag, (s) => {
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

// The range as real bound fields, for the reason int32 carries its own. The
// check accepts 0, which the emitted `minimum: 0` has always advertised and
// the old `>0` check contradicted — a schema and its description now agree.
export const port: Internal = /* @__PURE__ */ initSchema(numberTag, (s) => {
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

export const email: Internal = /* @__PURE__ */ initSchema(stringTag, (s) => {
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

export const uuid: Internal = /* @__PURE__ */ initSchema(stringTag, (s) => {
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

export const cuid: Internal = /* @__PURE__ */ initSchema(stringTag, (s) => {
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

export const url: Internal = /* @__PURE__ */ initSchema(stringTag, (s) => {
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
