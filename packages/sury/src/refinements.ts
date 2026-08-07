// Refinements — checks layered onto an existing schema — and the string
// formats, which are the same idea with a canned predicate.

import {
  arrayTag,
  bigintTag,
  type Check,
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
  updateOutput,
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
  const written = schema.bounds ?? 0;
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
  // A divisor narrows the subject the bounds range over rather than adding a
  // bound of its own: `-50 < (number % 2) < 50`. Only a sized schema can't
  // carry one (multipleOf rejects string | array), so `.length` never mixes
  // with `%`.
  const mo = schema.multipleOf;
  const subject0 = sized ? `${base}.length` : base;
  if (low === U && high === U) {
    // Only reachable with a bare divisor — nothing wraps it, so no parens.
    return `${subject0} % ${lit(mo!)}`;
  }
  const subject = mo !== U ? `(${subject0} % ${lit(mo)})` : subject0;
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

// Only the first bound or divisor on a schema captures the rendering it
// wraps — a later one inherits this override through the copy and must reuse
// the same base, or the wrapping nests into `1 <= (1 <= number <= 9) <= 9`.
// `skipOverride` is what stops the base rendering from re-entering this.
const setBoundExpression = (mut: Internal, schema: Internal): void => {
  if (schema.bounds === U && schema.multipleOf === U) {
    const base = schema.expression;
    mut.expression = (s: Internal) =>
      withBounds(s, base !== U ? base(s) : inputExpression(s, true));
  }
};

// Whether the stored double is certainly the divisor the caller wrote. Every
// integer is; `0.0001` is not — it stores as 0.000100000000000000004792…, so
// `x % 0.0001 === 0` asks whether x is a multiple of *that* and answers no
// for 0.0075. This is the whole reason the two checks below differ: `%` is
// exact in IEEE-754, which makes it the right answer to the wrong question
// whenever the divisor itself is inexact.
//
// A binary fraction (0.5, 1.5) is exact too and would be safe with `%`, but
// it takes the tolerant path here — same verdicts either way, and one
// predicate beats a second one that has to be right about representability.
//
// It also gates the build-time emptiness check: reasoning about which
// multiples fall in a range means doing the same arithmetic, and on an
// inexact divisor that reports `0.3 <= number <= 0.3, multipleOf 0.1` as
// empty when 0.3 is a multiple by every reading the caller has.
const exactDivisor = (d: number | bigint): boolean =>
  typeof d === bigintTag || Number.isInteger(d);

// An inexact divisor is compared on the ratio instead, which collapses the
// representation error into a rounding rather than leaving it as a remainder
// the size of the divisor. The tolerance is relative because the error in
// `ratio` grows with its magnitude. Overflow answers itself: 1e308 / 1e-308
// is Infinity, so the difference is NaN and every comparison against it is
// false — the rejection the JSON Schema suite asks for.
const multipleOfValidator = (d: number) => (value: number): boolean => {
  const ratio = value / d;
  return Math.abs(ratio - Math.round(ratio)) < Number.EPSILON * Math.max(Math.abs(ratio), 1);
};

// One refiner serves every bound and divisor on a schema, reading the fields
// at codegen time instead of closing over the value each call captured. That
// is what lets a narrowing call *replace* a check rather than stack a second
// one after it: `gte(5).gte(10)` compiles to the single `>=10`, and `length(3)`
// after `maxLength(5)` retracts the `<6` — refinements intersect, ArkType
// style, rather than append. Reading `input.e` is sound because a refiner is
// only ever invoked through the schema that owns it (`val.e.refiner(val)`,
// and the reversed copy carries the same fields), and a bound can never land
// on a union (assertNumericBound rejects the anyOf tag), so the one context
// that re-attaches refiners to other schemas — the union compiler — can't
// receive this one.
const boundsRefiner = (input: Val): Check[] => {
  const s = input.e;
  const written = s.bounds ?? 0;
  const checks: Check[] = [];
  if (s.type === stringTag || s.type === arrayTag) {
    const minKey = sizeKey(s, false);
    const maxKey = sizeKey(s, true);
    const min = written & 1 ? (s[minKey] as number) : U;
    const max = written & 2 ? (s[maxKey] as number) : U;
    const em = s.errorMessage as Record<string, string | undefined> | undefined;
    // Collapsing to `===` folds both directions into one check with one
    // message — sound only when both directions would say the same thing.
    // Independent minLength(5)/maxLength(5, "custom") calls converge without
    // either superseding the other, and a too-short value must not report
    // "custom": those keep a check per direction, each with its own key.
    if (min !== U && min === max && (em !== U ? em[minKey] : U) === (em !== U ? em[maxKey] : U)) {
      checks.push({
        c: (inputVar) => `${inputVar}.length===${min}`,
        f: B_failWithErrorMessage(minKey),
      });
    } else {
      if (min !== U) {
        checks.push({
          c: (inputVar) => `${inputVar}.length>${min - 1}`,
          f: B_failWithErrorMessage(minKey),
        });
      }
      if (max !== U) {
        checks.push({
          c: (inputVar) => `${inputVar}.length<${max + 1}`,
          f: B_failWithErrorMessage(maxKey),
        });
      }
    }
  } else {
    const exMin = written & 4 ? s.exclusiveMinimum : U;
    const min = exMin !== U ? exMin : written & 1 ? s.minimum : U;
    if (min !== U) {
      checks.push({
        c: (inputVar) => `${inputVar}${exMin !== U ? ">" : ">="}${lit(min)}`,
        f: B_failWithErrorMessage(exMin !== U ? "exclusiveMinimum" : "minimum"),
      });
    }
    const exMax = written & 8 ? s.exclusiveMaximum : U;
    const max = exMax !== U ? exMax : written & 2 ? s.maximum : U;
    if (max !== U) {
      checks.push({
        c: (inputVar) => `${inputVar}${exMax !== U ? "<" : "<="}${lit(max)}`,
        f: B_failWithErrorMessage(exMax !== U ? "exclusiveMaximum" : "maximum"),
      });
    }
    const mo = s.multipleOf;
    if (mo !== U) {
      let cond: (inputVar: string) => string;
      if (exactDivisor(mo)) {
        // A bigint remainder is `0n`, which `===0` never matches — the zero
        // literal has to be the schema's own numeric type.
        const zero = typeof mo === bigintTag ? "0n" : "0";
        cond = (inputVar) => `${inputVar}%${lit(mo)}===${zero}`;
      } else {
        const embedded = B_embed(input, multipleOfValidator(mo as number));
        cond = (inputVar) => `${embedded}(${inputVar})`;
      }
      checks.push({ c: cond, f: B_failWithErrorMessage("multipleOf") });
    }
  }
  return checks;
};

// A divisor and a range can exclude each other while neither is empty alone:
// no multiple of 10 lies in `0 < number < 5`. The smallest multiple at or
// above the low bound decides it — past the high bound and the schema has no
// inhabitants, which is the caller bug `conflict` already reports for a pair
// of bounds. `%` is the only arithmetic here so number and bigint share it;
// `Math.trunc` would not.
//
// An open side is never empty: the multiples run to ±Infinity, so one always
// falls past whichever single bound was written.
//
// The inclusive sides read the raw fields, not the bits: a format's range
// (int32, port) has no bit but is enforced all the same, and the emptiness
// it causes is just as real — the same fields assertLower measures a written
// bound against. Only the exclusive sides are bit-gated, since no format
// carries one.
const rangeExcludes = (schema: Internal, d: number | bigint): boolean => {
  const written = schema.bounds ?? 0;
  const exMin = written & 4 ? (schema.exclusiveMinimum as number) : U;
  const low = exMin !== U ? exMin : (schema.minimum as number | undefined);
  const exMax = written & 8 ? (schema.exclusiveMaximum as number) : U;
  const high = exMax !== U ? exMax : (schema.maximum as number | undefined);
  if (low === U || high === U) return false;
  const divisor = d as number;
  // `low - (low % divisor)` is the multiple at or beyond `low` toward zero, so
  // it lands below `low` only on the positive side — one bump either way.
  let first = ((low - (low % divisor)) as number);
  if (first < low || (exMin !== U && first === low)) first = (first + divisor) as number;
  return exMax !== U ? first >= high : first > high;
};

// One bound or divisor rendered without the other half, for the two sides of
// the message. Both inherit `expression` through the spread, and it renders
// whatever fields the copy carries.
const asDivisorOnly = (schema: Internal, divisor: number | bigint): Internal => {
  const mut = { ...schema, bounds: U, multipleOf: divisor } as unknown as Internal;
  setBoundExpression(mut, schema);
  return mut;
};

const asRangeOnly = (schema: Internal): Internal => {
  // A format-carried bound has no bit; grant the copy bits for whichever raw
  // fields exist so the conflict renders the range the check actually used —
  // `1 <= int32 <= 2147483647`, not a bare `int32` hiding half the story.
  const bits =
    ((schema.bounds ?? 0) & 12) |
    (schema.minimum !== U ? 1 : 0) |
    (schema.maximum !== U ? 2 : 0);
  const mut = { ...schema, multipleOf: U, bounds: bits } as unknown as Internal;
  setBoundExpression(mut, schema);
  return mut;
};

// The refiner is installed once, by whichever bound or divisor lands first;
// every later one only mutates the fields it reads. The emptiness check runs
// on the assembled result, so it covers a divisor landing on a range and a
// range landing on a divisor with one call site rather than five.
const updateBounds = (schema: Internal, update: (mut: Internal) => void): Internal => {
  const next: Internal =
    schema.bounds !== U || schema.multipleOf !== U
      ? updateOutput(schema, update)
      : internalRefine(schema, (mut: Internal) => {
          update(mut);
          return boundsRefiner;
        });
  const divisor = next.multipleOf;
  if (divisor !== U && exactDivisor(divisor) && rangeExcludes(next, divisor)) {
    conflict(asDivisorOnly(next, divisor), asRangeOnly(next));
  }
  return next;
};

// A message on a call that doesn't narrow used to vanish silently; it now
// carries onto the check that survived — the one that actually fires for the
// violations the caller described. `key` is U when nothing survives to carry
// it (a format's own range is enforced by the decoder, not a keyed check).
const carryMessage = (
  schema: Internal,
  key: string | undefined,
  maybeMessage: string | undefined
): Internal =>
  maybeMessage === U || key === U
    ? schema
    : updateOutput(schema, (mut: Internal) => {
        (getMutErrorMessage(mut) as Record<string, string>)[key] = maybeMessage;
      });

// A message tracks the bound value it was written with: a narrowing
// replacement without its own message clears the stale text, or the surviving
// check would report a bound the caller never described. `replaced` is the
// opposite form's key — `S.gte` clears `exclusiveMinimum` and vice versa —
// since the field it cleared can no longer produce the check that message
// belongs to, leaving it behind as a key nothing reads.
const setBoundMessage = (
  mut: Internal,
  schema: Internal,
  key: string,
  maybeMessage: string | undefined,
  replaced?: string
): void => {
  const existing = schema.errorMessage as Record<string, string | undefined> | undefined;
  if (maybeMessage !== U) {
    (getMutErrorMessage(mut) as Record<string, string>)[key] = maybeMessage;
  } else if (existing !== U && existing[key] !== U) {
    (getMutErrorMessage(mut) as Record<string, string | undefined>)[key] = U;
  }
  if (replaced !== U && existing !== U && existing[replaced] !== U) {
    (getMutErrorMessage(mut) as Record<string, string | undefined>)[replaced] = U;
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
  if (!narrowsLower(schema, minValue, false)) {
    const written = schema.bounds ?? 0;
    return carryMessage(schema, written & 4 ? "exclusiveMinimum" : written & 1 ? "minimum" : U, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~4) | 1;
    mut.minimum = minValue;
    mut.exclusiveMinimum = U;
    setBoundMessage(mut, schema, "minimum", maybeMessage, "exclusiveMinimum");
  });
}

// @__NO_SIDE_EFFECTS__
export const lte = (schema: Internal, maxValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("lte", schema, maxValue);
  assertUpper(schema, maxValue, false);
  if (!narrowsUpper(schema, maxValue, false)) {
    const written = schema.bounds ?? 0;
    return carryMessage(schema, written & 8 ? "exclusiveMaximum" : written & 2 ? "maximum" : U, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~8) | 2;
    mut.maximum = maxValue;
    mut.exclusiveMaximum = U;
    setBoundMessage(mut, schema, "maximum", maybeMessage, "exclusiveMaximum");
  });
}

// @__NO_SIDE_EFFECTS__
export const gt = (schema: Internal, minValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("gt", schema, minValue);
  assertLower(schema, minValue, true);
  if (!narrowsLower(schema, minValue, true)) {
    const written = schema.bounds ?? 0;
    return carryMessage(schema, written & 4 ? "exclusiveMinimum" : written & 1 ? "minimum" : U, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~1) | 4;
    mut.exclusiveMinimum = minValue;
    mut.minimum = U;
    setBoundMessage(mut, schema, "exclusiveMinimum", maybeMessage, "minimum");
  });
}

// @__NO_SIDE_EFFECTS__
export const lt = (schema: Internal, maxValue: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("lt", schema, maxValue);
  assertUpper(schema, maxValue, true);
  if (!narrowsUpper(schema, maxValue, true)) {
    const written = schema.bounds ?? 0;
    return carryMessage(schema, written & 8 ? "exclusiveMaximum" : written & 2 ? "maximum" : U, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = ((schema.bounds ?? 0) & ~2) | 8;
    mut.exclusiveMaximum = maxValue;
    mut.maximum = U;
    setBoundMessage(mut, schema, "exclusiveMaximum", maybeMessage, "maximum");
  });
}

// @__NO_SIDE_EFFECTS__
export const multipleOf = (schema: Internal, value: number | bigint, maybeMessage?: string): Internal => {
  assertNumericBound("multipleOf", schema, value);
  // JSON Schema requires a strictly positive divisor, and `x % Infinity`
  // (=== x) would compile to a check that rejects everything but 0.
  if ((value as number) <= 0 || (value as number) === Infinity) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects("multipleOf", "a positive finite divisor", stringify(value)),
    });
  }
  const bound = value as number;
  // assertNumericBound pinned `value` to the schema's own numeric type, and a
  // stored divisor went through the same gate, so the arithmetic below never
  // mixes number with bigint despite the casts saying `number`.
  // A remainder is checked by truthiness, not `=== 0`: a bigint remainder is
  // `0n`, which `=== 0` never matches.
  const existing = schema.multipleOf as number | undefined;
  if (existing !== U && !(existing % bound)) return carryMessage(schema, "multipleOf", maybeMessage);
  let divisor: number | bigint = bound;
  if (existing !== U && bound % existing) {
    // Neither divisor implies the other: together they admit exactly the
    // multiples of the LCM, which is what gets stored and checked so the
    // schema never advertises a divisor weaker than what it validates.
    const refuse = (): never =>
      panic(`multipleOf ${stringify(bound)} cannot be combined with multipleOf ${stringify(existing)}`);
    // No finite float LCM exists for fractional divisors.
    if (typeof value === numberTag && !(Number.isInteger(bound) && Number.isInteger(existing))) {
      refuse();
    }
    let a = bound;
    let b = existing;
    while (b) {
      const r = a % b;
      a = b;
      b = r;
    }
    divisor = (bound / a) * existing;
    // An LCM past 2^53 rounds, and an inexact divisor validates the wrong
    // set — refuse rather than silently drift.
    if (typeof divisor === numberTag && !Number.isSafeInteger(divisor)) {
      refuse();
    }
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.multipleOf = divisor;
    setBoundMessage(mut, schema, "multipleOf", maybeMessage);
  });
}

// @__NO_SIDE_EFFECTS__
export const minLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertLengthBound("minLength", schema, length);
  assertSize(schema, length, false);
  const key = sizeKey(schema, false);
  if (!narrowsSize(schema[key], length, false)) {
    return carryMessage(schema, (schema.bounds ?? 0) & 1 ? key : U, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) | 1;
    mut[key] = length;
    setBoundMessage(mut, schema, key, maybeMessage);
  });
}

// @__NO_SIDE_EFFECTS__
export const maxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertLengthBound("maxLength", schema, length);
  assertSize(schema, length, true);
  const key = sizeKey(schema, true);
  if (!narrowsSize(schema[key], length, true)) {
    return carryMessage(schema, (schema.bounds ?? 0) & 2 ? key : U, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) | 2;
    mut[key] = length;
    setBoundMessage(mut, schema, key, maybeMessage);
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
  // non-narrowing bound is for the others. The `===` check reports under
  // minKey, so that's where a message carries.
  if (schema[minKey] === length && schema[maxKey] === length) {
    return carryMessage(schema, minKey, maybeMessage);
  }
  return updateBounds(schema, (mut: Internal) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) | 3;
    mut[minKey] = length;
    mut[maxKey] = length;
    setBoundMessage(mut, schema, minKey, maybeMessage);
    setBoundMessage(mut, schema, maxKey, maybeMessage);
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
export const nullable = (schema: Internal): Internal => {
  return unionFactory([schema, unit, nullLiteral]);
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
