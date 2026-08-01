import { test, expect } from "vitest";
import * as S from "sury";

// Bound values are interpolated straight into the source of a compiled
// operation rather than embedded as `e[n]`, so the asserts in front of them
// are the only thing between a caller-supplied value and arbitrary code in
// that function. These are the tests for that boundary.

// Anything whose `String()` is not a bare numeric literal. A schema-building
// API is not usually reachable by an attacker, but a config-driven one can be
// — `S.string.with(S.minLength, cfg.min)` with `cfg` from a file or a request.
const HOSTILE: [string, unknown][] = [
  ["statement injection", "0;globalThis.__SURY_PWNED=true;//"],
  ["template interpolation", "${globalThis.__SURY_PWNED=true}"],
  ["backtick break", "0`+`"],
  ["comment break", "0*/1/*"],
  ["toString coercion", { toString: () => "0;globalThis.__SURY_PWNED=true;//" }],
  ["valueOf coercion", { valueOf: () => 0, toString: () => "0;globalThis.__SURY_PWNED=true;//" }],
  ["boxed number", new Number(5)],
  ["numeric string", "5"],
  ["array", [5]],
  ["null", null],
  ["undefined", undefined],
  ["boolean", true],
  ["object", {}],
];

const NUMERIC = ["gt", "gte", "lt", "lte"] as const;
const SIZED = ["minLength", "maxLength", "length"] as const;

test("a bound rejects any value it could not safely inline", () => {
  (globalThis as Record<string, unknown>).__SURY_PWNED = false;

  for (const [label, value] of HOSTILE) {
    for (const fn of NUMERIC) {
      expect(
        () => (S as never as Record<string, (...a: unknown[]) => unknown>)[fn]!(S.number, value),
        `S.${fn} accepted ${label}`,
      ).toThrow(/expects number, got/);
    }
    for (const fn of SIZED) {
      expect(
        () => (S as never as Record<string, (...a: unknown[]) => unknown>)[fn]!(S.string, value),
        `S.${fn} accepted ${label}`,
      ).toThrow(/expects integer >= 0/);
    }
  }

  // Nothing above reached a compiled function body.
  expect((globalThis as Record<string, unknown>).__SURY_PWNED).toBe(false);
});

test("a numeric bound rejects the wrong numeric type", () => {
  // Mixing the two silently compares across types in JS, so each schema takes
  // its own and nothing else.
  expect(() => S.gte(S.number, 5n as never)).toThrow(`S.gte expects number, got 5n`);
  expect(() => S.gte(S.bigint, 5 as never)).toThrow(`S.gte expects bigint, got 5`);
  expect(() => S.gt(S.number, NaN)).toThrow(`S.gt expects number, got NaN`);
});

test("a length rejects values that are not counts", () => {
  // Each of these compiles to a check nothing can satisfy — `i.length>Infinity`,
  // `i.length===-1` — so they fail where they are written instead.
  for (const value of [Infinity, -Infinity, -1, 1.5, -0.5, NaN, 2 ** 53]) {
    expect(() => S.minLength(S.string, value), `minLength(${value})`).toThrow(
      /expects integer >= 0/,
    );
    expect(() => S.maxLength(S.array(S.string), value), `maxLength(${value})`).toThrow(
      /expects integer >= 0/,
    );
  }
  expect(S.toExpression(S.string.with(S.minLength, 0))).toBe("string.length >= 0");
});

test("values that are safe to inline still round-trip through codegen", () => {
  // The flip side: everything `String()` renders as a valid literal must work,
  // including the forms that look unusual in source.
  for (const [value, rendered] of [
    [5.5, "5.5"],
    [-0, "0"],
    [1e21, "1e+21"],
    [Infinity, "Infinity"],
    [-Infinity, "-Infinity"],
    [Number.MAX_VALUE, "1.7976931348623157e+308"],
  ] as [number, string][]) {
    expect(S.parser(S.number.with(S.gte, value)).toString()).toContain(`i>=${rendered}`);
  }
  expect(S.parser(S.bigint.with(S.gte, 2n ** 64n)).toString()).toContain(
    "i>=18446744073709551616n",
  );
  expect(S.parser(S.bigint.with(S.lte, -5n)).toString()).toContain("i<=-5n");
});
