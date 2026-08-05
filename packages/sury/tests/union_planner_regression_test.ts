import { test } from "vitest";

import * as S from "../index.mjs";

test("union semantic discriminators use identity and SameValueZero", (t) => {
  const firstSymbol = Symbol("same-description");
  const secondSymbol = Symbol("same-description");

  const symbolSchema = S.union([
    S.schema({
      kind: S.schema(firstSymbol),
      value: S.string,
    }).with(S.refine, () => false, { error: "first symbol member rejected" }),
    S.schema({
      kind: S.schema(secondSymbol),
      value: S.string,
    }),
    S.schema({
      kind: S.schema(firstSymbol),
      value: S.string,
    }),
  ]);
  const parseSymbol = S.parser(symbolSchema);

  const firstValue = { kind: firstSymbol, value: "first" };
  const secondValue = { kind: secondSymbol, value: "second" };
  t.expect(parseSymbol(firstValue)).toEqual(firstValue);
  t.expect(parseSymbol(secondValue)).toEqual(secondValue);
  t.expect(() =>
    parseSymbol({ kind: Symbol("same-description"), value: "other" }),
  ).toThrow(S.Error);

  const parseNaN = S.parser(
    S.union([
      S.schema(NaN).with(S.refine, () => false, {
        error: "first NaN member rejected",
      }),
      S.schema(NaN),
    ]),
  );
  t.expect(parseNaN(NaN)).toBeNaN();

  const parseZero = S.parser(
    S.union([
      S.schema(-0).with(S.to, S.string, () => "first-zero-member"),
      S.schema(0).with(S.to, S.string, () => "second-zero-member"),
    ]),
  );
  t.expect(parseZero(-0)).toBe("first-zero-member");
  t.expect(parseZero(0)).toBe("first-zero-member");

  const parseNullish = S.parser(
    S.union([
      S.schema(null).with(S.refine, () => false, {
        error: "first null member rejected",
      }),
      null,
      undefined,
    ]),
  );
  t.expect(parseNullish(null)).toBeNull();
  t.expect(parseNullish(undefined)).toBeUndefined();

  for (const discriminator of [NaN, -0, 0, null, undefined]) {
    const value = { kind: discriminator, value: "same-value-zero" };
    const parseField = S.parser(
      S.union([
        S.schema({
          kind: S.schema(discriminator),
          value: S.string,
        }).with(S.refine, () => false, {
          error: "first field discriminator rejected",
        }),
        S.schema({
          kind: S.schema(discriminator),
          value: S.string,
        }),
      ]),
    );
    t.expect(parseField(value)).toEqual(value);
  }
});

test("NaN-to-number reachability follows the global validation mode", (t) => {
  const schema = S.union([
    S.schema(NaN).with(S.refine, () => false, {
      error: "NaN member rejected",
    }),
    S.number,
  ]);

  const strict = S.decoder(S.schema(NaN), schema);
  t.expect(() => strict(NaN)).toThrow(/NaN member rejected/);

  S.global({ disableNanNumberValidation: true });
  try {
    t.expect(S.parser(schema)(NaN)).toBeNaN();
    t.expect(S.decoder(S.schema(NaN), schema)(NaN)).toBeNaN();
  } finally {
    S.global({});
  }
});

test("factory normalization preserves duplicate effects and nested metadata", (t) => {
  let calls = 0;
  const repeated = S.schema({
    value: S.string.with(S.refine, () => ++calls > 1),
  });
  t.expect(
    S.parser(S.union([repeated, repeated]))({ value: "fallback" }),
  ).toEqual({ value: "fallback" });
  t.expect(calls).toBe(2);

  const inner = S.meta(
    S.union([S.string.with(S.minLength, 3), S.number]),
    {
      name: "named-inner",
      errorMessage: { _: "named inner rejected" },
    },
  );
  t.expect(S.inputExpression(S.union([inner, S.boolean]))).toBe(
    "named-inner | boolean",
  );
});

test("custom decoder foreign errors escape while Sury errors fall through", (t) => {
  const foreign = new RangeError("custom decoder failed");
  let foreignFallbackCalls = 0;
  const foreignSchema = S.union([
    S.to(
      S.string,
      S.string,
      () => {
        throw foreign;
      },
      (value) => value,
    ),
    S.string.with(S.refine, () => {
      foreignFallbackCalls++;
      return true;
    }),
  ]);
  try {
    S.parser(foreignSchema)("value");
    t.expect.fail("the foreign exception should escape");
  } catch (error) {
    t.expect(error).toBe(foreign);
  }
  t.expect(foreignFallbackCalls).toBe(0);

  let nestedFallbackCalls = 0;
  const nestedForeignSchema = S.union([
    S.schema({
      value: S.string.with(S.to, S.string, () => {
        throw foreign;
      }),
    }),
    S.schema({}).with(S.refine, () => {
      nestedFallbackCalls++;
      return true;
    }),
  ]);
  try {
    S.parser(nestedForeignSchema)({ value: "nested" });
    t.expect.fail("the nested foreign exception should escape");
  } catch (error) {
    t.expect(error).toBe(foreign);
  }
  t.expect(nestedFallbackCalls).toBe(0);

  let suryFallbackCalls = 0;
  const surySchema = S.union([
    S.to(
      S.string,
      S.string,
      () => {
        S.parser(S.number)("not a number");
        return "";
      },
      (value) => value,
    ),
    S.string.with(S.refine, () => {
      suryFallbackCalls++;
      return true;
    }),
  ]);
  t.expect(S.parser(surySchema)("value")).toBe("value");
  t.expect(suryFallbackCalls).toBe(1);
});

test("built-in JSON validation failures remain eligible for fallback", (t) => {
  let fallbackCalls = 0;
  const parse = S.parser(
    S.union([
      S.schema({ value: S.jsonString }),
      S.schema({}).with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
    ]),
  );

  t.expect(parse({ value: "not-json" })).toEqual({});
  t.expect(fallbackCalls).toBe(1);

  fallbackCalls = 0;
  t.expect(parse({ value: "true" })).toEqual({ value: "true" });
  t.expect(fallbackCalls).toBe(0);
});

test("foreign exceptions escape a union without trying a fallback", (t) => {
  const foreignError = new RangeError("foreign refinement failure");
  let fallbackCalls = 0;
  const schema = S.union([
    S.string.with(S.refine, () => {
      throw foreignError;
    }),
    S.string.with(S.refine, () => {
      fallbackCalls++;
      return true;
    }),
  ]);

  try {
    S.parser(schema)("value");
    t.expect.fail("the foreign exception should escape");
  } catch (error) {
    t.expect(error).toBe(foreignError);
  }
  t.expect(fallbackCalls).toBe(0);

  const getterError = new TypeError("foreign property access failure");
  const throwingObject = Object.defineProperty({}, "value", {
    get() {
      throw getterError;
    },
  });
  const objectFallback = S.parser(
    S.union([S.schema({ value: S.string }), S.schema({})]),
  );
  try {
    objectFallback(throwingObject);
    t.expect.fail("the foreign property exception should escape");
  } catch (error) {
    t.expect(error).toBe(getterError);
  }
});

test("a matched Sury failure runs before a later literal fallback", (t) => {
  let firstCalls = 0;
  const schema = S.union([
    S.string.with(
      S.refine,
      () => {
        firstCalls++;
        return false;
      },
      { error: "first member rejected" },
    ),
    S.schema("fallback"),
  ]);
  const parse = S.parser(schema);

  t.expect(parse("fallback")).toBe("fallback");
  t.expect(firstCalls).toBe(1);

  firstCalls = 0;
  const decodeExact = S.decoder(S.schema("fallback"), schema);
  t.expect(decodeExact("fallback")).toBe("fallback");
  t.expect(firstCalls).toBe(1);
});

test("an accepted first match does not evaluate a same-tier fallback", (t) => {
  const calls: string[] = [];
  const parse = S.parser(
    S.union([
      S.string.with(S.refine, () => {
        calls.push("first");
        return true;
      }),
      S.string.with(S.refine, () => {
        calls.push("second");
        return true;
      }),
    ]),
  );

  t.expect(parse("value")).toBe("value");
  t.expect(calls).toEqual(["first"]);
});

test("reachable rejection and unreachable conversion stay distinct", (t) => {
  const reachableRejection = S.parser(
    S.union([S.string.with(S.to, S.never), S.schema("fallback")]),
  );
  t.expect(reachableRejection("fallback")).toBe("fallback");
  t.expect(() => reachableRejection("other")).toThrow(S.Error);

  const chainedTerminalRejection = S.string
    .with(S.to, S.never)
    .with(S.to, S.string);
  const chainedReachableRejection = S.parser(
    S.union([chainedTerminalRejection, S.schema("fallback")]),
  );
  t.expect(chainedReachableRejection("fallback")).toBe("fallback");
  t.expect(() => chainedReachableRejection("other")).toThrow(S.Error);

  const unreachableConversion = S.parser(
    S.union([S.never.with(S.to, S.string), S.number]),
  );
  t.expect(unreachableConversion(42)).toBe(42);
  t.expect(() => unreachableConversion("not-a-bridge")).toThrow(S.Error);

  const uncoveredTarget = S.union([
    S.never.with(S.to, S.string),
    S.number,
  ]).with(S.to, S.union([S.string, S.number]));
  t.expect(() => S.parser(uncoveredTarget)).toThrow(
    /string has no same-type variant on the other side/,
  );

  const chainedUncoveredTarget = S.union([
    chainedTerminalRejection,
    S.number,
  ]).with(S.to, S.union([S.string, S.number]));
  t.expect(() => S.parser(chainedUncoveredTarget)).toThrow(
    /string has no same-type variant on the other side/,
  );
});

test("instance specificity stays ahead of an earlier generic object transform", (t) => {
  class SpecificInstance {}

  const parse = S.parser(
    S.union([
      S.schema({}).with(S.to, S.string, () => "generic-object"),
      S.instance(SpecificInstance).with(
        S.to,
        S.string,
        () => "specific-instance",
      ),
    ]),
  );

  t.expect(parse(new SpecificInstance())).toBe("specific-instance");
  t.expect(parse({})).toBe("generic-object");
});

test("nested and recursive union deoptimization terminates and falls through", (t) => {
  type RecursiveValue = string | RecursiveValue[];

  const recursive = S.recursive<RecursiveValue, RecursiveValue>(
    "PlannerRecursiveValue",
    (self) => S.union([S.array(self), S.string]),
  );
  const guardedRecursive = recursive.with(
    S.refine,
    (value) => value !== "fallback",
    { error: "guarded recursive member rejected" },
  );
  const parse = S.parser(
    S.union([guardedRecursive, S.union([S.string, S.number])]),
  );

  t.expect(parse([["deep"], "value"])).toEqual([["deep"], "value"]);
  t.expect(parse("fallback")).toBe("fallback");
  t.expect(parse(42)).toBe(42);
  t.expect(() => parse({ value: "invalid" })).toThrow(S.Error);
});

test("recursive transform exceptions keep their identity across compile order", (t) => {
  type RecursiveValue = string | RecursiveValue[];

  const verify = (name: string, unionFirst: boolean) => {
    const foreign = new RangeError(`${name} foreign transform`);
    let fallbackCalls = 0;
    const recursive = S.recursive<RecursiveValue, RecursiveValue>(
      name,
      (self) =>
        S.union([
          S.array(self),
          S.string.with(S.to, S.string, (value) => {
            if (value === "explode") throw foreign;
            return value;
          }),
        ]),
    );
    const outer = S.union([
      recursive,
      S.string.with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
    ]);
    const parseUnion = unionFirst ? S.parser(outer) : undefined;
    const parseStandalone = S.parser(recursive);
    const parse = parseUnion || S.parser(outer);

    t.expect(parseStandalone("safe")).toBe("safe");
    try {
      parse("explode");
      t.expect.fail("the recursive foreign exception should escape");
    } catch (error) {
      t.expect(error).toBe(foreign);
    }
    t.expect(fallbackCalls).toBe(0);
  };

  verify("PlannerRecursiveStandaloneFirst", false);
  verify("PlannerRecursiveUnionFirst", true);
});

test("a non-transparent nested union falls through but foreign errors escape", (t) => {
  let rejectionCalls = 0;
  const rejectingInner = S.union([S.string, S.number]).with(
    S.refine,
    () => {
      rejectionCalls++;
      return false;
    },
    { error: "nested union rejected" },
  );
  const parse = S.parser(S.union([rejectingInner, S.string]));

  t.expect(parse("fallback")).toBe("fallback");
  t.expect(rejectionCalls).toBe(1);

  const foreign = new RangeError("nested union foreign error");
  let fallbackCalls = 0;
  const throwingInner = S.union([S.string, S.number]).with(S.refine, () => {
    throw foreign;
  });
  const parseForeign = S.parser(
    S.union([
      throwingInner,
      S.string.with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
    ]),
  );

  try {
    parseForeign("value");
    t.expect.fail("the nested foreign exception should escape");
  } catch (error) {
    t.expect(error).toBe(foreign);
  }
  t.expect(fallbackCalls).toBe(0);
});

test("function schemas remain explicit deoptimization boundaries", (t) => {
  const fn = () => 1;
  let calls = 0;
  const parse = S.parser(
    S.union([
      S.schema(fn).with(S.refine, () => {
        calls++;
        return false;
      }),
      S.unknown.with(S.refine, () => {
        calls++;
        return true;
      }),
    ]),
  );

  t.expect(parse(fn)).toBe(fn);
  t.expect(calls).toBe(2);
});

test("large heterogeneous unions preserve each dispatch path", (t) => {
  class Box {
    constructor(readonly value: string) {}
  }

  type RecursiveValue = string | RecursiveValue[];
  const recursive = S.recursive<RecursiveValue, RecursiveValue>(
    "LargeUnionRecursiveValue",
    (self) => S.union([S.array(self), S.string]),
  );
  const schema = S.union([
    "literal-0",
    "literal-1",
    "literal-2",
    "literal-3",
    "literal-4",
    "literal-5",
    "literal-6",
    "literal-7",
    { kind: "number", value: S.number },
    { kind: "string", value: S.string },
    { kind: "boolean", value: S.boolean },
    S.instance(Box),
    recursive,
    S.boolean,
    S.bigint,
    S.symbol,
  ]);
  const parse = S.parser(schema);

  t.expect(parse("literal-6")).toBe("literal-6");
  t.expect(parse({ kind: "number", value: 6 })).toEqual({
    kind: "number",
    value: 6,
  });
  const box = new Box("boxed");
  t.expect(parse(box)).toBe(box);
  t.expect(parse([["recursive"]])).toEqual([["recursive"]]);
  t.expect(parse(true)).toBe(true);
  t.expect(parse(6n)).toBe(6n);
  const symbol = Symbol("large-union");
  t.expect(parse(symbol)).toBe(symbol);
  t.expect(() => parse({ kind: "number", value: "wrong" })).toThrow(S.Error);
});
