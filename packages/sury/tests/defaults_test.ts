import { expect, test } from "vitest";
import * as S from "../index.mjs";

// A default is checked against the Output type, refinements included. A spec
// can't hold these: the schema panics while it is built, so there is nothing
// for its other dimensions to describe.

const refinedUnion = S.union([S.string, S.number.with(S.to, S.string)]).with(
  S.refine,
  (value) => value !== "",
  { error: "empty" },
);

test("a union's own refinement rejects an S.optional default", () => {
  expect(() => S.optional(refinedUnion, "")).toThrow(
    "[Sury] Invalid default for string | number | undefined: empty",
  );
});

test("a union's own refinement rejects a fieldOr default", () => {
  expect(() => S.object((s) => ({ f: s.fieldOr("f", refinedUnion, "") }))).toThrow(
    "[Sury] Invalid default for string | number | undefined: empty",
  );
});

test("a default the refinement accepts is kept", () => {
  expect(S.parseOrThrow({}, S.object((s) => ({ f: s.fieldOr("f", refinedUnion, "x") })))).toEqual({
    f: "x",
  });
});

test("a union refuses null that an earlier member's default replaces", () => {
  expect(() => S.union([S.nullable(S.boolean, false), null])).toThrow(
    "[Sury] S.union can't keep null: an earlier member decodes it to boolean. Drop null from the later member, or wrap the union: S.nullable(S.union([...]), default)",
  );
  expect(() => S.union([S.nullable(S.boolean, () => false), S.nullable(S.string)])).toThrow(
    "an earlier member decodes it to boolean",
  );
  expect(() => S.union([S.nullable(S.boolean, false), S.meta(S.nullable(S.string), { description: "d" })])).toThrow(
    "an earlier member decodes it to boolean",
  );
});

test("a union refuses undefined that an earlier member's default replaces", () => {
  expect(() => S.union([S.optional(S.string, "none"), S.optional(S.number)])).toThrow(
    "[Sury] S.union can't keep undefined: an earlier member decodes it to string. Drop undefined from the later member, or wrap the union: S.optional(S.union([...]), default)",
  );
});

test("a union refuses undefined an earlier env link reads as its default", () => {
  expect(() => S.union([S.env.with(S.to, S.optional(S.string, "dev")), S.optional(S.url)])).toThrow(
    "an earlier member decodes it to string",
  );
  expect(S.parseOrThrow(undefined, S.union([S.env.with(S.to, S.port), S.optional(S.number)]))).toBe(undefined);
});

test("a union keeps an empty value an earlier member already keeps", () => {
  expect(S.parseOrThrow(null, S.union([S.nullable(S.nullable(S.boolean, false)), S.nullable(S.string)]))).toBe(null);
  expect(S.parseOrThrow(undefined, S.union([S.optional(S.optional(S.number, 0)), S.optional(S.string)]))).toBe(
    undefined,
  );
  expect(S.parseOrThrow(null, S.union([S.nullish(S.nullable(S.number, 0)), S.nullable(S.string)]))).toBe(null);
});

test("the fixes the refusal names construct", () => {
  expect(S.parseOrThrow(undefined, S.optional(S.union([S.string, S.number]), "none"))).toBe("none");
  expect(S.parseOrThrow(null, S.union([S.nullable(S.boolean, false), S.string]))).toBe(false);
  expect(S.parseOrThrow(null, S.union([null, S.nullable(S.boolean, false)]))).toBe(null);
});
