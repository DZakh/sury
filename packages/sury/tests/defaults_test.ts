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
    "[Sury] S.union can't keep null: an earlier member decodes it to boolean. Drop null from the later member, or from both and wrap the union: S.nullable(S.union([...]), default)",
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
    "[Sury] S.union can't keep undefined: an earlier member decodes it to string. Drop undefined from the later member, or from both and wrap the union: S.optional(S.union([...]), default)",
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

test("a union reads an env member's unset var the way the env does", () => {
  expect(S.parseOrThrow(null, S.union([S.env.with(S.to, S.nullable(S.string, "d")), S.schema(null)]))).toBe(null);
  expect(() => S.union([S.env.with(S.to, S.nullable(S.string, "d")), S.schema(undefined)])).toThrow(
    "an earlier member decodes it to string",
  );
  expect(() => S.union([S.env.with(S.to, S.nullable(S.string)), S.schema(undefined)])).toThrow(
    "can't keep undefined",
  );
});

test("a union keeps undefined an env member hands back through its null arm", () => {
  const nullableToOptional = S.nullable(S.string).with(S.to, S.optional(S.string));
  expect(S.parseOrThrow(undefined, S.union([S.env.with(S.to, nullableToOptional), S.optional(S.number)]))).toBe(undefined);
  expect(S.parseOrThrow(undefined, S.union([S.env.with(S.to, (S as any).$nullAsOption(S.string)), S.optional(S.number)]))).toBe(
    undefined,
  );
  expect(() => S.union([S.optional(S.number, 3), S.env])).toThrow("can't keep undefined");
});

test("a union refuses undefined a default replaces behind a .to or a ref", () => {
  expect(() => S.union([S.optional(S.string, "0").with(S.to, S.number), S.optional(S.boolean)])).toThrow(
    "an earlier member decodes it to number",
  );
  expect(() => S.union([S.recursive("R", () => S.optional(S.string, "r")), S.optional(S.boolean)])).toThrow(
    "can't keep undefined",
  );
  // Past a `.to` the arms' answer is not the member's: accepted, not guessed.
  expect(S.parseOrThrow(undefined, S.union([S.optional(S.string).with(S.to, S.nullable(S.string)), S.optional(S.boolean)]))).toBe(
    null,
  );
});

test("the rewrites the refusal suggests are accepted", () => {
  expect(S.parseOrThrow(undefined, S.optional(S.union([S.string, S.number]), "none"))).toBe("none");
  expect(S.parseOrThrow(null, S.union([S.nullable(S.boolean, false), S.string]))).toBe(false);
  expect(S.parseOrThrow(null, S.union([null, S.nullable(S.boolean, false)]))).toBe(null);
});
