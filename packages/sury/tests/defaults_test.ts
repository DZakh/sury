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

const R = S as unknown as Record<string, (...args: unknown[]) => S.Schema<unknown>>;
const nested = () => R.$option!(R.$option!(S.string));
const blankAsNone = () =>
  R.$option!(
    S.string.with(S.to, R.$option!(S.string), {
      decode: (v: string) => (v === "" ? undefined : v),
      encode: (v: unknown) => (v === undefined ? "" : v),
    } as never),
  );
const refused =
  "[Sury] Can't set default for string | undefined: an earlier default already takes undefined. Set one default";

test("getOr after a Some(None) default is refused", () => {
  expect(() =>
    R.$Option_getOr!(R.$Option_getOr!(R.$option!(nested()), { BS_PRIVATE_NESTED_SOME_NONE: 0 }), undefined),
  ).toThrow(refused);
});

test("getOr after a None default over a coder is refused", () => {
  expect(() => R.$Option_getOr!(R.$Option_getOr!(blankAsNone(), undefined), "y")).toThrow(refused);
});

test("getOr after a lazy None default over a coder is refused", () => {
  expect(() => R.$Option_getOr!(R.$Option_getOrWith!(blankAsNone(), () => undefined), "y")).toThrow(refused);
});

test("getOr after a lazy Some(None) default is refused", () => {
  expect(() =>
    R.$Option_getOr!(
      R.$Option_getOrWith!(R.$option!(nested()), () => ({ BS_PRIVATE_NESTED_SOME_NONE: 0 })),
      "y",
    ),
  ).toThrow(refused);
});

test("getOr after a value default is refused, since it never applies", () => {
  expect(() => R.$Option_getOr!(R.$Option_getOr!(nested(), "x"), "y")).toThrow(refused);
});

// Beside a recursive ref still being defined, the default is checked against
// the arms already built. A union with its own refinement can't be split into
// them without dropping that check, and a ref under two described unions is
// still the definition being built.
const unbuilt = "the default is read as N, which would need a default of its own";

test("a recursive union's own refinement refuses a default beside its ref", () => {
  expect(() =>
    S.recursive("N", (self) =>
      S.schema({
        v: S.number,
        kid: S.optional(
          S.union([self, S.string]).with(S.refine, (x) => x !== "bad", { error: "no bad" }),
          "bad",
        ),
      }),
    ),
  ).toThrow(unbuilt);
});

test("a recursive ref under two described unions is found for the default check", () => {
  const kid = (self: S.Schema<unknown>) =>
    S.union([S.union([self, S.literal(null)]).with(S.meta, { description: "inner" }), S.string]).with(S.meta, {
      description: "outer",
    });
  expect(() =>
    S.recursive("N", (self) => S.schema({ v: S.number, kid: S.optional(kid(self), 5 as never) })),
  ).toThrow(unbuilt);
  expect(() =>
    S.recursive("N", (self) => S.schema({ v: S.number, kid: S.optional(kid(self), () => "x") })),
  ).toThrow(unbuilt);
  expect(
    S.parseOrThrow(
      { v: 1 },
      S.recursive("N", (self) => S.schema({ v: S.number, kid: S.optional(kid(self), "x") })),
    ),
  ).toEqual({ v: 1, kid: "x" });
});
