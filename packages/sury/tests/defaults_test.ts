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

// ReScript's `S.Option.getOr` over a nested option, chained. A second default
// that its first already shadows is refused rather than read wrong.
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
  "[Sury] Can't set default for string | undefined: its default already takes undefined. Set one default";

test("getOr after a value default is refused", () => {
  expect(() => R.$Option_getOr!(R.$Option_getOr!(nested(), "x"), "y")).toThrow(refused);
});

test("getOr after a Some(None) default is refused", () => {
  expect(() =>
    R.$Option_getOr!(R.$Option_getOr!(R.$option!(nested()), { BS_PRIVATE_NESTED_SOME_NONE: 0 }), undefined),
  ).toThrow(refused);
});

test("getOr after a None default over a coder is refused", () => {
  expect(() => R.$Option_getOr!(R.$Option_getOr!(blankAsNone(), undefined), "y")).toThrow(refused);
});
