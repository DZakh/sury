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
