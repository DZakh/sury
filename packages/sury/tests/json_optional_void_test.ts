import { expect, test } from "vitest";

import * as S from "../index.mjs";

// A spec can't hold `S.optional(S.void)`: the JSON Schema it publishes is one
// no validator compiles (an empty `enum`), and the format refuses that.
const read = S.parseOrThrow(S.json.with(S.to, S.schema({ f: S.optional(S.void) })));

test("an S.optional(S.void) field read through S.json takes an absent key", () => {
  expect(read({})).toEqual({ f: undefined });
});

test("an S.optional(S.void) field read through S.json rejects a present null, as parse does", () => {
  expect(() => read({ f: null })).toThrow("Failed at f: Expected void | undefined, received null");
});
