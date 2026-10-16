import { expect, test } from "vitest";

import * as S from "../index.mjs";

// A spec can't print a promise held inside its output, so this one lives here.
const exclaim = S.string.with(S.to, S.string, {
  decode: { async: (value: string) => Promise.resolve(value + "!") },
  encode: (value: string) => value.slice(0, -1),
});
const schema = S.object((s) => ({ a: s.fieldOr("a", exclaim, "d") }));

test("s.fieldOr over an async item fills its default for an absent key", async () => {
  expect(await S.parseAsPromiseOrReject(schema)({})).toEqual({ a: "d" });
});

test("s.fieldOr over an async item leaves a present value's promise unresolved", async () => {
  // FIXME: s.fieldOr's parser reads a present item from unknown and never awaits
  // it, so the output holds the item's promise instead of "x!".
  const output = (await S.parseAsPromiseOrReject(schema)({ a: "x" })) as { a: unknown };
  expect(output.a).toBeInstanceOf(Promise);
  expect(await output.a).toBe("x!");
});
