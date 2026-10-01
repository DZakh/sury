import { expect, test } from "vitest";
import * as S from "sury";

// The throwing operations crash and the Result ones answer `{ a: undefined }`,
// and a spec example holds one answer for all of them.
test("FIXME: a shape that picks an async field declares the field after joining it", async () => {
  const schema = S.reverse(
    S.schema({
      w: S.schema({
        a: S.string.with(S.to, S.string, {
          decode: { async: async (x: string) => x + "!" },
          encode: { async: async (x: string) => x.slice(0, -1) },
        }),
      }).with(S.shape, (v) => ({ A: v.a })),
    }),
  );
  await expect(S.decodeAsPromiseOrReject({ w: { A: "a!" } }, schema)).rejects.toThrow(
    "Cannot access 'v2' before initialization",
  );
  expect(await S.decodeAsResultPromise({ w: { A: "a!" } }, schema)).toEqual({
    success: true,
    value: { w: { a: undefined } },
  });
});
