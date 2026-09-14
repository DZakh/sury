import { test, expect } from "vitest";
import * as S from "sury";

// Converting a definition consumes it: each field is replaced in place with
// the schema it describes, and the object itself becomes the schema's
// `properties`. Building twice from one definition still has to work - the
// second pass sees schemas where the first saw raw values.
test("one definition builds two working schemas", () => {
  const definition = { tag: "a", n: S.number };
  const first = S.array(definition);
  const second = S.record(definition);

  expect(S.parseOrThrow(first)([{ tag: "a", n: 1 }])).toEqual([{ tag: "a", n: 1 }]);
  expect(S.parseOrThrow(second)({ k: { tag: "a", n: 1 } })).toEqual({
    k: { tag: "a", n: 1 },
  });
  expect(() => S.parseOrThrow(first)([{ tag: "b", n: 1 }])).toThrow();
});
