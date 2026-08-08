import { test, expect } from "vitest";
import * as S from "sury";

// Converting a definition consumes it: each field is replaced in place with
// the schema it describes, and the object itself becomes the schema's
// `properties`. Building twice from one definition still has to work — the
// second pass sees schemas where the first saw raw values.
test("one definition builds two working schemas", () => {
  const definition = { tag: "a", n: S.number };
  const first = S.array(definition);
  const second = S.record(definition);

  expect(S.parser(first)([{ tag: "a", n: 1 }])).toEqual([{ tag: "a", n: 1 }]);
  expect(S.parser(second)({ k: { tag: "a", n: 1 } })).toEqual({
    k: { tag: "a", n: 1 },
  });
  expect(() => S.parser(first)([{ tag: "b", n: 1 }])).toThrow();
});

// `undefined` is a forgotten argument far more often than a request for the
// undefined literal, so the containers reject it and name the spelling that
// does mean the literal.
test("a missing argument names the fix", () => {
  const cases: [string, () => unknown][] = [
    ["array", () => S.array(undefined as never)],
    ["record", () => S.record(undefined as never)],
    ["optional", () => S.optional(undefined as never)],
    ["nullable", () => S.nullable(undefined as never)],
    ["nullish", () => S.nullish(undefined as never)],
    ["list", () => (S as never as Record<string, () => unknown>)["list"]!()],
  ];

  for (const [name, build] of cases) {
    expect(build, `S.${name} accepted undefined`).toThrow(
      "[Sury] Ambiguous undefined. Fix the schema or use S.schema(undefined)",
    );
  }

  expect(S.parser(S.schema(undefined))(undefined)).toBe(undefined);
});
