import { test, expect } from "vitest";
import * as S from "sury";

// A definition is the caller's own object — often a config or a constant they
// keep using after building the schema. Converting it must read it, never
// write back into it, and never alias it as the schema's `properties`.

const FACTORIES: [string, (definition: unknown) => unknown][] = [
  ["schema", (d) => S.schema(d as never)],
  ["object", (d) => S.object(d as never)],
  ["array", (d) => S.array(d as never)],
  ["record", (d) => S.record(d as never)],
  ["optional", (d) => S.optional(d as never)],
  ["nullable", (d) => S.nullable(d as never)],
  ["nullish", (d) => S.nullish(d as never)],
  ["union", (d) => S.union([d as never])],
];

test("building a schema leaves the definition untouched", () => {
  for (const [name, build] of FACTORIES) {
    const definition = { id: S.string, tag: "a", count: 1, nested: { flag: true } };
    build(definition);

    expect(definition.tag, `S.${name} rewrote a literal field`).toBe("a");
    expect(definition.count, `S.${name} rewrote a number field`).toBe(1);
    expect(definition.id, `S.${name} rewrote a schema field`).toBe(S.string);
    expect(definition.nested, `S.${name} rewrote a nested definition`).toEqual({
      flag: true,
    });
  }
});

test("an array definition keeps its items", () => {
  const definition = [S.string, "lit", 2];
  S.array(definition as never);

  expect(definition[0]).toBe(S.string);
  expect(definition[1]).toBe("lit");
  expect(definition[2]).toBe(2);
});

test("one definition builds independent schemas", () => {
  const definition = { tag: "a" };
  const first = S.array(definition);
  const second = S.record(definition);

  expect(S.parser(first)([{ tag: "a" }])).toEqual([{ tag: "a" }]);
  expect(S.parser(second)({ k: { tag: "a" } })).toEqual({ k: { tag: "a" } });
  expect(() => S.parser(first)([{ tag: "b" }])).toThrow();
});

// `undefined` is a forgotten argument far more often than a request for the
// undefined literal, so the containers reject it and point at the spelling
// that does mean the literal.
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
      "[Sury] Missing schema. Use S.schema(undefined) for the literal",
    );
  }

  expect(S.parser(S.schema(undefined))(undefined)).toBe(undefined);
});
