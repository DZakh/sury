import { test } from "vitest";

import * as S from "../index.mjs";

type Issue = { message: string; reason: string; path: string; code: string };

const issuesOf = (fn: () => unknown): Issue[] => {
  try {
    fn();
  } catch (error) {
    return (error as { issues: Issue[] }).issues;
  }
  throw new Error("Expected the operation to throw");
};

const brief = (issues: Issue[]) => issues.map((i) => `${i.path}|${i.reason}`);

test("object collects type, refinement and format issues across fields", (t) => {
  const schema = S.schema({
    id: S.string.with(S.minLength, 3),
    age: S.number,
    tags: S.array(S.string),
  });
  t.expect(
    brief(
      issuesOf(() =>
        S.parser(schema)({ id: "x", age: "no", tags: ["a", 1, "c", true] }),
      ),
    ),
  ).toEqual([
    `["id"]|Expected string.length >= 3, received "x"`,
    `["age"]|Expected number, received "no"`,
    `["tags"]["1"]|Expected string, received 1`,
    `["tags"]["3"]|Expected string, received true`,
  ]);
});

test("thrown error still reports fail-fast; issues is the lazy full list", (t) => {
  const schema = S.schema({ a: S.string, b: S.number });
  try {
    S.parser(schema)({ a: 1, b: "x" });
    t.expect.unreachable();
  } catch (error) {
    const e = error as Error & { issues: Issue[] };
    t.expect(e.message).toBe(`Failed at ["a"]: Expected string, received 1`);
    t.expect(e.issues.length).toBe(2);
    // Cached: the second access returns the same array without a re-run.
    t.expect(e.issues).toBe(e.issues);
  }
});

test("a scalar reports every failed refinement, not just the first", (t) => {
  const schema = S.string.with(S.minLength, 8).with(S.pattern, /[A-Z]/);
  t.expect(brief(issuesOf(() => S.parser(schema)("abc")))).toEqual([
    `|Expected string.length >= 8, received "abc"`,
    `|Invalid pattern`,
  ]);
});

test("nested containers keep absolute paths, including dynamic segments", (t) => {
  const schema = S.schema({
    profile: { name: S.string },
    points: S.array(S.schema({ x: S.number })),
    meta: S.record(S.number),
  });
  t.expect(
    brief(
      issuesOf(() =>
        S.parser(schema)({
          profile: { name: 1 },
          points: [{ x: 1 }, { x: "a" }],
          meta: { good: 1, bad: "b" },
        }),
      ),
    ),
  ).toEqual([
    `["profile"]["name"]|Expected string, received 1`,
    `["points"]["1"]["x"]|Expected number, received "a"`,
    `["meta"]["bad"]|Expected number, received "b"`,
  ]);
});

test("strict mode reports each unrecognized key and keeps field issues", (t) => {
  const schema = S.strict(S.schema({ a: S.string }));
  t.expect(brief(issuesOf(() => S.parser(schema)({ a: 1, b: 2, c: 3 })))).toEqual([
    `["a"]|Expected string, received 1`,
    `|Unrecognized key "b"`,
    `|Unrecognized key "c"`,
  ]);
});

test("a union member stays one aggregated issue and siblings still report", (t) => {
  const schema = S.schema({
    v: S.union([S.string, S.number]),
    w: S.boolean,
  });
  const issues = issuesOf(() => S.parser(schema)({ v: null, w: "x" }));
  t.expect(brief(issues)).toEqual([
    `["v"]|Expected string | number, received null`,
    `["w"]|Expected boolean, received "x"`,
  ]);
});

test("root-level failure degrades to a single issue equal to the error", (t) => {
  try {
    S.parser(S.string)(42);
    t.expect.unreachable();
  } catch (error) {
    const e = error as Error & { issues: Issue[] };
    t.expect(e.issues.length).toBe(1);
    // The re-run mints a fresh instance; contentwise it is the thrown error.
    t.expect(e.issues[0]!.reason).toBe(e.message);
    t.expect(e.issues[0]!.path).toBe("");
  }
});

test("issues works on the encode direction too", (t) => {
  const schema = S.schema({
    a: S.string.with(S.minLength, 2),
    b: S.string.with(S.maxLength, 1),
  });
  t.expect(
    brief(issuesOf(() => S.encoder(schema)({ a: "x", b: "yy" }))),
  ).toEqual([
    `["a"]|Expected string.length >= 2, received "x"`,
    `["b"]|Expected string.length <= 1, received "yy"`,
  ]);
});

test("an error without operation context falls back to itself", (t) => {
  const error = new (S.Error as unknown as new (details: unknown) => Error & {
    issues: Issue[];
  })({ code: "invalid_operation", path: "", reason: "manual" });
  t.expect(error.issues.length).toBe(1);
  t.expect(error.issues[0]!.reason).toBe("manual");
});

test("standard schema validate surfaces every issue", (t) => {
  const schema = S.schema({ a: S.string, b: S.number });
  const result = (
    schema as unknown as {
      "~standard": {
        validate: (input: unknown) => {
          issues?: { message: string; path?: unknown[] }[];
        };
      };
    }
  )["~standard"].validate({ a: 1, b: "x" });
  t.expect(result.issues).toEqual([
    { message: "Expected string, received 1", path: ["a"] },
    { message: `Expected number, received "x"`, path: ["b"] },
  ]);
});

test("safe results expose issues through the error", (t) => {
  const schema = S.schema({ a: S.string, b: S.number });
  const result = S.safe(() => S.parser(schema)({ a: 1, b: "x" }));
  t.expect(result.success).toBe(false);
  if (!result.success) {
    t.expect(
      (result.error as unknown as { issues: Issue[] }).issues.length,
    ).toBe(2);
  }
});

test("recursive schemas fall back to fail-fast inside the def", (t) => {
  const nodeSchema: S.Schema<unknown> = S.recursive("Node", (node) =>
    S.schema({ id: S.string, children: S.array(node) }),
  );
  const issues = issuesOf(() =>
    S.parser(nodeSchema)({ id: 1, children: [{ id: "ok", children: [{ id: 2, children: [] }] }] }),
  );
  // Each recursion level aborts on its first inner failure, but sibling
  // fields at the top level still report independently.
  t.expect(issues.length).toBeGreaterThanOrEqual(1);
  for (const issue of issues) {
    t.expect(issue.reason).toContain("Expected string");
  }
});
