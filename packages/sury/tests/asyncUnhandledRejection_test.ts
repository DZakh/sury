import { expect, test } from "vitest";

import * as S from "../index.mjs";

// A spec can't hold this: an unhandled rejection is a process-level event, and
// the harness runs every example in the one process it would crash.

const item = S.string
  .with(S.to, S.string, {
    decode: {
      async: (value) => (value === "t" ? Promise.reject(new Error("taken")) : Promise.resolve(value)),
    },
    encode: (value) => value,
  })
  .with(S.refine, (value) => value !== "bad", { error: "Expected not bad" });

// Each input makes promises first and then fails synchronously further on.
const cases: [string, S.Schema<unknown, unknown>, unknown][] = [
  ["array", S.array(item), ["bad", "t", "x", 1, "bad"]],
  ["object", S.schema({ a: item, b: item, c: S.string }), { a: "t", b: "x", c: 1 }],
  ["strict object", S.schema({ a: item, b: S.string }).with(S.strict), { a: "t", b: "x", c: 1 }],
  ["tuple", S.schema([item, item, S.string]), ["t", "x", 1]],
  ["record", S.record(item), { a: "t", b: "x", c: 1 }],
  ["nested object", S.schema({ a: item, b: S.schema({ x: item, y: S.string }) }), { a: "t", b: { x: "t", y: 1 } }],
  ["array of objects", S.array(S.schema({ a: item, b: S.string })), [{ a: "t", b: "x" }, { a: "t", b: 1 }]],
  [
    "discriminated union",
    S.union([S.schema({ k: S.literal(1), a: item, b: S.string }), S.schema({ k: S.literal(2) })]),
    { k: 1, a: "t", b: 1 },
  ],
  [
    "union falling back to a later member",
    S.union([S.schema({ a: item, b: S.string }), S.schema({ a: S.string, b: S.number })]),
    { a: "t", b: 1 },
  ],
];

const operations = {
  parseAsPromiseOrReject: S.parseAsPromiseOrReject,
  parseAsResultPromise: S.parseAsResultPromise,
  parseAsPromisableResult: S.parseAsPromisableResult,
  decodeAsPromiseOrReject: S.decodeAsPromiseOrReject,
  assertInputAsPromiseOrReject: S.assertInputAsPromiseOrReject,
  isInputAsPromise: S.isInputAsPromise,
} as Record<string, (schema: S.Schema<unknown, unknown>, input: unknown) => unknown>;

// Vitest's own listener would fail the run instead of letting the test count
// them, so it is set aside for the duration.
const unhandledDuring = async (run: () => unknown): Promise<string[]> => {
  const seen: string[] = [];
  const listener = (reason: unknown) => seen.push((reason as Error)?.message ?? String(reason));
  const previous = process.listeners("unhandledRejection");
  process.removeAllListeners("unhandledRejection");
  process.on("unhandledRejection", listener);
  try {
    try {
      await run();
    } catch {}
    await new Promise((resolve) => setTimeout(resolve, 10));
  } finally {
    process.off("unhandledRejection", listener);
    for (const restored of previous) process.on("unhandledRejection", restored);
  }
  return seen;
};

// FIXME: what leaks before the fix; an operation that collects every failure
// reaches its join anyway, short of a union member it gives up on.
const leaks = (opName: string, caseName: string) =>
  !/Result|^decode/.test(opName) || caseName === "union falling back to a later member";

for (const [opName, operation] of Object.entries(operations)) {
  for (const [caseName, schema, input] of cases) {
    (leaks(opName, caseName) ? test.fails : test)(
      `${opName}: ${caseName} leaves no promise rejecting unhandled`,
      async () => {
        expect(await unhandledDuring(() => operation(schema, input))).toEqual([]);
      },
    );
  }
}
