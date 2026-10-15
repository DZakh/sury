import { expect, test } from "vitest";

import * as S from "../index.mjs";

// A spec can't name what leaked: an unhandled rejection is a process-level
// event, and it crashes the harness that runs every example in one process
// (the `sync-failure-after-promises` examples). This counts them instead,
// across every promise-returning outcome.

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
  ["union of arrays", S.union([S.array(item), S.array(S.number)]), ["t", 1]],
  [
    "recursive",
    S.recursive("Node", (node) => S.schema({ v: item, kids: S.array(node) })),
    { v: "x", kids: [{ v: "t", kids: [] }, { v: "x", kids: 1 }] },
  ],
  ["from a JSON string", S.jsonString.with(S.to, S.schema({ a: item, b: S.string })), '{"a":"t","b":1}'],
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

const encodedItem = S.string
  .with(S.to, S.number, {
    decode: (value) => value.length,
    encode: { async: (value) => (value === 0 ? Promise.reject(new Error("zero")) : Promise.resolve("x".repeat(value))) },
  });
const encodedEntry = S.schema({ a: encodedItem, b: S.number.with(S.refine, (value) => value > 0) });

test.each([
  ["object", encodedEntry, { a: 0, b: -1 }],
  ["array of objects", S.array(encodedEntry), [{ a: 0, b: 1 }, { a: 1, b: -1 }]],
  ["into a JSON string", S.jsonString.with(S.to, encodedEntry), { a: 0, b: -1 }],
] as [string, S.Schema<unknown, unknown>, unknown][])(
  "encodeAsPromiseOrReject: %s leaves no promise rejecting unhandled",
  async (_, schema, value) => {
    expect(await unhandledDuring(() => S.encodeAsPromiseOrReject(schema, value))).toEqual([]);
  },
);

for (const [opName, operation] of Object.entries(operations)) {
  test.each(cases)(`${opName}: %s leaves no promise rejecting unhandled`, async (_, schema, input) => {
    expect(await unhandledDuring(() => operation(schema, input))).toEqual([]);
  });
}
