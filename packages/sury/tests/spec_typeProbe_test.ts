import { test, expect } from "vitest";
import { deriveTypeInfo } from "../../spec/harness";

test("a probe that fails rejects alone, and every probe queued with it settles", async () => {
  const results = await Promise.allSettled([
    deriveTypeInfo("S.string.foo"),
    ...Array.from({ length: 12 }, () => deriveTypeInfo("S.string")),
    deriveTypeInfo("S.number"),
  ]);
  expect(results[0]).toMatchObject({ status: "rejected" });
  expect((results[0] as PromiseRejectedResult).reason.message).toContain("does not typecheck");
  const counts = new Set(results.slice(1, -1).map((r) => (r as PromiseFulfilledResult<{ instantiations: number }>).value.instantiations));
  expect(counts.size).toBe(1);
  expect(results.at(-1)).toMatchObject({ status: "fulfilled", value: { input: "number", output: "number" } });
});
