// Tests for the spec harness (packages/spec). Generated test files are not
// committed; this suite is the committed record of the harness's behavior —
// including a file snapshot of exactly what a generated test looks like.
import { readFileSync } from "node:fs";
import { test, expect, describe } from "vitest";
import {
  listSpecFiles,
  specId,
  readSpec,
  serialize,
  recomputeGoldens,
  generateTest,
} from "../../spec/harness";
import { validate } from "../../spec/format";

const specs = listSpecFiles().map((file) => ({ id: specId(file), file }));

test("there is at least one spec", () => {
  expect(specs.length).toBeGreaterThan(0);
});

describe.each(specs)("spec: $id", ({ id, file }) => {
  const spec = readSpec(file);

  test("is valid against the format schema", () => {
    const v = validate(spec);
    expect(v.ok, v.ok ? "" : v.error).toBe(true);
  });

  test("is in canonical form (run `pnpm spec fmt`)", () => {
    expect(readFileSync(file, "utf8")).toBe(serialize(spec));
  });

  test("goldens match live behavior (run `pnpm spec update`)", async () => {
    expect(serialize(await recomputeGoldens(spec))).toBe(serialize(spec));
  });
});

test("generateTest output matches snapshot (how a generated file looks)", async () => {
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "string")!);
  await expect(generateTest("string", spec)).toMatchFileSnapshot(
    "./__snapshots__/string.gen_test.ts.snap",
  );
});

test("the format is defined as a Sury schema (closed world)", () => {
  // Unknown keys are rejected — the closed-world guarantee (via published sury).
  expect(validate({}).ok).toBe(false);
  const ok = readSpec(listSpecFiles()[0]!);
  const bad = validate({ ...ok, bogus: 1 });
  expect(bad.ok).toBe(false);
  if (!bad.ok) expect(bad.error).toMatch(/Unrecognized key/);
});
