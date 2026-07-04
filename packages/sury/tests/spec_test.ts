// Tests for the spec harness (packages/spec). There is no code-generation
// step — this file IS the test: it dynamically loops over every spec at
// run time and calls straight into the harness, so example execution and
// jsonSchema/instantiations/bundleBytes drift are exercised (and covered) by
// this real Vitest run, same as any hand-written test.
import { readFileSync } from "node:fs";
import { test, expect, describe } from "vitest";
import {
  listSpecFiles,
  specId,
  readSpec,
  serialize,
  recomputeGoldens,
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

test("the format is defined as a Sury schema (closed world)", () => {
  // Unknown keys are rejected — the closed-world guarantee (via published sury).
  expect(validate({}).ok).toBe(false);
  const ok = readSpec(listSpecFiles()[0]!);
  const bad = validate({ ...ok, bogus: 1 });
  expect(bad.ok).toBe(false);
  if (!bad.ok) expect(bad.error).toMatch(/Unrecognized key/);
});
