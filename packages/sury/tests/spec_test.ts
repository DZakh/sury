// Tests for the spec harness (packages/spec). There is no code-generation
// step — this file IS the test: it dynamically loops over every spec at
// run time and calls straight into the harness, so example execution and
// jsonSchema/instantiations/bundleBytes drift are exercised (and covered) by
// this real Vitest run, same as any hand-written test.
import { readFileSync } from "node:fs";
import { test, expect, describe, vi } from "vitest";
import {
  SCHEMA_PATH,
  listSpecFiles,
  specId,
  readSpec,
  serialize,
  recomputeGoldens,
  evalSchema,
  identityViolations,
  lintSkips,
  lintSpecsDir,
} from "../../spec/harness";
import { validate, schemaJson } from "../../spec/format";

// recomputeGoldens does a TS-program introspection pass plus an esbuild
// child-process build per spec; the first spec processed pays the ~1s
// cold-start cost the spec skill documents, which a slower/more contended CI
// runner can push past Vitest's 5000ms default. Scoped to this file (and
// spec_errors_test.ts, which exercises the same path via checkSpec) rather
// than raised globally, so the rest of the suite keeps a tight default.
vi.setConfig({ testTimeout: 20_000 });

const specs = listSpecFiles().map((file) => ({ id: specId(file), file }));

test("there is at least one spec", () => {
  expect(specs.length).toBeGreaterThan(0);
});

// Otherwise only `pnpm spec check` (which CI doesn't run) would notice a
// format change whose spec.schema.json wasn't re-emitted.
test("spec.schema.json is fresh (run `pnpm spec schema`)", () => {
  expect(readFileSync(SCHEMA_PATH, "utf8")).toBe(schemaJson());
});

test("specs dir contains only valid spec files (run `pnpm spec check`)", () => {
  const errs = lintSpecsDir();
  expect(errs, errs.join("\n")).toEqual([]);
});

test("lintSpecsDir rejects a non-yaml file and a dotted/invalid id", () => {
  const errs = lintSpecsDir(["good-id.yaml", "notes.txt", "bad.dotted.yaml", "spec.schema.json"]);
  expect(errs).toEqual([
    `specs dir: unexpected file "notes.txt" (only *.yaml and spec.schema.json allowed)`,
    `specs dir: invalid spec id "bad.dotted" (only letters, digits, and - allowed)`,
  ]);
});

describe.each(specs)("spec: $id", ({ file }) => {
  const spec = readSpec(file);

  test("is valid against the format schema", () => {
    const v = validate(spec);
    expect(v.ok, v.ok ? "" : v.error).toBe(true);
  });

  test("is in canonical form (run `pnpm spec format`)", () => {
    expect(readFileSync(file, "utf8")).toBe(serialize(spec));
  });

  // Only checkSpec (the pnpm spec check gate) runs these two — nothing else
  // in `pnpm test`/CI did, so a spec's identity marker or _skip reason could
  // drift with no test ever catching it. Same checks `spec check` makes,
  // just run here too so they're part of the coverage CI actually gates on.
  test("has no identity-invariant violations (run `pnpm spec check`)", () => {
    const schema = evalSchema(spec.ts.schema);
    const violations = identityViolations(schema, spec);
    expect(violations, violations.join("\n")).toEqual([]);
  });

  test("every _skip reason is valid (run `pnpm spec check`)", () => {
    const errs: string[] = [];
    lintSkips(spec, "", errs);
    expect(errs, errs.join("\n")).toEqual([]);
  });

  test("goldens match live behavior (run `pnpm spec check --write`)", async () => {
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
