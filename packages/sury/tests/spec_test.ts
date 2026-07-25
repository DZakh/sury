// Tests for the spec harness (packages/spec). There is no code-generation
// step — this file IS the test: it dynamically loops over every spec at
// run time and calls straight into the harness, so example execution and
// jsonSchema/instantiations drift are exercised (and covered) by this real
// Vitest run, same as any hand-written test.
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
  checkAliases,
  lintSkips,
  lintSpecsDir,
  checkBundleSize,
} from "../../spec/harness";
import { validate, schemaJson } from "../../spec/format";
import { summarize } from "../../spec/summary";

// recomputeGoldens does a TS-program introspection pass per spec, and the
// bundleSize check an esbuild build over every export; the first spec processed
// pays the ~1s cold-start cost the spec skill documents, which a slower/more contended CI
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

// Same reasoning as the spec.schema.json freshness test above: CI runs
// `pnpm test`, not `pnpm spec check`, so without this the bundle-size ratchet
// would only bite on a manual run.
test("bundleSize.yaml is fresh (run `pnpm spec check --write`)", async () => {
  const { errs } = await checkBundleSize();
  expect(errs, errs.join("\n")).toEqual([]);
});

test("specs dir contains only valid spec files (run `pnpm spec check`)", () => {
  const errs = lintSpecsDir();
  expect(errs, errs.join("\n")).toEqual([]);
});

test("lintSpecsDir rejects a non-yaml file and a dotted/invalid id", () => {
  const errs = lintSpecsDir([
    "good-id.yaml",
    "notes.txt",
    "bad.dotted.yaml",
    "spec.schema.json",
    "bundleSize.yaml",
  ]);
  expect(errs).toEqual([
    `specs dir: unexpected file "notes.txt" (only *.yaml and spec.schema.json/bundleSize.yaml allowed)`,
    `specs dir: invalid spec id "bad.dotted" (only letters, digits, and - allowed)`,
  ]);
});

// The `--write` summary is what a caller reads instead of the golden diff, so
// its exact rendering is asserted rather than left to whatever it happens to
// print: one list per metric ordered worst-regression-first, aligned columns,
// and an unchanged row (`string` below) omitted rather than shown at 0%.
test("summarize renders ranked metric moves and behavior changes", () => {
  const before = readSpec(listSpecFiles().find((f) => specId(f) === "string")!);
  const after = structuredClone(before);
  after.ts.instantiations = 300;
  after.ts.output = "string | undefined";
  if (after.operations.parse !== "identity") {
    after.operations.parse.expression = "i=>i";
    const ex = after.operations.parse.examples.valid;
    if (ex && "output" in ex) ex.output = '"HELLO"';
  }
  const improvedBefore = readSpec(listSpecFiles().find((f) => specId(f) === "never")!);
  const improvedAfter = structuredClone(improvedBefore);
  improvedAfter.ts.instantiations = 100;
  expect(
    summarize(
      [
        { id: "string", before, after },
        { id: "never", before: improvedBefore, after: improvedAfter },
      ],
      {
        before: {
          total: 20000,
          exports: { string: 3790, toJSONSchema: 4000, fromJSONSchema: 20000, oldExport: 10 },
        },
        after: {
          total: 20690,
          exports: { string: 3790, toJSONSchema: 5229, fromJSONSchema: 15165, newExport: 20 },
        },
      },
    ),
  ).toMatchInlineSnapshot(`
    "ts.instantiations:
      string  254 → 300  +18.1%
      never   254 → 100  -60.6%
    operations.expression (chars):
      string.parse  42 → 4  -90.5%
    bundleSize:
      total  20000 → 20690  +3.5%
      added: newExport 20
      removed: oldExport
      toJSONSchema     4000 →  5229  +30.7%
      fromJSONSchema  20000 → 15165  -24.2%
    behavior changed:
      string.ts.output  string → string | undefined
      string.parse.valid  output "hello" → output "HELLO""
  `);
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

  // Only checkSpec runs this too — same reasoning as the identity-invariant
  // test above: a drifting `ts.aliases` entry should fail `pnpm test`, not
  // just the occasional manual `pnpm spec check`.
  test("aliases (if any) are equivalent to the schema (run `pnpm spec check`)", async () => {
    const errs = await checkAliases(spec);
    expect(errs, errs.join("\n")).toEqual([]);
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
