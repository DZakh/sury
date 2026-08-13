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
  asyncViolations,
  checkAliases,
  collectComments,
  lintComments,
  lintExamples,
  lintSkips,
  lintSpecsDir,
  checkBundleSize,
  checkScenarios,
  readScenarios,
  SCENARIOS_SCHEMA_PATH,
} from "../../spec/harness";
import { validate, schemaJson, scenariosSchemaJson, isCreationError } from "../../spec/format";
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

test("scenarios.schema.json is fresh (run `pnpm spec schema`)", () => {
  expect(readFileSync(SCENARIOS_SCHEMA_PATH, "utf8")).toBe(scenariosSchemaJson());
});

// Scenarios have no goldens, so nothing else would ever execute them — a
// broken one would only show up in a perf run, as an indistinguishable "new".
test("scenarios.yaml is valid and every scenario runs (run `pnpm spec check`)", () => {
  const errs = checkScenarios();
  expect(errs, errs.join("\n")).toEqual([]);
});

test("there is at least one scenario", () => {
  expect(Object.keys(readScenarios()).length).toBeGreaterThan(0);
});

test("checkScenarios reports a bad shape, a colliding id, and one that throws", () => {
  expect(checkScenarios("standard: { run: 1 }", [])[0]).toMatch(/^schema: /);
  expect(
    checkScenarios(["string:", "  run: S.parser(S.string)"].join("\n"), ["string"]),
  ).toEqual(["string: id collides with a spec of the same name"]);
  expect(
    checkScenarios(["broken:", "  run: S.parse(S.string)"].join("\n"), [])[0],
  ).toMatch(/^broken: did not run: /);
  // A `prepare` binding has to reach `run`, or every scenario would have to
  // inline its whole setup into the measured expression.
  expect(
    checkScenarios(
      ["ok:", "  prepare: const schema = S.string", "  run: S.parser(schema)"].join("\n"),
      [],
    ),
  ).toEqual([]);
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
    "scenarios.yaml",
    "scenarios.schema.json",
  ]);
  expect(errs).toEqual([
    `specs dir: unexpected file "notes.txt" (only *.yaml and spec.schema.json/bundleSize.yaml/scenarios.yaml/scenarios.schema.json allowed)`,
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
  after.jsonSchema.fromInputType = "unknown";
  if (after.operations.parse !== "identity" && !isCreationError(after.operations.parse)) {
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
    operations.expression:
      string.parse:
        chars  42 → 4  -90.5%
        before  i=>{typeof i==="string"||e[0](i);return i}
        after   i=>i
    bundleSize:
      total  20000 → 20690  +3.5%
      added: newExport 20
      removed: oldExport
      toJSONSchema     4000 →  5229  +30.7%
      fromJSONSchema  20000 → 15165  -24.2%
    behavior changed:
      string.jsonSchema.fromInputType  omitted → unknown
      string.ts.output  string → string | undefined
      string.parse.valid  output "hello" → output "HELLO""
  `);
});

// An op flipping between compiling and being rejected at operation creation is
// the change a conversion-rules rework produces, so the summary has to render
// it rather than skip it as an unreadable kind change.
test("summarize renders creation-error flips and message drift", () => {
  const compiling = readSpec(listSpecFiles().find((f) => specId(f) === "string")!);
  const rejected = readSpec(listSpecFiles().find((f) => specId(f) === "codec-bool-number-unsupported")!);

  const nowRejected = structuredClone(compiling);
  nowRejected.operations.parse = { creationError: "SuryError: Can't decode string to number" };

  const messageDrifted = structuredClone(rejected);
  messageDrifted.operations.parse = { creationError: "SuryError: some new wording" };

  expect(
    summarize(
      [
        { id: "string", before: compiling, after: nowRejected },
        { id: "codec-bool-number-unsupported", before: rejected, after: messageDrifted },
      ],
      { after: { total: 20000, exports: {} } },
    ),
  ).toMatchInlineSnapshot(`
    "bundleSize:
      first recorded — 0 exports, total 20000
    behavior changed:
      string.parse  compiled → creationError SuryError: Can't decode string to number
      codec-bool-number-unsupported.parse.creationError  SuryError: Can't decode boolean to number. Use S.to to define a custom decoder → SuryError: some new wording"
  `);
});

describe.each(specs)("spec: $id", ({ file }) => {
  const spec = readSpec(file);

  test("is valid against the format schema", () => {
    const v = validate(spec);
    expect(v.ok, v.ok ? "" : v.error).toBe(true);
  });

  test("is in canonical form (run `pnpm spec format`)", () => {
    const raw = readFileSync(file, "utf8");
    expect(raw).toBe(serialize(spec, collectComments(raw)));
  });

  test("every comment is a `FIXME:` (run `pnpm spec check`)", () => {
    const errs: string[] = [];
    lintComments(collectComments(readFileSync(file, "utf8")), errs);
    expect(errs, errs.join("\n")).toEqual([]);
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

  test("every `isAsync` marker matches the schema (run `pnpm spec check`)", () => {
    const violations = asyncViolations(evalSchema(spec.ts.schema), spec);
    expect(violations, violations.join("\n")).toEqual([]);
  });

  test("every _skip reason is valid (run `pnpm spec check`)", () => {
    const errs: string[] = [];
    lintSkips(spec, "", errs);
    expect(errs, errs.join("\n")).toEqual([]);
  });

  test("every compiled op block has examples (run `pnpm spec check`)", () => {
    const errs: string[] = [];
    lintExamples(spec, errs);
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
