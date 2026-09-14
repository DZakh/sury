// Tests for the spec harness (packages/spec). There is no code-generation
// step - this file IS the test: it dynamically loops over every spec at
// run time and calls straight into the harness, so example execution and
// jsonSchema/instantiations drift are exercised (and covered) by this real
// Vitest run, same as any hand-written test.
import { readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { test, expect, describe, vi } from "vitest";
import {
  SCHEMA_PATH,
  listSpecFiles,
  specId,
  readSpec,
  serialize,
  checkSpec,
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

// Every golden in this file is computed from index.mjs, the bundle pack.ts
// builds out of src/. `pnpm test` and `pnpm coverage` rebuild it first, but a
// bare `vitest run` does not - and against a stale bundle the whole suite
// passes on code nobody is editing, which is the one failure a golden cannot
// show as a diff.
test("index.mjs is not older than src/ (run `pnpm build:entry`)", () => {
  const root = fileURLToPath(new URL("..", import.meta.url));
  const built = statSync(join(root, "index.mjs")).mtimeMs;
  const newer = readdirSync(join(root, "src"), { recursive: true, encoding: "utf8" })
    .filter((file) => file.endsWith(".ts"))
    .filter((file) => statSync(join(root, "src", file)).mtimeMs > built);
  expect(newer, `changed since index.mjs was built: ${newer.join(", ")}`).toEqual([]);
});

// Otherwise only `pnpm spec check` (which CI doesn't run) would notice a
// format change whose spec.schema.json wasn't re-emitted.
test("spec.schema.json is fresh (run `pnpm spec schema`)", () => {
  expect(readFileSync(SCHEMA_PATH, "utf8")).toBe(schemaJson());
});

test("scenarios.schema.json is fresh (run `pnpm spec schema`)", () => {
  expect(readFileSync(SCENARIOS_SCHEMA_PATH, "utf8")).toBe(scenariosSchemaJson());
});

// Scenarios have no goldens, so nothing else would ever execute them - a
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
    checkScenarios(["string:", "  run: S.parseOrThrow(S.string)"].join("\n"), ["string"]),
  ).toEqual(["string: id collides with a spec of the same name"]);
  expect(
    checkScenarios(["broken:", "  run: S.parse(S.string)"].join("\n"), [])[0],
  ).toMatch(/^broken: did not run: /);
  // A `prepare` binding has to reach `run`, or every scenario would have to
  // inline its whole setup into the measured expression.
  expect(
    checkScenarios(
      ["ok:", "  prepare: const schema = S.string", "  run: S.parseOrThrow(schema)"].join("\n"),
      [],
    ),
  ).toEqual([]);
});

// Same reasoning as the spec.schema.json freshness test above: CI runs
// `pnpm test`, not `pnpm spec check`, so without this the bundle-size ratchet
// would only bite on a manual run.
// buildScenarioRunner executes a scenario once while constructing it, so
// reporting this one without skipping it would still reconfigure the shared
// library for every scenario and spec after it - the exact thing the rule
// exists to prevent. Reported by CodeRabbit on #434.
test("a scenario that calls S.global is reported and never run", () => {
  const errs = checkScenarios(
    ["bad:", "  run: S.global({}) ?? S.parseOrThrow(S.string)"].join("\n"),
    [],
  );
  expect(errs).toEqual([
    "bad: run calls S.global - it sets process-wide configuration that every spec and " +
      "scenario in the run then compiles against",
  ]);
});

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
    "url-codec.yaml",
    "codec.yaml",
    "flatten-field-codec.yaml",
    "codec-string-url.yaml",
    "spec.schema.json",
    "bundleSize.yaml",
    "scenarios.yaml",
    "scenarios.schema.json",
  ]);
  expect(errs).toEqual([
    `specs dir: unexpected file "notes.txt" (only *.yaml and spec.schema.json/bundleSize.yaml/scenarios.yaml/scenarios.schema.json allowed)`,
    `specs dir: invalid spec id "bad.dotted" (only letters, digits, and - allowed)`,
    `specs dir: "url-codec" names a codec spec backwards - use codec-<from>-<to>, not <from>-codec`,
    `specs dir: "codec" names a codec spec backwards - use codec-<from>-<to>, not <from>-codec`,
  ]);
});

test("jsonstring-object records dialect jsonSchema fields only when they differ from draft-07", () => {
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "jsonstring-object")!);
  expect(spec.jsonSchema["draft-2020-12"]?.output).toContain("contentSchema");
  expect(spec.jsonSchema["openapi-3.0"]?.output).not.toContain("contentMediaType");
  expect(spec.jsonSchema["draft-2020-12"]?.input).toBeUndefined();
});

test("serialize quotes a tab in an error golden instead of writing a raw control", () => {
  const spec = structuredClone(readSpec(listSpecFiles().find((f) => specId(f) === "string")!));
  if (spec.operations.parse !== "identity" && !isCreationError(spec.operations.parse))
    spec.operations.parse.examples.tab = { input: '"x"', error: 'Expected string, received "a\tb"' };
  const yaml = serialize(spec);
  expect(yaml).not.toMatch(/\t/);
  expect(yaml).toContain("\\t");
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
      first recorded - 0 exports, total 20000
    behavior changed:
      string.parse  compiled → creationError SuryError: Can't decode string to number
      codec-bool-number-unsupported.parse.creationError  SuryError: Can't decode boolean -> number. Define custom codec with S.to → SuryError: some new wording"
  `);
});

// One call to the function `pnpm spec check` itself runs, rather than a
// hand-kept list of the checks it makes. The list drifted: the operation
// matrix, `vs.zod`, and the jsonSchema round-trip type rules were checked by
// nothing but a manual run, so a divergence in any of them reached main with
// the whole suite green. There is one answer to "what is wrong with this
// spec", and this is where CI asks for it.
describe.each(specs)("spec: $id", ({ id, file }) => {
  test("passes `pnpm spec check`", async () => {
    const raw = readFileSync(file, "utf8");
    const errs = await checkSpec(id, readSpec(file), raw);
    expect(errs, errs.join("\n")).toEqual([]);
  });
});


test("the format is defined as a Sury schema (closed world)", () => {
  // Unknown keys are rejected - the closed-world guarantee (via published sury).
  expect(validate({}).ok).toBe(false);
  const ok = readSpec(listSpecFiles()[0]!);
  const bad = validate({ ...ok, bogus: 1 });
  expect(bad.ok).toBe(false);
  if (!bad.ok) expect(bad.error).toMatch(/Unrecognized key/);
});
