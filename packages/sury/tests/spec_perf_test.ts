// The perf dimension's OUTPUT, not its measurement: what `spec check` prints
// once numbers exist, and the rules that decide which numbers get printed at
// all. Everything here is a pure function fed synthetic ratios, so the suite
// never runs a benchmark — a real run forks a process per target and is far too
// slow (and, being wall-clock, far too machine-dependent) to assert on.
//
// The statistics are the part worth pinning down: conservativePct is the single
// rule standing between a noisy machine and a confident "18% faster" in a
// commit message, so its behaviour is asserted directly rather than inferred
// from a rendered report.
import { test, expect } from "vitest";
import { conservativePct, deriveTargets, type Perf } from "../../spec/bench";
import { renderPerformance } from "../../spec/summary";
import { renderComment } from "../../spec/perfComment";
import { listSpecFiles, specId } from "../../spec/harness";

const ratios = (...xs: number[]) => xs;

test("conservativePct reports nothing when the blocks disagree on direction", () => {
  // Eight blocks straddling 1.0 — the interval contains "no change", so there
  // is no evidence of one however large the individual samples are.
  expect(conservativePct(ratios(0.7, 1.4, 0.9, 1.3, 0.8, 1.2, 0.95, 1.1))).toBe(0);
});

test("conservativePct reports the interval end nearest no-change, not the middle", () => {
  // Every block agrees the current side is slower, but by between 5% and 40%.
  // The reported number is the 5%: the optimistic reading is exactly as likely
  // as the pessimistic one, and overstating a regression is how a noisy machine
  // ends up quoted in a changelog.
  expect(conservativePct(ratios(1.4, 1.3, 1.2, 1.05, 1.35, 1.25, 1.15, 1.1))).toBeCloseTo(5, 5);
});

test("conservativePct keeps the sign of an improvement", () => {
  expect(conservativePct(ratios(0.6, 0.7, 0.8, 0.9, 0.65, 0.75, 0.85, 0.88))).toBeCloseTo(-10, 5);
});

test("conservativePct needs unanimity: one dissenting block is enough to report nothing", () => {
  expect(conservativePct(ratios(1.4, 1.3, 1.2, 1.25, 1.35, 1.28, 1.22, 0.99))).toBe(0);
});

// ---- targets ---------------------------------------------------------------

// `[]` for the scenarios: they aren't files, so a run narrowed to one spec
// selects none of them (an omitted argument means "every scenario").
const targetsFor = (id: string) =>
  deriveTargets([listSpecFiles().find((f) => specId(f) === id)!], []);

test("a constant schema contributes no creation targets", () => {
  // `S.string` is a module-level constant, so there is nothing to construct and
  // its compiled operation is cached on the singleton — measuring either would
  // time the cache, not the library.
  const { targets, skippedConstants } = targetsFor("string");
  expect(skippedConstants).toBe(1);
  expect(targets.filter((t) => !t.control).map((t) => t.name)).toEqual([
    "string · parse · valid",
    "string · parse · empty",
    "string · parse · invalid-number",
    "string · parse · invalid-null",
  ]);
});

test("a factory schema contributes creation and compilation targets alongside every example", () => {
  const names = targetsFor("object1")
    .targets.filter((t) => !t.control)
    .map((t) => t.name);
  expect(names.slice(0, 2)).toEqual(["object1 · create", "object1 · create+compile · parse"]);
  expect(names.length).toBeGreaterThan(2);
});

// A scenario carries its own setup and expression instead of a schema, and is
// selected by name — the one target kind that comes from scenarios.yaml rather
// than from a spec file.
test("a scenario contributes one target built from its own prepare and run", () => {
  const { targets } = deriveTargets([], ["standard-schema-validate"]);
  const real = targets.filter((t) => !t.control);
  expect(real.map((t) => t.name)).toEqual(["standard-schema-validate · scenario"]);
  expect(real[0]!.phase).toBe("scenario");
  expect(real[0]!.schemaSrc).toBe(undefined);
  expect(real[0]!.prepareSrc).toContain("S.schema(");
  // Parenthesized by stripTypes, same as a spec's ts.schema.
  expect(real[0]!.runSrc).toBe('(schema["~standard"].validate(data))');
});

test("scenarios are selected by name, so narrowing to a spec picks up none of them", () => {
  expect(targetsFor("string").targets.some((t) => t.phase === "scenario")).toBe(false);
  expect(
    deriveTargets([], []).targets.length,
  ).toBe(0);
});

test("an example expecting an error is marked as throwing, so it is measured in a try/catch", () => {
  const { targets } = targetsFor("string");
  expect(targets.find((t) => t.name === "string · parse · invalid-number")!.throws).toBe(true);
  expect(targets.find((t) => t.name === "string · parse · valid")!.throws).toBe(false);
});

test("controls duplicate real targets so a run measures the baseline against itself", () => {
  const { targets } = targetsFor("object1");
  const controls = targets.filter((t) => t.control);
  expect(controls.length).toBeGreaterThan(0);
  for (const c of controls) {
    expect(c.name.startsWith("control · ")).toBe(true);
    // Same work as a real target, so its result is that target's noise.
    expect(targets.some((t) => !t.control && `control · ${t.name}` === c.name)).toBe(true);
  }
});

// ---- report ----------------------------------------------------------------

const perf = (changed: Perf["changed"], over: Partial<Perf> = {}): Perf => ({
  baselineLabel: "merge-base with main",
  baselineSha: "93999e3",
  floors: [
    { phase: "create", pct: 8.2 },
    { phase: "create+compile", pct: 3 },
    { phase: "run", pct: 3 },
  ],
  changed,
  unchanged: 137,
  added: [],
  skippedConstants: 13,
  errors: [],
  meta: "node 24.16.0 · linux x64 · 4 cores · 8×2 rounds · confirmed",
  ...over,
});

const row = (name: string, phase: Perf["changed"][number]["phase"], pct: number) => ({
  name,
  phase,
  pct,
  median: 1 + pct / 100,
  batch: 1000,
});

test("renderPerformance ranks worst regression first and states the floor per phase", () => {
  expect(
    renderPerformance(
      perf([
        row("object10 · create", "create", 12.4),
        row("object1 · parse · valid", "run", 4.1),
        row("union2 · parse · nested", "run", -6.1),
      ]),
    ),
  ).toMatchInlineSnapshot(`
    "performance vs 93999e3 (merge-base with main) · noise floor create 8.2% · create+compile 3.0% · run 3.0%
      object10 · create        +12.4%
      object1 · parse · valid  +4.1%
      union2 · parse · nested  -6.1%
      137 unchanged · 13 constant-schema targets skipped · advisory only
      node 24.16.0 · linux x64 · 4 cores · 8×2 rounds · confirmed"
  `);
});

test("renderPerformance says so plainly when nothing cleared the floor", () => {
  expect(renderPerformance(perf([], { unchanged: 140 }))).toMatchInlineSnapshot(`
    "performance vs 93999e3 (merge-base with main) · noise floor create 8.2% · create+compile 3.0% · run 3.0%
      no significant changes
      140 unchanged · 13 constant-schema targets skipped · advisory only
      node 24.16.0 · linux x64 · 4 cores · 8×2 rounds · confirmed"
  `);
});

test("renderPerformance reports a target the baseline cannot run as new, not as a failure", () => {
  const out = renderPerformance(perf([], { added: ["merge · parse · sparse"] }));
  expect(out).toContain("new: merge · parse · sparse");
});

test("renderPerformance surfaces a measurement failure without pretending it was a result", () => {
  const out = renderPerformance(perf([], { errors: [{ name: "union5 · create", error: "boom" }] }));
  expect(out).toContain("could not measure union5 · create: boom");
});

// ---- PR comment ------------------------------------------------------------

const report = (rows: string[]) =>
  [
    "performance vs 93999e3 (merge-base with main) · noise floor create 8.2% · create+compile 3.0% · run 3.0%",
    ...rows,
    "  137 unchanged · 13 constant-schema targets skipped · advisory only",
    "  node 24.16.0 · linux x64 · 4 cores · 8×2 rounds · confirmed",
  ].join("\n");

test("renderComment builds a table, links the full report, and carries the sticky marker", () => {
  const out = renderComment(report(["  object10 · create        +12.4%", "  union2 · parse · nested  -6.1%"]), "https://x/artifact");
  expect(out).toContain("| `object10 · create` | +12.4% |");
  expect(out).toContain("| `union2 · parse · nested` | -6.1% |");
  expect(out).toContain("[Full report ↗](https://x/artifact)");
  // Without the marker the posting step can't find its own comment and would
  // open a new one on every push.
  expect(out).toContain("<!-- spec-perf -->");
});

test("renderComment truncates to the worst rows and says how many it dropped", () => {
  const rows = Array.from({ length: 14 }, (_, i) => `  target-${i} · create  +${20 - i}.0%`);
  const out = renderComment(report(rows));
  expect(out).toContain("| `target-0 · create` | +20.0% |");
  expect(out).toContain("| `target-9 · create` | +11.0% |");
  // The CLI already ordered them worst-first, so the dropped rows are the least
  // interesting ones.
  expect(out).not.toContain("target-10 · create");
  expect(out).toContain("…and 4 more.");
});

test("renderComment still posts when nothing changed, so a missing comment means a broken job", () => {
  const out = renderComment(report(["  no significant changes"]));
  expect(out).toContain("No significant changes.");
  expect(out).toContain("<!-- spec-perf -->");
});

test("renderComment degrades to a pointer at the artifact rather than inventing a summary", () => {
  const out = renderComment("something went badly wrong", "https://x/artifact");
  expect(out).toContain("report could not be parsed");
  expect(out).toContain("[Full report ↗](https://x/artifact)");
});
