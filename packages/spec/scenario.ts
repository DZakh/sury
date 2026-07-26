// How a scenario becomes something runnable — shared by benchChild.ts (which
// measures it) and harness.ts (which gates it), so a scenario that passes
// `spec check` is exactly one the measurement can build.
//
// Source arrives type-stripped: benchChild has no TypeScript to strip it with
// (bundling the compiler into the child would cost more than the measurement),
// so bench.ts and harness.ts both strip before calling in — same contract as
// Target.schemaSrc.

export type ScenarioSource = {
  prepareSrc?: string;
  runSrc: string;
};

// One `new Function` per scenario per library version, matching how
// benchChild builds every other runner: closures created at a shared site can
// share a feedback vector, which would make the measured call megamorphic
// across a run.
//
// `prepare` runs where the loop can close over its bindings, and the loop runs
// once before being handed back — so a version that can't execute the scenario
// at all (a baseline predating the API it calls) fails here, where the caller
// can report it as "new", rather than mid-measurement.
export const buildScenarioRunner = (
  S: unknown,
  source: ScenarioSource,
  box: { v: unknown },
): ((n: number) => void) =>
  new Function(
    "S",
    "box",
    `${source.prepareSrc ?? ""}
const run = (n) => { for (let i = 0; i < n; i++) box.v = (${source.runSrc}); };
run(1);
return run;`,
  )(S, box) as (n: number) => void;
