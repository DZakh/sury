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
//
// The harness's own identifiers are `__`-prefixed because `prepare` shares
// their scope: a scenario binding a plain `box` or `run` would otherwise
// shadow them — silently, for the `var`/`function` forms the gate's
// SyntaxError can't catch — and disconnect the sink that keeps the JIT from
// dead-code-eliminating the measured expression. `S` stays unprefixed: it IS
// the scenario's contract.
export const buildScenarioRunner = (
  S: unknown,
  source: ScenarioSource,
  box: { v: unknown },
): ((n: number) => void) =>
  new Function(
    "S",
    "__box",
    `${source.prepareSrc ?? ""}
const __run = (__n) => { for (let __i = 0; __i < __n; __i++) __box.v = (${source.runSrc}); };
__run(1);
return __run;`,
  )(S, box) as (n: number) => void;
