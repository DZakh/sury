// How a scenario becomes something runnable — shared by benchChild.ts (which
// measures) and harness.ts (which gates), so what passes `spec check` is
// exactly what the measurement can build. Source arrives type-stripped, same
// contract as Target.schemaSrc: benchChild has no TypeScript to strip with.

export type ScenarioSource = {
  prepareSrc?: string;
  runSrc: string;
};

// One `new Function` per scenario, like every other runner: closures created
// at a shared site can share a feedback vector, making the measured call
// megamorphic. The loop runs once before being handed back, so a version that
// can't execute the scenario fails here — reportable as "new" — rather than
// mid-measurement.
//
// Harness identifiers are `__`-prefixed because `prepare` shares their scope:
// a scenario binding `box` or `run` via `var`/`function` would silently
// disconnect the sink that keeps the JIT from dead-code-eliminating the
// measured expression. `S` stays unprefixed — it is the scenario's contract.
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
