import { defineConfig } from "vitest/config";
import codspeedPlugin from "@codspeed/vitest-plugin";

export default defineConfig({
  // Instruments `bench()`es for CodSpeed when run under CodSpeedHQ/action
  // (deterministic instruction-count measurement); inert for local `pnpm bench`.
  plugins: [codspeedPlugin()],
  test: {
    include: ["tests/**/*_test.res.mjs", "tests/**/*_test.ts"],
    // recomputeGoldens (spec_test.ts, spec_errors_test.ts) does a TS-program
    // introspection pass plus an esbuild child-process build per spec; the
    // first spec processed pays the ~1s cold-start cost documented in the
    // spec skill, which a slower/more contended CI runner can push past
    // Vitest's 5000ms default (observed: a passing run at 4890ms, a timed-out
    // one at 5093ms) even though nothing is actually hung.
    testTimeout: 20_000,
    benchmark: {
      include: ["tests/**/*.bench.ts"],
    },
    typecheck: {
      enabled: true,
      include: ["tests/**/*_test.ts"],
      tsconfig: "./tsconfig.json",
    },
  },
});
