import { defineConfig } from "vitest/config";
import codspeedPlugin from "@codspeed/vitest-plugin";

export default defineConfig({
  // Instruments `bench()`es for CodSpeed when run under CodSpeedHQ/action
  // (deterministic instruction-count measurement); inert for local `pnpm bench`.
  plugins: [codspeedPlugin()],
  test: {
    include: ["tests/**/*_test.res.mjs", "tests/**/*_test.ts"],
    // Runtime benchmarks. types.bench.ts (@ark/attest) and bundle.bench.ts
    // (esbuild size measurement) are standalone scripts run via tsx
    // (pnpm bench:types / bench:bundle), not Vitest benchmarks — exclude them.
    benchmark: {
      include: ["tests/**/*.bench.ts"],
      exclude: ["tests/types.bench.ts", "tests/bundle.bench.ts"],
    },
    typecheck: {
      enabled: true,
      include: ["tests/**/*_test.ts"],
      tsconfig: "./tsconfig.json",
    },
  },
});
