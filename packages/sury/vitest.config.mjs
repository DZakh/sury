import { defineConfig } from "vitest/config";
import codspeedPlugin from "@codspeed/vitest-plugin";

export default defineConfig({
  // Instruments `bench()`es for CodSpeed when run under CodSpeedHQ/action
  // (deterministic instruction-count measurement); inert for local `pnpm bench`.
  plugins: [codspeedPlugin()],
  test: {
    include: ["tests/**/*_test.res.mjs", "tests/**/*_test.ts"],
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
