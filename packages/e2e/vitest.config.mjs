import { defineConfig } from "vitest/config";
import codspeedPlugin from "@codspeed/vitest-plugin";

export default defineConfig({
  // Instruments `bench()`es for CodSpeed when run under CodSpeedHQ/action;
  // inert for a local `vitest bench`.
  plugins: [codspeedPlugin()],
  test: {
    include: ["src/**/*_test.res.mjs"],
    benchmark: {
      include: ["src/**/*.bench.ts"],
    },
  },
});
