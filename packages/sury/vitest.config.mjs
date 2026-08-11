import { globSync } from "node:fs";
import { defineConfig } from "vitest/config";

const rescriptTests = globSync("tests/**/*_test.res", {
  cwd: import.meta.dirname,
}).map((path) => `${path}.mjs`);

export default defineConfig({
  test: {
    include: [...rescriptTests, "tests/**/*_test.ts"],
    typecheck: {
      enabled: true,
      include: ["tests/**/*_test.ts"],
      tsconfig: "./tsconfig.json",
    },
  },
});
