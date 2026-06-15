import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["tests/**/*_test.res.mjs", "tests/**/*_test.ts"],
    typecheck: {
      include: ["tests/**/*_test.ts"],
      tsconfig: "./tsconfig.json",
    },
  },
});
