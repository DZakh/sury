import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/**/*_test.res.mjs"],
    // The *_test_type.ts files assert types only; without this they match no
    // include pattern and go unchecked, which is how one of them came to
    // import a path that hadn't existed for releases.
    typecheck: {
      enabled: true,
      include: ["src/**/*_test_type.ts"],
      tsconfig: "./tsconfig.json",
    },
    benchmark: {
      include: ["src/**/*.bench.ts"],
    },
  },
});
