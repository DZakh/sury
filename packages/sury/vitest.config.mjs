import { globSync } from "node:fs";
import { join } from "node:path";
import { defineConfig } from "vitest/config";

const specDir = join(import.meta.dirname, "..", "spec");

const rescriptTests = globSync("tests/**/*_test.res", {
  cwd: import.meta.dirname,
}).map((path) => `${path.replaceAll("\\", "/")}.mjs`);

export default defineConfig({
  test: {
    include: [...rescriptTests, "tests/**/*_test.ts"],
    coverage: {
      // `index.mjs` rather than `src/**/*.ts`: the tests import the bundle
      // pack.ts builds, so the sources themselves are never loaded and naming
      // them reports every one of them at 0%.
      //
      // The harness is test infrastructure, but it is also what decides whether
      // every spec in the suite passes, so an unexercised branch in it is a check nobody is
      // running. It lives outside this package, which needs both an absolute
      // pattern and `allowExternal`: a relative `../spec/**` is resolved against
      // this root, matches nothing, and reports nothing at all.
      allowExternal: true,
      // CI hands `coverage/lcov.info` to codecov, and the default reporters
      // (text, html, clover, json) never write one - so the upload had nothing
      // to upload. `lcov` emits lcov.info and the html report together.
      reporter: ["text", "lcov"],
      include: ["index.mjs", `${specDir}/**/*.ts`],
      exclude: [
        // A script, not a library: importing it throws by design (see its own
        // guard), so no test can reach a line of it.
        `${specDir}/cli.ts`,
        // Bundled into a child process and driven by wall-clock measurement.
        // spec_perf_test.ts covers the statistics and the target derivation,
        // which is the part a test can hold still.
        `${specDir}/benchChild.ts`,
        `${specDir}/.bench-cache/**`,
        `${specDir}/node_modules/**`,
      ],
    },
    typecheck: {
      enabled: true,
      include: ["tests/**/*_test.ts"],
      tsconfig: "./tsconfig.json",
    },
  },
});
