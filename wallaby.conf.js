const suryPath = "./packages/sury";
// const e2ePath = "./packages/e2e";

export default () => ({
  files: [
    suryPath + "/package.json",
    suryPath + "/src/S.res.mjs",
    suryPath + "/src/Sury.res.mjs",
    suryPath + "/src/JSONSchema.res.mjs",
    suryPath + "/src/S.js",
    suryPath + "/tests/U.res.mjs",
    // e2ePath + "/src/utils/U.res.mjs",
  ],
  tests: [
    suryPath + "/tests/**/*_test.res.mjs",
    suryPath + "/tests/**/*_test.ts",
    // e2ePath + "/src/**/*_test.res.mjs",
  ],
  env: {
    type: "node",
    params: {
      runner: "--experimental-vm-modules", // Improtant for Ava ESM
    },
  },
  workers: { restart: true }, // Improtant for Ava ESM
  testFramework: "ava",
});
