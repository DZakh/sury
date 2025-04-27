const suryPath = "./packages/sury";

export default () => ({
  files: [
    suryPath + "/package.json",
    suryPath + "/src/S.res.mjs",
    suryPath + "/src/Sury.res.mjs",
    suryPath + "/src/JSONSchema.res.mjs",
    suryPath + "/src/S.js",
    suryPath + "/tests/U.res.mjs",
  ],
  tests: [
    suryPath + "/tests/**/*_test.res.mjs",
    suryPath + "/tests/**/*_test.ts",
  ],
  env: {
    type: "node",
    params: {
      runner: "--experimental-vm-modules",
    },
  },
  testFramework: "ava",
});
