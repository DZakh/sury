import fs from "fs";

const packageJson = JSON.parse(fs.readFileSync("./package.json", "utf8"));

const tests = packageJson.ava.files;

export default () => ({
  files: [
    "package.json",
    "src/S.res.mjs",
    "src/Sury.res.mjs",
    "src/S.js",
    "packages/tests/src/utils/U.res.mjs",
  ],
  tests,
  env: {
    type: "node",
    params: {
      runner: "--experimental-vm-modules",
    },
  },
  testFramework: "ava",
});
