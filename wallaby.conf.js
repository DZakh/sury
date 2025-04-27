import fs from "fs";

let suryPath = "./packages/sury";

const packageJson = JSON.parse(
  fs.readFileSync(suryPath + "/package.json", "utf8")
);

const tests = packageJson.ava.files.map((file) => suryPath + "/" + file);

export default () => ({
  files: [
    suryPath + "/package.json",
    suryPath + "/src/S.res.mjs",
    suryPath + "/src/Sury.res.mjs",
    suryPath + "/src/JSONSchema.res.mjs",
    suryPath + "/src/S.js",
    suryPath + "/tests/U.res.mjs",
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
