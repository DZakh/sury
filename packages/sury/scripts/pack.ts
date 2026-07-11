// Build & packaging script (TypeScript port of the former scripts/pack/Pack.res,
// merged with the former scripts/build-core.mjs). Run via tsx:
//
//   pnpm build:core   -> tsx scripts/pack.ts --core-only
//   pnpm build        -> tsx scripts/pack.ts
//
// Stage 1 (always): build src/core.ts into the two runtime artifacts consumed
// via the "sury/core" package export (see package.json "exports"."./core"):
// src/core.mjs (import condition) and src/core.cjs (require condition).
// This lets Sury.res bind to it as `@module("sury/core")` regardless of which
// module format a consumer's own ReScript compiler targets — a plain relative
// `@module("./core.mjs")` would break under a "commonjs" target (require()-ing
// an ESM file throws ERR_REQUIRE_ESM). Both files are gitignored; this stage
// is what keeps them fresh (it runs before rescript/vitest via pnpm scripts).
//
// Stage 2 (full pack only): assemble the publishable package in ./artifacts —
// copy sources, generate the S.js/S.mjs entry shims, compile ReScript there,
// inline the ReScript runtime with rollup, and flip package.json to commonjs.

import { build } from "esbuild";
import { rollup, type ModuleFormat } from "rollup";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import { execaSync } from "execa";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectPath = path.join(__dirname, "..");
const artifactsPath = path.join(projectPath, "artifacts");
const sourcePaths = ["package.json", "src", "rescript.json", "README.md", "jsr.json"];

// ── Stage 1: core.ts -> core.mjs + core.cjs ─────────────────────────────────

// core.ts has no runtime imports (see its header comment), so this is a
// straight transpile, not a bundle.
async function buildCoreFormat(format: "esm" | "cjs", outfile: string): Promise<void> {
  await build({
    entryPoints: [path.join(projectPath, "src/core.ts")],
    outfile,
    bundle: false,
    write: true,
    format,
    target: "es2020",
    platform: "neutral",
    banner: { js: "// Generated from core.ts by scripts/pack.ts, PLEASE EDIT WITH CARE" },
    logLevel: "silent",
  });
}

async function buildCore(): Promise<void> {
  await buildCoreFormat("esm", path.join(projectPath, "src/core.mjs"));
  await buildCoreFormat("cjs", path.join(projectPath, "src/core.cjs"));
}

// ── Stage 2: the publishable artifact ───────────────────────────────────────

// Maps each `S.js`/`S.mjs` export name to the Sury.res.mjs expression backing
// it. Spliced verbatim into the generated shims — every RHS is a compiled
// identifier of Sury.res.mjs, so renames there break this silently; keep in
// sync with Sury.resi.
const filesMapping: Array<[name: string, value: string]> = [
  ["Error", "S.$$Error.$$class"],
  ["string", "/*#__PURE__*/ S.string()"],
  ["boolean", "/*#__PURE__*/ S.bool()"],
  ["int32", "/*#__PURE__*/ S.int()"],
  ["number", "/*#__PURE__*/ S.float()"],
  ["bigint", "/*#__PURE__*/ S.bigint()"],
  ["symbol", "/*#__PURE__*/ S.symbol()"],
  ["never", "/*#__PURE__*/ S.never_()"],
  ["unknown", "S.unknown"],
  ["any", "S.unknown"],
  ["optional", "S.js_optional"],
  ["nullable", "S.js_nullable"],
  ["nullish", "S.nullable"],
  ["array", "S.array"],
  ["compactColumns", "S.compactColumns"],
  ["instance", "S.instance"],
  ["record", "S.dict"],
  ["json", "/*#__PURE__*/ S.json()"],
  ["jsonString", "/*#__PURE__*/ S.jsonString()"],
  ["jsonStringWithSpace", "S.jsonStringWithSpace"],
  ["uint8Array", "/*#__PURE__*/ S.uint8Array()"],
  ["date", "/*#__PURE__*/ S.date()"],
  ["isoDateTime", "/*#__PURE__*/ S.isoDateTime()"],
  ["union", "S.js_union"],
  ["object", "S.object"],
  ["schema", "S.js_schema"],
  ["safe", "S.js_safe"],
  ["safeAsync", "S.js_safeAsync"],
  ["reverse", "S.reverse"],
  ["parser", "S.js_parser"],
  ["asyncParser", "S.js_asyncParser"],
  ["decoder", "S.getDecoder"],
  ["asyncDecoder", "S.js_asyncDecoder"],
  ["encoder", "S.js_encoder"],
  ["asyncEncoder", "S.js_asyncEncoder"],
  ["assert", "S.js_assert"],
  ["is", "S.js_is"],
  ["recursive", "S.recursive"],
  ["merge", "S.js_merge"],
  ["strict", "S.strict"],
  ["deepStrict", "S.deepStrict"],
  ["strip", "S.strip"],
  ["deepStrip", "S.deepStrip"],
  ["to", "S.js_to"],
  ["toJSONSchema", "S.toJSONSchema"],
  ["fromJSONSchema", "S.fromJSONSchema"],
  ["extendJSONSchema", "S.extendJSONSchema"],
  ["enableStandardJSONSchema", "S.enableStandardJSONSchema"],
  ["shape", "S.shape"],
  ["tuple", "S.tuple"],
  ["asyncDecoderAssert", "S.js_asyncDecoderAssert"],
  ["refine", "S.js_refine"],
  ["meta", "S.meta"],
  ["toExpression", "S.toExpression"],
  ["noValidation", "S.noValidation"],
  ["port", "/*#__PURE__*/ S.port()"],
  ["min", "S.min"],
  ["max", "S.max"],
  ["length", "S.length"],
  ["email", "/*#__PURE__*/ S.email()"],
  ["uuid", "/*#__PURE__*/ S.uuid()"],
  ["cuid", "/*#__PURE__*/ S.cuid()"],
  ["url", "/*#__PURE__*/ S.url()"],
  ["pattern", "S.pattern"],
  ["trim", "S.trim"],
  ["global", "S.global"],
  ["brand", "S.brand"],
];

function writeSjsEsm(filePath: string): void {
  fs.writeFileSync(
    filePath,
    [
      `/* @ts-self-types="./S.d.ts" */`,
      `import * as S from "./Sury.res.mjs"`,
      `var _void = /*#__PURE__*/ S.unit(); export { _void as void }`,
      ...filesMapping.map(([name, value]) => `export var ${name} = ${value}`),
    ].join("\n"),
    "utf8"
  );
}

function updateJsonFile(src: string, keyPath: string[], value: unknown): void {
  const json = JSON.parse(fs.readFileSync(src, "utf8")) as Record<string, unknown>;
  let target: Record<string, unknown> = json;
  for (const key of keyPath.slice(0, -1)) {
    if (typeof target[key] !== "object" || target[key] === null) target[key] = {};
    target = target[key] as Record<string, unknown>;
  }
  target[keyPath[keyPath.length - 1]!] = value;
  fs.writeFileSync(src, JSON.stringify(json, null, 2), "utf8");
}

// Inline "rescript" runtime dependencies, so it's not required for JS/TS to
// install the ReScript compiler. And if the package is used together by TS
// and ReScript, the file will be overwritten by the compiler and share the
// same code. Also inlines src/core.mjs wherever `sury/core` is imported
// (nodeResolve supports package self-references via the exports map).
async function resolveRescriptRuntime(
  format: ModuleFormat,
  input: string,
  output: string
): Promise<void> {
  const bundle = await rollup({
    input: path.join(artifactsPath, input),
    plugins: [nodeResolve()],
  });
  await bundle.write({
    file: path.join(artifactsPath, output),
    format,
    exports: "named",
  });
  await bundle.close();
}

async function pack(): Promise<void> {
  if (fs.existsSync(artifactsPath)) {
    fs.rmSync(artifactsPath, { recursive: true, force: true });
  }
  fs.mkdirSync(artifactsPath);

  // Add empty dev dirs to prevent `pnpm rescript` from failing
  fs.mkdirSync(path.join(artifactsPath, "tests"));
  fs.mkdirSync(path.join(artifactsPath, "scripts"));

  for (const p of sourcePaths) {
    fs.cpSync(path.join(projectPath, p), path.join(artifactsPath, p), { recursive: true });
  }

  // Sync the original source as well. Call it S.js to make .d.ts resolve correctly
  writeSjsEsm(path.join(projectPath, "./src/S.js"));

  writeSjsEsm(path.join(artifactsPath, "./src/S.mjs"));

  // This should overwrite S.js with the commonjs version
  fs.writeFileSync(
    path.join(artifactsPath, "./src/S.js"),
    [
      `/* @ts-self-types="./S.d.ts" */`,
      `var S = require("./Sury.res.js");`,
      ...filesMapping.map(([name, value]) => `exports.${name} = ${value}`),
      `exports.void = S.unit()`,
    ].join("\n"),
    "utf8"
  );

  execaSync("pnpm", ["rescript"], { cwd: artifactsPath });

  await resolveRescriptRuntime("es", "src/Sury.res.mjs", "src/Sury.res.mjs");
  // Even though the generated code is shitty, let's still have it for the sake of some users
  await resolveRescriptRuntime("cjs", "src/Sury.res.mjs", "src/Sury.res.js");

  // Also build cjs version, in case some ReScript libraries will use sury without running a compiler (rescript-stdlib-vendorer)
  await resolveRescriptRuntime("cjs", "src/S.res.mjs", "src/S.res.js");

  // ReScript applications don't work with type: module set on packages
  updateJsonFile(path.join(artifactsPath, "package.json"), ["type"], "commonjs");
  updateJsonFile(path.join(artifactsPath, "package.json"), ["private"], false);

  // Clean up before uploading artifacts
  fs.rmSync(path.join(artifactsPath, "lib"), { force: true, recursive: true });
  fs.rmSync(path.join(artifactsPath, "node_modules"), { force: true, recursive: true });
}

async function main(): Promise<void> {
  await buildCore();
  if (!process.argv.includes("--core-only")) {
    await pack();
  }
}

main();
