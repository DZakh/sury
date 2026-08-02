// Build & packaging script. Run via tsx:
//
//   pnpm build:entry  -> tsx scripts/pack.ts entry-only
//   pnpm build        -> tsx scripts/pack.ts for-publish
//
// Stage 1 (always): bundle src/entry.ts (the single public entry re-exporting
// src/*.ts) into the gitignored index.mjs; this stage keeps it fresh (it runs
// before rescript/vitest via pnpm scripts). Types for index.mjs importers
// resolve through the checked-in index.d.mts -> index.d.ts. The ReScript
// bindings (S.res) reference the same entry as `@module("sury")`, resolved
// through the package's "." conditional export — which is why the published
// package (see stage 2) also ships a CJS index.js for the require condition
// and for consumers compiling ReScript to commonjs.
//
// Stage 2 (full pack only): assemble the publishable package in ./artifacts —
// copy sources, compile ReScript there, emit the two entries consumers load
// (index.mjs / index.js), produce a CJS S.res.js for ReScript consumers that
// don't run the compiler (with "sury" kept external so the implementation ships
// exactly once), strip everything dev-only, and flip package.json to commonjs.
//
// Everything esbuild emits is named index.*, never S.*, because `src/S.res`
// compiles to a JS file named after itself: an S.mjs / S.js sitting beside it
// would be one rescript.json "suffix" change away from being overwritten.

import { build } from "esbuild";
import { rollup, type ModuleFormat } from "rollup";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import { execaSync } from "execa";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectPath = path.join(__dirname, "..");
const repoRootPath = path.join(projectPath, "../..");
const artifactsPath = path.join(projectPath, "artifacts");
const sourcePaths = ["package.json", "src", "index.d.ts", "rescript.json", "README.md", "jsr.json"];
// Sury's user-facing docs live at the repo root, next to the README they link
// from; LICENSE has to sit in the packed root for npm to pick it up.
const repoRootPaths = ["LICENSE", "docs"];

// ── Stage 1: entry.ts -> index.mjs (ESM) ─────────────────────────────────────

async function buildEntry(
  format: "esm" | "cjs",
  outfile: string,
  selfTypes: string
): Promise<void> {
  await build({
    entryPoints: [path.join(projectPath, "src/entry.ts")],
    outfile,
    bundle: true,
    write: true,
    format,
    target: "es2020",
    platform: "neutral",
    banner: {
      js: [
        `/* @ts-self-types="${selfTypes}" */`,
        "// Generated from src/entry.ts by scripts/pack.ts — do not edit.",
      ].join("\n"),
    },
    logLevel: "silent",
  });
}

const buildDevEntries = (): Promise<void> =>
  buildEntry("esm", path.join(projectPath, "index.mjs"), "./index.d.ts");

// ── Stage 2: the publishable artifact ────────────────────────────────────────

// The artifact is consumed, never built from, so src/ keeps only what a
// consumer loads or type-checks against: ReScript sources, what the compiler
// emitted from them, and the declarations. The TypeScript the entries were
// bundled out of goes (and src/advanced/ with it), as does anything a dirty
// working tree left behind — src/ is copied wholesale and is gitignore'd in
// places, so an allowlist is the only way to know what ends up here.
const KEEP = /\.res$|\.res\.m?js$|\.d\.ts$/;

function stripSources(dir: string): void {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const entryPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      stripSources(entryPath);
      if (fs.readdirSync(entryPath).length === 0) fs.rmdirSync(entryPath);
    } else if (!KEEP.test(entry.name)) {
      fs.rmSync(entryPath);
    }
  }
}

function writeArtifactJson(file: string, update: (json: any) => void): void {
  const filePath = path.join(artifactsPath, file);
  const json = JSON.parse(fs.readFileSync(filePath, "utf8"));
  update(json);
  fs.writeFileSync(filePath, `${JSON.stringify(json, null, 2)}\n`, "utf8");
}

// Inline the "rescript" runtime dependency into the compiled S.res output, so
// ReScript consumers that don't run the compiler don't need it installed. The
// `sury` self-import stays external — the implementation must ship exactly
// once (index.mjs / CJS index.js), or mixed usage would load two instances
// (two Exn identities, two schema caches).
async function resolveRescriptRuntime(
  format: ModuleFormat,
  input: string,
  output: string
): Promise<void> {
  const bundle = await rollup({
    input: path.join(artifactsPath, input),
    external: ["sury"],
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
  for (const p of repoRootPaths) {
    fs.cpSync(path.join(repoRootPath, p), path.join(artifactsPath, p), { recursive: true });
  }

  execaSync("pnpm", ["rescript"], { cwd: artifactsPath, stdio: "inherit" });

  // The artifact package is commonjs (see below), so index.js must be the CJS
  // build — the "." require condition points at it.
  await buildEntry("cjs", path.join(artifactsPath, "index.js"), "./index.d.ts");
  await buildEntry("esm", path.join(artifactsPath, "index.mjs"), "./index.d.ts");

  // CJS build of the ReScript-facing module, in case some ReScript libraries
  // will use sury without running a compiler (rescript-stdlib-vendorer)
  await resolveRescriptRuntime("es", "src/S.res.mjs", "src/S.res.mjs");
  await resolveRescriptRuntime("cjs", "src/S.res.mjs", "src/S.res.js");

  stripSources(path.join(artifactsPath, "src"));

  // Every field below is rewritten rather than inherited, because the dev
  // package and the artifact are genuinely different packages: the dev tree is
  // ESM-only and its entry is the gitignored index.mjs that the spec harness,
  // the fuzzer and the TS tests all import by name. Don't try to make the two
  // package.json files agree — make this function the single source of truth.
  writeArtifactJson("package.json", (pkg) => {
    pkg.private = false;
    // ReScript applications don't work with type: module set on packages
    pkg.type = "commonjs";
    pkg.main = "./index.js";
    pkg.module = "./index.mjs";
    pkg.types = "./index.d.ts";
    // TypeScript only honors "types" when it precedes the runtime conditions.
    pkg.exports = {
      ".": {
        types: "./index.d.ts",
        import: "./index.mjs",
        require: "./index.js",
      },
      "./src/*": "./src/*",
      "./S.gen.js": { types: "./src/S.gen.d.ts" },
      "./package.json": "./package.json",
    };
    pkg.files = ["index.mjs", "index.js", "index.d.ts", "src", "rescript.json", "docs"];
    // Nothing here builds the artifact, and dropping the scripts also drops the
    // prepublishOnly guard that makes publishing the dev package fail.
    delete pkg.devDependencies;
    delete pkg.scripts;
  });
  writeArtifactJson("jsr.json", (jsr) => {
    jsr.exports = "./index.mjs";
    jsr.exclude = [
      "!index.mjs",
      "!index.d.ts",
      "!src",
      "!rescript.json",
      "!README.md",
      "!LICENSE",
      "!package.json",
      "!docs",
    ];
  });

  // Clean up before uploading artifacts
  fs.rmSync(path.join(artifactsPath, "lib"), { force: true, recursive: true });
  fs.rmSync(path.join(artifactsPath, "node_modules"), { force: true, recursive: true });
  fs.rmSync(path.join(artifactsPath, "tests"), { force: true, recursive: true });
  fs.rmSync(path.join(artifactsPath, "scripts"), { force: true, recursive: true });
}

async function main(): Promise<void> {
  const mode = process.argv[2];
  if (mode !== "entry-only" && mode !== "for-publish") {
    console.error(`Usage: tsx scripts/pack.ts <entry-only|for-publish>`);
    process.exit(1);
  }
  await buildDevEntries();
  if (mode === "for-publish") {
    await pack();
  }
}

main();
