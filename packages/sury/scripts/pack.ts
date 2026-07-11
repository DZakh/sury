// Build & packaging script. Run via tsx:
//
//   pnpm build:entry  -> tsx scripts/pack.ts entry-only
//   pnpm build        -> tsx scripts/pack.ts for-publish
//
// Stage 1 (always): bundle src/entry.ts (the single public entry re-exporting
// src/core/*.ts) into src/S.mjs, plus an identical ESM src/S.js twin (kept so
// relative imports and the S.d.ts adjacency lookup keep working in the dev
// repo, where package.json has "type": "module"). Both are gitignored; this
// stage keeps them fresh (it runs before rescript/vitest via pnpm scripts).
// The ReScript bindings (S.res) reference the same entry as
// `@module("sury")`, resolved through the package's "." conditional export —
// which is why the entry must exist in both formats in the published package
// (see stage 2) for consumers compiling ReScript to commonjs.
//
// Stage 2 (full pack only): assemble the publishable package in ./artifacts —
// copy sources, compile ReScript there, overwrite the artifact's S.js with a
// CJS build (the "." require condition), produce a CJS S.res.js for ReScript
// consumers that don't run the compiler (with "sury" kept external so the
// implementation ships exactly once), and flip package.json to commonjs.

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

// ── Stage 1: S.ts -> S.mjs + S.js (ESM) ──────────────────────────────────────

async function buildEntry(format: "esm" | "cjs", outfile: string): Promise<void> {
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
        `/* @ts-self-types="./S.d.ts" */`,
        "// Generated from S.ts by scripts/pack.ts, PLEASE EDIT WITH CARE",
      ].join("\n"),
    },
    logLevel: "silent",
  });
}

async function buildDevEntries(): Promise<void> {
  await buildEntry("esm", path.join(projectPath, "src/S.mjs"));
  // ESM twin under the .js name: package.json has "type": "module" in the dev
  // repo, and S.js is what tests/tools import relatively (S.d.ts adjacency).
  fs.copyFileSync(path.join(projectPath, "src/S.mjs"), path.join(projectPath, "src/S.js"));
}

// ── Stage 2: the publishable artifact ────────────────────────────────────────

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

// Inline the "rescript" runtime dependency into the compiled S.res output, so
// ReScript consumers that don't run the compiler don't need it installed. The
// `sury` self-import stays external — the implementation must ship exactly
// once (S.mjs / CJS S.js), or mixed usage would load two instances (two Exn
// identities, two schema caches).
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

  execaSync("pnpm", ["rescript"], { cwd: artifactsPath });

  // The artifact package is commonjs (see below), so its S.js must be the CJS
  // build — the "." require condition points at it.
  await buildEntry("cjs", path.join(artifactsPath, "src/S.js"));

  // CJS build of the ReScript-facing module, in case some ReScript libraries
  // will use sury without running a compiler (rescript-stdlib-vendorer)
  await resolveRescriptRuntime("es", "src/S.res.mjs", "src/S.res.mjs");
  await resolveRescriptRuntime("cjs", "src/S.res.mjs", "src/S.res.js");

  // ReScript applications don't work with type: module set on packages
  updateJsonFile(path.join(artifactsPath, "package.json"), ["type"], "commonjs");
  updateJsonFile(path.join(artifactsPath, "package.json"), ["private"], false);

  // Clean up before uploading artifacts
  fs.rmSync(path.join(artifactsPath, "lib"), { force: true, recursive: true });
  fs.rmSync(path.join(artifactsPath, "node_modules"), { force: true, recursive: true });
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
