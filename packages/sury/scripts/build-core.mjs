// Builds src/core.ts into the two runtime artifacts consumed via the
// "sury/core" package export (see package.json "exports"."./core"):
//   src/core.mjs (import condition) and src/core.cjs (require condition).
//
// This lets Sury.res bind to it as `@module("sury/core")` regardless of
// which module format a consumer's own ReScript compiler targets — a plain
// relative `@module("./core.mjs")` would break under a "commonjs" target
// (require()-ing an ESM file throws ERR_REQUIRE_ESM).
//
// core.ts has no runtime imports (see its header comment), so this is a
// straight transpile, not a bundle.
//
// Run directly (`node scripts/build-core.mjs`) or via `pnpm build:core`;
// also runs as a pretest/prebuild step (see package.json).

import { build } from "esbuild";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.join(__dirname, "..");
const entryPoint = path.join(projectRoot, "src/core.ts");

async function buildFormat(format, outfile) {
  await build({
    entryPoints: [entryPoint],
    outfile,
    bundle: false,
    write: true,
    format,
    target: "es2020",
    platform: "neutral",
    banner: { js: "// Generated from core.ts by scripts/build-core.mjs, PLEASE EDIT WITH CARE" },
  });
}

await buildFormat("esm", path.join(projectRoot, "src/core.mjs"));
await buildFormat("cjs", path.join(projectRoot, "src/core.cjs"));
