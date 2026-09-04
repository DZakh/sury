// Bundle+minify+gzip every public export of the dev entry (index.mjs) in
// isolation, for `bundleSize.yaml` — the whole-package size ratchet.
//
// One row per export, rather than per schema: a schema's bundle cost is the
// cost of the exports it reaches plus the byte cost of its own source literal
// (the author's text, not library code), and bundle reachability from several
// entry symbols is the union of their graphs — so no composite measurement can
// grow without at least one export row growing too. Per-export rows attribute
// a regression to the module that caused it, and cover surface no schema
// expression reaches at all (`inputJSONSchema`, `fromJSONSchema`).
//
// The bare `sury` specifier is aliased to the dev source so tree-shaking and
// size reflect exactly what's under test, not a stale published snapshot.
import { fileURLToPath, pathToFileURL } from "node:url";
import path from "node:path";
import { gzipSync } from "node:zlib";
import { build, type Plugin } from "esbuild";
import type { BundleSize } from "./format";

const SURY_ROOT = fileURLToPath(new URL("../sury/", import.meta.url));
const SURY_ENTRY = path.join(SURY_ROOT, "index.mjs");

const NAMESPACE = "bundle-size-entry";

// A member read on a namespace import — the shape real consumer code takes.
// esbuild folds the literal key, so this tree-shakes exactly like `S.foo`
// while also handling names that are reserved words (`S["void"]`).
const exportEntry = (name: string): string =>
  `import * as S from "sury";\nexport default S[${JSON.stringify(name)}];\n`;

// The whole entry, as the anchor row: per-export sizes share the core runtime,
// so they don't sum to anything meaningful, and `total` is what answers "did
// the library grow".
const TOTAL_ENTRY = `export * from "sury";\n`;

export const deriveBundleSize = async (): Promise<BundleSize> => {
  const names = Object.keys(await import(pathToFileURL(SURY_ENTRY).href)).sort();
  const entries = [...names.map(exportEntry), TOTAL_ENTRY];

  // Every entry point in ONE esbuild invocation (with `splitting` off, each
  // output is an independent bundle) so the source graph is parsed once —
  // ~5x faster than a build() per export, byte-for-byte identical output.
  const virtual: Plugin = {
    name: NAMESPACE,
    setup: (b) => {
      b.onResolve({ filter: new RegExp(`^${NAMESPACE}:`) }, (a) => ({ path: a.path, namespace: NAMESPACE }));
      b.onLoad({ filter: /.*/, namespace: NAMESPACE }, (a) => ({
        contents: entries[Number(a.path.slice(NAMESPACE.length + 1))],
        loader: "js",
        resolveDir: SURY_ROOT,
      }));
    },
  };

  const result = await build({
    entryPoints: entries.map((_, i) => ({ in: `${NAMESPACE}:${i}`, out: String(i) })),
    // Nothing is written (write: false), but esbuild needs an outdir to name
    // multiple outputs — the index in that name is how sizes map back.
    outdir: "out",
    absWorkingDir: SURY_ROOT,
    plugins: [virtual],
    bundle: true,
    minify: true,
    treeShaking: true,
    format: "esm",
    target: "es2020",
    legalComments: "none",
    write: false,
    alias: { sury: SURY_ENTRY },
    // Silences esbuild's warning that package.json orders the "types" export
    // condition after "import"/"require" — unrelated to size. `build` still
    // rejects on real errors regardless of logLevel.
    logLevel: "silent",
  });

  // Recorded exactly — no tolerance band. A band (formerly ±1%, on the removed
  // per-spec dimension) let consistent sub-1% drift accumulate against stale
  // goldens and misattributed the whole delta to whichever change finally
  // crossed the line. A toolchain bump now re-records every row at once, which
  // is the honest diff.
  const sizes = new Map<number, number>();
  for (const file of result.outputFiles!)
    sizes.set(Number(path.basename(file.path, ".js")), gzipSync(file.contents, { level: 9 }).byteLength);

  return {
    total: sizes.get(names.length)!,
    exports: Object.fromEntries(names.map((name, i) => [name, sizes.get(i)!])),
  };
};
