// Derives the tree-shaken, minified+gzipped bundle size (`ts.bundleBytes`) for
// a schema's most representative real-world usage: `S.parser(schema)`.
//
// Vendors the same technique the project's own (now-removed) standalone
// tests/bundle.bench.ts project-health benchmark used: esbuild bundles a tiny
// entry that imports only what the schema/operation needs, aliasing the bare
// `sury` specifier to the dev source (src/S.js) so tree-shaking and size
// reflect exactly what's under test — not a stale published snapshot. That
// benchmark measured a handful of fixed, hand-picked scenarios against a
// committed JSON snapshot with a separate CI gate; this derives the number
// per spec instead, the same way ts.instantiations is derived, so `spec
// update` stays the single source of truth.
//
// Uses esbuild's async `build()` (not `buildSync`) so multiple specs' bundle
// measurements can run concurrently via Promise.all — each is an independent
// child-process build with no shared state, unlike the TS-introspection
// environment in introspect.ts, which genuinely needs to stay one memoized,
// sequential instance.
import { fileURLToPath } from "node:url";
import path from "node:path";
import { gzipSync } from "node:zlib";
import { build } from "esbuild";

const SURY_ROOT = fileURLToPath(new URL("../sury/", import.meta.url));
const SURY_ENTRY = path.join(SURY_ROOT, "src/S.js");

export const deriveBundleBytes = async (schemaTs: string): Promise<number> => {
  const code = `
    import * as S from "sury";
    export default S.parser(${schemaTs});
  `;
  const result = await build({
    stdin: { contents: code, resolveDir: SURY_ROOT, sourcefile: "entry.js", loader: "js" },
    bundle: true,
    minify: true,
    treeShaking: true,
    format: "esm",
    target: "es2020",
    legalComments: "none",
    write: false,
    // Resolve the bare `sury` specifier to the local dev entry so the
    // measurement reflects the code under test, not a published version.
    alias: { sury: SURY_ENTRY },
    // Silences esbuild's warning that package.json orders the "types" export
    // condition after "import"/"require" — unrelated to size. `build` still
    // rejects on real errors regardless of logLevel.
    logLevel: "silent",
  });
  const out = result.outputFiles![0]!.contents;
  return gzipSync(out, { level: 9 }).byteLength;
};
