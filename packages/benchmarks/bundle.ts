// What a consumer ships for one job, per library: the entry snippet bundled,
// minified and gzipped.
//
// A snippet rather than a whole-library import, because the number that
// matters is what survives tree-shaking for the code someone actually wrote.
// Every entry is built in one esbuild call so the shared graph is parsed once.
//
// `sury` resolves to the dev entry, not to whatever is published, so a page
// generated from a working tree describes that working tree.
import { fileURLToPath } from "node:url";
import path from "node:path";
import { gzipSync } from "node:zlib";
import { build, type Plugin } from "esbuild";

const SURY_ROOT = fileURLToPath(new URL("../sury/", import.meta.url));
const SURY_ENTRY = path.join(SURY_ROOT, "index.mjs");
const NAMESPACE = "benchmark-entry";

export type Entry = { label: string; code: string };
export type Size = { minified: number; gzip: number };

export const measureBundles = async (entries: Entry[]): Promise<Map<string, Size>> => {
  const virtual: Plugin = {
    name: NAMESPACE,
    setup: (b) => {
      b.onResolve({ filter: new RegExp(`^${NAMESPACE}:`) }, (a) => ({ path: a.path, namespace: NAMESPACE }));
      b.onLoad({ filter: /.*/, namespace: NAMESPACE }, (a) => ({
        contents: entries[Number(a.path.slice(NAMESPACE.length + 1))]!.code,
        loader: "js",
        // Bare specifiers in a snippet resolve from this package, which is
        // where the competitor libraries are installed.
        resolveDir: fileURLToPath(new URL(".", import.meta.url)),
      }));
    },
  };

  const result = await build({
    entryPoints: entries.map((_, i) => ({ in: `${NAMESPACE}:${i}`, out: String(i) })),
    outdir: "out",
    absWorkingDir: fileURLToPath(new URL(".", import.meta.url)),
    plugins: [virtual],
    bundle: true,
    minify: true,
    treeShaking: true,
    format: "esm",
    target: "es2020",
    legalComments: "none",
    write: false,
    alias: { sury: SURY_ENTRY },
    // esbuild warns that sury's package.json orders "types" after "import";
    // unrelated to size, and a real error still rejects.
    logLevel: "silent",
  });

  const sizes = new Map<string, Size>();
  for (const file of result.outputFiles!) {
    const entry = entries[Number(path.basename(file.path, ".js"))]!;
    sizes.set(entry.label, { minified: file.contents.length, gzip: gzipSync(file.contents, { level: 9 }).byteLength });
  }
  return sizes;
};
