// Bundle-size & tree-shaking benchmark.
//
// Sury's third core goal (after DX and runtime perf) is bundle size — see
// CLAUDE.md. This is the size analogue of the runtime bench (sury.bench.ts) and
// the type-instantiation bench (types.bench.ts): each scenario is a tiny entry
// module that imports only specific APIs, esbuild bundles + minifies it, and we
// gzip the result. Because the entry touches only part of the surface, the
// output size *is* the tree-shaking measurement — `string` should pull in a
// fraction of what `total` does. The `/*#__PURE__*/` annotations in src/S.js are
// what make that possible, so this guards them against regression.
//
// Run:   pnpm bench:bundle            (check against the committed snapshot)
//        pnpm bench:bundle --update   (re-baseline after an intentional change)
//        pnpm bench:bundle --tolerance=2
//
// Measures the dev source (src/S.js -> src/Sury.res.mjs), so run after the
// ReScript compiler has produced a current Sury.res.mjs (CI runs `pnpm build`
// first; locally `pnpm res` keeps it fresh). The published artifact inlines the
// same Sury.res.mjs, so the bundled size matches what npm consumers ship.

import { build } from "esbuild";
import { gzipSync, brotliCompressSync, constants } from "node:zlib";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const SURY_ROOT = path.join(__dirname, "..");
const SURY_ENTRY = path.join(SURY_ROOT, "src/S.js");
const SNAPSHOT_PATH = path.join(__dirname, "bundle-size.snapshot.json");

const args = process.argv.slice(2);
const UPDATE = args.includes("--update");
const TOLERANCE_PCT = Number(
  (args.find((a) => a.startsWith("--tolerance=")) || "--tolerance=1").split("=")[1]
);

// Each entry imports only what it names; the bundler drops the rest. Keep these
// aligned with the bundlejs recipes in CONTRIBUTING.md and the README table.
const SCENARIOS = [
  {
    // Whole public surface — the library ceiling (README "Total size").
    name: "total (export *)",
    code: `export * from "sury";`,
  },
  {
    // Smallest useful program: one primitive + a compiled parser. The gap
    // between this and `total` is the headline tree-shaking win.
    name: "string + parser",
    code: `
      import * as S from "sury";
      export default S.parser(S.string);
    `,
  },
  {
    // The schema used by the runtime and comparison benches (README
    // "Benchmark size").
    name: "object schema + parser",
    code: `
      import * as S from "sury";
      const schema = S.schema({
        number: S.number, negNumber: S.number, maxNumber: S.number,
        string: S.string, longString: S.string, boolean: S.boolean,
        deeplyNested: { foo: S.string, num: S.number, bool: S.boolean },
      });
      export default S.parser(schema);
    `,
  },
  {
    // Union dispatch + a string->number transform branch.
    name: "union + transform",
    code: `
      import * as S from "sury";
      export default S.parser(S.union([{ box: S.string }, S.string.with(S.to, S.number)]));
    `,
  },
  {
    // S.recursive together with union dispatch and array recursion.
    name: "recursive + array",
    code: `
      import * as S from "sury";
      const tree = S.recursive("Tree", (self) =>
        S.union([S.schema({ type: "node", children: S.array(self) }), S.string]));
      export default S.parser(tree);
    `,
  },
  {
    // JSON Schema generation — a heavier, mostly self-contained feature. Its
    // cost should land on users of `toJSONSchema` only, not on plain parsers.
    name: "toJSONSchema",
    code: `
      import * as S from "sury";
      export default S.toJSONSchema(S.schema({ a: S.string, b: S.number }));
    `,
  },
];

async function measure(scenario) {
  const result = await build({
    stdin: {
      contents: scenario.code,
      resolveDir: SURY_ROOT,
      sourcefile: "entry.js",
      loader: "js",
    },
    bundle: true,
    minify: true,
    treeShaking: true,
    format: "esm",
    target: "es2020",
    legalComments: "none",
    write: false,
    // Resolve the bare `sury` specifier to the local dev entry so scenario code
    // reads like real consumer code.
    alias: { sury: SURY_ENTRY },
    // src/S.js intentionally re-exports a couple of names that don't yet exist
    // on Sury.res.mjs; those warnings are unrelated to size. `build` still
    // rejects on real errors regardless of logLevel.
    logLevel: "silent",
  });
  const out = result.outputFiles[0].contents;
  return {
    min: out.byteLength,
    gzip: gzipSync(out, { level: 9 }).byteLength,
    brotli: brotliCompressSync(out, {
      params: { [constants.BROTLI_PARAM_QUALITY]: 11 },
    }).byteLength,
  };
}

const esbuildVersion = JSON.parse(
  readFileSync(path.join(SURY_ROOT, "node_modules/esbuild/package.json"), "utf8")
).version;

const kB = (n) => (n / 1024).toFixed(2) + " kB";
const padR = (s, w) => String(s).padEnd(w);
const padL = (s, w) => String(s).padStart(w);

const measured = {};
for (const s of SCENARIOS) {
  measured[s.name] = await measure(s);
}

// ── Update mode ────────────────────────────────────────────────────────────
if (UPDATE || !existsSync(SNAPSHOT_PATH)) {
  const snapshot = {
    _comment:
      "Baseline sizes for the bundle-size & tree-shaking benchmark. Regenerate with `pnpm bench:bundle --update` after an intentional size change. Assertions are on `gzip`.",
    esbuildVersion,
    scenarios: measured,
  };
  writeFileSync(SNAPSHOT_PATH, JSON.stringify(snapshot, null, 2) + "\n");
  console.log(`\nWrote baseline for ${SCENARIOS.length} scenarios to`);
  console.log(`  ${path.relative(process.cwd(), SNAPSHOT_PATH)} (esbuild ${esbuildVersion})\n`);
  for (const s of SCENARIOS) {
    const m = measured[s.name];
    console.log(`  ${padR(s.name, 24)} ${padL(kB(m.gzip), 10)} gzip`);
  }
  console.log("");
  process.exit(0);
}

// ── Check mode ───────────────────────────────────────────────────────────────
const snapshot = JSON.parse(readFileSync(SNAPSHOT_PATH, "utf8"));
if (snapshot.esbuildVersion !== esbuildVersion) {
  console.log(
    `\n⚠  esbuild ${esbuildVersion} differs from snapshot ${snapshot.esbuildVersion}; ` +
      `size drift may be the bundler, not Sury. Re-baseline with --update if so.`
  );
}

const NAME_W = Math.max(...SCENARIOS.map((s) => s.name.length), "scenario".length);
console.log("\nSury bundle size & tree-shaking (esbuild, minified)\n");
console.log(
  `${padR("scenario", NAME_W)}  ${padL("min", 9)}  ${padL("min+gzip", 9)}  ` +
    `${padL("brotli", 9)}  ${padL("Δ gzip", 16)}  status`
);
console.log("-".repeat(NAME_W + 56));

const failures = [];
for (const s of SCENARIOS) {
  const cur = measured[s.name];
  const base = snapshot.scenarios[s.name];
  let delta = "—";
  let status = "NEW";
  if (base) {
    const diff = cur.gzip - base.gzip;
    const pct = (diff / base.gzip) * 100;
    delta = `${diff >= 0 ? "+" : ""}${diff} B (${pct >= 0 ? "+" : ""}${pct.toFixed(2)}%)`;
    if (Math.abs(pct) <= TOLERANCE_PCT) {
      status = "ok";
    } else {
      status = diff > 0 ? "REGRESSION" : "improved";
      failures.push({ name: s.name, diff, pct, base: base.gzip, cur: cur.gzip });
    }
  } else {
    failures.push({ name: s.name, diff: cur.gzip, pct: Infinity, base: 0, cur: cur.gzip });
  }
  console.log(
    `${padR(s.name, NAME_W)}  ${padL(kB(cur.min), 9)}  ${padL(kB(cur.gzip), 9)}  ` +
      `${padL(kB(cur.brotli), 9)}  ${padL(delta, 16)}  ${status}`
  );
}
console.log("");

if (failures.length) {
  console.error(
    `✗ ${failures.length} scenario(s) drifted beyond ±${TOLERANCE_PCT}% gzip:\n`
  );
  for (const f of failures) {
    const kind = f.base === 0 ? "no baseline" : f.diff > 0 ? "regression" : "improvement";
    console.error(
      `   ${f.name}: ${f.base} → ${f.cur} B (${f.pct >= 0 ? "+" : ""}${f.pct.toFixed(2)}%, ${kind})`
    );
  }
  console.error(
    `\n  If intentional, re-baseline:  pnpm bench:bundle --update\n`
  );
  process.exit(1);
}

console.log(`✓ all ${SCENARIOS.length} scenarios within ±${TOLERANCE_PCT}% of baseline\n`);
