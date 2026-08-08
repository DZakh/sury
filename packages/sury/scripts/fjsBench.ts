// fast-json-stringify's own benchmark suite, run against Sury.
//
//   pnpm --filter=sury bench:fjs
//
// Cases and inputs are read from the installed fast-json-stringify's
// benchmark/bench.js (its `benchmarks` array), so the comparison uses their
// schemas verbatim rather than ones picked to flatter Sury. Each case builds
// both serializers from the same JSON Schema document — fjs via its factory,
// Sury via `S.fromJSONSchema(...)` + `S.encoder(schema, S.jsonString)` — and
// times them in the same process with tinybench, the harness fjs uses.
//
// A case Sury can't build, or where the two disagree on output, is reported
// as such instead of being silently dropped.

import { Bench } from "tinybench";
import fastJson from "fast-json-stringify";
import { createRequire } from "node:module";
import * as S from "../index.mjs";

const require = createRequire(import.meta.url);
const benchPath = require.resolve("fast-json-stringify/benchmark/bench.js");

// bench.js has no exports and runs on import, so read its `benchmarks` array
// by evaluating the module body with its runner call stripped.
const src: string = require("node:fs").readFileSync(benchPath, "utf8");
const body = src
  .replace(/^'use strict'/, "")
  .replace(/const \{ Worker \}[^\n]*\n/, "")
  .replace(/runBenchmarks\(\)\s*$/, "")
  .replace(/async function runBenchmarks[\s\S]*$/, "")
  .replace(/async function runBenchmark[\s\S]*?^\}/m, "");
const benchmarks: { name: string; schema: any; input: unknown }[] = new Function(
  "require",
  "__dirname",
  `${body}; return benchmarks;`,
)(require, require("node:path").dirname(benchPath));

type Row = { name: string; fjs: string; sury: string; note: string };
const rows: Row[] = [];
const trim = (s: string): string => (s.length > 40 ? `${s.slice(0, 40)}…` : s);

const main = async (): Promise<void> => {
for (const b of benchmarks) {
  const name = b.name.replace(/\.+$/, "");
  const fjsFn = fastJson(b.schema);
  let suryFn: ((v: unknown) => string) | undefined;
  let note = "";
  try {
    suryFn = S.encoder(S.fromJSONSchema(b.schema) as any, S.jsonString) as any;
  } catch (e) {
    note = `unsupported: ${(e as Error).message.split("\n")[0]}`;
  }

  let fjsOut = "";
  try {
    fjsOut = fjsFn(b.input);
  } catch (e) {
    note = note || `fjs threw: ${(e as Error).message}`;
  }
  if (suryFn) {
    try {
      const suryOut = suryFn(b.input);
      if (suryOut !== fjsOut) {
        // Equivalent JSON with different text (key order, number form) still
        // counts as agreement; only a semantic difference is worth flagging.
        const eq =
          JSON.stringify(JSON.parse(suryOut)) === JSON.stringify(JSON.parse(fjsOut));
        note = eq ? "" : `differs: fjs=${trim(fjsOut)} sury=${trim(suryOut)}`;
      }
    } catch (e) {
      note = `sury threw: ${(e as Error).message.split("\n")[0]}`;
      suryFn = undefined;
    }
  }

  const bench = new Bench({
    time: 300,
    setup: (_t, mode) => {
      if (mode === "warmup" && typeof globalThis.gc === "function") globalThis.gc();
    },
  });
  bench.add("fjs", () => {
    fjsFn(b.input);
  });
  if (suryFn) {
    const fn = suryFn;
    bench.add("sury", () => {
      fn(b.input);
    });
  }
  await bench.run();
  const hz = (n: string): string => {
    const task = bench.tasks.find((t) => t.name === n);
    // tinybench moved ops/sec from `hz` to `throughput.mean`; accept either.
    const r = task?.result as
      | { hz?: number; throughput?: { mean: number } }
      | undefined;
    const v = r?.throughput?.mean ?? r?.hz;
    return v === undefined ? "—" : Math.round(v).toLocaleString("en-US");
  };
  rows.push({ name, fjs: hz("fjs"), sury: suryFn ? hz("sury") : "—", note });
}

  const cols: [keyof Row, string][] = [
    ["name", "case"],
    ["fjs", "fast-json-stringify"],
    ["sury", "Sury"],
  ];
  const width = (k: keyof Row, header: string): number =>
    Math.max(header.length, ...rows.map((r) => r[k].length));
  console.log(`node ${process.version} · ops/sec, higher is better\n`);
  console.log(cols.map(([k, h]) => h.padEnd(width(k, h))).join("  "));
  for (const r of rows) {
    const line = cols
      .map(([k, h]) => (k === "name" ? r[k].padEnd(width(k, h)) : r[k].padStart(width(k, h))))
      .join("  ");
    console.log(r.note ? `${line}  ${r.note}` : line);
  }
};

main();
