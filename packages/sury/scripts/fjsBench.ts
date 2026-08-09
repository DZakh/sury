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
// The strip above is pinned to bench.js's current layout, which is a private
// path fjs can change in any release. Without this, a stripped-to-nothing body
// silently benchmarks zero cases and prints an empty table.
if (!Array.isArray(benchmarks) || benchmarks.length === 0) {
  throw new Error(
    `Could not extract \`benchmarks\` from ${benchPath} — fast-json-stringify's benchmark layout changed.`,
  );
}

type Row = { name: string; fjs: string; sury: string; note: string };
const rows: Row[] = [];
const trim = (s: string): string => (s.length > 40 ? `${s.slice(0, 40)}…` : s);

const main = async (): Promise<void> => {
for (const b of benchmarks) {
  const name = b.name.replace(/\.+$/, "");
  let note = "";

  // Every step below is attributed to the side it came from. Sharing one
  // try/catch made a fjs failure surface as `sury threw` and drop Sury's row:
  // their `format: "unsafe"` cases emit unescaped (invalid) JSON on purpose,
  // and the comparison's `JSON.parse` was the thing that threw.
  let fjsFn: ((v: unknown) => string) | undefined;
  try {
    fjsFn = fastJson(b.schema) as (v: unknown) => string;
  } catch (e) {
    note = `fjs unsupported: ${(e as Error).message.split("\n")[0]}`;
  }
  let suryFn: ((v: unknown) => string) | undefined;
  try {
    suryFn = S.encoder(S.fromJSONSchema(b.schema) as any, S.jsonString) as any;
  } catch (e) {
    note = note || `unsupported: ${(e as Error).message.split("\n")[0]}`;
  }

  let fjsOut: string | undefined;
  if (fjsFn) {
    try {
      fjsOut = fjsFn(b.input);
    } catch (e) {
      note = note || `fjs threw: ${(e as Error).message.split("\n")[0]}`;
      fjsFn = undefined;
    }
  }
  let suryOut: string | undefined;
  if (suryFn) {
    try {
      suryOut = suryFn(b.input);
    } catch (e) {
      note = `sury threw: ${(e as Error).message.split("\n")[0]}`;
      suryFn = undefined;
    }
  }

  if (fjsOut !== undefined && suryOut !== undefined && suryOut !== fjsOut) {
    // Equivalent JSON with different text (key order, number form) still
    // counts as agreement; only a semantic difference is worth flagging.
    const canonical = (s: string): string | undefined => {
      try {
        return JSON.stringify(JSON.parse(s));
      } catch {
        return undefined;
      }
    };
    const suryJson = canonical(suryOut);
    const fjsJson = canonical(fjsOut);
    if (suryJson === undefined) {
      note = `sury emitted invalid JSON: ${trim(suryOut)}`;
    } else if (fjsJson === undefined) {
      note = `fjs emitted invalid JSON: ${trim(fjsOut)}`;
    } else if (suryJson !== fjsJson) {
      note = `differs: fjs=${trim(fjsOut)} sury=${trim(suryOut)}`;
    }
  }

  const bench = new Bench({
    time: 300,
    setup: (_t, mode) => {
      if (mode === "warmup" && typeof globalThis.gc === "function") globalThis.gc();
    },
  });
  if (fjsFn) {
    const fn = fjsFn;
    bench.add("fjs", () => {
      fn(b.input);
    });
  }
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
