// Deterministic runtime-cost measurement for the spec harness: retired
// instruction counts for building a schema (`ts.createPerf`) and for compiling
// each operation (`operations.<op>.compilePerf`).
//
// Unlike introspect.ts/bundleSize.ts, this can't run in-process: it needs a
// child Node launched under Valgrind's Callgrind. Each spec gets its own
// worker process (native/perf-worker.mjs) so V8 inline-cache state can't leak
// between shapes; the native/callgrind.node addon fences each region and
// Callgrind writes one dump per region, which we read back here. See the
// engine notes in perf-worker.mjs and the `spec` skill's Perf section.
import { spawn, execFileSync } from "node:child_process";
import { readdirSync, readFileSync, mkdtempSync, rmSync, existsSync } from "node:fs";
import { tmpdir, cpus } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { OP_ORDER, NOT_MEASURED, isSkip, type OpName, type Spec } from "./format";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
const ADDON = here("./native/callgrind.node");
const WORKER = here("./native/perf-worker.mjs");
const SURY_ENTRY = here("../sury/src/S.mjs");

export type PerfCounts = { create: number; ops: Partial<Record<OpName, number>> };
export type PerfRequest = { id: string; schema: string; ops: OpName[] };

let valgrindOk: boolean | undefined;
const hasValgrind = (): boolean => {
  if (valgrindOk === undefined) {
    try {
      execFileSync("valgrind", ["--version"], { stdio: "ignore" });
      valgrindOk = true;
    } catch {
      valgrindOk = false;
    }
  }
  return valgrindOk;
};

// Perf goldens can only be measured where both the addon built and valgrind is
// installed. Everywhere else the harness leaves existing perf values untouched
// (see cli.ts) rather than blocking the rest of `spec check`.
export const perfUnavailableReason = (): string | undefined => {
  if (!existsSync(ADDON))
    return "native/callgrind.node not built — run `pnpm --filter=spec build:native` (needs a C compiler)";
  if (!hasValgrind()) return "valgrind not found on PATH — install valgrind to measure perf";
  return undefined;
};

// Each `CALLGRIND_DUMP_STATS_AT(name)` writes a file whose header carries the
// trigger name and whose `totals:` line is the region's retired-instruction
// count. Termination dumps (name not among our regions) are ignored.
const readDumps = (dir: string): Record<string, number> => {
  const out: Record<string, number> = {};
  for (const f of readdirSync(dir)) {
    const text = readFileSync(join(dir, f), "utf8");
    const name = text.match(/^desc: Trigger: Client Request:\s*(.+)$/m)?.[1]?.trim();
    const totals = text.match(/^totals:\s*(\d+)/m)?.[1];
    if (name && totals) out[name] = Number(totals);
  }
  return out;
};

const measureOne = (req: PerfRequest): Promise<PerfCounts | null> =>
  new Promise((resolve) => {
    const dir = mkdtempSync(join(tmpdir(), "sury-perf-"));
    const child = spawn(
      "valgrind",
      [
        "--tool=callgrind",
        "--instr-atstart=no",
        "--quiet",
        `--callgrind-out-file=${join(dir, "cg")}`,
        process.execPath,
        "--predictable",
        WORKER,
      ],
      {
        stdio: "ignore",
        env: {
          ...process.env,
          CG_ADDON: ADDON,
          SURY_ENTRY,
          SPEC_SCHEMA: req.schema,
          SPEC_OPS: req.ops.join(","),
        },
      },
    );
    child.on("close", (code) => {
      try {
        if (code !== 0) return resolve(null);
        const dumps = readDumps(dir);
        if (dumps.create === undefined) return resolve(null);
        const ops: Partial<Record<OpName, number>> = {};
        for (const op of req.ops) if (dumps[op] !== undefined) ops[op] = dumps[op];
        resolve({ create: dumps.create, ops });
      } finally {
        rmSync(dir, { recursive: true, force: true });
      }
    });
    child.on("error", () => {
      rmSync(dir, { recursive: true, force: true });
      resolve(null);
    });
  });

// Bounded to the core count: each worker pins a core under valgrind, so more
// than that just adds context-switching. Returns id -> counts (or null for a
// spec whose worker failed, so the caller keeps that spec's prior goldens).
export const derivePerf = async (
  reqs: PerfRequest[],
): Promise<Map<string, PerfCounts | null>> => {
  const limit = Math.max(1, cpus().length);
  const result = new Map<string, PerfCounts | null>();
  let next = 0;
  const worker = async (): Promise<void> => {
    while (next < reqs.length) {
      const req = reqs[next++]!;
      result.set(req.id, await measureOne(req));
    }
  };
  await Promise.all(Array.from({ length: Math.min(limit, reqs.length) }, worker));
  return result;
};

// ±1% band, matching bundleBytes: instruction counts are exact within one
// Node/V8 build, but shift slightly across toolchain versions. Within the band
// the recorded golden is kept so an unrelated Node bump doesn't churn every
// spec at once; a real codegen change (>1%) re-records exactly.
const PERF_TOLERANCE = 0.01;
const bandKeep = (prior: unknown, measured: number): number =>
  typeof prior === "number" && Math.abs(measured - prior) <= prior * PERF_TOLERANCE ? prior : measured;

// The non-identity operations whose compilePerf this spec should carry.
export const perfOps = (spec: Spec): OpName[] =>
  OP_ORDER.filter((op) => spec.operations[op] !== "identity");

// Folds perf goldens into a spec.
//
// A `not-measured` golden is a not-yet-baselined field: it only gates once it
// holds a real number. So `write` matters — read-only `spec check` KEEPS a
// `not-measured` (no staleness against a machine that could measure), while
// `--write` upgrades it to the measured count. A field that already holds a
// number always gates (band-compared) in both modes; the tolerance keeps an
// unrelated toolchain wobble from churning every spec at once. `counts === null`
// (no valgrind / worker failed) only ensures the required field exists, never
// discarding a baselined number.
const put = (prior: unknown, measured: number | undefined, write: boolean): unknown => {
  if (measured === undefined) return typeof prior === "number" || isSkip(prior) ? prior : NOT_MEASURED;
  if (typeof prior === "number") return bandKeep(prior, measured);
  if (isSkip(prior)) return write ? measured : prior; // upgrade a not-measured only on --write
  return write ? measured : NOT_MEASURED;
};

export const applyPerf = (spec: Spec, counts: PerfCounts | null, write: boolean): Spec => {
  const next = structuredClone(spec);
  next.ts.createPerf = put(next.ts.createPerf, counts?.create, write) as Spec["ts"]["createPerf"];
  for (const opName of perfOps(next)) {
    const op = next.operations[opName] as { compilePerf?: unknown };
    op.compilePerf = put(op.compilePerf, counts?.ops[opName], write);
  }
  return next;
};
