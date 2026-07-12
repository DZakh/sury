// Deterministic runtime-cost measurement for the spec harness: retired
// instruction counts for building a schema (`ts.createPerf`) and for compiling
// each operation (`operations.<op>.compilePerf`).
//
// Unlike introspect.ts/bundleSize.ts, this can't run in-process: it needs a
// child Node launched under Valgrind's Callgrind. Each spec gets its own
// worker process (native/perf-worker.ts) so V8 inline-cache state can't leak
// between shapes; the native/callgrind.node addon fences each region and
// Callgrind writes one dump per region, which we read back here. See the
// engine notes in perf-worker.ts and the `spec` skill's Perf section.
import { spawn, execFileSync } from "node:child_process";
import { readdirSync, readFileSync, mkdtempSync, rmSync, existsSync } from "node:fs";
import { tmpdir, cpus } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { OP_ORDER, type OpName, type Spec } from "./format";
import { stripTypes } from "./harness";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
const ADDON = here("./native/callgrind.node");
const WORKER = here("./native/perf-worker.ts");
const SURY_ENTRY = here("../sury/src/S.mjs");

// The worker emits this warm fencing-floor region (a bare-constant return); we
// subtract it from every measured region so a schema that's just a constant
// reference (e.g. `S.string`) reads ~0 construction cost, not the ~800-2000
// instructions the fence + call themselves take.
const BASELINE = "__baseline";

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
        // --predictable: deterministic GC/scheduling so counts are exact.
        // --experimental-strip-types: run the .ts worker directly (a transpiling
        // loader like tsx perturbs the counts; type-stripping doesn't).
        "--predictable",
        "--experimental-strip-types",
        WORKER,
      ],
      {
        stdio: "ignore",
        env: {
          ...process.env,
          CG_ADDON: ADDON,
          SURY_ENTRY,
          // Strip TS-only syntax (e.g. `as const`) so the worker's `new
          // Function` sees plain JS, same as harness.evalSchema.
          SPEC_SCHEMA: stripTypes(req.schema),
          SPEC_OPS: req.ops.join(","),
        },
      },
    );
    child.on("close", (code) => {
      try {
        if (code !== 0) return resolve(null);
        const dumps = readDumps(dir);
        const baseline = dumps[BASELINE];
        if (baseline === undefined || dumps.create === undefined) return resolve(null);
        // Subtract the warm fencing floor; clamp so a region cheaper than the
        // baseline (a bare constant) reads 0 rather than a tiny negative.
        const net = (n: number): number => Math.max(0, n - baseline);
        const ops: Partial<Record<OpName, number>> = {};
        for (const op of req.ops) if (dumps[op] !== undefined) ops[op] = net(dumps[op]!);
        resolve({ create: net(dumps.create), ops });
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

// Folds measured perf into a spec's optional createPerf / compilePerf fields.
// A field already holding a number is band-compared (kept within tolerance,
// re-recorded past it); an absent field is filled with the measurement — so a
// valgrind-equipped read-only `spec check` surfaces a missing golden as a
// stale diff (canon lacks it, fresh has it) suggesting `--write`. `measured`
// undefined (no valgrind / worker failed for this metric) leaves the field
// exactly as-is, never inventing or discarding a value.
const put = (prior: number | undefined, measured: number | undefined): number | undefined =>
  measured === undefined ? prior : typeof prior === "number" ? bandKeep(prior, measured) : measured;

export const applyPerf = (spec: Spec, counts: PerfCounts | null): Spec => {
  const next = structuredClone(spec);
  const createPerf = put(next.ts.createPerf, counts?.create);
  if (createPerf === undefined) delete next.ts.createPerf;
  else next.ts.createPerf = createPerf;
  for (const opName of perfOps(next)) {
    const op = next.operations[opName] as { compilePerf?: number };
    const filled = put(op.compilePerf, counts?.ops[opName]);
    if (filled === undefined) delete op.compilePerf;
    else op.compilePerf = filled;
  }
  return next;
};
