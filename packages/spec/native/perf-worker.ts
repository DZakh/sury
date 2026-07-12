// The measured child: launched once per spec under
// `valgrind --tool=callgrind --instr-atstart=no node --predictable
//  --experimental-strip-types`.
//
// Run as .ts via Node's native type-stripping — NOT a transpiling loader like
// tsx, which perturbs the instruction counts; stripping only removes type
// annotations, leaving execution (and its counts) identical to plain JS.
//
// Exactly ONE schema shape is ever built here, so V8's inline caches never go
// megamorphic across shapes — that's what makes the fenced counts exactly
// reproducible (a shared process measuring many shapes drifts a few % by
// measurement order). Boot is excluded because instrumentation starts OFF and
// each region is fenced with the callgrind addon.
//
// Env in: CG_ADDON (callgrind.node path), SURY_ENTRY (dev S.mjs path),
// SPEC_SCHEMA (schema source), SPEC_OPS (comma-separated ops to compile).
// Out: valgrind writes one dump per region; perf.ts reads the counts back and
// subtracts the __baseline region from each.
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const cg: { start: (name: string) => void; stop: (name: string) => void } = require(
  process.env.CG_ADDON!,
);
const S: any = await import(process.env.SURY_ENTRY!);

const ops = (process.env.SPEC_OPS || "").split(",").filter(Boolean);

// One Function compile; the returned arrow rebuilds a FRESH schema each call so
// operation compiles hit Sury's per-schema compile path, not its parser cache.
const factory: () => unknown = new Function("S", `return () => (${process.env.SPEC_SCHEMA});`)(S);
const build: Record<string, (s: unknown) => unknown> = {
  parse: S.parser,
  decode: S.decoder,
  encode: S.encoder,
};

// Baseline: an arrow returning a bare constant — the fencing + call floor every
// measured region below also pays. perf.ts subtracts it, so a schema that's
// just a constant reference (`S.string`) reads ~0 construction, not the floor.
const noop = (): unknown => S.string;

// Warm every code path a measured region touches (build + all directions + the
// baseline) so no fenced region is the first to hit a lazy-initialized global.
for (let i = 0; i < 10; i++) {
  const w = factory();
  S.parser(w);
  S.decoder(w);
  S.encoder(w);
  noop();
}

// A full GC on a clean slate just BEFORE each region (never between start/stop,
// so it's never counted) empties the heap, so V8 can't trigger a GC INSIDE the
// fence and pollute the count with millions of collection instructions. Live
// values built before the fence (e.g. `fresh` below) survive it. --expose-gc
// makes global.gc available.
const gc = (): void => (globalThis as { gc?: () => void }).gc?.();

// A discarded region first: the extra one-time cost the FIRST fenced region
// pays lands here, not on __baseline or a real measurement.
gc();
cg.start("__warm");
noop();
cg.stop("__warm");

gc();
cg.start("__baseline");
noop();
cg.stop("__baseline");

gc();
cg.start("create");
const created = factory();
cg.stop("create");
if (!created) process.exit(2);

for (const op of ops) {
  const compile = build[op];
  if (!compile) continue;
  const fresh = factory(); // built OUTSIDE the fence so create cost isn't counted
  gc();
  cg.start(op);
  compile(fresh);
  cg.stop(op);
}
