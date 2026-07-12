// The measured child: launched once per spec under
// `valgrind --tool=callgrind --instr-atstart=no node --predictable`.
//
// Exactly ONE schema shape is ever built in this process, so V8's inline
// caches never go megamorphic across shapes — that's what makes the fenced
// counts exactly reproducible (a shared process measuring many shapes drifts a
// few % by measurement order). Boot is excluded because instrumentation starts
// OFF and each region is fenced with the callgrind addon.
//
// Env in: CG_ADDON (callgrind.node path), SURY_ENTRY (dev S.mjs path),
// SPEC_SCHEMA (schema source), SPEC_OPS (comma-separated ops to compile).
// Out: valgrind writes one dump per region; perf.ts reads the counts back.
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const cg = require(process.env.CG_ADDON);
const S = await import(process.env.SURY_ENTRY);

const src = process.env.SPEC_SCHEMA;
const ops = (process.env.SPEC_OPS || "").split(",").filter(Boolean);

// One Function compile; the returned arrow rebuilds a FRESH schema each call so
// operation compiles hit Sury's per-schema compile path, not its parser cache.
const factory = new Function("S", `return () => (${src});`)(S);
const build = { parse: S.parser, decode: S.decoder, encode: S.encoder };

// Warm every code path this shape touches (build + all three directions) so no
// fenced region below is the first to trigger a lazy-initialized global — that
// one-time cost would otherwise land on whichever region ran first.
for (let i = 0; i < 10; i++) {
  const w = factory();
  S.parser(w);
  S.decoder(w);
  S.encoder(w);
}

cg.start("create");
const created = factory();
cg.stop("create");
if (!created) process.exit(2);

for (const op of ops) {
  const compile = build[op];
  if (!compile) continue;
  const fresh = factory(); // built OUTSIDE the fence so create cost isn't counted
  cg.start(op);
  compile(fresh);
  cg.stop(op);
}
