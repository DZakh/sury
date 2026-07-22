// TEMPORARY perf harness — measures wall time of schema *creation* and
// operation *compilation* against the bundled dev build (src/S.mjs), the
// exact code that ships. Not part of the spec ratchet; delete before merge.
//
//   pnpm build:entry && node --expose-gc bench-perf.mjs [--save baseline.json] [--vs baseline.json]
//
// Reports per-op nanoseconds (median of ROUNDS, each the mean of BATCH ops)
// for two dimensions per shape:
//   create        — build the schema graph from scratch
//   compile       — build a fresh schema AND compile its decoder (cold path);
//                   the isolated codegen cost is `compile - create`.
import * as S from "./src/S.mjs";

const BATCH = Number(process.env.BATCH || 2000);
const ROUNDS = Number(process.env.ROUNDS || 25);
const WARMUP = Number(process.env.WARMUP || 8);

const gc = globalThis.gc || (() => {});

// Each shape is a thunk returning a freshly-built schema. Reusing eager
// primitive constants (S.string, …) is intentional — that's real usage.
const shapes = {
  string: () => S.string,
  "string.min": () => S.string.with(S.min, 3),
  int32: () => S.int32,
  literalStr: () => S.schema("hello"),
  literalObj: () => S.schema({ kind: "a", value: S.number }),
  object1: () => S.schema({ foo: S.string }),
  object5: () =>
    S.schema({ a: S.string, b: S.number, c: S.boolean, d: S.string, e: S.number }),
  object10: () =>
    S.schema({
      a: S.string, b: S.number, c: S.boolean, d: S.string, e: S.number,
      f: S.string, g: S.number, h: S.boolean, i: S.string, j: S.number,
    }),
  objectNested: () =>
    S.schema({
      id: S.string,
      user: S.schema({ name: S.string, age: S.number }),
      tags: S.array(S.string),
    }),
  objectOptional: () =>
    S.schema({ a: S.string, b: S.optional(S.number), c: S.optional(S.string) }),
  tuple2: () => S.tuple([S.string, S.number]),
  tuple10: () =>
    S.tuple([
      S.string, S.number, S.boolean, S.string, S.number,
      S.boolean, S.string, S.number, S.boolean, S.string,
    ]),
  array: () => S.array(S.string),
  arrayObject: () => S.array(S.schema({ a: S.string, b: S.number })),
  record: () => S.record(S.string, S.number),
  union5: () => S.union([S.string, S.number, S.boolean, S.null, S.undefined]),
  unionDiscriminated: () =>
    S.union([
      S.schema({ kind: "a", a: S.string }),
      S.schema({ kind: "b", b: S.number }),
      S.schema({ kind: "c", c: S.boolean }),
    ]),
  transform: () => S.string.with(S.to, S.number),
  objectTransform: () =>
    S.schema({ foo: S.string.with(S.to, S.number), bar: S.number }),
};

const median = (xs) => {
  const s = [...xs].sort((a, b) => a - b);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};

// Measure per-op ns for `fn` over ROUNDS rounds of BATCH calls each. A sink
// accumulator defeats dead-code elimination of the returned schema/fn.
const measure = (fn) => {
  let sink = 0;
  for (let w = 0; w < WARMUP; w++) {
    for (let i = 0; i < BATCH; i++) sink += typeof fn(i);
  }
  const perOp = [];
  for (let r = 0; r < ROUNDS; r++) {
    gc();
    const t0 = process.hrtime.bigint();
    for (let i = 0; i < BATCH; i++) sink += typeof fn(i);
    const t1 = process.hrtime.bigint();
    perOp.push(Number(t1 - t0) / BATCH);
  }
  if (sink === -1) console.log("");
  return median(perOp);
};

const results = {};
for (const [name, make] of Object.entries(shapes)) {
  const create = measure(() => make());
  // Fresh schema each call → seq++ → no operation-cache hit → real compile.
  const compile = measure(() => S.decoder(make()));
  results[name] = { create, compile, codegen: compile - create };
}

const args = process.argv.slice(2);
const save = args.includes("--save") ? args[args.indexOf("--save") + 1] : null;
const vsPath = args.includes("--vs") ? args[args.indexOf("--vs") + 1] : null;

let baseline = null;
if (vsPath) {
  try {
    baseline = JSON.parse(await (await import("node:fs/promises")).readFile(vsPath, "utf8"));
  } catch {}
}

const pad = (s, n) => String(s).padEnd(n);
const padL = (s, n) => String(s).padStart(n);
const fmt = (ns) => ns.toFixed(0);
const delta = (cur, base) => {
  if (base == null) return "";
  const pct = ((cur - base) / base) * 100;
  const sign = pct >= 0 ? "+" : "";
  return `${sign}${pct.toFixed(1)}%`;
};

console.log(
  `\n${pad("shape", 20)}${padL("create", 9)}${padL("Δ", 8)}${padL("compile", 10)}${padL("Δ", 8)}${padL("codegen", 10)}${padL("Δ", 8)}`
);
console.log("-".repeat(73));
let totCreate = 0, totCompile = 0, totCodegen = 0;
let baseCreate = 0, baseCompile = 0, baseCodegen = 0;
for (const [name, r] of Object.entries(results)) {
  const b = baseline?.[name];
  totCreate += r.create; totCompile += r.compile; totCodegen += r.codegen;
  if (b) { baseCreate += b.create; baseCompile += b.compile; baseCodegen += b.codegen; }
  console.log(
    pad(name, 20) +
      padL(fmt(r.create), 9) + padL(delta(r.create, b?.create), 8) +
      padL(fmt(r.compile), 10) + padL(delta(r.compile, b?.compile), 8) +
      padL(fmt(r.codegen), 10) + padL(delta(r.codegen, b?.codegen), 8)
  );
}
console.log("-".repeat(73));
console.log(
  pad("TOTAL", 20) +
    padL(fmt(totCreate), 9) + padL(baseline ? delta(totCreate, baseCreate) : "", 8) +
    padL(fmt(totCompile), 10) + padL(baseline ? delta(totCompile, baseCompile) : "", 8) +
    padL(fmt(totCodegen), 10) + padL(baseline ? delta(totCodegen, baseCodegen) : "", 8)
);
console.log("\n(ns per op; codegen = compile − create)");

if (save) {
  await (await import("node:fs/promises")).writeFile(save, JSON.stringify(results, null, 2));
  console.log(`\nsaved baseline → ${save}`);
}
