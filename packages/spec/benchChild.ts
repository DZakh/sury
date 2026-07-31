// The measurement half of `--perf`: runs in a forked process, one per spec,
// with BOTH library versions loaded at once.
//
// Interleaving is the whole point. Wall-clock on a laptop or a shared CI
// runner drifts by far more than the deltas worth catching, so nothing here
// reports an absolute time — only the ratio between batches that ran
// milliseconds apart, which drift affects equally. Rounds are ordered ABBA
// rather than ABAB: under ABAB any linear drift within a round biases whichever
// side runs second, while ABBA cancels it exactly.
//
// Bundled to .bench-cache/child.mjs (see bench.ts) instead of run through tsx,
// because 32 tsx startups would cost more than the measurement itself.
import type { ChildPayload, ChildResult, Target } from "./bench";

const OP_BUILDER = { parse: "parser", decode: "decoder", encode: "encoder" } as const;

// Every measured value is stored into a box so V8 can't delete the work as
// dead. The boxes are kept alive here (and read at exit) so escape analysis
// can't scalar-replace them either.
const boxes: { v: unknown }[] = [];

// Each runner is built with its own `new Function` rather than by closing over
// a shared loop: closures created at the same site can share a feedback vector,
// which would make the inner call megamorphic across the 100+ targets in a run
// and measure the driver instead of the schema. A distinct function per target
// per side keeps every call site monomorphic.
// Returns the runner, plus — for run-phase targets — whether the operation
// threw on this input. The two sides are timed against each other, so an
// outcome that differs between them makes the ratio meaningless: returning a
// value and raising a `SuryError` are different work, not the same work at a
// different speed.
const buildRunner = (
  S: any,
  target: Target,
): { run: (n: number) => void; threw?: boolean } => {
  const box: { v: unknown } = { v: undefined };
  boxes.push(box);
  const factory = new Function("S", `return ${target.schemaSrc};`) as (s: any) => unknown;

  if (target.phase === "create")
    return {
      run: new Function(
        "factory",
        "S",
        "box",
        "return (n) => { for (let i = 0; i < n; i++) box.v = factory(S); };",
      )(factory, S, box),
    };

  const builder = S[OP_BUILDER[target.op!]];

  if (target.phase === "create+compile")
    return {
      run: new Function(
        "factory",
        "S",
        "build",
        "box",
        "return (n) => { for (let i = 0; i < n; i++) box.v = build(factory(S)); };",
      )(factory, S, builder, box),
    };

  // Input is evaluated per side, not shared: an operation that mutates its
  // input would otherwise have one side's runs observed by the other.
  const op = builder(factory(S));
  const input = new Function(`return ${target.inputSrc};`)();
  let threw = false;
  try {
    op(input);
  } catch (_) {
    threw = true;
  }
  const run = target.throws
    ? new Function(
        "op",
        "input",
        "box",
        "return (n) => { for (let i = 0; i < n; i++) { try { box.v = op(input); } catch (e) { box.v = e; } } };",
      )(op, input, box)
    : new Function("op", "input", "box", "return (n) => { for (let i = 0; i < n; i++) box.v = op(input); };")(
        op,
        input,
        box,
      );
  return { run, threw };
};

const time = (run: (n: number) => void, n: number): number => {
  const start = process.hrtime.bigint();
  run(n);
  return Number(process.hrtime.bigint() - start);
};

// A batch has to be long enough that the two clock reads around it are noise
// rather than signal — at ~25ns per `hrtime` call, a 1ms batch puts clock
// overhead under 0.1%. (Timing each iteration individually, which is what
// tinybench does, would measure the clock ~10x more than a `S.string` parse.)
const MAX_BATCH = 1 << 24;
const calibrate = (run: (n: number) => void, targetNs: number): number => {
  let n = 1;
  for (;;) {
    const elapsed = time(run, n);
    if (elapsed >= targetNs || n >= MAX_BATCH) return n;
    const scale = elapsed > 0 ? Math.max(2, Math.min(64, Math.ceil((targetNs / elapsed) * 1.2))) : 64;
    n = Math.min(MAX_BATCH, n * scale);
  }
};

const measure = (baseline: any, current: any, payload: ChildPayload, target: Target): ChildResult => {
  let a: { run: (n: number) => void; threw?: boolean };
  let b: { run: (n: number) => void; threw?: boolean };
  try {
    a = buildRunner(baseline, target);
  } catch (e) {
    // The baseline predates whatever this target needs — a new schema, a new
    // API. Reported as "new", not as a failure.
    return { name: target.name, unsupported: (e as Error).message };
  }
  try {
    // A control measures the baseline against itself, so its reported delta is
    // pure noise — that's how a run states its own confidence.
    b = buildRunner(target.control ? baseline : current, target);
  } catch (e) {
    return { name: target.name, error: (e as Error).message };
  }

  // Timing them against each other would compare a returned value with a
  // thrown error and report the difference as a slowdown — a correctness fix
  // that starts rejecting an input shows up as several hundred times "slower".
  if (a.threw !== b.threw)
    return {
      name: target.name,
      outcomeChanged: a.threw ? "baseline rejected it, now accepted" : "baseline accepted it, now rejected",
    };

  const n = Math.max(calibrate(a.run, payload.batchTargetNs), calibrate(b.run, payload.batchTargetNs));
  // Long enough for both sides to reach their final tier. A side still being
  // re-optimised when measurement starts stays slow for the whole target, which
  // no amount of interleaving can cancel — it is not drift, it is one side
  // running different machine code than it will a moment later.
  for (let i = 0; i < payload.warmupBatches; i++) {
    a.run(n);
    b.run(n);
  }

  // Each block reduces to the ratio of its two FASTEST batches. Scheduler
  // noise is strictly additive — an interrupted batch is slow, never fast — so
  // the minimum is the one estimator that noise cannot move, while an average
  // or a median over rounds treats every interrupt as signal. Blocks then give
  // back the repetition needed for an interval (see conservativePct): a delta
  // is only reported when every block independently agrees on its direction.
  // A round is ABBA followed by BAAB, which is the shortest sequence giving both
  // sides the same set of positions. Plain ABBA does not: it puts B's two
  // batches back to back while A's are separated by B's work, so B's minimum
  // starts from a warmer cache every time — invisible when the round is reduced
  // by a sum, but a standing bias once it is reduced by a minimum.
  const ratios: number[] = [];
  for (let block = 0; block < payload.blocks; block++) {
    // Once per block, not per batch (a full collection costs more than a
    // batch does). Creation targets allocate hard enough to drive the heap
    // through collection cycles, and without a reset the two sides enter the
    // block at different points in that cycle — which decides who pays for the
    // next collection, systematically rather than randomly.
    global.gc?.();
    let minA = Infinity;
    let minB = Infinity;
    for (let round = 0; round < payload.roundsPerBlock; round++) {
      const a1 = time(a.run, n);
      const b1 = time(b.run, n);
      const b2 = time(b.run, n);
      const a2 = time(a.run, n);
      const b3 = time(b.run, n);
      const a3 = time(a.run, n);
      const a4 = time(a.run, n);
      const b4 = time(b.run, n);
      minA = Math.min(minA, a1, a2, a3, a4);
      minB = Math.min(minB, b1, b2, b3, b4);
    }
    ratios.push(minB / minA);
  }
  return { name: target.name, batch: n, ratios };
};

const readStdin = (): Promise<string> =>
  new Promise((resolve, reject) => {
    let raw = "";
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => (raw += chunk));
    process.stdin.on("end", () => resolve(raw));
    process.stdin.on("error", reject);
  });

const main = async (): Promise<void> => {
  const payload: ChildPayload = JSON.parse(await readStdin());
  const [baseline, current] = await Promise.all([import(payload.baseline), import(payload.current)]);

  const results: ChildResult[] = [];
  for (const target of payload.targets) {
    try {
      results.push(measure(baseline, current, payload, target));
    } catch (e) {
      results.push({ name: target.name, error: (e as Error).message });
    }
  }

  // Reading the boxes keeps every stored value observable, so none of the
  // measured work can be optimised away as unused.
  process.stdout.write(JSON.stringify({ results, sink: boxes.length }));
};

main();
