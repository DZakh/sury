// ns/op for a synchronous call, as the pages report it.
//
// The median of several rounds rather than the mean: one round landing on a GC
// pause reads double, and which library pays it is luck of the draw. The
// iteration count is derived from a probe run so every case takes roughly the
// same wall time whether it costs 50 ns or 50 µs.
const ROUNDS = 7;

export const timeNs = (fn: () => unknown): number => {
  const run = (n: number): number => {
    const start = process.hrtime.bigint();
    for (let i = 0; i < n; i++) fn();
    return Number(process.hrtime.bigint() - start) / n;
  };
  run(10_000);
  const iterations = Math.max(1000, Math.min(2_000_000, Math.round(20_000_000 / run(1000))));
  const samples: number[] = [];
  for (let i = 0; i < ROUNDS; i++) samples.push(run(iterations));
  return samples.sort((a, b) => a - b)[ROUNDS >> 1]!;
};

export const opsPerMs = (ns: number): number => 1e6 / ns;
