// CPU time, collector time and retained memory per operation, the three
// costs a wall-clock benchmark hides. Run under `node --expose-gc`: the
// retained figure needs a forced collection on both sides of holding 2,000
// results.
import { PerformanceObserver } from "node:perf_hooks";
import * as S from "sury";
import { libraries, WORKLOADS } from "./bench";
import { suryMessage } from "./cases";

export type CompareRow = {
  work: string;
  library: string;
  op: "encode" | "decode";
  cpuUsPerOp: number;
  gcNsPerOp: number;
  retainedBytes: number;
};

export const runCompare = async (opsPerCell = 3e6): Promise<CompareRow[]> => {
  const gc = (globalThis as { gc?: () => void }).gc;
  if (!gc) throw new Error("run with node --expose-gc");
  let gcTime = 0;
  new PerformanceObserver((list) => {
    for (const entry of list.getEntries()) gcTime += entry.duration;
  }).observe({ entryTypes: ["gc"] });
  const heap = (): number => {
    gc();
    gc();
    return process.memoryUsage().heapUsed;
  };
  const rows: CompareRow[] = [];
  for (const work of WORKLOADS) {
    const bytes = S.decoder(suryMessage(work.fields), S.protobuf)(work.value);
    const n = Math.max(20000, Math.round(opsPerCell / (bytes.length + 20)));
    for (const library of libraries) {
      const codec = await library.codec(work, bytes);
      for (const op of ["encode", "decode"] as const) {
        const fn = codec[op];
        for (let i = 0; i < 2000; i++) fn();
        gc();
        gcTime = 0;
        const cpu0 = process.cpuUsage();
        for (let i = 0; i < n; i++) fn();
        const cpu = process.cpuUsage(cpu0);
        await new Promise((resolve) => setTimeout(resolve, 20));
        const keep = 2000;
        const before = heap();
        const held: unknown[] = [];
        for (let i = 0; i < keep; i++) held.push(fn());
        const after = heap();
        held.length = 0;
        rows.push({
          work: work.id,
          library: library.id,
          op,
          cpuUsPerOp: (cpu.user + cpu.system) / n,
          gcNsPerOp: (gcTime * 1e6) / n,
          retainedBytes: (after - before) / keep,
        });
      }
    }
  }
  return rows;
};

export const formatCompare = (rows: CompareRow[]): string => {
  const lines = ["work     library              op      cpu µs/op   gc ns/op  retained B"];
  for (const r of rows) {
    lines.push(
      `${r.work.padEnd(8)} ${r.library.padEnd(20)} ${r.op.padEnd(7)} ${r.cpuUsPerOp.toFixed(3).padStart(9)} ${r.gcNsPerOp.toFixed(0).padStart(10)} ${r.retainedBytes.toFixed(0).padStart(11)}`,
    );
  }
  return lines.join("\n");
};
