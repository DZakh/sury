// The type probes (typeProbe.ts) run on worker threads: the compiler API is
// synchronous, so only a thread of its own lets probes overlap.
import { createRequire } from "node:module";
import { availableParallelism } from "node:os";
import { pathToFileURL } from "node:url";
import { Worker } from "node:worker_threads";
import type { Probe, probes } from "./typeProbe";

export type TypeInfo = {
  input: string;
  output: string;
  instantiations: number;
  fromInput?: string;
  fromOutput?: string;
  inputMatches?: boolean;
  outputMatches?: boolean;
};

// One core stays with the main thread, which runs every spec's examples.
const MAX_WORKERS = Math.max(1, availableParallelism() - 1);
// Resolved rather than inherited from execArgv: under Vitest there is no tsx
// flag to inherit, and the worker entry is TypeScript.
const TSX = pathToFileURL(createRequire(import.meta.url).resolve("tsx")).href;

type Job = { probe: Probe; args: unknown[]; resolve: (v: any) => void; reject: (e: Error) => void };
const queue: Job[] = [];
const idle: (() => void)[] = [];
let workers = 0;

const spawn = (): void => {
  const worker = new Worker(new URL("./typeProbe.ts", import.meta.url), { execArgv: ["--import", TSX] });
  workers++;
  let job: Job | undefined;
  // Unref'd while idle so a finished run exits without tearing the pool down,
  // ref'd while a probe is out so the process waits for its reply.
  const next = (): void => {
    job = queue.shift();
    if (job) {
      worker.ref();
      worker.postMessage([job.probe, job.args]);
    } else {
      worker.unref();
      idle.push(next);
    }
  };
  worker.on("message", ([ok, value]: [boolean, any]) => {
    const done = job!;
    next();
    if (ok) done.resolve(value);
    else done.reject(new Error(value));
  });
  worker.on("error", (e) => {
    workers--;
    job?.reject(e);
    job = undefined;
    if (queue.length) spawn();
  });
  next();
};

const run =
  <P extends Probe>(probe: P) =>
  (...args: Parameters<(typeof probes)[P]>): Promise<ReturnType<(typeof probes)[P]>> =>
    new Promise((resolve, reject) => {
      queue.push({ probe, args, resolve, reject });
      const wake = idle.pop();
      if (wake) wake();
      else if (workers < MAX_WORKERS) spawn();
    });

// The count carries a fixed per-builder-kind dispatch cost on top of per-field
// cost, so it doesn't compare across kinds: a plain value like `S.string`
// measures far lower than any `S.schema({...})` call regardless of field count.
// A jump for one kind of schema and not another is a real signal, not noise.
export const deriveTypeInfo = run("deriveTypeInfo");

export const deriveRoundTripTypeInfo = run("deriveRoundTripTypeInfo");

// The inferred input/output type strings of a `vs` cross-library schema, read
// through the Standard Schema (`~standard`) interface rather than any one
// library's own `Infer*` helper - so the same probe works for every
// Standard-Schema vendor (Zod today, Valibot/ArkType tomorrow) and reads the
// value's *published* type contract, exactly what a downstream user gets. No
// instantiation count - only Sury's own schema owns that golden.
export const deriveVsTypeInfo = run("deriveVsTypeInfo");
