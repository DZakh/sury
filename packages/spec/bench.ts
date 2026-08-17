// The relative-performance dimension of `spec check`.
//
// Nothing here is snapshotted. A "baseline" is the library itself, bundled
// from a git ref, so there is no number to commit, go stale, or be quietly
// edited to make a regression disappear — the only thing recorded anywhere is
// the ref you compared against. Both versions are then measured in one process,
// interleaved (see benchChild.ts), and only their ratio is reported, which is
// what makes the result mean something on a laptop under load or a shared CI
// runner.
//
// Every run measures a few control pairs — the baseline against itself — and
// reports the largest delta they produce as that run's noise floor. So a report
// states its own confidence instead of needing a separate calibration command,
// and nothing below that floor is shown.
import { execFileSync, spawn } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { cpus } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { build } from "esbuild";
import { OP_ORDER, isCreationError, type OpName } from "./format";
import { evalSchema, readScenarios, readSpec, scenarioSource, specId, stripTypes } from "./harness";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
const REPO_ROOT = here("../../");
const CACHE = here("./.bench-cache/");
const SURY_SRC = "packages/sury/src";

// Long enough that the two clock reads bracketing a batch are noise rather
// than signal: ~25ns each against a 500µs batch is under 0.01%, which leaves
// no reason to pay for a longer one — and batches are the bulk of a run.
const BATCH_TARGET_NS = 500_000;
// Half the cores for the screening pass, so processes overlap without every
// one fighting for a core. A two-core CI runner falls back to serial.
const SCREEN_JOBS = Math.max(1, Math.floor(cpus().length / 2));
const WARMUP_BATCHES = 20;
// Confidence comes from the BLOCK count, not the round count: the interval
// spans every block, so a delta is reported only when all of them independently
// agree on its direction. If block ratios were pure noise that happens with
// probability 2^(1-BLOCKS) — ~3% at six blocks, which over ~150 targets is a
// handful of false positives every run, and ~0.2% at ten. Rounds within a block
// only sharpen its minimum, so blocks are the cheaper place to spend: ten
// blocks of three rounds costs a quarter more than six of four and is an order
// of magnitude stricter.
const BLOCKS = 8;
const ROUNDS_PER_BLOCK = 2;
// A whole child process can land in one JIT state and stay there — IC and
// feedback shapes settle early, after which every block in that process agrees
// with itself. The identical build has measured "unchanged" and "−44%" against
// the same baseline in back-to-back runs, each individually "confirmed", so
// within-process repetition (blocks) cannot see this failure mode at all. A
// candidate is therefore confirmed by fresh PROCESSES, and kept only when the
// screening process and every confirm process agree on direction.
const CONFIRM_PROCESSES = 2;
// Controls are sampled PER PHASE, and the floor is computed per phase too,
// because the phases are not equally measurable: `create` allocates millions of
// schemas and so runs against the garbage collector, which is a real cost but a
// far noisier one than a `run` target that allocates nothing. A single pooled
// floor would either drown creation's noise in run's quiet or suppress genuine
// run regressions to accommodate creation.
const CONTROLS_PER_PHASE = 6;
// Nothing below this is reported even on a perfectly quiet machine — at some
// point a real but sub-noise delta is not actionable, and listing it trains
// the reader to ignore the section.
const MIN_FLOOR_PCT = 3;
const PHASES = ["create", "create+compile", "run", "scenario"] as const;

export type Phase = "create" | "create+compile" | "run" | "scenario";

export type Target = {
  name: string;
  specId: string;
  phase: Phase;
  op?: OpName;
  /** Build the operation with the async builder — the only way an async schema compiles. */
  isAsync?: boolean;
  /** Type-stripped already: the child has no TypeScript to strip it with. */
  schemaSrc?: string;
  /** Every example of this target's outcome, run in one batch. */
  inputSrcs?: string[];
  /** Parallel to `inputSrcs`, so a changed outcome can name the example. */
  exampleNames?: string[];
  /** `scenario` phase only — same type-stripped contract as schemaSrc. */
  prepareSrc?: string;
  runSrc?: string;
  throws: boolean;
  control: boolean;
};

export type ChildPayload = {
  baseline: string;
  current: string;
  targets: Target[];
  batchTargetNs: number;
  warmupBatches: number;
  blocks: number;
  roundsPerBlock: number;
};

export type ChildResult =
  | { name: string; batch: number; ratios: number[] }
  | { name: string; unsupported: string }
  | { name: string; error: string }
  // The two sides disagree on whether the input is accepted, so there is no
  // like-for-like timing to report — only the behavior change itself.
  | { name: string; outcomeChanged: string };

export type PerfResult = { name: string; phase: Phase; pct: number; median: number; batch: number };

export type Perf = {
  baselineLabel: string;
  baselineSha: string;
  floors: { phase: Phase; pct: number }[];
  changed: PerfResult[];
  unchanged: number;
  added: string[];
  skippedConstants: number;
  skippedAsync: number;
  errors: { name: string; error: string }[];
  // Targets whose accept/reject outcome moved. Not timings — a behavior change
  // that a percentage would misreport as an enormous slowdown.
  outcomeChanged: { name: string; note: string }[];
  meta: string;
};

// ---- git -------------------------------------------------------------------

const git = (...args: string[]): string =>
  execFileSync("git", args, { cwd: REPO_ROOT, encoding: "utf8", maxBuffer: 1 << 28 });

const gitLine = (...args: string[]): string => git(...args).trim();

// Explicit `--against` wins; otherwise CI compares against the PR's base and a
// local run against the point the branch left main — the anchor that stays put
// for a whole change, so every measurement in a session shares one "before".
export const resolveBaseline = (against?: string): { sha: string; label: string } => {
  const resolve = (rev: string, label: string) => {
    try {
      return { sha: gitLine("rev-parse", rev), label };
    } catch {
      throw new Error(
        `could not resolve baseline ${JSON.stringify(rev)} — pass --against <ref>` +
          (process.env.CI ? " (CI checkouts are shallow by default; fetch-depth: 0 is needed)" : ""),
      );
    }
  };
  if (against) return resolve(against, against);
  const base = process.env.GITHUB_BASE_REF;
  if (base) return resolve(`origin/${base}`, `origin/${base}`);
  try {
    return { sha: gitLine("merge-base", "HEAD", "main"), label: "merge-base with main" };
  } catch {
    return resolve("HEAD", "HEAD");
  }
};

// ---- bundling --------------------------------------------------------------

// Both sides are bundled by this one function so the comparison can never
// include a difference in how they were built. `absWorkingDir` is why it takes
// a root: esbuild stamps each module's path into the output as a comment, so
// without it the baseline (which lives under the cache dir) and the working
// tree would differ byte-for-byte in a comparison that is supposed to isolate
// the code itself.
const bundleEntry = (root: string, entry: string, outfile: string): Promise<unknown> =>
  build({
    entryPoints: [entry],
    absWorkingDir: root,
    outfile,
    bundle: true,
    write: true,
    format: "esm",
    target: "es2020",
    platform: "neutral",
    logLevel: "silent",
  });

// Sury has no runtime dependencies, so a checkout of `src` at any ref bundles
// standalone — no worktree, no install.
const materializeBaseline = async (sha: string): Promise<string> => {
  const out = join(CACHE, `${sha}.mjs`);
  if (existsSync(out)) return out;
  const srcDir = join(CACHE, `src-${sha}`);
  for (const file of gitLine("ls-tree", "-r", "--name-only", sha, "--", SURY_SRC).split("\n").filter(Boolean)) {
    const dest = join(srcDir, file);
    mkdirSync(dirname(dest), { recursive: true });
    writeFileSync(dest, git("show", `${sha}:${file}`));
  }
  await bundleEntry(srcDir, join(srcDir, SURY_SRC, "entry.ts"), out);
  return out;
};

const buildChild = (): Promise<unknown> =>
  build({
    entryPoints: [here("./benchChild.ts")],
    outfile: join(CACHE, "child.mjs"),
    bundle: true,
    write: true,
    format: "esm",
    platform: "node",
    target: "node20",
    logLevel: "silent",
  });

// ---- targets ---------------------------------------------------------------

const SEP = " · ";

// Skipped rather than measured for a schema whose source is a module-level
// constant (`S.string`): there is nothing to construct, and its compiled
// operation is cached on the singleton, so a second call measures the cache.
const isConstantSchema = (src: string): boolean => evalSchema(src) === evalSchema(src);

// Scenarios are selected by their own id, since they aren't files: a narrowed
// run naming only spec ids gets no scenarios, and vice versa.
export const deriveTargets = (
  files: string[],
  scenarioIds?: string[],
): { targets: Target[]; skippedConstants: number; skippedAsync: number } => {
  const targets: Target[] = [];
  let skippedConstants = 0;
  let skippedAsync = 0;

  for (const [id, scenario] of Object.entries(readScenarios())) {
    if (scenarioIds && !scenarioIds.includes(id)) continue;
    targets.push({
      name: `${id}${SEP}scenario`,
      specId: id,
      phase: "scenario",
      ...scenarioSource(scenario),
      throws: false,
      control: false,
    });
  }

  for (const file of files) {
    const id = specId(file);
    const spec = readSpec(file);
    let constant: boolean;
    let schemaSrc: string;
    try {
      constant = isConstantSchema(spec.ts.schema);
      schemaSrc = stripTypes(spec.ts.schema);
    } catch {
      // Not evaluable — `check`'s golden pass reports that properly; there is
      // nothing useful to measure here.
      continue;
    }
    const base = { specId: id, schemaSrc, throws: false, control: false };

    if (constant) skippedConstants++;
    else targets.push({ ...base, name: `${id}${SEP}create`, phase: "create" });

    for (const op of OP_ORDER) {
      const block = spec.operations[op];
      if (typeof block === "string") continue;
      // Rejected at operation creation: there is no compiled operation to time
      // and no examples to run. Timing how fast it throws would measure error
      // construction, not the schema.
      if (isCreationError(block)) continue;
      const isAsync = block.isAsync === true;
      if (!constant)
        targets.push({ ...base, name: `${id}${SEP}create+compile${SEP}${op}`, phase: "create+compile", op, isAsync });
      // Compiling an async operation is ordinary synchronous work (above), but
      // running one is not: the batch loop can only start the promises, so the
      // resolution it is supposed to be timing lands in microtasks after the
      // clock is read. Counted, not silently dropped — the report says how many.
      if (isAsync) {
        skippedAsync += Object.keys(block.examples).length;
        continue;
      }
      // One target per outcome, its batch iterating every example of that
      // outcome, rather than one target per example. Same coverage at a third
      // of the child processes — and no example has to be elected the
      // representative, which nothing can do well: the first example is within
      // 5% of its group's cheapest 66% of the time, and the longest input is
      // the priciest only 42% of the time.
      //
      // Accepted and rejected stay apart. One loop over both would have to run
      // behind the try/catch the rejecting side needs, which times the catch
      // rather than the schema.
      for (const throws of [false, true]) {
        const examples = Object.entries(block.examples).filter(
          ([, ex]) => !("output" in ex) === throws,
        );
        if (examples.length === 0) continue;
        targets.push({
          ...base,
          name: `${id}${SEP}${op}${SEP}${throws ? "rejects" : "accepts"}${
            examples.length > 1 ? ` ×${examples.length}` : ""
          }`,
          phase: "run",
          op,
          inputSrcs: examples.map(([, ex]) => stripTypes(ex.input)),
          exampleNames: examples.map(([name]) => name),
          throws,
        });
      }
    }
  }

  // Spread within each phase rather than clustered, so a floor reflects
  // conditions throughout the run and not just at the start.
  const controls: Target[] = [];
  for (const phase of PHASES) {
    const inPhase = targets.filter((t) => t.phase === phase);
    const stride = Math.max(1, Math.floor(inPhase.length / CONTROLS_PER_PHASE));
    for (let i = 0, taken = 0; i < inPhase.length && taken < CONTROLS_PER_PHASE; i += stride, taken++)
      controls.push({ ...inPhase[i]!, name: `control${SEP}${inPhase[i]!.name}`, control: true });
  }

  return { targets: [...targets, ...controls], skippedConstants, skippedAsync };
};

// ---- statistics ------------------------------------------------------------

const sorted = (xs: number[]): number[] => [...xs].sort((a, b) => a - b);

const choose = (n: number, k: number): number => {
  let r = 1;
  for (let i = 0; i < k; i++) r = (r * (n - i)) / (i + 1);
  return r;
};

// Largest k with P(Bin(n,½) ≤ k) ≤ 0.025, so the order-statistic interval
// [r₍ₖ₊₁₎, r₍n₋ₖ₎] covers the median ratio with ≥95% confidence. Distribution-free
// on purpose: creation targets allocate hard enough that a GC spike lands in
// some round of every run, and that would drag a mean-and-stddev interval.
// At the six blocks this runs, k is 0 — the interval is the full range, so
// every block has to land on the same side of 1 for anything to be reported.
const ciRank = (n: number): number => {
  const total = 2 ** n;
  let cum = 0;
  let k = -1;
  for (let i = 0; i < n; i++) {
    cum += choose(n, i);
    if (cum / total > 0.025) break;
    k = i;
  }
  return k;
};

// The edge of the interval nearest "no change", so a wide interval reports as
// nothing rather than as its (equally likely) optimistic end. Without this a
// noisy +20% ±30% becomes "18% faster" in a commit message.
export const conservativePct = (ratios: number[]): number => {
  const s = sorted(ratios);
  const k = ciRank(s.length);
  if (k < 0) return 0;
  const lo = s[k]!;
  const hi = s[s.length - 1 - k]!;
  if (lo <= 1 && hi >= 1) return 0;
  return ((lo > 1 ? lo : hi) - 1) * 100;
};

const medianOf = (xs: number[]): number => {
  const s = sorted(xs);
  const mid = s.length >> 1;
  return s.length % 2 ? s[mid]! : (s[mid - 1]! + s[mid]!) / 2;
};

// ---- run -------------------------------------------------------------------

export const runPerf = async (
  files: string[],
  against?: string,
  scenarioIds?: string[],
): Promise<Perf> => {
  const { sha, label } = resolveBaseline(against);
  mkdirSync(CACHE, { recursive: true });

  const currentPath = join(CACHE, "current.mjs");
  const [baselinePath] = await Promise.all([
    materializeBaseline(sha),
    bundleEntry(REPO_ROOT, join(REPO_ROOT, SURY_SRC, "entry.ts"), currentPath),
    buildChild(),
  ]);

  const { targets, skippedConstants, skippedAsync } = deriveTargets(files, scenarioIds);
  const childPath = join(CACHE, "child.mjs");
  const payloadFor = (list: Target[]): ChildPayload => ({
    baseline: pathToFileURL(baselinePath).href,
    current: pathToFileURL(currentPath).href,
    targets: list,
    batchTargetNs: BATCH_TARGET_NS,
    warmupBatches: WARMUP_BATCHES,
    blocks: BLOCKS,
    roundsPerBlock: ROUNDS_PER_BLOCK,
  });

  // Progress is a redrawn line, so it's for a terminal only — in CI (where the
  // run is captured into an artifact) it would be one line per target.
  const progress = (text: string) => process.stderr.isTTY && process.stderr.write(text);

  const runChild = (target: Target): Promise<ChildResult[]> =>
    new Promise((resolve, reject) => {
      // Deliberately no NODE_COMPILE_CACHE here. It saves ~9ms of the ~70ms
      // startup, but the two bundles are byte-identical, so the second import
      // deserializes the first one's cached bytecode instead of compiling its
      // own — and the sides then enter measurement in different states. Trying
      // it produced reproducible, direction-consistent phantoms up to 37% (the
      // same targets, to a tenth of a percent, run after run).
      const child = spawn(process.execPath, ["--expose-gc", childPath], { stdio: ["pipe", "pipe", "inherit"] });
      let out = "";
      child.stdout.setEncoding("utf8");
      child.stdout.on("data", (chunk) => (out += chunk));
      child.on("error", reject);
      child.on("close", (code) =>
        code === 0
          ? resolve(JSON.parse(out).results as ChildResult[])
          : reject(new Error(`measuring ${target.name} exited with ${code}`)),
      );
      child.stdin.end(JSON.stringify(payloadFor([target])));
    });

  // One process per target, so every target starts from an identical fresh
  // heap: grouped by spec, targets inherited whatever the previous one left
  // behind, which mattered most for the creation targets that churn it hardest.
  const measureAll = async (list: Target[], label: string, jobs: number): Promise<ChildResult[]> => {
    const out: ChildResult[] = [];
    let next = 0;
    let done = 0;
    const worker = async (): Promise<void> => {
      for (let i = next++; i < list.length; i = next++) {
        out.push(...(await runChild(list[i]!)));
        progress(`\rperf ${label} ${++done}/${list.length}${" ".repeat(12)}`);
      }
    };
    await Promise.all(Array.from({ length: Math.max(1, Math.min(jobs, list.length)) }, worker));
    progress(`\r${" ".repeat(78)}\r`);
    return out;
  };

  const real = targets.filter((t) => !t.control);
  const controls = targets.filter((t) => t.control);

  const added: string[] = [];
  const errors: { name: string; error: string }[] = [];
  const outcomeChanged: { name: string; note: string }[] = [];
  const collect = (results: ChildResult[]) => {
    const map = new Map<string, { pct: number; median: number; batch: number }>();
    for (const r of results) {
      if ("error" in r) errors.push({ name: r.name, error: r.error });
      else if ("unsupported" in r) added.push(r.name);
      else if ("outcomeChanged" in r) outcomeChanged.push({ name: r.name, note: r.outcomeChanged });
      else map.set(r.name, { pct: conservativePct(r.ratios), median: medianOf(r.ratios), batch: r.batch });
    }
    return map;
  };

  // Screening runs in parallel. Contention makes it noisier, but noise only
  // widens an interval — it can hide a regression, never invent one — and
  // everything that survives is re-measured serially below. The bar here is
  // deliberately the loosest one, since a false candidate costs a second and a
  // missed one is gone for good.
  const screened = collect(await measureAll(real, "measure", SCREEN_JOBS));
  const candidates = real.filter((t) => Math.abs(screened.get(t.name)?.pct ?? 0) >= MIN_FLOOR_PCT);

  // Controls are measured HERE, not in the screening pass, so the floors they
  // set are established under the same quiet serial conditions as the values
  // those floors gate. Screened under contention they would read as noisier
  // than the run they describe, and suppress real regressions to match.
  // Each confirm pass spawns a fresh child process per target (see measureAll),
  // so the CONFIRM_PROCESSES passes are independent JIT states, not repeats of
  // one. Controls ride along in EVERY pass, not just the first: the floor they
  // set gates the candidates, so a floor read from a single process would keep
  // the one-sample failure mode this loop exists to remove.
  const confirmRuns: Map<string, { pct: number; median: number; batch: number }>[] = [];
  for (let i = 0; i < CONFIRM_PROCESSES; i++)
    confirmRuns.push(
      collect(await measureAll([...candidates, ...controls], `confirm ${i + 1}/${CONFIRM_PROCESSES}`, 1)),
    );

  const floors = PHASES.map((phase) => {
    const measured = controls
      .filter((c) => c.phase === phase)
      .flatMap((c) => confirmRuns.flatMap((run) => run.get(c.name) ?? []));
    // Both statistics: the conservative bound catches a control that produced
    // an outright false positive, the median the subtler case of a phase that
    // is visibly biased without any single control clearing the bar.
    return {
      phase,
      pct: Math.max(
        MIN_FLOOR_PCT,
        ...measured.map((m) => Math.abs(m.pct)),
        ...measured.map((m) => Math.abs(m.median - 1) * 100),
      ),
    };
  });
  const floorFor = (phase: Phase) => floors.find((f) => f.phase === phase)!.pct;

  // Kept only if every serial re-measurement agrees on direction, reporting the
  // smallest magnitude. Deliberately not an average: re-measuring only the
  // large values and pooling them pulls a real regression toward the mean
  // exactly as hard as a false one, so this confirms rather than estimates.
  const changed = candidates
    .flatMap((t) => {
      const first = screened.get(t.name)!;
      const samples = [first];
      for (const run of confirmRuns) {
        const again = run.get(t.name);
        if (!again || Math.sign(again.pct) !== Math.sign(first.pct)) return [];
        samples.push(again);
      }
      const pct = Math.sign(first.pct) * Math.min(...samples.map((s) => Math.abs(s.pct)));
      if (Math.abs(pct) < floorFor(t.phase)) return [];
      const last = samples[samples.length - 1]!;
      return [{ name: t.name, phase: t.phase, pct, median: last.median, batch: last.batch }];
    })
    .sort((a, b) => b.pct - a.pct);

  const cpu = cpus();
  return {
    baselineLabel: label,
    baselineSha: sha.slice(0, 7),
    floors,
    changed,
    unchanged: real.length - changed.length,
    added: [...new Set(added)],
    skippedConstants,
    skippedAsync,
    // Deduped by name like outcomeChanged: collect runs over the screening pass
    // plus every confirm pass, so one target can report the same failure twice.
    errors: [...new Map(errors.map((e) => [e.name, e])).values()],
    outcomeChanged: [...new Map(outcomeChanged.map((o) => [o.name, o])).values()],
    meta:
      `node ${process.versions.node} · ${process.platform} ${process.arch} · ${cpu.length} cores · ` +
      `${BLOCKS}×${ROUNDS_PER_BLOCK} rounds · ${SCREEN_JOBS} screening jobs · confirmed by ${CONFIRM_PROCESSES} fresh processes`,
  };
};
