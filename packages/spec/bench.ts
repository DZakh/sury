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
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { cpus } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { build } from "esbuild";
import { OP_ORDER, type OpName } from "./format";
import { evalSchema, readSpec, specId, stripTypes } from "./harness";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
const REPO_ROOT = here("../../");
const CACHE = here("./.bench-cache/");
const SURY_SRC = "packages/sury/src";

// Long enough that the two clock reads bracketing a batch are noise rather
// than signal (~25ns each against a 1ms batch).
const BATCH_TARGET_NS = 1_000_000;
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
const PHASES = ["create", "create+compile", "run"] as const;

export type Phase = "create" | "create+compile" | "run";

export type Target = {
  name: string;
  specId: string;
  phase: Phase;
  op?: OpName;
  /** Type-stripped already: the child has no TypeScript to strip it with. */
  schemaSrc: string;
  inputSrc?: string;
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
  | { name: string; error: string };

export type PerfResult = { name: string; phase: Phase; pct: number; median: number; batch: number };

export type Perf = {
  baselineLabel: string;
  baselineSha: string;
  floors: { phase: Phase; pct: number }[];
  changed: PerfResult[];
  unchanged: number;
  added: string[];
  skippedConstants: number;
  errors: { name: string; error: string }[];
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

export const deriveTargets = (files: string[]): { targets: Target[]; skippedConstants: number } => {
  const targets: Target[] = [];
  let skippedConstants = 0;

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
      if (!constant)
        targets.push({ ...base, name: `${id}${SEP}create+compile${SEP}${op}`, phase: "create+compile", op });
      for (const [example, ex] of Object.entries(block.examples))
        targets.push({
          ...base,
          name: `${id}${SEP}${op}${SEP}${example}`,
          phase: "run",
          op,
          inputSrc: stripTypes(ex.input),
          throws: !("output" in ex),
        });
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

  return { targets: [...targets, ...controls], skippedConstants };
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

export const runPerf = async (files: string[], against?: string): Promise<Perf> => {
  const { sha, label } = resolveBaseline(against);
  mkdirSync(CACHE, { recursive: true });

  const currentPath = join(CACHE, "current.mjs");
  const [baselinePath] = await Promise.all([
    materializeBaseline(sha),
    bundleEntry(REPO_ROOT, join(REPO_ROOT, SURY_SRC, "entry.ts"), currentPath),
    buildChild(),
  ]);

  const { targets, skippedConstants } = deriveTargets(files);
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

  // One process per target, not one per spec. Every target then starts from an
  // identical fresh heap, which is what makes a control able to calibrate the
  // targets it stands in for — grouped by spec, controls ran last, measuring a
  // heap that spec's creation targets had already churned. Targets run one at a
  // time: benchmark processes in parallel contend for CPU and cache.
  const measureAll = (list: Target[], label: string): ChildResult[] => {
    const out: ChildResult[] = [];
    let done = 0;
    for (const target of list) {
      progress(`\rperf ${label} ${++done}/${list.length} ${target.name}${" ".repeat(12)}`);
      const raw = execFileSync(process.execPath, ["--expose-gc", childPath], {
        input: JSON.stringify(payloadFor([target])),
        encoding: "utf8",
        maxBuffer: 1 << 26,
        stdio: ["pipe", "pipe", "inherit"],
      });
      out.push(...(JSON.parse(raw).results as ChildResult[]));
    }
    progress(`\r${" ".repeat(78)}\r`);
    return out;
  };

  const results = measureAll(targets, "measure");

  const targetByName = new Map(targets.map((t) => [t.name, t]));
  const measured = new Map<string, { phase: Phase; pct: number; median: number; batch: number }>();
  const added: string[] = [];
  const errors: { name: string; error: string }[] = [];
  for (const r of results) {
    if ("error" in r) errors.push({ name: r.name, error: r.error });
    else if ("unsupported" in r) added.push(r.name);
    else
      measured.set(r.name, {
        phase: targetByName.get(r.name)!.phase,
        pct: conservativePct(r.ratios),
        median: medianOf(r.ratios),
        batch: r.batch,
      });
  }

  const entries = [...measured];
  const isControl = (name: string) => name.startsWith(`control${SEP}`);

  // A phase's floor is whatever its own controls — identical code on both
  // sides — still managed to report. Both statistics are used: the conservative
  // bound catches a control that produced an outright false positive, and the
  // median catches the subtler case where a phase is visibly biased without any
  // single control clearing the bar. Anything a run can produce from nothing is
  // not evidence about the change.
  const floors = PHASES.map((phase) => {
    const controls = entries.filter(([name, m]) => isControl(name) && m.phase === phase).map(([, m]) => m);
    return {
      phase,
      pct: Math.max(
        MIN_FLOOR_PCT,
        ...controls.map((m) => Math.abs(m.pct)),
        ...controls.map((m) => Math.abs(m.median - 1) * 100),
      ),
    };
  });
  const floorFor = (phase: Phase) => floors.find((f) => f.phase === phase)!.pct;

  const real = entries.filter(([name]) => !isControl(name));
  const candidates = real
    .filter(([, m]) => Math.abs(m.pct) >= floorFor(m.phase))
    .map(([name, m]) => ({ name, ...m }));

  // Everything that cleared its floor is measured again from scratch and kept
  // only if the second run agrees on the direction — an independent
  // confirmation, not a second sample to pool with the first. Averaging the two
  // would be actively harmful: re-measuring only the large values and taking
  // their mean pulls them toward it, damping a real regression exactly as much
  // as a false one. The magnitude kept is the smaller of the two, for the same
  // reason the interval bound is its conservative end. Only a handful of
  // targets ever reach this pass, so it costs a second or two.
  const confirmed = new Map<string, number>();
  if (candidates.length)
    for (const r of measureAll(candidates.map((c) => targetByName.get(c.name)!), "confirm"))
      if ("ratios" in r) confirmed.set(r.name, conservativePct(r.ratios));

  const changed = candidates
    .flatMap((c) => {
      const again = confirmed.get(c.name);
      if (again === undefined || Math.sign(again) !== Math.sign(c.pct)) return [];
      const pct = Math.sign(c.pct) * Math.min(Math.abs(c.pct), Math.abs(again));
      return Math.abs(pct) >= floorFor(c.phase) ? [{ ...c, pct }] : [];
    })
    .sort((a, b) => b.pct - a.pct);

  const cpu = cpus();
  return {
    baselineLabel: label,
    baselineSha: sha.slice(0, 7),
    floors,
    changed,
    unchanged: real.length - changed.length,
    added,
    skippedConstants,
    errors,
    meta:
      `node ${process.versions.node} · ${process.platform} ${process.arch} · ${cpu.length} cores · ` +
      `${BLOCKS}×${ROUNDS_PER_BLOCK} rounds · confirmed`,
  };
};
