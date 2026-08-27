// Union compiler fuzzer.
//
// Default: the working compiler is compared to a sequential try of each
// variant's own parser/encoder (CODEC_SPEC: grouping is codegen, not
// semantics). `--ref` is an optional changelog against a git commit and is
// not the correctness gate.
//
//   pnpm --filter=sury fuzz:union
//   pnpm --filter=sury fuzz:union --cases=50 --seed=1
//   pnpm --filter=sury fuzz:union --ref=HEAD~1

import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, rmSync, symlinkSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { generateMembers, rngFromSeed } from "./unionFuzz/generate";
import { issue347OptionVoidLastSchema, issue347Schema } from "./unionFuzz/issue347";
import { issue392Case } from "./unionFuzz/issue392";
import { classify, describeOutcome, show } from "./unionFuzz/outcome";
import { compiledEncode, compiledParse } from "./unionFuzz/reference";
import {
  describeMembers,
  diffsForUnion,
  diffsForValue,
  emptyStats,
  type Comparison,
} from "./unionFuzz/run";
import type { DiffClass, Sury } from "./unionFuzz/types";

const arg = (name: string, fallback?: string): string | undefined => {
  const hit = process.argv.find((a) => a.startsWith(`--${name}=`));
  if (hit !== undefined) return hit.slice(name.length + 3);
  return fallback;
};

const repoRoot = execFileSync("git", ["rev-parse", "--show-toplevel"], {
  encoding: "utf8",
}).trim();

const build = (cwd: string): void => {
  execFileSync("npx", ["tsx", "./scripts/pack.ts", "entry-only"], {
    cwd: join(cwd, "packages/sury"),
    stdio: "inherit",
  });
};

const entryPath = (tree: string): string => {
  const current = join(tree, "packages/sury/index.mjs");
  return existsSync(current) ? current : join(tree, "packages/sury/src/S.mjs");
};

const checkout = (ref: string): { dir: string; tree: string } => {
  const dir = mkdtempSync(join(tmpdir(), "sury-fuzz-"));
  const tree = join(dir, "tree");
  execFileSync("git", ["worktree", "add", "--detach", tree, ref], {
    cwd: repoRoot,
    stdio: "inherit",
  });
  symlinkSync(join(repoRoot, "node_modules"), join(tree, "node_modules"));
  symlinkSync(
    join(repoRoot, "packages/sury/node_modules"),
    join(tree, "packages/sury/node_modules"),
  );
  build(tree);
  return { dir, tree };
};

const printDiff = (
  label: string,
  members: string,
  diff: Comparison,
  shown: Record<DiffClass, number>,
  budget: Record<DiffClass, number>,
): void => {
  if (shown[diff.class]++ >= budget[diff.class]) return;
  console.log(
    `\n[${diff.class}/${diff.direction}] ${members} <- ${show(diff.input)}`,
  );
  console.log(`  ${label}: ${describeOutcome(diff.compiled)}`);
  console.log(`  reference: ${describeOutcome(diff.reference)}`);
};

const budget: Record<DiffClass, number> = {
  acceptance: 40,
  "exception-kind": 40,
  reasons: 5,
  message: 10,
};

const num = (name: string, fallback: string): number => {
  const value = Number(arg(name, fallback));
  if (!Number.isFinite(value)) {
    throw new Error(`--${name} must be a number`);
  }
  return value;
};

const main = async (): Promise<void> => {
  const cases = num("cases", "400");
  const seed = num("seed", "1");
  const maxMembers = num("max-members", "4");
  const ref = arg("ref");

  build(repoRoot);
  const S: Sury = await import(entryPath(repoRoot));

  const stats = emptyStats();
  const shown: Record<DiffClass, number> = {
    acceptance: 0,
    "exception-kind": 0,
    reasons: 0,
    message: 0,
  };

  const issue347 = issue347Schema(S);
  console.log("pinned issue-347");
  for (const { label, value, encode } of [
    { label: "null", value: null, encode: true },
    { label: "Tagged", value: { TAG: "Tagged", _0: "abc" }, encode: true },
    { label: "Plain", value: { TAG: "Plain", _0: { name: "n" } }, encode: true },
  ]) {
    const { diffs, compared } = diffsForValue(S, issue347, value, encode);
    stats.compared += compared;
    for (const diff of diffs) {
      stats.diffs += 1;
      stats.byClass[diff.class] += 1;
      printDiff("compiled", `issue-347 ${label}`, diff, shown, budget);
    }
    if (!diffs.length) {
      console.log(`  ${label}: compiled matches reference`);
    }
  }
  const none = compiledEncode(S, issue347, null);
  if (!none.ok) {
    stats.diffs += 1;
    stats.byClass[none.kind === "foreign" ? "exception-kind" : "acceptance"] += 1;
    console.log(`\n[issue-347/encode] null: ${describeOutcome(none)}`);
  }

  const issue347VoidLast = issue347OptionVoidLastSchema(S);
  console.log("pinned issue-347 option(union(custom, void))");
  const noneVoidLast = compiledEncode(S, issue347VoidLast, undefined);
  if (!noneVoidLast.ok) {
    stats.diffs += 1;
    stats.byClass[noneVoidLast.kind === "foreign" ? "exception-kind" : "acceptance"] += 1;
    console.log(`\n[issue-347/encode] undefined-last: ${describeOutcome(noneVoidLast)}`);
  } else {
    console.log("  undefined: compiled encode succeeded");
  }

  const pinned = issue392Case(S);
  const pinnedUnion = S.union(pinned.members.map((m) => m.schema));
  console.log(`pinned ${pinned.id}`);
  for (const { label, value } of pinned.allWitnesses) {
    const { diffs, compared } = diffsForValue(S, pinnedUnion, value);
    stats.compared += compared;
    for (const diff of diffs) {
      stats.diffs += 1;
      stats.byClass[diff.class] += 1;
      printDiff(
        "compiled",
        `${pinned.id} ${label}`,
        diff,
        shown,
        budget,
      );
    }
    if (!diffs.length) {
      console.log(`  ${label}: compiled matches reference`);
    }
  }

  const next = rngFromSeed(seed);
  for (let c = 0; c < cases; c++) {
    const size = 2 + Math.floor(next() * Math.max(1, maxMembers - 1));
    const members = generateMembers(S, next, size);
    const result = diffsForUnion(S, members);
    stats.compared += result.compared;
    stats.skipped += result.skipped;
    for (const diff of result.diffs) {
      stats.diffs += 1;
      stats.byClass[diff.class] += 1;
      printDiff("compiled", describeMembers(members), diff, shown, budget);
    }
  }

  console.log(
    `\n${stats.compared} compiled-vs-reference comparisons, ${stats.diffs} diff(s), ${stats.skipped} skipped (seed ${seed}, ${cases} unions)`,
  );
  for (const kind of Object.keys(stats.byClass) as DiffClass[]) {
    const total = stats.byClass[kind];
    if (!total) continue;
    const printed = Math.min(shown[kind], budget[kind]);
    console.log(
      `  ${kind}: ${total}${total > printed ? ` (${printed} shown)` : ""}`,
    );
  }

  if (ref) {
    const { dir, tree } = checkout(ref);
    try {
      const baseline: Sury = await import(entryPath(tree));
      let refDiffs = 0;
      const sample = issue392Case(S);
      const currentUnion = S.union(sample.members.map((m) => m.schema));
      const baselineUnion = baseline.union(sample.members.map((m) => m.schema));
      for (const { value } of sample.allWitnesses) {
        const before = compiledParse(baseline, baselineUnion, value);
        const after = compiledParse(S, currentUnion, value);
        if (describeOutcome(before) !== describeOutcome(after)) {
          refDiffs += 1;
          const kind = classify(before, after);
          console.log(`\n[vs ${ref}/${kind}] issue-392 <- ${show(value)}`);
          console.log(`  ${ref}: ${describeOutcome(before)}`);
          console.log(`  working: ${describeOutcome(after)}`);
        }
      }
      console.log(
        `\n${refDiffs} changelog diff(s) vs ${ref} on the pinned case (not a gate)`,
      );
    } finally {
      rmSync(dir, { recursive: true, force: true });
      execFileSync("git", ["worktree", "prune"], { cwd: repoRoot });
    }
  }

  if (stats.byClass.acceptance || stats.byClass["exception-kind"]) {
    process.exitCode = 1;
  }
};

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
