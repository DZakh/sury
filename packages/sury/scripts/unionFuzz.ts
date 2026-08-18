// Differential fuzz for the union compiler.
//
// The union planner decides which member a value is dispatched to, whether a
// failing member falls through to a later one, and which reasons end up in the
// aggregated error. None of that is visible in a golden until someone writes the
// spec for exactly the right member permutation, so a planner refactor can pass
// `spec check` and still change dispatch for a shape nobody snapshotted.
//
// This builds Sury twice — the working tree and a git ref — and drives the same
// randomized unions through both, comparing accepted/rejected, produced value,
// error message and the length of `unionErrors`. A refactor that is meant to
// preserve behavior must report zero diffs.
//
//   pnpm --filter=sury fuzz:union --ref=HEAD~1
//   pnpm --filter=sury fuzz:union --ref=aeae685 --cases=800 --seed=7
//
// Generation is seeded, so a reported diff reproduces from its seed alone.

import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, rmSync, symlinkSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

type Sury = Record<string, any>;

const arg = (name: string, fallback?: string): string => {
  const hit = process.argv.find((a) => a.startsWith(`--${name}=`));
  if (hit !== undefined) return hit.slice(name.length + 3);
  if (fallback !== undefined) return fallback;
  throw new Error(`Missing required --${name}=…`);
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

// The ref tree builds with its own scripts/pack.ts, and revisions from before
// the entry was renamed emit src/S.mjs instead.
const entryPath = (tree: string): string => {
  const current = join(tree, "packages/sury/index.mjs");
  return existsSync(current) ? current : join(tree, "packages/sury/src/S.mjs");
};

// A worktree has no node_modules of its own; the bundler and its deps are
// identical for both sides, so borrowing the main install is both correct and
// much faster than a second install.
const checkout = (ref: string): string => {
  const dir = mkdtempSync(join(tmpdir(), "sury-fuzz-"));
  const tree = join(dir, "tree");
  execFileSync("git", ["worktree", "add", "--detach", tree, ref], {
    cwd: repoRoot,
    stdio: "inherit",
  });
  symlinkSync(join(repoRoot, "node_modules"), join(tree, "node_modules"));
  symlinkSync(
    join(repoRoot, "packages/sury/node_modules"),
    join(tree, "packages/sury/node_modules")
  );
  build(tree);
  return tree;
};

// Mulberry32 — the generator has to be reproducible from the seed so a reported
// counterexample can be replayed, which `Math.random` can't give us.
const rng = (seed: number) => () => {
  seed = (seed + 0x6d2b79f5) | 0;
  let t = seed;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

// Each member is a name plus a builder, so a counterexample prints as a
// reproducible union spelling rather than an opaque schema dump.
type MemberSpec = { readonly id: string; readonly of: (S: Sury) => unknown };

// `S.to`'s custom-coder argument changed shape (positional decode/encode ->
// one `{decode, encode}` object). The baseline is built from a git ref, so a
// corpus pinned to either spelling makes the harness unable to build one side,
// which is the same as having no gate. The old signature took four parameters,
// the new one three, so `length` picks the spelling each revision understands.
//
// A decode-only member is the interesting one for dispatch, and the old API
// left its encode direction on the built-in conversion, which the new API
// spells `"auto"`. Anything else would compare two different schemas.
const codec = (S: any, from: any, target: any, decode: (v: any) => any) =>
  S.to.length === 3
    ? S.to(from, target, { decode, encode: "auto" })
    : S.to(from, target, decode);

const members: MemberSpec[] = [
  { id: "string", of: (S) => S.string },
  { id: "number", of: (S) => S.number },
  { id: "boolean", of: (S) => S.boolean },
  { id: "bigint", of: (S) => S.bigint },
  { id: "null", of: (S) => S.null },
  { id: "undefined", of: (S) => S.undefined },
  { id: "nan", of: (S) => S.nan },
  { id: "unknown", of: (S) => S.unknown },
  { id: "literal-a", of: (S) => S.literal("a") },
  { id: "literal-b", of: (S) => S.literal("b") },
  { id: "literal-0", of: (S) => S.literal(0) },
  { id: "literal-minus0", of: (S) => S.literal(-0) },
  { id: "literal-1", of: (S) => S.literal(1) },
  { id: "literal-true", of: (S) => S.literal(true) },
  { id: "array-string", of: (S) => S.array(S.string) },
  { id: "array-number", of: (S) => S.array(S.number) },
  { id: "tuple-t-number", of: (S) => S.tuple(["t", S.number]) },
  { id: "tuple-t-string", of: (S) => S.tuple(["t", S.string]) },
  { id: "obj-kind-a-v-string", of: (S) => S.schema({ kind: "a", v: S.string }) },
  { id: "obj-kind-a-v-number", of: (S) => S.schema({ kind: "a", v: S.number }) },
  { id: "obj-kind-b-v-string", of: (S) => S.schema({ kind: "b", v: S.string }) },
  { id: "obj-tag-x", of: (S) => S.schema({ tag: "x", n: S.number }) },
  { id: "obj-open", of: (S) => S.schema({ n: S.number }) },
  { id: "instance-date", of: (S) => S.instance(Date) },
  { id: "instance-error", of: (S) => S.instance(Error) },
  // Refined members reject values of their own type, which is the only way a
  // same-tag fallback edge gets exercised.
  {
    // Both spellings, because the baseline is built from a git ref and the
    // rename of `S.min` landed partway through this file's history — pinning
    // either name alone makes the harness unable to build one side, which is
    // the same as having no gate.
    id: "string-min3",
    of: (S) => S.string.with(S.minLength ?? (S as any).min, 3),
  },
  {
    // `refine` takes a predicate, not an effect ctx — returning false is how a
    // check rejects. This used to call `fail` on a second argument that was
    // never passed, so the member rejected by throwing a TypeError instead.
    id: "string-refine-fail",
    of: (S) => S.string.with(S.refine, () => false),
  },
  {
    id: "number-int32",
    of: (S) => S.int32,
  },
  // Transforming members distinguish "dispatched here" from "merely accepted".
  {
    id: "string-to-length",
    of: (S) => codec(S, S.string, S.number, (v: string) => v.length),
  },
  {
    id: "number-to-string",
    of: (S) => codec(S, S.number, S.string, (v: number) => `n${v}`),
  },
  // A member whose transform throws a foreign error: must escape, never be
  // treated as "this member didn't match".
  {
    id: "string-to-throws",
    of: (S) =>
      codec(S, S.string, S.number, () => {
        throw new RangeError("foreign");
      }),
  },
  { id: "never", of: (S) => S.never },
  { id: "nested-union-str-num", of: (S) => S.union([S.string, S.number]) },
  { id: "nested-union-bool-null", of: (S) => S.union([S.boolean, S.null]) },
  { id: "optional-string", of: (S) => S.optional(S.string) },
  { id: "nullable-number", of: (S) => S.nullable(S.number) },
];

const inputs: readonly unknown[] = [
  "a",
  "b",
  "abc",
  "",
  "xy",
  0,
  -0,
  1,
  1.5,
  -3,
  NaN,
  Infinity,
  2147483648,
  true,
  false,
  null,
  undefined,
  10n,
  Symbol.for("s"),
  [],
  ["t", 1],
  ["t", "s"],
  ["a", "b"],
  {},
  { kind: "a", v: "s" },
  { kind: "a", v: 1 },
  { kind: "b", v: "s" },
  { kind: "c" },
  { tag: "x", n: 1 },
  { n: 1 },
  new Date(0),
  new Error("e"),
  () => {},
];

// What the two builds are compared on. An error is reduced to its message plus
// the number of aggregated member reasons: both are public API, and the count is
// what regressed when hoisted-cond members stopped recording a reason.
type Outcome =
  | { ok: true; value: string }
  | { ok: false; kind: "sury"; message: string; reasons: number }
  | { ok: false; kind: "foreign"; name: string; message: string };

// The directions a union compiles for. `parse` validates from `unknown`; the
// other two decode *from the union itself*, the trusted source that lets a case
// skip re-validating the variant its discriminant already selected — so they are
// the only ones that exercise that path at all. `json` wraps the union in an
// array, because a union reached as a container item resolves its target per
// variant and reaches the compiler differently than a top-level one.
//
// Both trusted directions run only on values `parse` accepted, and on `parse`'s
// own output — that is exactly what a decoder is allowed to assume it was
// handed. Feeding them a value the union rejects would compare undefined
// behavior, where skipping validation is *supposed* to change the answer.
type Direction = "parse" | "convert" | "json";

const show = (value: unknown): string => {
  if (typeof value === "bigint") return `${value}n`;
  if (typeof value === "symbol") return value.toString();
  if (typeof value === "function") return "[Function]";
  if (typeof value === "number" && Object.is(value, -0)) return "-0";
  if (value instanceof Date) return `Date(${value.getTime()})`;
  if (value instanceof Error) return `${value.name}(${value.message})`;
  if (value === undefined) return "undefined";
  try {
    return JSON.stringify(value) ?? String(value);
  } catch {
    return String(value);
  }
};

// `outcome` also hands back the raw produced value, because the trusted
// directions are driven with what `parse` returned rather than the input.
const outcome = (
  S: Sury,
  produce: () => unknown,
): { o: Outcome; value?: unknown } => {
  try {
    const value = produce();
    return { o: { ok: true, value: show(value) }, value };
  } catch (error: any) {
    // `S.Error` is the "this didn't match" signal; anything else is a foreign
    // exception, and the two must never be confused.
    if (error instanceof S.Error) {
      return {
        o: {
          ok: false,
          kind: "sury",
          message: error.message,
          reasons: error.unionErrors?.length ?? 0,
        },
      };
    }
    return {
      o: {
        ok: false,
        kind: "foreign",
        name: error?.constructor?.name ?? "unknown",
        message: String(error?.message ?? error),
      },
    };
  }
};

const run = (
  S: Sury,
  spec: readonly MemberSpec[],
  input: unknown,
  direction: Direction,
): { o: Outcome; value?: unknown } =>
  outcome(S, () => {
    const schema = S.union(spec.map((m) => m.of(S)));
    // Schema creation itself throws for a union that can't be reversed, and
    // that is a behavior worth comparing — so it stays inside the try.
    if (direction === "parse") return S.parser(schema)(input);
    if (direction === "convert") return S.decoder(schema, S.unknown)(input);
    return S.encoder(S.array(schema), S.jsonString)([input]);
  });

const describe = (outcome: Outcome): string =>
  outcome.ok
    ? `ok(${outcome.value})`
    : outcome.kind === "foreign"
      ? `foreign(${outcome.name}: ${outcome.message})`
      : `sury(${outcome.reasons} reasons): ${outcome.message}`;

// Not every difference weighs the same. Whether a value is accepted, and what it
// decodes to, is the contract; how much detail a rejection carries is a
// judgement call. Classifying them means a refactor can prove it changed only
// error detail instead of asking a reader to skim the diff list.
type DiffClass = "acceptance" | "exception-kind" | "reasons" | "message";

const classify = (before: Outcome, after: Outcome): DiffClass => {
  if (before.ok !== after.ok) return "acceptance";
  if (before.ok && after.ok) return "acceptance";
  const b = before as Extract<Outcome, { ok: false }>;
  const a = after as Extract<Outcome, { ok: false }>;
  if (b.kind !== a.kind) return "exception-kind";
  if (b.kind === "sury" && a.kind === "sury") {
    // The first line is the union's own "Expected … received …"; the rest are
    // the per-member reasons.
    const top = (m: string) => m.split("\n")[0];
    if (top(b.message) === top(a.message)) return "reasons";
  }
  return "message";
};

const main = async (): Promise<void> => {
  const ref = arg("ref");
  const cases = Number(arg("cases", "400"));
  const seed = Number(arg("seed", "1"));
  const maxMembers = Number(arg("max-members", "4"));

  const refTree = checkout(ref);
  let refModule: Sury;
  let currentModule: Sury;
  try {
    build(repoRoot);
    refModule = await import(entryPath(refTree));
    currentModule = await import(entryPath(repoRoot));

    const next = rng(seed);
    const pick = <T,>(list: readonly T[]): T =>
      list[Math.floor(next() * list.length)]!;

    let diffs = 0;
    let compared = 0;
    const byClass: Record<DiffClass, number> = {
      acceptance: 0,
      "exception-kind": 0,
      reasons: 0,
      message: 0,
    };
    // Show every diff of a serious class, but only a sample of the chatty ones —
    // an intentional error-detail change otherwise buries an acceptance flip.
    const shown: Record<DiffClass, number> = {
      acceptance: 0,
      "exception-kind": 0,
      reasons: 0,
      message: 0,
    };
    const budget: Record<DiffClass, number> = {
      acceptance: 40,
      "exception-kind": 40,
      reasons: 5,
      message: 10,
    };

    for (let c = 0; c < cases; c++) {
      const size = 2 + Math.floor(next() * (maxMembers - 1));
      const spec: MemberSpec[] = [];
      for (let m = 0; m < size; m++) spec.push(pick(members));

      for (const input of inputs) {
        const report = (
          direction: Direction,
          value: unknown,
          before: Outcome,
          after: Outcome
        ): void => {
          compared++;
          if (describe(before) === describe(after)) return;
          diffs++;
          const kind = classify(before, after);
          byClass[kind]++;
          if (shown[kind]++ < budget[kind]) {
            console.log(
              `\n[${kind}/${direction}] S.union([${spec
                .map((m) => m.id)
                .join(", ")}]) <- ${show(value)}`
            );
            console.log(`  ${ref}: ${describe(before)}`);
            console.log(`  working: ${describe(after)}`);
          }
        };

        const before = run(refModule, spec, input, "parse");
        const after = run(currentModule, spec, input, "parse");
        report("parse", input, before.o, after.o);

        // Only a value both builds parsed to the same thing can drive the
        // trusted directions: if they already disagree, the parse diff above is
        // the finding, and feeding two different values downstream would report
        // it a second time as a phantom decode diff.
        if (
          before.o.ok &&
          after.o.ok &&
          describe(before.o) === describe(after.o)
        ) {
          for (const direction of ["convert", "json"] as const) {
            report(
              direction,
              before.value,
              run(refModule, spec, before.value, direction).o,
              run(currentModule, spec, after.value, direction).o
            );
          }
        }
      }
    }

    console.log(
      `\n${compared} comparisons, ${diffs} diff(s) vs ${ref} (seed ${seed}, ${cases} unions)`
    );
    for (const kind of Object.keys(byClass) as DiffClass[]) {
      const total = byClass[kind];
      if (!total) continue;
      const printed = Math.min(shown[kind], budget[kind]);
      console.log(
        `  ${kind}: ${total}${total > printed ? ` (${printed} shown)` : ""}`
      );
    }
    // An acceptance or exception-kind change is a behavior change; error detail
    // alone is for the author to accept or reject.
    if (byClass.acceptance || byClass["exception-kind"]) process.exitCode = 1;
  } finally {
    rmSync(refTree, { recursive: true, force: true });
    execFileSync("git", ["worktree", "prune"], { cwd: repoRoot });
  }
};

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
