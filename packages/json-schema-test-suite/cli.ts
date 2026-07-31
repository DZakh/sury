#!/usr/bin/env tsx
// `compliance` — runs the official JSON-Schema-Test-Suite through
// `S.fromJSONSchema` and holds the score to a committed golden.
//
// The contract mirrors packages/spec: goldens are generated, never hand-edited,
// and `check` fails on ANY drift — including tests that started passing. A fix
// that improves the score is supposed to show up as a golden diff in the same
// PR, which is what makes coverage change reviewable.
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import {
  DIALECTS,
  type Dialect,
  type Golden,
  isDialect,
  rate,
  runDialect,
  serializeGolden,
  toGolden,
} from "./runner";
import { ensureSuite, PKG_DIR, SUITE_COMMIT } from "./suite";

if (fileURLToPath(import.meta.url) !== process.argv[1])
  throw new Error("cli.ts is a script, not a library — import from runner.ts instead");

const red = (s: string): string => (process.stderr.isTTY ? `\x1b[31m${s}\x1b[0m` : s);
const green = (s: string): string => (process.stdout.isTTY ? `\x1b[32m${s}\x1b[0m` : s);
const dim = (s: string): string => (process.stdout.isTTY ? `\x1b[2m${s}\x1b[0m` : s);

const GOLDENS_DIR = join(PKG_DIR, "goldens");
const goldenPath = (dialect: Dialect): string => join(GOLDENS_DIR, `${dialect}.json`);

const HELP = `compliance — Sury vs. the official JSON-Schema-Test-Suite

Usage: pnpm compliance [command] [options]

Commands:
  check                 Recompute and compare against goldens/ (default).
                        Exits 1 on any drift, in either direction.
  update                Rewrite goldens/ from the current run.
  report [dialect]      Per-file breakdown for eyeballing where the gaps are.

Options:
  --update              Alias for the update command.
  --dialect <name>      Limit to one dialect (${DIALECTS.join(", ")}).
  --optional            report only: include tests/<dialect>/optional/**.
  --failures            report only: list every failing test id.
  --divergent           report only: list tests where S.is disagrees with S.parser.

Goldens cover the required tests only; optional/ (formats, bignum, content
encoding) is exploratory and deliberately unsnapshotted.`;

const args = process.argv.slice(2);
const flag = (name: string): boolean => args.includes(name);
const option = (name: string): string | undefined => {
  const idx = args.indexOf(name);
  return idx === -1 ? undefined : args[idx + 1];
};

// A declaration, not an arrow: TS only narrows on a `never`-returning callee
// when it can see the signature at the call site (same reason packages/spec
// declares its `fail` this way).
function fail(msg: string): never {
  console.error(red(msg));
  process.exit(1);
}

const VALUE_OPTIONS = new Set(["--dialect"]);
const positional: string[] = [];
for (let idx = 0; idx < args.length; idx++) {
  const arg = args[idx]!;
  if (!arg.startsWith("--")) positional.push(arg);
  else if (VALUE_OPTIONS.has(arg)) idx++;
}
const cmd = flag("--update") ? "update" : (positional[0] ?? "check");

if (flag("--help") || flag("-h")) {
  console.log(HELP);
  process.exit(0);
}

const dialectOption = option("--dialect");
if (dialectOption !== undefined && !isDialect(dialectOption))
  fail(`unknown dialect: ${dialectOption} (expected one of ${DIALECTS.join(", ")})`);
const targets: readonly Dialect[] = dialectOption ? [dialectOption as Dialect] : DIALECTS;

const suiteDir = ensureSuite();
if (suiteDir === null) fail("could not fetch the test suite");

const readGolden = (dialect: Dialect): Golden | null => {
  const path = goldenPath(dialect);
  return existsSync(path) ? (JSON.parse(readFileSync(path, "utf8")) as Golden) : null;
};

const diffLists = (before: string[], after: string[]) => {
  const beforeSet = new Set(before);
  const afterSet = new Set(after);
  return {
    added: after.filter((id) => !beforeSet.has(id)),
    removed: before.filter((id) => !afterSet.has(id)),
  };
};

// Caps the per-section listing so a suite bump doesn't bury the summary under
// hundreds of lines; the golden diff itself is always the complete record.
const LIST_LIMIT = 15;
const listing = (ids: string[]): string[] => [
  ...ids.slice(0, LIST_LIMIT).map((id) => `      ${id}`),
  ...(ids.length > LIST_LIMIT ? [dim(`      … and ${ids.length - LIST_LIMIT} more`)] : []),
];

if (cmd === "update") {
  mkdirSync(GOLDENS_DIR, { recursive: true });
  for (const dialect of targets) {
    const result = runDialect(suiteDir, dialect);
    writeFileSync(goldenPath(dialect), serializeGolden(toGolden(result, SUITE_COMMIT)));
    console.log(
      green(
        `✓ ${dialect}  ${result.passed}/${result.assertions} (${rate(result.passed, result.assertions)})`
      )
    );
  }
  process.exit(0);
}

if (cmd === "report") {
  const dialect = positional[1] ?? dialectOption ?? DIALECTS[DIALECTS.length - 1]!;
  if (!isDialect(dialect)) fail(`unknown dialect: ${dialect}`);
  const optional = flag("--optional");
  const result = runDialect(suiteDir, dialect as Dialect, { optional });

  console.log(`\n${dialect}${optional ? " (incl. optional/)" : ""} @ ${SUITE_COMMIT.slice(0, 10)}`);
  console.log(
    `  parse  ${result.passed}/${result.assertions} (${rate(result.passed, result.assertions)})` +
      `   is  ${result.assertPassed}/${result.assertions} (${rate(result.assertPassed, result.assertions)})` +
      `   errored ${result.errored}   divergent ${result.divergent.length}\n` +
      `  accepts-invalid ${result.falseAccept}   rejects-valid ${result.falseReject}\n`
  );

  const worst = [...result.files].sort(
    (a, b) => b.assertions - b.passed - (a.assertions - a.passed)
  );
  for (const file of worst) {
    const missed = file.assertions - file.passed;
    const label = `${file.passed}/${file.assertions}`.padStart(8);
    console.log(`  ${missed === 0 ? green("✓") : " "} ${file.file.padEnd(34)}${label}`);
  }

  if (flag("--failures")) {
    console.log("\nfailing:");
    for (const id of result.failing) console.log(`  ${id}`);
  }
  if (flag("--divergent")) {
    console.log("\nS.is disagrees with S.parser:");
    for (const id of result.divergent) console.log(`  ${id}`);
  }
  process.exit(0);
}

if (cmd !== "check") fail(`unknown command: ${cmd}\n\n${HELP}`);

let drifted = false;
for (const dialect of targets) {
  const golden = readGolden(dialect);
  const result = runDialect(suiteDir, dialect);
  const current = toGolden(result, SUITE_COMMIT);

  if (golden === null) {
    drifted = true;
    console.error(red(`✗ ${dialect}`));
    console.error(`    no golden recorded — run \`pnpm compliance --update\``);
    continue;
  }

  const sections: string[] = [];
  if (golden.suite !== current.suite)
    sections.push(`    suite pinned at ${current.suite}, golden recorded ${golden.suite}`);

  for (const [label, before, after] of [
    // `falseAccepting` first: a regression here means the validator started
    // saying yes to data the suite calls invalid.
    ["accepting invalid data", golden.falseAccepting, current.falseAccepting],
    ["failing", golden.failing, current.failing],
    ["errored", golden.erroredCases, current.erroredCases],
  ] as const) {
    const { added, removed } = diffLists(before, after);
    if (removed.length)
      sections.push(`    ${removed.length} no longer ${label}:`, ...listing(removed));
    if (added.length) sections.push(`    ${added.length} newly ${label}:`, ...listing(added));
  }

  // The `is`/`parse` numbers don't follow from the failing list, so a change
  // confined to them would otherwise leave the golden silently stale.
  if (golden.summary.divergent !== current.summary.divergent)
    sections.push(
      `    S.is vs S.parser divergence: ${golden.summary.divergent} → ${current.summary.divergent}` +
        ` (\`pnpm compliance report ${dialect} --divergent\` to list)`
    );
  if (golden.summary.assertOpPassed !== current.summary.assertOpPassed)
    sections.push(
      `    S.is score: ${golden.summary.assertOpPassed} → ${current.summary.assertOpPassed}` +
        ` of ${current.summary.assertions}`
    );

  if (sections.length === 0) {
    console.log(
      green(`✓ ${dialect}  ${current.summary.passed}/${current.summary.assertions} (${current.summary.rate})`)
    );
    continue;
  }

  drifted = true;
  console.error(red(`✗ ${dialect}`));
  console.error(
    `    ${golden.summary.passed}/${golden.summary.assertions} (${golden.summary.rate})` +
      ` → ${current.summary.passed}/${current.summary.assertions} (${current.summary.rate})`
  );
  // A net-positive pass count can still hide a keyword flipping from
  // "rejects valid data" to "accepts invalid data", so report the split.
  console.error(
    `    accepts-invalid ${golden.summary.falseAccept} → ${current.summary.falseAccept}` +
      `   rejects-valid ${golden.summary.falseReject} → ${current.summary.falseReject}` +
      `   errored ${golden.summary.errored} → ${current.summary.errored}`
  );
  console.error(sections.join("\n"));
}

if (drifted) {
  console.error(red("\ncompliance goldens are out of date — run `pnpm compliance --update`"));
  process.exit(1);
}
