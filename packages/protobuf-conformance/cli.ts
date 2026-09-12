#!/usr/bin/env tsx
// Drives Google's conformance_test_runner against runner.ts and holds the
// score to a committed golden, the way json-schema-test-suite does for its
// upstream.
import { spawnSync } from "node:child_process";
import { chmodSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { divergences } from "./checkSchema";
import { PKG_DIR, UPSTREAM_COMMIT, ensureUpstream, runnerBinary, runnerVersion } from "./upstream";

if (fileURLToPath(import.meta.url) !== process.argv[1]) {
  throw new Error("cli.ts is a script, not a library");
}

const GOLDEN_PATH = join(PKG_DIR, "goldens", "conformance.json");
const FAILURE_LIST = join(PKG_DIR, "failing_tests.txt");

const red = (s: string): string => (process.stderr.isTTY ? `\x1b[31m${s}\x1b[0m` : s);
const green = (s: string): string => (process.stdout.isTTY ? `\x1b[32m${s}\x1b[0m` : s);

// Annotated on the binding, not just the arrow: TS only narrows past a `never`
// call when the declaration says so, and `upstream()` below relies on it.
const fail: (msg: string) => never = (msg) => {
  console.error(red(msg));
  process.exit(1);
};

const HELP = `protobuf-conformance-suite - S.protobuf vs Google's conformance runner

Usage: pnpm conformance [command]

Commands:
  check    Run and compare against goldens/conformance.json (default).
  update   Rewrite the golden from the current run.
  report   Print the runner's full log, every failing test named.
  schema   Diff testMessages.ts against the pinned .proto, field by field.

The runner is Google's own C++ conformance_test_runner, from the
protobuf-conformance npm package that bufbuild/protobuf-conformance also uses.
The cases live inside that binary rather than in any file, so its version is
pinned as tightly as the corpus commit.

S.protobuf speaks the binary format only, so ProtoJSON, text format and the
proto2 message types are answered "skipped" and are not counted in the rate.
`;

type Score = {
  $comment: string;
  upstream: string;
  runner: string;
  /** Binary-format proto3 cases the runner actually put to us. */
  attempted: number;
  passed: number;
  /** Named in failing_tests.txt with a reason. Expected, not a regression. */
  expectedFailures: number;
  /** ProtoJSON, text format, and the message types S.protobuf does not model. */
  skipped: number;
  skippedBy: Record<string, number>;
  rate: string;
};

// Everything the runner says - the per-suite summaries, the failing-test list,
// and our harness's own skip tally - arrives on stderr; stdout stays empty. It
// runs two suites in one go (binary+ProtoJSON, then text format), each with its
// own summary line and its own spawn of the harness, so both are summed.
type Run = {
  log: string;
  successes: number;
  skipped: number;
  expected: number;
  unexpected: string[];
  skippedBy: Record<string, number>;
};

const SUMMARY =
  /CONFORMANCE SUITE (?:PASSED|FAILED): (\d+) successes, (\d+) skipped, (\d+) expected failures, (\d+) unexpected failures\./g;

const parseLog = (log: string): Omit<Run, "log"> => {
  let successes = 0;
  let skipped = 0;
  let expected = 0;
  for (const m of log.matchAll(SUMMARY)) {
    successes += Number(m[1]);
    skipped += Number(m[2]);
    expected += Number(m[3]);
  }

  // Failures the runner did not expect are listed under this header - two
  // lines of prose, then one test per line until the blank line that ends it.
  const unexpected: string[] = [];
  for (const section of log.matchAll(/These tests failed\.[^\n]*\n[^\n]*\n([\s\S]*?)(?:\n\n|$)/g)) {
    for (const line of section[1]!.split("\n")) {
      const name = line.trim().replace(/\s*#.*$/, "");
      if (name !== "") unexpected.push(name);
    }
  }

  // One tally per harness spawn; merged so the reasons read as one breakdown.
  const skippedBy: Record<string, number> = Object.create(null);
  for (const m of log.matchAll(/sury-skip-tally (\{.*\})/g)) {
    for (const [why, count] of Object.entries(JSON.parse(m[1]!) as Record<string, number>)) {
      skippedBy[why] = (skippedBy[why] ?? 0) + count;
    }
  }

  return { successes, skipped, expected, unexpected, skippedBy };
};

// The runner takes one executable and pipes every test through it. A shebang of
// `npx tsx` is what upstream's harnesses use; a generated wrapper that execs
// the workspace's own tsx keeps the spawn off the network and off whatever
// `npx` would resolve to on a given machine.
const testProgram = (outputDir: string): string => {
  const require_ = createRequire(import.meta.url);
  const tsx = join(dirname(require_.resolve("tsx/package.json")), "dist", "cli.mjs");
  const path = join(outputDir, "run-sury");
  writeFileSync(
    path,
    `#!/bin/sh\nexec "${process.execPath}" "${tsx}" "${join(PKG_DIR, "runner.ts")}"\n`
  );
  chmodSync(path, 0o755);
  return path;
};

const upstream = (): string => {
  const dir = ensureUpstream({ offlineOk: true });
  if (dir === null) {
    fail(
      `Could not fetch the pinned corpus (${UPSTREAM_COMMIT}). Needs network once; it is cached in .upstream afterwards.`
    );
  }
  return dir;
};

// The fields testMessages.ts leaves undeclared on purpose, each because
// S.protobuf cannot express it. Anything else missing is a corpus bump
// outrunning the schema, which is the whole reason the commit is pinned.
const UNDECLARED: Record<number, string> = {
  27: "recursive_message: TestAllTypesProto3 itself; S.protobuf has no recursive messages",
  304: "optional_struct: Struct/Value/ListValue are mutually recursive",
  306: "optional_value: as above",
  316: "repeated_value: as above",
  317: "repeated_list_value: as above",
  324: "repeated_struct: as above",
};

const checkSchema = (): string[] => {
  const found = divergences(upstream());
  return found
    .filter((d) => !(d.detail === "not declared" && d.number in UNDECLARED))
    .map((d) => `  ${d.name} = ${d.number}: ${d.detail}`);
};

const run = (): Run => {
  upstream();
  const outputDir = join(PKG_DIR, "output_dir");
  mkdirSync(outputDir, { recursive: true });
  if (!existsSync(FAILURE_LIST)) writeFileSync(FAILURE_LIST, "");
  // spawnSync, not execFileSync: the runner reports unexpected failures by
  // exiting non-zero, and everything it writes is on stderr, so both streams
  // are wanted whichever way it ends.
  const proc = spawnSync(
    runnerBinary(),
    [
      "--enforce_recommended",
      "--failure_list",
      FAILURE_LIST,
      "--output_dir",
      outputDir,
      testProgram(outputDir),
    ],
    { cwd: PKG_DIR, encoding: "utf8", maxBuffer: 1 << 28 }
  );
  if (proc.error !== undefined) throw proc.error;
  const log = `${proc.stderr ?? ""}${proc.stdout ?? ""}`;
  if (!SUMMARY.test(log)) {
    throw new Error(`the conformance runner printed no summary:\n${log.slice(0, 4000)}`);
  }
  SUMMARY.lastIndex = 0;
  return { log, ...parseLog(log) };
};

const rate = (passed: number, total: number): string =>
  total === 0 ? "0.0%" : `${((100 * passed) / total).toFixed(1)}%`;

const toScore = (r: Run): Score => {
  const attempted = r.successes + r.expected + r.unexpected.length;
  return {
    $comment: "Generated by `pnpm conformance update`. Do not edit by hand.",
    upstream: UPSTREAM_COMMIT,
    runner: runnerVersion(),
    attempted,
    passed: r.successes,
    expectedFailures: r.expected,
    skipped: r.skipped,
    skippedBy: Object.fromEntries(Object.entries(r.skippedBy).sort(([a], [b]) => (a < b ? -1 : 1))),
    rate: rate(r.successes, attempted),
  };
};

const serialize = (score: Score): string => `${JSON.stringify(score, null, 2)}\n`;

// The one line CI reads. Says the rate, what is behind it, and what was left
// out - a percentage with no denominator in view is how a suite quietly stops
// meaning anything.
export const summary = (score: Score): string =>
  `protobuf conformance ${score.passed}/${score.attempted} (${score.rate})` +
  ` · ${score.expectedFailures} known failure(s) · ${score.skipped} skipped: ` +
  Object.entries(score.skippedBy)
    .map(([why, count]) => `${count} ${why.replace(/ (is|are) not supported$/, "")}`)
    .join(", ");

const args = process.argv.slice(2);
if (args.includes("--help") || args.includes("-h")) {
  console.log(HELP);
  process.exit(0);
}
const cmd = args[0] ?? "check";

if (cmd === "schema") {
  const drift = checkSchema();
  if (drift.length) fail(`testMessages.ts has drifted from the pinned .proto\n\n${drift.join("\n")}`);
  console.log(green(`testMessages.ts matches the pinned .proto${
    Object.keys(UNDECLARED).length ? `, bar ${Object.keys(UNDECLARED).length} fields it cannot express` : ""
  }`));
  process.exit(0);
}

// Every run checks it: a field the corpus added or retyped would otherwise
// show up only as a case failing for a reason nobody could see.
const drift = checkSchema();
if (drift.length) {
  fail(`testMessages.ts has drifted from the pinned .proto\n\n${drift.join("\n")}`);
}

const result = run();
const score = toScore(result);

if (cmd === "report") {
  console.log(result.log);
  console.log(serialize(score));
  process.exit(result.unexpected.length ? 1 : 0);
}

if (cmd === "update") {
  mkdirSync(join(PKG_DIR, "goldens"), { recursive: true });
  writeFileSync(GOLDEN_PATH, serialize(score));
  console.log(`wrote ${GOLDEN_PATH}  ${score.passed}/${score.attempted} (${score.rate})`);
  process.exit(0);
}

if (cmd !== "check") fail(`unknown command ${cmd}`);

if (result.unexpected.length) {
  fail(
    `protobuf conformance: ${result.unexpected.length} unexpected failure(s)\n\n` +
      `${result.unexpected.map((n) => `  ${n}`).join("\n")}\n\n` +
      `Fix them, or add each to failing_tests.txt with a reason and re-run \`pnpm conformance update\`.`
  );
}

if (!existsSync(GOLDEN_PATH)) fail(`missing ${GOLDEN_PATH}. Run \`pnpm conformance update\` first.`);
const expected = readFileSync(GOLDEN_PATH, "utf8");
const actual = serialize(score);
if (expected !== actual) {
  fail(`conformance golden drifted. Run \`pnpm conformance update\` if the new score is intended.\n\n${actual}`);
}

console.log(green(summary(score)));
