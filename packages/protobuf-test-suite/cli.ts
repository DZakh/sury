#!/usr/bin/env tsx
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { formatBench, runBench } from "./bench";
import { formatBundleSize, runBundleSize } from "./bundlesize";
import { formatCompare, runCompare } from "./compare";
import { formatHillclimb, runHillclimb } from "./hillclimb";
import {
  type Golden,
  runSuite,
  serializeGolden,
  toGolden,
} from "./runner";

if (fileURLToPath(import.meta.url) !== process.argv[1]) {
  throw new Error("cli.ts is a script, not a library");
}

const PKG_DIR = fileURLToPath(new URL(".", import.meta.url));
const GOLDENS_DIR = join(PKG_DIR, "goldens");
const GOLDEN_PATH = join(GOLDENS_DIR, "coverage.json");

const red = (s: string): string => (process.stderr.isTTY ? `\x1b[31m${s}\x1b[0m` : s);
const green = (s: string): string => (process.stdout.isTTY ? `\x1b[32m${s}\x1b[0m` : s);

function fail(msg: string): never {
  console.error(red(msg));
  process.exit(1);
}

const HELP = `protobuf-test-suite — S.protobuf vs protobufjs

Usage: pnpm protobuf:compliance [command]

Commands:
  check      Recompute and compare against goldens/coverage.json (default).
  update     Rewrite goldens/coverage.json from the current run.
  report     Print every case id and its status.
  bench      Best of 7 samples against protobufjs (reflect and static), protobuf-es
             and pbf on five workloads.
  compare    CPU µs, collector ns and retained bytes per op for every library
             and workload. Needs node --expose-gc (the package compare script).
  bundle     Minified+gzip size of one message's codec per library, and of a
             decode-only entry.
  hillclimb  Frozen ruler. Median of 7 on tiny/typical/large/common vs protobufjs.

protobufjs is the JS implementation that passes Google's official
conformance suite. google-protobuf does not. Cases include the encoding-guide
vectors from protobuf.dev plus the wire types S.protobuf claims.
`;

const args = process.argv.slice(2);
const cmd = args[0] ?? "check";
if (args.includes("--help") || args.includes("-h")) {
  console.log(HELP);
  process.exit(0);
}

const loadGolden = (): Golden => {
  if (!existsSync(GOLDEN_PATH)) fail(`missing ${GOLDEN_PATH}. Run with update first.`);
  return JSON.parse(readFileSync(GOLDEN_PATH, "utf8")) as Golden;
};

if (cmd === "bench") {
  console.log(formatBench(await runBench()));
  process.exit(0);
}

if (cmd === "compare") {
  console.log(formatCompare(await runCompare(Number(args[1] ?? 3e6))));
  process.exit(0);
}

if (cmd === "bundle") {
  console.log(formatBundleSize(await runBundleSize()));
  process.exit(0);
}

if (cmd === "hillclimb") {
  const score = runHillclimb();
  console.log(formatHillclimb(score));
  console.log(JSON.stringify(score));
  process.exit(0);
}

const suite = runSuite();
const golden = toGolden(suite);

if (cmd === "report") {
  for (const result of suite.results) {
    const tag = result.status === "pass" ? green("pass") : red(result.status);
    const extra = result.detail ? `  ${result.detail}` : "";
    console.log(`${tag}  ${result.id}${extra}`);
  }
  console.log(
    `\n${golden.summary.passed}/${golden.summary.cases} passed (${golden.summary.rate}). skipped: ${golden.summary.skipped}`,
  );
  process.exit(suite.failed.length || suite.errored.length ? 1 : 0);
}

if (cmd === "update") {
  mkdirSync(GOLDENS_DIR, { recursive: true });
  writeFileSync(GOLDEN_PATH, serializeGolden(golden));
  console.log(`wrote ${GOLDEN_PATH}  ${golden.summary.passed}/${golden.summary.cases} (${golden.summary.rate})`);
  process.exit(0);
}

if (cmd !== "check") fail(`unknown command ${cmd}`);

const expected = serializeGolden(loadGolden());
const actual = serializeGolden(golden);
if (expected !== actual) {
  fail(
    `coverage golden drifted. Run \`pnpm protobuf:compliance update\` if the new score is intended.\n\n${actual}`,
  );
}
console.log(
  green(`protobuf coverage ${golden.summary.passed}/${golden.summary.cases} (${golden.summary.rate})`),
);
if (suite.failed.length || suite.errored.length) process.exit(1);
