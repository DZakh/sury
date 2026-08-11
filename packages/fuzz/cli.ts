#!/usr/bin/env node

import { existsSync, readFileSync } from "node:fs";
import { isAbsolute, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import {
  artifactFor,
  renderCase,
  renderCoverage,
  runCase,
  shrinkFailure,
  writeFailureArtifact,
} from "./engine";
import { generateCases } from "./generate";
import { Coverage, type SuryApi } from "./schema";
import { CASE_VERSION, type CompilerCase, type FailureArtifact } from "./types";

type Command =
  | { kind: "check" }
  | { kind: "replay"; file: string }
  | { kind: "help" };

const CAMPAIGN_SEEDS = [1, 0x5eed, 0x51a7, 0x5a17c0de] as const;
const CASES_PER_SEED = 2_000;
const MAX_SCHEMA_DEPTH = 4;
const ASYNC_TIMEOUT_MS = 1_000;
const MAX_SHRINK_ATTEMPTS = 250;
const MAX_TIMEOUT_SHRINK_ATTEMPTS = 20;

const fuzzDirectory = fileURLToPath(new URL(".", import.meta.url));
const repositoryDirectory = resolve(fuzzDirectory, "../..");
const artifactDirectory = resolve(fuzzDirectory, "artifacts");

const usage = `Sury schema compiler fuzzer

Usage:
  pnpm fuzz
  pnpm fuzz -- replay <artifact.json>

The campaign is intentionally zero-configuration. It always runs the checked-in,
deterministic reliability profile with canonical witnesses and failure shrinking.
PPX is deliberately outside this engine's scope.`;

const parseCommand = (args: string[]): Command => {
  const values = args.filter((argument) => argument !== "--");
  if (values.length === 0 || (values.length === 1 && values[0] === "check")) {
    return { kind: "check" };
  }
  if (values.length === 1 && ["help", "--help", "-h"].includes(values[0]!)) {
    return { kind: "help" };
  }
  if (values.length === 2 && values[0] === "replay") {
    return { kind: "replay", file: values[1]! };
  }
  throw new Error("The fuzz campaign does not accept configuration. Run `pnpm fuzz`.");
};

const loadSury = async (): Promise<SuryApi> => (await import("sury")) as SuryApi;

const validateCase = (value: unknown): CompilerCase => {
  if (!value || typeof value !== "object") throw new Error("Invalid replay file");
  const candidate = value as Partial<CompilerCase>;
  if (
    candidate.version !== CASE_VERSION ||
    typeof candidate.id !== "string" ||
    typeof candidate.operation !== "string" ||
    !Array.isArray(candidate.schemas) ||
    typeof candidate.runWitness !== "boolean"
  ) {
    throw new Error(`Replay case must use case version ${CASE_VERSION}`);
  }
  return candidate as CompilerCase;
};

const readReplay = (path: string): { testCase: CompilerCase; expectedSignature?: string } => {
  const fromWorkingDirectory = resolve(path);
  const resolvedPath =
    isAbsolute(path) || existsSync(fromWorkingDirectory)
      ? fromWorkingDirectory
      : resolve(repositoryDirectory, path);
  const value = JSON.parse(readFileSync(resolvedPath, "utf8")) as unknown;
  if (value && typeof value === "object" && "artifactVersion" in value) {
    const artifact = value as FailureArtifact;
    return {
      testCase: validateCase(artifact.minimized),
      expectedSignature: artifact.failure?.signature,
    };
  }
  return { testCase: validateCase(value) };
};

const check = async (S: SuryApi): Promise<number> => {
  const coverage = new Coverage();
  const counts = { compiled: 0, expectedError: 0 };
  const totalCases = CAMPAIGN_SEEDS.length * CASES_PER_SEED;
  let completedCases = 0;

  for (const seed of CAMPAIGN_SEEDS) {
    const cases = generateCases(seed, CASES_PER_SEED, MAX_SCHEMA_DEPTH);
    for (const testCase of cases) {
      completedCases++;
      const result = await runCase(testCase, S, coverage, ASYNC_TIMEOUT_MS);
      if (result.status === "compiled") {
        counts.compiled++;
        continue;
      }
      if (result.status === "expected-error") {
        counts.expectedError++;
        continue;
      }

      const maxShrinkAttempts =
        result.failure.phase === "timeout"
          ? MAX_TIMEOUT_SHRINK_ATTEMPTS
          : MAX_SHRINK_ATTEMPTS;
      const minimized = await shrinkFailure(
        testCase,
        result.failure.signature,
        S,
        ASYNC_TIMEOUT_MS,
        maxShrinkAttempts,
      );
      const minimizedResult = await runCase(
        minimized,
        S,
        new Coverage(),
        ASYNC_TIMEOUT_MS,
      );
      const finalFailure =
        minimizedResult.status === "bug" ? minimizedResult.failure : result.failure;
      const artifact = artifactFor(seed, testCase, minimized, finalFailure);
      const artifactPath = writeFailureArtifact(artifactDirectory, artifact);
      console.error(
        `BUG after ${completedCases}/${totalCases} cases: ${finalFailure.signature}`,
      );
      console.error(finalFailure.message);
      console.error(`Original:  ${renderCase(testCase)}`);
      console.error(`Minimized: ${renderCase(minimized)}`);
      console.error(`Replay: pnpm fuzz -- replay ${artifactPath}`);
      console.log(`\n${renderCoverage(coverage)}`);
      return 1;
    }
  }

  console.log(
    `OK: ${totalCases} cases across ${CAMPAIGN_SEEDS.length} deterministic seeds ` +
      `(compiled ${counts.compiled}, controlled errors ${counts.expectedError})`,
  );
  console.log(`\n${renderCoverage(coverage)}`);
  return 0;
};

const replay = async (file: string, S: SuryApi): Promise<number> => {
  const replayValue = readReplay(file);
  const result = await runCase(
    replayValue.testCase,
    S,
    new Coverage(),
    ASYNC_TIMEOUT_MS,
  );
  console.log(renderCase(replayValue.testCase));
  if (result.status === "bug") {
    const matches =
      replayValue.expectedSignature === undefined ||
      replayValue.expectedSignature === result.failure.signature;
    console.log(`${matches ? "REPRODUCED" : "CHANGED"}: ${result.failure.signature}`);
    console.log(result.failure.message);
    return matches ? 1 : 2;
  }
  console.log(`NOT REPRODUCED: ${result.status}`);
  return 2;
};

const main = async (): Promise<void> => {
  const command = parseCommand(process.argv.slice(2));
  if (command.kind === "help") {
    console.log(usage);
    return;
  }
  const S = await loadSury();
  process.exitCode =
    command.kind === "replay" ? await replay(command.file, S) : await check(S);
};

main().catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 2;
});
