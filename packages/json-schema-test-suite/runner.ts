// Evaluates the suite against `S.fromJSONSchema` and reduces it to the shape
// that gets snapshotted into goldens/.
import { readdirSync, readFileSync } from "node:fs";
import { join, relative, sep } from "node:path";
import { isDeepStrictEqual } from "node:util";
import * as S from "sury";

export const DIALECTS = ["draft7", "draft2020-12"] as const;
export type Dialect = (typeof DIALECTS)[number];

export const isDialect = (value: string): value is Dialect =>
  (DIALECTS as readonly string[]).includes(value);

type SuiteTest = { description: string; data: unknown; valid: boolean };
type SuiteCase = { description: string; schema: unknown; tests: SuiteTest[] };

export type FileResult = { file: string; passed: number; assertions: number };

export type DialectResult = {
  dialect: Dialect;
  assertions: number;
  passed: number;
  failed: number;
  // Split of `failed` by direction — see the push sites below.
  falseAccept: number;
  falseReject: number;
  errored: number;
  // `S.is` score over the same corpus. Tracked alongside the canonical parse
  // score because the two disagreeing is always a Sury bug, never a JSON
  // Schema gap — that delta is what surfaced the `S.json` assert break.
  assertPassed: number;
  identityAssertions: number;
  identityPassed: number;
  files: FileResult[];
  failing: string[];
  falseAccepting: string[];
  erroredCases: string[];
  divergent: string[];
  mutated: string[];
};

const jsonFilesIn = (dir: string): string[] =>
  readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) return entry.name === "optional" ? [] : jsonFilesIn(path);
    return entry.name.endsWith(".json") ? [path] : [];
  });

const optionalFilesIn = (dir: string): string[] => {
  const optional = join(dir, "optional");
  const walk = (d: string): string[] =>
    readdirSync(d, { withFileTypes: true }).flatMap((entry) => {
      const path = join(d, entry.name);
      if (entry.isDirectory()) return walk(path);
      return entry.name.endsWith(".json") ? [path] : [];
    });
  return walk(optional);
};

// Test ids are the human-readable descriptions upstream already writes, joined
// by " :: " — they survive a suite bump far better than file offsets would,
// which is what keeps a golden diff readable.
const caseId = (file: string, testCase: SuiteCase): string => `${file} :: ${testCase.description}`;
const testId = (file: string, testCase: SuiteCase, test: SuiteTest): string =>
  `${caseId(file, testCase)} :: ${test.description}`;

const attempt = (fn: () => unknown): boolean => {
  try {
    fn();
    return true;
  } catch {
    return false;
  }
};

export const runDialect = (
  suiteDir: string,
  dialect: Dialect,
  { optional = false } = {}
): DialectResult => {
  const dialectDir = join(suiteDir, "tests", dialect);
  const files = [
    ...jsonFilesIn(dialectDir),
    ...(optional ? optionalFilesIn(dialectDir) : []),
  ].sort();

  const result: DialectResult = {
    dialect,
    assertions: 0,
    passed: 0,
    failed: 0,
    falseAccept: 0,
    falseReject: 0,
    errored: 0,
    assertPassed: 0,
    identityAssertions: 0,
    identityPassed: 0,
    files: [],
    failing: [],
    falseAccepting: [],
    erroredCases: [],
    divergent: [],
    mutated: [],
  };

  for (const path of files) {
    const file = relative(dialectDir, path).split(sep).join("/");
    const cases = JSON.parse(readFileSync(path, "utf8")) as SuiteCase[];
    const fileResult: FileResult = { file, passed: 0, assertions: 0 };

    for (const testCase of cases) {
      fileResult.assertions += testCase.tests.length;
      result.assertions += testCase.tests.length;

      let parse: (data: unknown) => unknown;
      let schema: unknown;
      try {
        schema = S.fromJSONSchema(testCase.schema as never);
        parse = S.parser(schema as never) as (data: unknown) => unknown;
      } catch {
        result.errored += testCase.tests.length;
        result.erroredCases.push(caseId(file, testCase));
        continue;
      }

      for (const test of testCase.tests) {
        let parseValid = false;
        let output: unknown;
        try {
          output = parse(structuredClone(test.data));
          parseValid = true;
        } catch {}
        const assertValid = attempt(() => {
          if (!S.inputValidator(structuredClone(test.data), schema as never)) throw new Error("invalid");
        });

        if (parseValid === test.valid) {
          result.passed++;
          fileResult.passed++;
        } else {
          result.failed++;
          // Direction matters more than the count. Accepting data the suite
          // calls invalid is a validator saying yes to bad input; rejecting
          // valid data is merely inconvenient. A net-positive total once hid a
          // whole keyword flipping from the second kind to the first.
          if (parseValid) {
            result.falseAccept++;
            result.falseAccepting.push(testId(file, testCase, test));
          } else {
            result.falseReject++;
          }
          result.failing.push(testId(file, testCase, test));
        }
        if (assertValid === test.valid) result.assertPassed++;
        if (assertValid !== parseValid) result.divergent.push(testId(file, testCase, test));
        if (test.valid && parseValid) {
          result.identityAssertions++;
          if (isDeepStrictEqual(output, test.data)) {
            result.identityPassed++;
          } else {
            result.mutated.push(testId(file, testCase, test));
          }
        }
      }
    }
    result.files.push(fileResult);
  }

  return result;
};

export const rate = (passed: number, total: number): string =>
  total === 0 ? "0.0%" : `${((passed / total) * 100).toFixed(1)}%`;

export type Golden = {
  $comment: string;
  suite: string;
  dialect: Dialect;
  summary: {
    assertions: number;
    passed: number;
    failed: number;
    falseAccept: number;
    falseReject: number;
    errored: number;
    rate: string;
    assertOpPassed: number;
    assertOpRate: string;
    identityPassed: number;
    identityAssertions: number;
    identityRate: string;
    divergent: number;
  };
  erroredCases: string[];
  // Listed, not just counted: this is the bucket that must never grow silently.
  falseAccepting: string[];
  mutated: string[];
  failing: string[];
};

export const toGolden = (result: DialectResult, suiteCommit: string): Golden => ({
  $comment: "Generated by `pnpm compliance --update`. Do not edit by hand.",
  suite: suiteCommit,
  dialect: result.dialect,
  summary: {
    assertions: result.assertions,
    passed: result.passed,
    failed: result.failed,
    falseAccept: result.falseAccept,
    falseReject: result.falseReject,
    errored: result.errored,
    rate: rate(result.passed, result.assertions),
    assertOpPassed: result.assertPassed,
    assertOpRate: rate(result.assertPassed, result.assertions),
    identityPassed: result.identityPassed,
    identityAssertions: result.identityAssertions,
    identityRate: rate(result.identityPassed, result.identityAssertions),
    divergent: result.divergent.length,
  },
  erroredCases: result.erroredCases,
  falseAccepting: result.falseAccepting,
  mutated: result.mutated,
  failing: result.failing,
});

export const serializeGolden = (golden: Golden): string => `${JSON.stringify(golden, null, 2)}\n`;
