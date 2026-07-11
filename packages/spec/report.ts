// The CLI's report formatting: colors, the "✗ name\n    detail..." failure
// block, and runCheck (the read-only check flow against spec source text).
// Split out from cli.ts so this stays importable — cli.ts itself is a script
// and throws if imported, but spec_errors_test.ts needs runCheck to assert on
// the exact text/stream a real `spec check <id>` run produces.
import { checkSpec, parseSpec } from "./harness";

// Colors only for a real terminal — piped/CI output (and captured test
// output, which is how spec_errors_test.ts asserts on this exact text) would
// otherwise carry raw, unreadable escape codes.
export const red = (s: string): string => (process.stderr.isTTY ? `\x1b[31m${s}\x1b[0m` : s);
export const green = (s: string): string => (process.stdout.isTTY ? `\x1b[32m${s}\x1b[0m` : s);

// The "✗ name\n    detail..." block a failing check prints to stderr — shared
// by every failure path in cli.ts (spec.schema.json, the specs dir lint, each
// spec) and exported so tests assert on this exact text/formatting instead of
// a bare message array.
export const formatFailure = (name: string, details: string[]): string =>
  [red(`✗ ${name}`), ...details.map((d) => `    ${d}`)].join("\n");

// Runs the same read-only check flow cmdCheck performs per file (no --write
// side effects) directly against spec source text, returning exactly the
// stdout/stderr a real `spec check <id>` run would produce for that one
// file — so tests exercise the real formatting and stream routing, not a
// re-implementation, without spawning a subprocess per scenario.
export const runCheck = async (id: string, raw: string): Promise<{ stdout: string; stderr: string }> => {
  const errs = await checkSpec(id, parseSpec(raw), raw);
  return errs.length ? { stdout: "", stderr: formatFailure(id, errs) } : { stdout: green(`✓ ${id}`), stderr: "" };
};
