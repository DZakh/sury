// Renders the `spec check --perf=only` report as markdown — the PR comment on
// the `performance` job, the run summary on `performance-drift`.
//
// It is a summary; the run's full output is uploaded as an artifact and linked,
// so a truncated table never hides a result. Parsing the report text (rather
// than adding a --json mode to the CLI) keeps the CLI at one output format —
// the artifact and this comment are then guaranteed to agree, because one is
// derived from the other.
import { readFileSync, writeFileSync } from "node:fs";

const MARKER = "<!-- spec-perf -->";
const MAX_ROWS = 10;

const ROW = /^ {2}(\S.*?) {2,}([+-]?\d+(?:\.\d+)?)%$/;
const HEADER = /^performance vs (\S+) \((.+?)\) · noise floor (.+)$/;

export const renderComment = (report: string, artifactUrl?: string, heading = "Spec performance"): string => {
  const lines = report.split("\n");
  const header = lines.map((l) => l.match(HEADER)).find(Boolean);
  const rows = lines.flatMap((l) => {
    const m = l.match(ROW);
    return m ? [{ name: m[1]!, pct: m[2]! }] : [];
  });
  const footer = lines.filter((l) => /advisory only$|^ {2}(new|could not measure)/.test(l.trim()) || /^ {2}node /.test(l));

  const out = [`### ${heading}`, ""];
  out.push(
    header
      ? `\`${header[1]}\` (${header[2]}) · noise floor ${header[3]}`
      : "_report could not be parsed — see the full report below._",
    "",
  );

  if (rows.length) {
    out.push("| target | Δ |", "|---|---:|");
    // Already ordered worst-regression-first by the CLI, so truncating from the
    // end drops the least interesting rows.
    for (const r of rows.slice(0, MAX_ROWS)) out.push(`| \`${r.name}\` | ${r.pct}% |`);
    out.push("");
    if (rows.length > MAX_ROWS) out.push(`…and ${rows.length - MAX_ROWS} more.`, "");
  } else {
    out.push("No significant changes.", "");
  }

  if (artifactUrl) out.push(`[Full report ↗](${artifactUrl})`, "");
  out.push(...footer.map((l) => `<sub>${l.trim()}</sub>`), "", MARKER);
  return out.join("\n");
};

const [, , input, output, heading] = process.argv;
if (input && output)
  writeFileSync(output, renderComment(readFileSync(input, "utf8"), process.env.ARTIFACT_URL, heading));
