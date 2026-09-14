// Renders the `spec check --perf=only` report as markdown - the PR comment on
// the `performance` job, the run summary on `performance-drift`.
//
// It is a summary; the run's full output is uploaded as an artifact and linked,
// so a truncated table never hides a result. Parsing the report text (rather
// than adding a --json mode to the CLI) keeps the CLI at one output format -
// the artifact and this comment are then guaranteed to agree, because one is
// derived from the other.
//
// What the shape is for: a reader scrolling a PR decides in one line whether to
// care, so the verdict comes first and the table explains it. Regressions and
// improvements are separate tables rather than one list sorted worst-first,
// because a change with eleven regressions would otherwise truncate away every
// improvement it also made. Anything the run could NOT time - an accept/reject
// flip, a measurement failure - is a finding, not metadata, so it sits above
// the tables instead of in the small print with the node version.
import { readFileSync, writeFileSync } from "node:fs";

// Per table, so a change that regressed a dozen targets still shows the ones it
// improved. The artifact holds every row.
const MAX_ROWS = 10;

// The direction word the CLI appends is captured rather than dropped: a bare
// signed percentage in a PR comment reads as ambiguous as it did in the
// terminal. Everything after the baseline label passes through verbatim, so a
// new header segment lands in the comment without a change here.
const ROW = /^ {2}(\S.*?) {2,}([+-]?\d+(?:\.\d+)?)% (slower|faster)$/;
const HEADER = /^performance vs (\S+) \((.+?)\) · (.+)$/;
// Two ways a target produced no timing, both of which a reader has to see: the
// run refused to compare a value against an exception, or it failed outright.
const NOT_TIMED = /^ {2}(behavior changed, not timed - |could not measure )(.+)$/;
// The advisory line carries the only count of what was measured and unchanged.
const UNCHANGED = /(\d+) unchanged/;

type Row = { name: string; pct: string; dir: string };

const table = (rows: Row[]): string[] => {
  const out = ["| target | Δ vs baseline |", "|---|---:|"];
  // Already ordered worst-regression-first by the CLI, so each table's kept
  // rows are its most interesting ones however it is sliced.
  for (const r of rows.slice(0, MAX_ROWS)) out.push(`| \`${r.name}\` | ${r.pct}% ${r.dir} |`);
  out.push("");
  if (rows.length > MAX_ROWS) out.push(`…and ${rows.length - MAX_ROWS} more.`, "");
  return out;
};

// `commit` names the head the numbers were taken from. A run posts a NEW
// comment rather than editing one in place, so a PR accumulates one per push
// and the reader needs each to say which head it measured - the header's other
// sha is the baseline, which doesn't move.
export const renderComment = (
  report: string,
  artifactUrl?: string,
  heading = "Spec performance",
  commit?: string,
): string => {
  const lines = report.split("\n");
  const header = lines.map((l) => l.match(HEADER)).find(Boolean);
  const rows = lines.flatMap((l) => {
    const m = l.match(ROW);
    return m ? [{ name: m[1]!, pct: m[2]!, dir: m[3]! }] : [];
  });
  const notTimed = lines.flatMap((l) => {
    const m = l.match(NOT_TIMED);
    return m ? [`${m[1]!.trim()} ${m[2]!}`] : [];
  });
  // Matched against the raw line, not `l.trim()`: these patterns are anchored on
  // the report's two-space indent, and trimming first made them unmatchable -
  // `new:` and `could not measure` had silently never reached a comment.
  const footer = lines.filter(
    (l) => /^ {2}(new|node )/.test(l) || /advisory only$/.test(l),
  );

  const slower = rows.filter((r) => r.dir === "slower");
  const faster = rows.filter((r) => r.dir === "faster");
  const unchanged = Number(lines.join("\n").match(UNCHANGED)?.[1] ?? NaN);
  const measured = Number.isNaN(unchanged) ? undefined : unchanged + rows.length;

  const out = [`### ${heading}`, ""];

  if (!header) {
    // Only promise a report there is one to reach: the drift job renders into a
    // run summary with nothing uploaded and no link to add below.
    out.push(
      artifactUrl
        ? "_report could not be parsed - see the full report below._"
        : "_report could not be parsed, and no full report was uploaded._",
      "",
    );
    if (artifactUrl) out.push(`[Full report ↗](${artifactUrl})`, "");
    return out.join("\n");
  }

  // The one line a reader who scrolls past everything else should still get
  // right: what moved, against what.
  const verdict = rows.length
    ? [slower.length && `${slower.length} slower`, faster.length && `${faster.length} faster`]
        .filter(Boolean)
        .join(", ")
    : "No significant changes";
  const against = `${commit ? `\`${commit.slice(0, 7)}\` vs ` : ""}\`${header[1]}\` (${header[2]})`;
  out.push(
    `**${verdict}**${measured === undefined ? "" : ` of ${measured} timed targets`} · ${against}`,
    "",
  );

  // Above the tables: a target that could not be timed is a result about this
  // change, and one that reads as a footnote gets read as boilerplate.
  if (notTimed.length) {
    out.push("**Not timed**", "");
    for (const line of notTimed) out.push(`- ${line}`);
    out.push("");
  }

  if (slower.length) out.push("**Slower**", "", ...table(slower));
  if (faster.length) out.push("**Faster**", "", ...table(faster));

  if (artifactUrl) out.push(`[Full report ↗](${artifactUrl})`, "");
  // The convention and the noise floor together say how to read a number and
  // how small a number the run could see at all - reference, not headline.
  out.push(`<sub>${header[3]}</sub>`);
  out.push(...footer.map((l) => `<sub>${l.trim()}</sub>`));
  return out.join("\n");
};

const [, , input, output, heading] = process.argv;
if (input && output)
  writeFileSync(
    output,
    renderComment(readFileSync(input, "utf8"), process.env.ARTIFACT_URL, heading, process.env.HEAD_SHA),
  );
