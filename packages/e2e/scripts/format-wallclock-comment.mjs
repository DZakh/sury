#!/usr/bin/env node
// Turns a `vitest bench --outputJson` report into a markdown PR comment body.
// Used by the CI "Wall-clock Benchmarks" job (see .github/workflows/ci.yml),
// which reports real timings as a complement to CodSpeed's simulated
// instruction counts.
import { readFileSync, writeFileSync } from "node:fs";

const [, , inputPath, outputPath] = process.argv;
if (!inputPath || !outputPath) {
  console.error("Usage: format-wallclock-comment.mjs <bench-results.json> <comment.md>");
  process.exit(1);
}

const report = JSON.parse(readFileSync(inputPath, "utf8"));

const formatHz = (hz) => hz.toLocaleString("en-US", { maximumFractionDigits: 0 });
const groupName = (fullName) => fullName.split(" > ").pop();
const benchLabel = (name) => name.split(": ").pop();

let tables = "";
for (const file of report.files) {
  for (const group of file.groups) {
    const rows = [...group.benchmarks].sort((a, b) => a.rank - b.rank);
    const fastestHz = rows[0]?.hz ?? 0;
    tables += `\n### ${groupName(group.fullName)}\n\n`;
    tables += `| Rank | Library | ops/sec | vs fastest | ±rme |\n`;
    tables += `|---|---|---|---|---|\n`;
    for (const b of rows) {
      const rel = b.hz === fastestHz ? "—" : `${(fastestHz / b.hz).toFixed(2)}x slower`;
      tables += `| ${b.rank} | ${benchLabel(b.name)} | ${formatHz(b.hz)} | ${rel} | ${b.rme.toFixed(2)}% |\n`;
    }
  }
}

const sha = process.env.GITHUB_SHA?.slice(0, 7);
const runUrl =
  process.env.GITHUB_SERVER_URL && process.env.GITHUB_REPOSITORY && process.env.GITHUB_RUN_ID
    ? `${process.env.GITHUB_SERVER_URL}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}`
    : null;

const marker = "<!-- wallclock-bench-comment -->";
const body = `${marker}
## 📊 Wall-clock benchmark (\`comparison.bench.ts\`)

Real wall-clock timings from \`vitest bench\`, measured directly on this CI runner — a complement to the *Benchmarks* job's CodSpeed-simulated instruction counts, not a replacement. Shared runners are noisy, so treat this as informational only; it's not used for regression gating.
${tables}
<sub>Commit \`${sha}\`${runUrl ? ` • [workflow run](${runUrl})` : ""}</sub>
`;

writeFileSync(outputPath, body);
console.log(`Wrote ${outputPath}`);
