// Turns a `vitest bench --outputJson` report into a markdown PR comment body.
// Used by the CI "Wall-clock Benchmarks" job (see .github/workflows/ci.yml).
import { readFileSync, writeFileSync } from "node:fs";

interface Benchmark {
  rank: number;
  name: string;
  hz: number;
  rme: number;
}

interface BenchReport {
  files: {
    groups: {
      fullName: string;
      benchmarks: Benchmark[];
    }[];
  }[];
}

const [, , inputPath, outputPath] = process.argv;
if (!inputPath || !outputPath) {
  console.error("Usage: format-wallclock-comment.ts <bench-results.json> <comment.md>");
  process.exit(1);
}

const report: BenchReport = JSON.parse(readFileSync(inputPath, "utf8"));

const formatHz = (hz: number) => hz.toLocaleString("en-US", { maximumFractionDigits: 0 });
const groupName = (fullName: string) => fullName.split(" > ").pop();
const benchLabel = (name: string) => name.split(": ").pop();

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
## 📊 Wall-clock benchmarks (\`comparison.bench.ts\`)
${tables}
<sub>Commit \`${sha}\`${runUrl ? ` • [workflow run](${runUrl})` : ""}</sub>
`;

writeFileSync(outputPath, body);
console.log(`Wrote ${outputPath}`);
