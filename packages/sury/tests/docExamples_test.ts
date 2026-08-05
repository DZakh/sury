// Every code example in the user-facing docs and in the interface docs
// (JSDoc in index.d.ts, docstrings in src/S.res) must compile. See
// docExamples.ts for how blocks are extracted and checked.
import path from "node:path";
import { fileURLToPath } from "node:url";
import { test, expect } from "vitest";
import {
  extractMarkdownBlocks,
  extractDocCommentBlocks,
  isTs,
  isRes,
  buildTsUnits,
  buildResUnit,
  buildResUnitPerBlock,
  compileTsUnits,
  compileResUnits,
  formatFailures,
  type TsUnit,
  type ResUnit,
} from "./docExamples";

const projectPath = fileURLToPath(new URL("..", import.meta.url));
const repoRootPath = path.join(projectPath, "../..");

const markdownDocs = [
  path.join(repoRootPath, "README.md"),
  path.join(repoRootPath, "docs/js-usage.md"),
  path.join(repoRootPath, "docs/rescript-usage.md"),
];

const tsUnits: TsUnit[] = [];
const resUnits: ResUnit[] = [];
let markdownTsBlocks = 0;
let markdownResBlocks = 0;

for (const doc of markdownDocs) {
  const blocks = extractMarkdownBlocks(doc);
  const slug = path.basename(doc, ".md").replace(/[^a-zA-Z0-9]/g, "_");
  const tsBlocks = blocks.filter(isTs);
  const resBlocks = blocks.filter(isRes);
  markdownTsBlocks += tsBlocks.length;
  markdownResBlocks += resBlocks.length;
  tsUnits.push(...buildTsUnits(tsBlocks, `doc_${slug}_`));
  const resUnit = buildResUnit(resBlocks, `DocExamples_${slug}`);
  if (resUnit) resUnits.push(resUnit);
}

// Interface docs: every example must stand alone — it's what an editor
// tooltip shows, with no surrounding tutorial to lean on.
const jsdocBlocks = extractDocCommentBlocks(path.join(projectPath, "index.d.ts"));
tsUnits.push(
  ...jsdocBlocks
    .filter(isTs)
    .flatMap((block, i) => buildTsUnits([block], `jsdoc_${i}_`))
);
const docstringBlocks = extractDocCommentBlocks(path.join(projectPath, "src/S.res"));
resUnits.push(...buildResUnitPerBlock(docstringBlocks.filter(isRes), "DocExamplesSres"));

test("docs contain the expected kinds of examples", () => {
  // If extraction silently broke, both compile tests would pass vacuously.
  expect(markdownTsBlocks).toBeGreaterThan(50);
  expect(markdownResBlocks).toBeGreaterThan(50);
  expect(jsdocBlocks.filter(isTs).length).toBeGreaterThan(15);
  expect(docstringBlocks.filter(isRes).length).toBeGreaterThan(15);
});

test("TypeScript doc examples compile", () => {
  const failures = compileTsUnits(tsUnits);
  expect(formatFailures(failures)).toBe("");
});

test("ReScript doc examples compile", async () => {
  const failures = await compileResUnits(resUnits);
  expect(formatFailures(failures)).toBe("");
}, 120_000);
