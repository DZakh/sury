// Reading and writing packages/benchmarks/goldens/<topic>.json, the measured
// record each page is rendered from.
//
// The page is generated from the golden rather than the other way round: a
// check run remeasures what it can and re-renders, so an edit to either the
// markdown or the numbers under it shows as a diff.
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";
import type { Topic } from "./table";

export type Golden = { $comment: string; machine: string } & Topic;

const GOLDENS = fileURLToPath(new URL("./goldens/", import.meta.url));

export const goldenPath = (id: string): string => path.join(GOLDENS, `${id}.json`);

export const readGolden = (id: string): Golden | undefined => {
  try {
    return JSON.parse(readFileSync(goldenPath(id), "utf8")) as Golden;
  } catch {
    return undefined;
  }
};

export const writeGolden = (golden: Golden): void =>
  writeFileSync(goldenPath(golden.id), `${JSON.stringify(golden, null, 2)}\n`);
