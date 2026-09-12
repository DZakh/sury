// The shape a generated benchmark page is built from, and how a number in it
// reads.
//
// Every page is four tables with the same shape, so one renderer covers all of
// them and a new topic is data rather than code. Cells hold raw numbers, not
// rendered text: `cli.ts` compares a fresh measurement against the committed
// golden, and "8.7 kB" against "8.7 kB" would hide a 40-byte move.

export type Cell = string | number | null;

// How a row's numbers read, and which way is better. `best: "low"` bolds the
// smallest cell of the row, `"high"` the largest, and an omitted `best` bolds
// nothing - the right answer for a row whose columns are not comparable.
export type Format = "bytes" | "ns" | "opsPerMs" | "count" | "text";

export type Row = {
  label: string;
  cells: Cell[];
  format?: Format;
  best?: "low" | "high";
  note?: string;
};

export type Table = {
  columns: string[];
  rows: Row[];
  // A column of numbers reads against the right edge; a column of check marks
  // reads under its heading. Defaults to right.
  align?: "right" | "center";
  // Prose under the table: what was measured, and what a reader would
  // otherwise have to guess.
  note?: string;
};

export type Topic = {
  id: string;
  // The page's H1 and the label it is linked by, which differ: a nav line has
  // room for "Protobuf" and the page itself says what it is about.
  title: string;
  label: string;
  blurb: string;
  // Exactly what was measured against what, as `name version`. A table without
  // this is a claim nobody can reproduce.
  versions: string[];
  bundleSize: Table;
  features: Table;
  performance: Table;
  conformance: Table;
};

const KB = (bytes: number): string =>
  bytes >= 1000 ? `${(bytes / 1000).toFixed(bytes >= 10_000 ? 1 : 2)} kB` : `${bytes} B`;

const NS = (ns: number): string =>
  ns >= 1e6 ? `${(ns / 1e6).toFixed(2)} ms` : ns >= 1000 ? `${(ns / 1000).toFixed(2)} µs` : `${ns.toFixed(0)} ns`;

const OPS = (opsPerMs: number): string =>
  opsPerMs >= 1000 ? Math.round(opsPerMs).toLocaleString("en-US") : opsPerMs.toPrecision(3);

export const formatCell = (cell: Cell, format: Format = "text"): string => {
  // A column a library has no answer for. An empty cell reads as an oversight,
  // so say it is not applicable.
  if (cell === null) return "n/a";
  if (typeof cell === "string") return cell;
  switch (format) {
    case "bytes":
      return KB(cell);
    case "ns":
      return NS(cell);
    case "opsPerMs":
      return OPS(cell);
    default:
      return String(cell);
  }
};

export const winners = (row: Row): boolean[] => {
  const numbers = row.cells.map((c) => (typeof c === "number" ? c : null));
  if (row.best === undefined || numbers.every((n) => n === null)) return row.cells.map(() => false);
  const defined = numbers.filter((n): n is number => n !== null);
  const target = row.best === "low" ? Math.min(...defined) : Math.max(...defined);
  return numbers.map((n) => n === target);
};
