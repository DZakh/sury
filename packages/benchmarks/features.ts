// The feature tables, and the one thing that keeps them honest.
//
// A cell is either a probe or a claim. A probe runs the call a user would
// write, against the library in that column, and the table prints what
// happened. A claim is read off that library's documentation, and is marked
// with a dagger so a reader can tell the two apart - because a claim is the
// kind of thing that is true when it is written and quietly false a year
// later, and the page should not hide which cells those are.
//
// Competitor libraries are installed in this workspace, so most cells can be
// probes. Reach for a claim only where running the call is not possible.
import type { Row, Table } from "./table";

export const YES = "✅";
export const NO = "❌";
export const PARTLY = "⭕";
const CLAIMED = "†";

// True when `fn` runs to completion and does not return `false`. A feature is
// supported when the call a user would write works.
export const works = (fn: () => unknown): boolean => {
  try {
    return fn() !== false;
  } catch {
    return false;
  }
};

// The opposite question, for a feature that is about refusing: true when `fn`
// throws. A codec that accepts a value it cannot represent is the bug.
export const rejects = (fn: () => unknown): boolean => {
  try {
    fn();
    return false;
  } catch {
    return true;
  }
};

export type CellSpec = string | (() => boolean);

export type Feature = {
  label: string;
  note?: string;
  cells: CellSpec[];
};

export const buildFeatures = (columns: string[], features: Feature[]): Table => {
  const rows: Row[] = [];
  let claimed = false;
  for (const feature of features) {
    if (feature.cells.length !== columns.length) {
      throw new Error(`feature "${feature.label}" has ${feature.cells.length} cells for ${columns.length} columns`);
    }
    rows.push({
      label: feature.label,
      note: feature.note,
      cells: feature.cells.map((cell) => {
        if (typeof cell !== "string") return cell() ? YES : NO;
        // The dagger goes on the symbol, not at the end of the sentence a
        // cell may carry after it.
        for (const symbol of [YES, NO, PARTLY]) {
          if (cell.startsWith(symbol)) {
            claimed = true;
            return `${symbol}${CLAIMED}${cell.slice(symbol.length)}`;
          }
        }
        return cell;
      }),
    });
  }
  return {
    columns,
    rows,
    align: "center",
    note: claimed
      ? `<sub>Unmarked ${YES} and ${NO} are probes: the call was run against that library at the version above, and the table prints what happened. ${CLAIMED} marks a cell read from the library's documentation instead.</sub>`
      : `<sub>Every ${YES} and ${NO} is a probe: the call was run against that library at the version above, and the table prints what happened.</sub>`,
  };
};
