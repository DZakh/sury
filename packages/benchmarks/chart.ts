// One topic's performance table as an SVG, in a light cut and a dark one.
//
// Timings move with the runner, so they are the one section that is not
// committed: main remeasures them, publishes the two files to the `benchmarks`
// branch, and the page embeds them from there. The page's own markdown never
// changes, which is what keeps a bot push to a protected `main` out of it.
//
// Nothing outside the file may be referenced. The reader's browser renders it
// inside an `<img>`, where an external font or stylesheet never loads and a
// script never runs.
import { type Format, type Row, type Table, formatCell, winners } from "./table";

// How the machine that timed a run is named on the chart it produced.
export const provenance = (): string =>
  `node ${process.version.slice(1)} · ${process.platform} ${process.arch} · ${new Date().toISOString().slice(0, 10)}`;

// A bar carries the measurement and the accent carries which one won, the same
// claim the markdown made by bolding a cell. Identity is never the colour's
// job: every bar is named and valued beside itself, so the two greys are
// de-emphasis rather than a second series. Both pairs clear 3:1 against the
// surface they sit on and read apart under every simulated deficiency.
type Theme = {
  emphasis: string;
  context: string;
  primary: string;
  secondary: string;
  muted: string;
};

const THEMES: Record<"light" | "dark", Theme> = {
  // GitHub's own canvas, not the chart's: the SVG paints no background, so it
  // sits on whichever of the two the reader is on.
  light: { emphasis: "#2a78d6", context: "#898781", primary: "#0b0b0b", secondary: "#52514e", muted: "#898781" },
  dark: { emphasis: "#3987e5", context: "#898781", primary: "#ffffff", secondary: "#c3c2b7", muted: "#898781" },
};

const WIDTH = 720;
const PAD = 4;
const BAND = 14;
const BAR = 7;
const HEADER = 17;
const GROUP_GAP = 10;
const FOOT = 24;
const FONT = `system-ui, -apple-system, "Segoe UI", sans-serif`;

// An SVG nobody measures a glyph for, so a column is sized from an average
// advance width instead. 0.58em is close enough for a system sans at 11px that
// a name has room and the value labels do not collide with their bars.
const textWidth = (text: string, size: number): number => text.length * size * 0.58;

const escape = (text: string): string =>
  text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

const text = (x: number, y: number, content: string, fill: string, size: number, extra = ""): string =>
  `<text x="${x.toFixed(1)}" y="${y}" fill="${fill}" font-size="${size}"${extra}>${escape(content)}</text>`;

// What a bar's length stands for. A duration is drawn as its own reciprocal,
// which is the throughput printed in the same label: at 116x between the
// fastest library and the slowest, drawing the duration itself leaves the
// winner a sliver and spends the chart's ink on the worst result. Every other
// format is drawn as the number it is.
const weight = (value: number, format: Format | undefined): number => (format === "ns" ? 1 / value : value);

// Square where it leaves the baseline, round where the measurement ends.
const bar = (x: number, y: number, length: number, fill: string): string => {
  const r = BAR / 2;
  return `<path d="M${x} ${y}h${(length - r).toFixed(1)}a${r} ${r} 0 0 1 0 ${BAR}H${x}z" fill="${fill}"/>`;
};

const group = (row: Row, columns: string[], theme: Theme, gutter: number, barArea: number, top: number): string => {
  const out: string[] = [];
  const label = `<tspan font-weight="600" fill="${theme.primary}">${escape(row.label)}</tspan>`;
  const note = row.note === undefined ? "" : `<tspan fill="${theme.muted}"> · ${escape(row.note)}</tspan>`;
  out.push(`<text x="${PAD}" y="${top + 11}" font-size="12">${label}${note}</text>`);

  const numbers = row.cells.map((cell) => (typeof cell === "number" ? weight(cell, row.format) : null));
  const defined = numbers.filter((n): n is number => n !== null);
  const max = defined.length === 0 ? 0 : Math.max(...defined);
  const won = winners(row);
  const barX = PAD + gutter;

  columns.forEach((column, index) => {
    const bandTop = top + HEADER + index * BAND;
    const baseline = bandTop + BAND / 2 + 3.5;
    out.push(text(barX - 8, baseline, column, theme.secondary, 11, ` text-anchor="end"`));

    const value = numbers[index];
    let end = barX;
    if (value !== null && value !== undefined && max > 0) {
      // A value two orders off the best is a sliver, which is the honest
      // reading. The floor is the round end's own radius, below which the mark
      // stops looking like a bar at all.
      const length = Math.max(BAR / 2, (value / max) * barArea);
      out.push(bar(barX, bandTop + (BAND - BAR) / 2, length, won[index] ? theme.emphasis : theme.context));
      end = barX + length;
    }
    out.push(
      text(end + 6, baseline, formatCell(row.cells[index]!, row.format), theme.secondary, 11, ` font-weight="600"`),
    );
  });
  return out.join("\n");
};

export const renderChart = (table: Table, machine: string, mode: "light" | "dark"): string => {
  const theme = THEMES[mode];
  const gutter = Math.min(220, Math.max(80, ...table.columns.map((c) => textWidth(c, 11) + 14)));
  const valueWidth = Math.max(
    ...table.rows.flatMap((row) => row.cells.map((cell) => textWidth(formatCell(cell, row.format), 11) + 12)),
  );
  const barArea = WIDTH - PAD * 2 - gutter - valueWidth;

  let y = 6;
  const groups: string[] = [];
  for (const row of table.rows) {
    groups.push(group(row, table.columns, theme, gutter, barArea, y));
    y += HEADER + table.columns.length * BAND + GROUP_GAP;
  }
  const height = y - GROUP_GAP + FOOT;
  // What the bars mean, in the one sentence a reader needs before reading them.
  // A page whose rows disagree on a direction gets no claim rather than a wrong
  // one.
  const directions = new Set(table.rows.map((row) => (row.format === "ns" ? "ns" : row.best)));
  const direction = directions.size === 1 ? [...directions][0] : undefined;
  const better =
    direction === undefined
      ? ""
      : `${direction === "ns" ? "Longer is faster" : direction === "low" ? "Lower is better" : "Higher is better"} · `;

  return `<svg xmlns="http://www.w3.org/2000/svg" width="${WIDTH}" height="${height}" viewBox="0 0 ${WIDTH} ${height}" font-family='${FONT}' role="img">
<desc>${escape(table.rows.map((r) => `${r.label}: ${table.columns.map((c, i) => `${c} ${formatCell(r.cells[i]!, r.format)}`).join(", ")}`).join(". "))}</desc>
${groups.join("\n")}
${text(PAD, height - 8, `${better}Timed on ${machine}`, theme.muted, 10)}
</svg>
`;
};
