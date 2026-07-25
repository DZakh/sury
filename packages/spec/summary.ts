// What `spec check --write` just changed, as a ranked summary.
//
// The golden diff is the deliverable, but reading it means opening every
// rewritten file. This renders the same information in one place: each tracked
// metric that regressed or improved (worst first, by percentage), plus the
// behavior changes that aren't better-or-worse but do need noticing.
import { OP_ORDER, isSkip, type Spec, type BundleSize, type Example } from "./format";

export type SpecChange = { id: string; before: Spec; after: Spec };
export type BundleSizeChange = { before?: BundleSize; after: BundleSize };

type Delta = { label: string; before: number; after: number };

// A metric that grew from nothing has no meaningful percentage, so it ranks
// above every finite one rather than sorting as 0%.
const pct = (d: Delta): number => (d.before === 0 ? Infinity * Math.sign(d.after - d.before) : ((d.after - d.before) / d.before) * 100);

const clip = (s: string, max = 80): string => (s.length > max ? `${s.slice(0, max - 1)}…` : s);

// Columns are padded to align across a section: the point of the summary is
// being scannable, and ragged numbers defeat that.
const render = (deltas: Delta[]): string[] => {
  const label = Math.max(...deltas.map((d) => d.label.length));
  const before = Math.max(...deltas.map((d) => String(d.before).length));
  const after = Math.max(...deltas.map((d) => String(d.after).length));
  return deltas.map((d) => {
    const p = pct(d);
    const percent = Number.isFinite(p) ? `${p > 0 ? "+" : ""}${p.toFixed(1)}%` : "";
    const nums = `${String(d.before).padStart(before)} → ${String(d.after).padStart(after)}`;
    return `${d.label.padEnd(label)}  ${nums}  ${percent}`.trimEnd();
  });
};

// One list ordered by percentage, worst regression to biggest improvement —
// no regression/improvement grouping, since the sign already separates them.
const section = (title: string, deltas: Delta[], lead: string[] = []): string[] => {
  const moved = deltas.filter((d) => d.after !== d.before).sort((a, b) => pct(b) - pct(a));
  if (!moved.length && !lead.length) return [];
  return [`${title}:`, ...lead.map((l) => `  ${l}`), ...render(moved).map((l) => `  ${l}`)];
};

const outcome = (ex: Example): string => ("output" in ex ? `output ${ex.output}` : `error ${ex.error}`);

const changed = (label: string, before: string, after: string, out: string[]): void => {
  if (before !== after) out.push(`${label}  ${clip(before)} → ${clip(after)}`);
};

const specDeltas = (
  changes: SpecChange[],
): { instantiations: Delta[]; expression: Delta[]; behavior: string[] } => {
  const instantiations: Delta[] = [];
  const expression: Delta[] = [];
  const behavior: string[] = [];

  for (const { id, before, after } of changes) {
    if (!isSkip(before.ts.instantiations) && !isSkip(after.ts.instantiations))
      instantiations.push({ label: id, before: before.ts.instantiations, after: after.ts.instantiations });

    for (const side of ["input", "output"] as const) {
      const b = before.ts[side];
      const a = after.ts[side];
      if (!isSkip(b) && !isSkip(a)) changed(`${id}.ts.${side}`, b, a, behavior);
      changed(`${id}.jsonSchema.${side}`, before.jsonSchema[side], after.jsonSchema[side], behavior);
    }

    for (const op of OP_ORDER) {
      const b = before.operations[op];
      const a = after.operations[op];
      // An op's shorthand can't change under --write (a shorthand mismatch
      // blocks the write), so a differing kind here means a hand edit.
      if (typeof b === "string" || typeof a === "string") continue;
      if (!isSkip(b.expression) && !isSkip(a.expression))
        expression.push({ label: `${id}.${op}`, before: b.expression.length, after: a.expression.length });
      for (const [name, ex] of Object.entries(a.examples)) {
        const prev = b.examples[name];
        if (prev) changed(`${id}.${op}.${name}`, outcome(prev), outcome(ex), behavior);
      }
    }
  }
  return { instantiations, expression, behavior };
};

const bundleSizeSection = (change: BundleSizeChange): string[] => {
  const { before, after } = change;
  if (!before) return [`bundleSize:`, `  first recorded — ${Object.keys(after.exports).length} exports, total ${after.total}`];

  const lead: string[] = [];
  if (before.total !== after.total) lead.push(...render([{ label: "total", before: before.total, after: after.total }]));

  const added = Object.keys(after.exports).filter((name) => !(name in before.exports));
  const removed = Object.keys(before.exports).filter((name) => !(name in after.exports));
  if (added.length) lead.push(`added: ${added.map((n) => `${n} ${after.exports[n]}`).join(", ")}`);
  if (removed.length) lead.push(`removed: ${removed.join(", ")}`);

  const deltas = Object.entries(after.exports)
    .filter(([name]) => name in before.exports)
    .map(([name, bytes]) => ({ label: name, before: before.exports[name]!, after: bytes }));

  return section("bundleSize", deltas, lead);
};

// Empty when nothing tracked moved — a formatting-only rewrite has no summary
// to give, and the `wrote <id>` lines already said what was touched.
export const summarize = (changes: SpecChange[], bundleSize?: BundleSizeChange): string => {
  const { instantiations, expression, behavior } = specDeltas(changes);
  const lines = [
    ...section("ts.instantiations", instantiations),
    ...section("operations.expression (chars)", expression),
    ...(bundleSize ? bundleSizeSection(bundleSize) : []),
    ...(behavior.length ? ["behavior changed:", ...behavior.map((l) => `  ${l}`)] : []),
  ];
  return lines.join("\n");
};
