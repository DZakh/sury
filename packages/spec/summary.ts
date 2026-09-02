// What `spec check --write` just changed, as a ranked summary.
//
// The golden diff is the deliverable, but reading it means opening every
// rewritten file. This renders the same information in one place: each tracked
// metric that regressed or improved (worst first, by percentage), plus the
// behavior changes that aren't better-or-worse but do need noticing.
import {
  OP_ORDER,
  JSON_SCHEMA_TARGETS,
  isSkip,
  isCreationError,
  type Spec,
  type BundleSize,
  type Example,
  type Operation,
} from "./format";
// Type-only: bench.ts bundles both library versions and forks processes, none
// of which spec_test.ts (which imports summarize) should pull in.
import type { Perf } from "./bench";

export type SpecChange = { id: string; before: Spec; after: Spec };
export type BundleSizeChange = { before?: BundleSize; after: BundleSize };

type Delta = { label: string; before: number; after: number };

// Generated code is the hot path, so the summary shows the codegen itself, not
// just how much of it there is. Clipped because a discriminated-union parse
// runs past 600 characters and the point is to be readable at a glance — the
// spec file has the untruncated text.
type ExpressionDelta = Delta & { beforeSrc: string; afterSrc: string };
const EXPRESSION_CLIP = 200;

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

// Ranked like any other metric, but each entry carries its own before/after
// block, so it renders as one stanza per operation rather than a flat row.
const expressionSection = (deltas: ExpressionDelta[]): string[] => {
  const moved = deltas.filter((d) => d.beforeSrc !== d.afterSrc).sort((a, b) => pct(b) - pct(a));
  if (!moved.length) return [];
  const chars = render(moved.map((d) => ({ ...d, label: "chars" })));
  return [
    "operations.expression:",
    ...moved.flatMap((d, i) => [
      `  ${d.label}:`,
      `    ${chars[i]}`,
      `    before  ${clip(d.beforeSrc, EXPRESSION_CLIP)}`,
      `    after   ${clip(d.afterSrc, EXPRESSION_CLIP)}`,
    ]),
  ];
};

const outcome = (ex: Example): string => ("output" in ex ? `output ${ex.output}` : `error ${ex.error}`);

// How an op resolved, for the behavior list — enough to read a flip between
// compiling and being rejected at operation creation at a glance.
const opKind = (op: Operation): string =>
  typeof op === "string" ? op : isCreationError(op) ? `creationError ${op.creationError}` : "compiled";

// `before` is the spec as it was on disk, so a hand-authored one is missing
// every derived field. Absent is not a value that changed into another one —
// there is nothing to diff, and formatting `undefined` is what used to crash
// the whole summary.
const changed = (
  label: string,
  before: string | undefined,
  after: string | undefined,
  out: string[],
): void => {
  if (before === undefined || after === undefined || before === after) return;
  out.push(`${label}  ${clip(before)} → ${clip(after)}`);
};

const changedOptional = (
  label: string,
  before: string | undefined,
  after: string | undefined,
  out: string[],
): void => {
  if (before === after) return;
  out.push(`${label}  ${clip(before ?? "omitted")} → ${clip(after ?? "omitted")}`);
};

// A spec written from scratch has no goldens on its `before` side, so every
// field would report as a change. That's noise — the `wrote <id>` line already
// named it. Listed as new instead, mirroring bundleSize's "first recorded".
const isNewSpec = (before: Spec): boolean =>
  (before as Partial<Spec>).jsonSchema === undefined;

const specDeltas = (
  changes: SpecChange[],
): { instantiations: Delta[]; expression: ExpressionDelta[]; behavior: string[]; added: string[] } => {
  const instantiations: Delta[] = [];
  const expression: ExpressionDelta[] = [];
  const behavior: string[] = [];
  const added: string[] = [];

  for (const { id, before, after } of changes) {
    if (isNewSpec(before)) {
      added.push(id);
      continue;
    }
    if (
      typeof before.ts.instantiations === "number" &&
      typeof after.ts.instantiations === "number"
    )
      instantiations.push({ label: id, before: before.ts.instantiations, after: after.ts.instantiations });

    for (const side of ["input", "output"] as const) {
      const b = before.ts[side];
      const a = after.ts[side];
      if (!isSkip(b) && !isSkip(a)) changed(`${id}.ts.${side}`, b, a, behavior);
      changed(`${id}.jsonSchema.${side}`, before.jsonSchema?.[side], after.jsonSchema?.[side], behavior);
      const typeSide = `from${side === "input" ? "Input" : "Output"}Type` as const;
      changedOptional(
        `${id}.jsonSchema.${typeSide}`,
        before.jsonSchema?.[typeSide],
        after.jsonSchema?.[typeSide],
        behavior,
      );
    }
    for (const name of JSON_SCHEMA_TARGETS) {
      changed(
        `${id}.jsonSchema.${name}`,
        JSON.stringify(before.jsonSchema?.[name] ?? null),
        JSON.stringify(after.jsonSchema?.[name] ?? null),
        behavior,
      );
    }

    for (const op of OP_ORDER) {
      const b = before.operations[op];
      const a = after.operations[op];
      // Rejected at operation creation on both sides: the thrown message is
      // this op's only golden, so a drifting message IS the behavior change.
      if (isCreationError(b) && isCreationError(a)) {
        changed(`${id}.${op}.creationError`, b.creationError, a.creationError, behavior);
        continue;
      }
      // Exactly one side is a creationError (both were handled above), so this
      // op flipped between compiling and being rejected at creation — the
      // loudest change an op can have. Reported even when the other side is a
      // shorthand, since `--write` does perform this flip (unlike a shorthand
      // mismatch, an op that newly fails at creation doesn't block the write).
      if (isCreationError(b) || isCreationError(a)) {
        behavior.push(`${id}.${op}  ${clip(opKind(b))} → ${clip(opKind(a))}`);
        continue;
      }
      // An op's shorthand can't change under --write (a shorthand mismatch
      // blocks the write), so a differing kind here means a hand edit.
      if (typeof b === "string" || typeof a === "string") continue;
      if (
        !isSkip(b.expression) &&
        !isSkip(a.expression) &&
        typeof b.expression === "string" &&
        typeof a.expression === "string"
      )
        expression.push({
          label: `${id}.${op}`,
          before: b.expression.length,
          after: a.expression.length,
          beforeSrc: b.expression,
          afterSrc: a.expression,
        });
      for (const [name, ex] of Object.entries(a.examples)) {
        const prev = b.examples[name];
        if (prev) changed(`${id}.${op}.${name}`, outcome(prev), outcome(ex), behavior);
      }
    }
  }
  return { instantiations, expression, behavior, added };
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

// Sorted worst-regression-first like every other section, so the perf table
// reads the same way as the instantiations and bundleSize ones. Positive is a
// slowdown, matching those (where growth is the bad direction) — and matching
// the ratio the child measures, current over baseline. That convention is not
// guessable from a bare percentage, so every row says which way it went and the
// header states the rule; `perfComment.ts` parses both back out.
export const renderPerformance = (perf: Perf): string => {
  const floors = perf.floors.map((f) => `${f.phase} ${f.pct.toFixed(1)}%`).join(" · ");
  const lines = [
    `performance vs ${perf.baselineSha} (${perf.baselineLabel})` +
      ` · +% slower than baseline, -% faster · noise floor ${floors}`,
  ];

  if (perf.changed.length) {
    const width = Math.max(...perf.changed.map((c) => c.name.length));
    for (const c of perf.changed)
      lines.push(
        `  ${c.name.padEnd(width)}  ${c.pct > 0 ? "+" : ""}${c.pct.toFixed(1)}%` +
          ` ${c.pct > 0 ? "slower" : "faster"}`,
      );
  } else {
    lines.push("  no significant changes");
  }

  if (perf.added.length) lines.push(`  new: ${perf.added.join(", ")}`);
  for (const o of perf.outcomeChanged)
    lines.push(`  behavior changed, not timed — ${o.name}: ${o.note}`);
  for (const e of perf.errors) lines.push(`  could not measure ${e.name}: ${e.error}`);
  lines.push(
    `  ${perf.unchanged} unchanged · ${perf.skippedConstants} constant-schema targets skipped · ` +
      (perf.skippedAsync ? `${perf.skippedAsync} async examples skipped · ` : "") +
      "advisory only",
    `  ${perf.meta}`,
  );
  return lines.join("\n");
};

// Empty when nothing tracked moved — a formatting-only rewrite has no summary
// to give, and the `wrote <id>` lines already said what was touched.
export const summarize = (changes: SpecChange[], bundleSize?: BundleSizeChange): string => {
  const { instantiations, expression, behavior, added } = specDeltas(changes);
  const lines = [
    ...(added.length ? [`new: ${added.join(", ")}`] : []),
    ...section("ts.instantiations", instantiations),
    ...expressionSection(expression),
    ...(bundleSize ? bundleSizeSection(bundleSize) : []),
    ...(behavior.length ? ["behavior changed:", ...behavior.map((l) => `  ${l}`)] : []),
  ];
  return lines.join("\n");
};
