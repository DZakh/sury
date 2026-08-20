import type { DiffClass, Outcome, Sury } from "./types";

export const show = (value: unknown): string => {
  if (typeof value === "bigint") return `${value}n`;
  if (typeof value === "symbol") return value.toString();
  if (typeof value === "function") return "[Function]";
  if (typeof value === "number" && Object.is(value, -0)) return "-0";
  if (value instanceof Date) return `Date(${value.getTime()})`;
  if (value instanceof URL) return `URL(${value.href})`;
  if (value instanceof Error) return `${value.name}(${value.message})`;
  if (value instanceof Uint8Array) return `Uint8Array(${value.length})`;
  if (typeof Blob !== "undefined" && value instanceof Blob) {
    return `Blob(${value.size})`;
  }
  if (value === undefined) return "undefined";
  try {
    return JSON.stringify(value) ?? String(value);
  } catch {
    return String(value);
  }
};

export const outcomeOf = (S: Sury, produce: () => unknown): Outcome => {
  try {
    return { ok: true, value: show(produce()) };
  } catch (error: any) {
    if (error instanceof S.Error) {
      return {
        ok: false,
        kind: "sury",
        message: error.message,
        reasons: error.unionErrors?.length ?? 0,
      };
    }
    return {
      ok: false,
      kind: "foreign",
      name: error?.constructor?.name ?? "unknown",
      message: String(error?.message ?? error),
    };
  }
};

export const describeOutcome = (outcome: Outcome): string =>
  outcome.ok
    ? `ok(${outcome.value})`
    : outcome.kind === "foreign"
      ? `foreign(${outcome.name}: ${outcome.message})`
      : `sury(${outcome.reasons} reasons): ${outcome.message}`;

export const classify = (before: Outcome, after: Outcome): DiffClass => {
  if (before.ok !== after.ok) return "acceptance";
  if (before.ok && after.ok) return "acceptance";
  const b = before as Extract<Outcome, { ok: false }>;
  const a = after as Extract<Outcome, { ok: false }>;
  if (b.kind !== a.kind) return "exception-kind";
  if (b.kind === "sury" && a.kind === "sury") {
    const top = (m: string) => m.split("\n")[0];
    if (top(b.message) === top(a.message)) return "reasons";
  }
  return "message";
};
