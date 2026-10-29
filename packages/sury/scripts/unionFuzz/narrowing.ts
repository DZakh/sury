// A union of consts, or of objects, tuples or arrays told apart by a const,
// converted into fewer of them: into a union of some, where each member the
// target holds passes through as it is and every other fails as a value
// (CODEC_SPEC rule 4), or into one, which the members holding another const
// make ambiguous, so the operation is refused when it is created (rule 3).
// The target may hold the kept consts as one field's union instead, sit
// beside a string member both sides hold, or nest the const a level down.
// Held member by member: the target never answers with a member the value
// wasn't.
import { type Rng } from "./generate";
import { show } from "./sample";
import type { Sury } from "./types";

const CONSTS = [0, 1, 2, -1, "a", "b", "", "0", true, false];

type Kind = { name: string; wrap: (S: Sury, value: unknown) => unknown; value: (value: unknown) => unknown; field?: (S: Sury, values: unknown[]) => unknown };
const kinds: Kind[] = [
  { name: "const", wrap: (S, value) => S.literal(value), value: (value) => value },
  {
    name: "object",
    wrap: (S, value) => S.schema({ t: S.literal(value), v: S.string }),
    value: (value) => ({ t: value, v: "x" }),
    field: (S, values) => S.schema({ t: S.union(values.map((value) => S.literal(value))), v: S.string }),
  },
  { name: "tuple", wrap: (S, value) => S.tuple([S.literal(value), S.string]), value: (value) => [value, "x"] },
  { name: "array", wrap: (S, value) => S.array(S.literal(value)), value: (value) => [value] },
  { name: "nested", wrap: (S, value) => S.schema({ o: S.schema({ t: S.literal(value) }) }), value: (value) => ({ o: { t: value } }) },
];

const pick = <T>(rng: Rng, list: readonly T[]): T => list[Math.floor(rng() * list.length)]!;

const same = (a: unknown, b: unknown): boolean => JSON.stringify(a) === JSON.stringify(b);

export const narrowingFindings = (S: Sury, rng: Rng, cases: number): string[] => {
  const findings: string[] = [];
  for (let c = 0; c < cases; c++) {
    const kind = pick(rng, kinds);
    const pool = [...new Set(Array.from({ length: 2 + Math.floor(rng() * 3) }, () => pick(rng, CONSTS)))];
    if (pool.length < 2) continue;
    const kept = pool.filter((_, idx) => idx === 0 || rng() < 0.5);
    // One field's union of the kept consts, as one target the values pass
    // into, only for consts of one type, a union of several being refused.
    const asField = kind.field !== U && rng() < 0.3 && kept.length > 1 && kept.every((value) => typeof value === typeof kept[0]);
    const single = kept.length === 1 && !asField;
    const beside = !asField && !single && kind.name !== "const" && rng() < 0.3;
    const members = pool.map((value) => kind.wrap(S, value));
    const source = S.union(beside ? [...members, S.string] : members);
    const keptMembers = kept.map((value) => kind.wrap(S, value));
    const target = asField ? kind.field!(S, kept) : single ? keptMembers[0] : S.union(beside ? [...keptMembers, S.string] : keptMembers);
    const label = `${kind.name} ${show(pool)} -> ${asField ? "field union " : single ? "" : beside ? "union with string " : "union "}${show(kept)}`;
    let parse: (value: unknown) => unknown;
    try {
      parse = S.parseOrThrow(source.with(S.to, target));
    } catch (error) {
      // Into one target, a member holding another const makes the conversion
      // ambiguous, and into a union, so does a const the target keeps nothing
      // of its type for (rule 4's coverage, or a field's consts it could
      // become any of).
      const uncovered = (kind.name === "const" || asField) && pool.some((value) => !kept.some((other) => typeof other === typeof value));
      if (!single && !uncovered) findings.push(`${label}: refused when created (${String((error as Error).message).split("\n")[0]})`);
      continue;
    }
    if (beside) {
      try {
        if (parse("s") !== "s") findings.push(`${label}: "s" should pass through`);
      } catch {
        findings.push(`${label}: "s" should pass through, got a failure`);
      }
    }
    for (const value of pool) {
      const input = kind.value(value);
      let output: unknown;
      let failed = false;
      try {
        output = parse(input);
      } catch {
        failed = true;
      }
      if (kept.includes(value)) {
        if (failed || !same(output, input)) findings.push(`${label}: ${show(input)} should pass through, got ${failed ? "a failure" : show(output)}`);
      } else if (!failed && same(output, kind.value(kept[0]))) {
        findings.push(`${label}: ${show(input)} read as ${show(output)}, a member it isn't`);
      } else if (!failed && !single) {
        findings.push(`${label}: ${show(input)} should fail, got ${show(output)}`);
      }
    }
  }
  return findings;
};

const U = undefined;
