// A union of consts, or of objects, tuples, arrays or objects holding an array
// told apart by a const, converted into fewer of them: into a union of some,
// where each member the target holds passes through as it is and every other
// fails as a value (CODEC_SPEC rule 4), or into one, which the members holding
// another const make ambiguous, so the operation is refused when it is created
// (rule 3). The target may hold the kept consts as one field's union instead,
// sit beside a string member both sides hold, or nest the const a level down,
// and the whole may sit in S.optional or S.nullable. An empty list is drawn
// too. Held member by member: the target never answers with a member the value
// wasn't.
import { type Rng } from "./generate";
import { show } from "./sample";
import { node, type Shape } from "./shape";
import type { Sury } from "./types";

const CONSTS = [0, 1, 2, -1, "a", "b", "", "0", true, false];

// `empty` holds none of the consts, so every member takes it as it is.
type Kind = {
  name: string;
  shape: Shape;
  wrap: (S: Sury, value: unknown) => Sury;
  value: (value: unknown) => unknown;
  field?: (S: Sury, values: unknown[]) => unknown;
  empty?: unknown;
};
const literal = node("literal");
const kinds: Kind[] = [
  { name: "const", shape: literal, wrap: (S, value) => S.literal(value), value: (value) => value },
  {
    name: "object",
    shape: node("object", literal, node("string")),
    wrap: (S, value) => S.schema({ t: S.literal(value), v: S.string }),
    value: (value) => ({ t: value, v: "x" }),
    field: (S, values) => S.schema({ t: S.union(values.map((value) => S.literal(value))), v: S.string }),
  },
  { name: "tuple", shape: node("tuple", literal, node("string")), wrap: (S, value) => S.tuple([S.literal(value), S.string]), value: (value) => [value, "x"] },
  { name: "array", shape: node("array", literal), wrap: (S, value) => S.array(S.literal(value)), value: (value) => [value], empty: [] },
  {
    name: "list field",
    shape: node("object", node("array", literal)),
    wrap: (S, value) => S.schema({ x: S.array(S.literal(value)) }),
    value: (value) => ({ x: [value] }),
    empty: { x: [] },
  },
  { name: "nested", shape: node("object", node("object", literal)), wrap: (S, value) => S.schema({ o: S.schema({ t: S.literal(value) }) }), value: (value) => ({ o: { t: value } }) },
];

export type NarrowingFinding = { shape: Shape; property: string; detail: string };

const pick = <T>(rng: Rng, list: readonly T[]): T => list[Math.floor(rng() * list.length)]!;

const same = (a: unknown, b: unknown): boolean => JSON.stringify(a) === JSON.stringify(b);

const firstLine = (error: unknown): string => String((error as Error).message).split("\n")[0]!;

export const narrowingFindings = (S: Sury, rng: Rng, cases: number): NarrowingFinding[] => {
  const findings: NarrowingFinding[] = [];
  for (let c = 0; c < cases; c++) {
    const kind = pick(rng, kinds);
    const pool = [...new Set(Array.from({ length: 2 + Math.floor(rng() * 3) }, () => pick(rng, CONSTS)))];
    if (pool.length < 2) continue;
    // A list union may drop its first member, the one an empty list is read
    // as, which then reaches the target through a member it doesn't keep.
    const kept = pool.filter((_, idx) => (idx === 0 && kind.empty === U) || rng() < 0.5);
    if (!kept.length) kept.push(pool[pool.length - 1]!);
    // One field's union of the kept consts, as one target the values pass
    // into, only for consts of one type, a union of several being refused.
    const asField = kind.field !== U && rng() < 0.3 && kept.length > 1 && kept.every((value) => typeof value === typeof kept[0]);
    const beside = !asField && kind.name !== "const" && rng() < 0.3;
    const single = kept.length === 1 && !asField && !beside;
    const members = pool.map((value) => kind.wrap(S, value));
    const source = S.union(beside ? [...members, S.string] : members);
    const keptMembers = kept.map((value) => kind.wrap(S, value));
    const target = asField ? kind.field!(S, kept) : single ? keptMembers[0] : S.union(beside ? [...keptMembers, S.string] : keptMembers);
    // Wrapped, the conversion answers as it does bare, its empty value aside.
    const wrap = pick(rng, [U, "optional", "nullable"] as const);
    const wrapped = (shape: Shape): Shape => (wrap ? node(wrap, shape) : shape);
    const unionOf = (count: number): Shape => node("union", ...Array.from({ length: count }, () => kind.shape), ...(beside ? [node("string")] : []));
    const shape = wrapped(node("to", unionOf(pool.length), asField ? node("field union") : single ? kind.shape : unionOf(kept.length)));
    const label = `${wrap ? `${wrap} of ` : ""}${kind.name} ${show(pool)} -> ${asField ? "field union " : single ? "" : beside ? "union with string " : "union "}${show(kept)}`;
    const find = (at: Shape, property: string, detail: string) => findings.push({ shape: at, property, detail });
    // A lone const facing only others of its type would never pass, so it is
    // refused when created, wrapped or not.
    const lone = pool[0];
    const others = CONSTS.filter((value) => typeof value === typeof lone && value !== lone).slice(0, 2);
    if (others.length === 2) {
      const conversion = S.literal(lone).with(S.to, S.union(others.map((value) => S.literal(value))));
      let created = true;
      try {
        S.parseOrThrow(wrap ? S[wrap](conversion) : conversion);
      } catch {
        created = false;
      }
      if (created) find(wrapped(node("to", literal, node("union", literal, literal))), "created", `${wrap ? `${wrap} of ` : ""}${show(lone)} -> union ${show(others)}: should be refused when created`);
      // A list of it holds none of it when empty, which any list member takes,
      // and holding it, is none of them.
      for (const list of kinds.filter((other) => other.empty !== U)) {
        const conversion = list.wrap(S, lone).with(S.to, S.union(others.map((value) => list.wrap(S, value))));
        const at = wrapped(node("to", list.shape, node("union", list.shape, list.shape)));
        const name = `${wrap ? `${wrap} of ` : ""}${list.name} ${show(lone)} -> union ${show(others)}`;
        let parse: (value: unknown) => unknown;
        try {
          parse = S.parseOrThrow(wrap ? S[wrap](conversion) : conversion);
        } catch (error) {
          find(at, "refused", `${name}: refused when created (${firstLine(error)})`);
          continue;
        }
        try {
          const back = parse(list.empty);
          if (!same(back, list.empty)) find(at, "empty", `${name}: ${show(list.empty)} read as ${show(back)}`);
        } catch (error) {
          find(at, "empty", `${name}: ${show(list.empty)} should pass through (${firstLine(error)})`);
        }
        try {
          find(at, "fails", `${name}: ${show(list.value(lone))} should fail, got ${show(parse(list.value(lone)))}`);
        } catch {}
      }
    }
    let parse: (value: unknown) => unknown;
    try {
      const conversion = source.with(S.to, target);
      parse = S.parseOrThrow(wrap ? S[wrap](conversion) : conversion);
    } catch (error) {
      // Into one target, a member holding another const makes the conversion
      // ambiguous, and into a union, so does a const the target keeps nothing
      // of its type for (rule 4's coverage, or a field's consts it could
      // become any of).
      const uncovered = (kind.name === "const" || asField) && pool.some((value) => !kept.some((other) => typeof other === typeof value));
      if (!single && !uncovered) find(shape, "refused", `${label}: refused when created (${firstLine(error)})`);
      continue;
    }
    const empty = wrap === "optional" ? U : null;
    if (wrap) {
      try {
        if (parse(empty) !== empty) find(shape, "empty", `${label}: ${show(empty)} should pass through`);
      } catch {
        find(shape, "empty", `${label}: ${show(empty)} should pass through, got a failure`);
      }
    }
    if (kind.empty !== U && !single) {
      try {
        if (!same(parse(kind.empty), kind.empty)) find(shape, "empty", `${label}: ${show(kind.empty)} should pass through`);
      } catch {
        find(shape, "empty", `${label}: ${show(kind.empty)} should pass through, got a failure`);
      }
    }
    if (beside) {
      try {
        if (parse("s") !== "s") find(shape, "string", `${label}: "s" should pass through`);
      } catch {
        find(shape, "string", `${label}: "s" should pass through, got a failure`);
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
        if (failed || !same(output, input)) find(shape, "passes", `${label}: ${show(input)} should pass through, got ${failed ? "a failure" : show(output)}`);
      } else if (!failed && same(output, kind.value(kept[0]))) {
        find(shape, "member", `${label}: ${show(input)} read as ${show(output)}, a member it isn't`);
      } else if (!failed && !single) {
        find(shape, "fails", `${label}: ${show(input)} should fail, got ${show(output)}`);
      }
    }
  }
  return findings;
};

const U = undefined;
