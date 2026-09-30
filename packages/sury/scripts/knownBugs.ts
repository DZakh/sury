// Every bug the fuzzers know about and nobody has fixed yet, in one place.
//
// A fuzzer that finds something not on this list fails. A fuzzer whose gate run
// matches nothing for an entry it is listed under also fails, which is how a fix
// announces itself: delete the entry, and turn the spec's FIXME example into the
// right answer. So an entry can neither hide a new bug nor outlive its own.
//
// Each `bug` names a spec that reproduces it with a `FIXME: known bug <id>`
// beside the example that records the wrong behaviour (CLAUDE.md: no spec, not
// fixed). `tests/knownBugs_test.ts` holds every entry to that. A `limitation`
// is behaviour that is right and that a property cannot tell from a bug, so it
// carries a reason instead of a spec.
//
// Entries are written against the SHAPE the grammar built the schema from,
// not its text: "a union with a member that carries a default" is one bug at
// every depth the grammar reaches, and a substring only covers the depths
// someone happened to list.

import {
  absorbs,
  admitsUndefined,
  hasDefault,
  type Shape,
  some,
  unionMembers,
} from "./unionFuzz/shape";

export type Fuzzer = "eq" | "codec" | "union";

export type Finding = {
  fuzzer: Fuzzer;
  shape: Shape;
  property: string;
  detail: string;
};

export type Known = {
  id: string;
  kind: "bug" | "limitation";
  summary: string;
  // The spec that reproduces it. Required for a bug.
  spec?: string;
  // The fuzzers whose gate run has to reach it; an entry matched by none of
  // them in its gate is stale.
  fuzzers: Fuzzer[];
  matches: (finding: Finding) => boolean;
};

const inUnionWithDefault = (shape: Shape): boolean =>
  some(shape, (node) => unionMembers(node).some(hasDefault));

export const KNOWN_BUGS: Known[] = [
  {
    id: "to-output-keeps-source-refinement",
    kind: "bug",
    summary:
      "After `S.to` with a custom coder, `isOutput` still runs the source's refinement: " +
      "`Schema<uuid, number>` tests the uuid pattern against the number, so every decoded value is rejected.",
    spec: "to-refined-source-output",
    fuzzers: ["codec"],
    matches: (f) =>
      f.fuzzer === "codec" &&
      f.property === "conformance" &&
      f.detail.includes("isOutput rejects") &&
      some(f.shape, (node) => node.name === "with" && node.raw === "to"),
  },
  {
    id: "fieldor-absent-item-output",
    kind: "bug",
    summary:
      "`s.fieldOr(name, S.nullish(x), d)` keeps `undefined` in its Output type although the default " +
      "always replaces it, so `isOutput({})` is true and `{}` does not round-trip.",
    spec: "fieldor-nullish-default",
    fuzzers: ["codec"],
    matches: (f) =>
      f.fuzzer === "codec" &&
      f.property === "round-trip" &&
      some(f.shape, (node) => node.name === "fieldOr" && admitsUndefined(node.args[0]!)),
  },
  {
    id: "union-defaulted-member",
    kind: "bug",
    summary:
      "A union member that carries a default takes over the union: `S.union([S.nullable(S.boolean, false), " +
      "S.string])` rejects `\"x\"`. `S.nullable(S.optional(x, d))` is the same bug, since nullable is a union.",
    spec: "union-defaulted-member",
    fuzzers: ["codec", "union"],
    matches: (f) =>
      (f.fuzzer === "union" ? f.property === "acceptance" : ["conformance", "round-trip"].includes(f.property)) &&
      inUnionWithDefault(f.shape),
  },
  {
    id: "union-never-member",
    kind: "bug",
    summary:
      "A union member with an `S.never` field makes the whole union's encode refuse to compile, " +
      "instead of that one member yielding to its siblings.",
    spec: "union-never-member",
    fuzzers: ["union"],
    matches: (f) =>
      f.fuzzer === "union" && f.detail.includes("Missing input for never") && some(f.shape, (n) => n.name === "never"),
  },
  {
    id: "union-overlapping-members",
    kind: "limitation",
    summary:
      "A member that takes every value of its kind - an object whose every field may be absent, a list of " +
      "`any` - claims values meant for a later member. The first member that accepts a value wins, which is " +
      "the documented rule; the round trip and the member-by-member reference cannot tell that from a bug.",
    fuzzers: ["codec", "union"],
    matches: (f) =>
      (f.fuzzer === "union" ? f.property === "acceptance" : f.property === "round-trip") &&
      some(f.shape, (node) => node.name === "union" && node.args.some(absorbs)),
  },
];

const matched = new Set<string>();

// The entry covering a finding, or `undefined` for one nobody has listed.
export const knownFor = (fuzzer: Fuzzer, shape: Shape, property: string, detail: string): Known | undefined => {
  const finding: Finding = { fuzzer, shape, property, detail };
  const entry = KNOWN_BUGS.find((known) => known.fuzzers.includes(fuzzer) && known.matches(finding));
  if (entry) matched.add(`${fuzzer}:${entry.id}`);
  return entry;
};

// Entries listed under one of `ran` that the run never matched. Meaningful only
// for a gate run: a narrower search reaching less says nothing about the bug.
export const staleFor = (ran: Fuzzer[]): string[] =>
  KNOWN_BUGS.flatMap((known) =>
    known.fuzzers
      .filter((fuzzer) => ran.includes(fuzzer) && !matched.has(`${fuzzer}:${known.id}`))
      .map(
        (fuzzer) =>
          `known ${known.kind} "${known.id}" was not reached by the ${fuzzer} gate - ` +
          (known.spec
            ? `if it is fixed, correct the FIXME in specs/${known.spec}.yaml and delete the entry`
            : `if it no longer holds, delete the entry`) +
          `; otherwise the grammar stopped drawing the shape`,
      ),
  );
