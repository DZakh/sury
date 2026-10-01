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
// not its text: "a member whose default takes a later member's `undefined`" is
// one entry at every depth the grammar reaches, and a substring only covers the
// depths someone happened to list.

import {
  absorbs,
  admitsUndefined,
  type Shape,
  shadowsEmpty,
  some,
} from "./unionFuzz/shape";

export type Fuzzer = "eq" | "codec" | "union" | "issues";

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

// A container carried into a JSON string (unionFuzz/generate.ts `generateSchema`).
// A leaf's `.with(S.to, ...)` wraps a node with no args; a container has some.
const intoJsonString = (shape: Shape): boolean =>
  shape.name === "with" && shape.raw === "to" && (shape.args[0]?.args.length ?? 0) > 0;

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
    id: "jsonstring-null-default-inlined",
    kind: "bug",
    summary:
      "`S.schema({ f: S.optional(S.nullable(S.string), null) }).with(S.to, S.jsonString)` fails to compile " +
      "its parse: the `null` default is inlined as a literal, and the field's conversion to JSON text then " +
      "assigns to it as if it were a variable (`null=...`). `S.nullable(x, null)` and ReScript's " +
      "`S.Option.getOr` over a nested option inline their default the same way.",
    spec: "jsonstring-object-optional-null-default",
    fuzzers: ["issues"],
    matches: (f) =>
      f.fuzzer === "issues" &&
      f.property === "setup" &&
      f.detail.includes("Invalid left-hand side in assignment") &&
      intoJsonString(f.shape) &&
      some(
        f.shape,
        (node) =>
          node.name === "getOr" ||
          ((node.name === "optional" || node.name === "nullable") && node.args[1]?.raw === "null"),
      ),
  },
  {
    id: "union-json-document-member",
    kind: "limitation",
    summary:
      "Under `S.json`, a union meets the value whole, so a `S.jsonString` anywhere in a member checks a string " +
      "where the member's own link would write the value's JSON text (CONTENT_CODEC_SPEC.md: a carrier's reading " +
      "stops at a union target). The member-by-member reference reads each member through its own link, so it " +
      "cannot tell that from a bug.",
    fuzzers: ["union"],
    matches: (f) =>
      f.fuzzer === "union" &&
      f.shape.name === "jsonTo" &&
      f.property === "acceptance" &&
      some(f.shape, (node) => node.name === "jsonString" || node.name === "jsonStringWithSpace"),
  },
  {
    id: "union-overlapping-members",
    kind: "limitation",
    summary:
      "A member that takes every value of its kind - an object whose every field may be absent, a record, " +
      "a list of `any` - claims values meant for a later member, and so does a wrapper whose default takes the " +
      "`null` or `undefined` a later member would keep. The first member that accepts a value wins, which is " +
      "the documented rule. The round trip cannot tell that from a bug, and the member-by-member reference " +
      "cannot either for a member that takes every value of its kind.",
    fuzzers: ["codec", "union"],
    matches: (f) =>
      (f.fuzzer === "union" ? f.property === "acceptance" : f.property === "round-trip") &&
      some(
        f.shape,
        (node) =>
          node.name === "union" &&
          (node.args.slice(0, -1).some(absorbs) || (f.fuzzer === "codec" && shadowsEmpty(node))),
      ),
  },
  {
    id: "jsonstring-fieldor-refined-url-encode",
    kind: "bug",
    summary:
      "`S.object((s) => ({ f: s.fieldOr(\"f\", S.nullable(S.url.with(S.refine, check)), null) })).with(S.to, S.jsonString)` " +
      "crashes the encode compile (`isOutput`, `encodeOrThrow`) with a TypeError from `B_merge` instead of building it.",
    spec: "jsonstring-fieldor-refined-url",
    fuzzers: ["issues"],
    matches: (f) =>
      f.fuzzer === "issues" &&
      f.property === "setup" &&
      f.detail.includes("reading 't'") &&
      some(f.shape, (node) => node.name === "fieldOr"),
  },
  {
    id: "conversion-after-failed-container",
    kind: "limitation",
    summary:
      "A conversion over a whole container - a JSON string rendering an `unknown` field, or checking a " +
      "number is finite on encode - runs only once every field passed, as a refine or transform does, so its " +
      "failure for one field is " +
      "reported alone but not beside another field's. The skip is the documented rule; the property that " +
      "breaks one field and then all of them cannot tell it from a lost issue.",
    fuzzers: ["issues"],
    matches: (f) =>
      f.fuzzer === "issues" &&
      f.property === "independent" &&
      intoJsonString(f.shape),
  },
  {
    id: "jsonstring-render-order",
    kind: "limitation",
    summary:
      "A throwing operation renders a container carried into `S.jsonString` in one pass, so a field whose " +
      "only check is being JSON (`unknown`, `any`) fails where it is rendered, ahead of a later field's " +
      "check. A Result checks the fields first and renders after them, so its first issue is the later " +
      "field's. Both answers are the value's; only their order differs.",
    fuzzers: ["issues"],
    matches: (f) =>
      f.fuzzer === "issues" &&
      f.property === "first" &&
      f.detail.includes("Expected JSON") &&
      intoJsonString(f.shape),
  },
  {
    id: "json-is-one-value",
    kind: "limitation",
    summary:
      "`S.json` validates one JSON value with a walk that stops at the first thing that isn't JSON, so an " +
      "array or object it accepts reports one issue, not one per item: it is a value, not a container.",
    fuzzers: ["issues"],
    matches: (f) => f.fuzzer === "issues" && f.property === "independent" && f.shape.name === "json",
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
