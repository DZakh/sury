// `S.formData` codec fuzzer.
//
//   pnpm --filter=sury fuzz:formdata
//   pnpm --filter=sury fuzz:formdata --show-known
//
// The union fuzzer differs against a reference implementation. This one has
// none - a form submission is whatever a browser sends, and nothing here can
// re-derive that - so it checks the four properties the codec claims for
// itself, over every wrapper crossed with every leaf:
//
//   symmetry     a field works in both directions or is rejected in both, with
//                a Sury error. A `TypeError` out of the compiler is always a
//                finding, and so is a field that only encodes: nothing tells
//                the author, and the data is unreadable by the time anyone
//                notices.
//   no mutation  encoding does not write into the value it was handed.
//   round-trip   `decode(encode(value))` is `value`.
//   wire         every entry list a client could send is either rejected with
//                a Sury error or read as a value the schema's own output type
//                accepts. This is the half a round-trip can't reach: a repeated
//                key, a file where text belongs, a blank entry - none of them
//                is something an encode would ever produce.
//
// Each has a list of the cases known not to hold, keyed by what the run prints,
// with the reason written out - a blank entry and an absent one are the same
// submission, so some pairs genuinely cannot survive the wire. The run fails on
// anything not listed, and on anything listed that has started to hold. A key
// may name `*` for the wrapper or for the leaf.
//
// The cross is exhaustive rather than sampled: it is a few hundred cases, so a
// seed would only re-find them more slowly. Add a leaf or a wrapper and the
// cross grows on its own.

import * as S from "../index.mjs";
import { catalog, compile, copy, same, show } from "./fuzzKit";

type Leaf = { schema: unknown; values: unknown[]; list?: boolean };

const file = (name: string, body: string): File => new File([body], name);

// One leaf per reading the codec has: text, each coercion, the entry taken as
// it is, and the unions whose arms disagree about what an entry means.
const LEAVES: Record<string, Leaf> = {
  string: { schema: S.string.with(S.nonEmpty), values: ["x", "on", "0", " "] },
  "string-blank-ok": { schema: S.string.with(S.minLength, 0), values: ["", "x"] },
  "string-bare": { schema: S.string, values: ["x", ""] },
  "literal-blank": { schema: S.schema(""), values: [""] },
  literal: { schema: S.schema("x"), values: ["x"] },
  number: { schema: S.number, values: [0, 42, -1.5, 1e21, Infinity] },
  int32: { schema: S.int32, values: [0, -7] },
  bigint: { schema: S.bigint, values: [0n, 10n ** 30n] },
  boolean: { schema: S.boolean, values: [true, false] },
  "literal-true": { schema: S.schema(true), values: [true] },
  date: { schema: S.date, values: [new Date(0), new Date("2026-09-06T00:00:00.000Z")] },
  file: { schema: S.file, values: [file("a.txt", "hi"), file("empty.bin", "")] },
  blob: { schema: S.blob, values: [new Blob(["hi"])] },
  enum: { schema: S.union(["yes", "no"]), values: ["yes", "no"] },
  "json-string": {
    schema: S.jsonString.with(S.to, S.schema({ a: S.number })),
    values: [{ a: 1 }],
  },
  "to-date": { schema: S.string.with(S.to, S.date), values: [new Date(0)] },
  "union-text": { schema: S.union([S.string, S.number]), values: ["x", 1] },
  "union-checkbox": { schema: S.union([S.boolean, S.number]), values: [true, false, 0, 1, 2] },
  "union-entry": { schema: S.union([S.string, S.file]), values: ["x", file("a.txt", "hi")] },
  trimmed: { schema: S.string.with(S.trim), values: ["x"] },
  email: { schema: S.email, values: ["a@b.co"] },
  "bounded-number": { schema: S.number.with(S.gte, 18), values: [18, 65] },
  "json-string-bare": { schema: S.jsonString, values: ['{"a":1}', '"x"'] },
  unknown: { schema: S.unknown, values: ["x"] },
  null: { schema: S.schema(null), values: [null] },
  void: { schema: S.void, values: [undefined] },
};

// Every way a field can wrap one of them.
const WRAPPERS: Record<string, (leaf: Leaf) => Leaf> = {
  bare: (leaf) => leaf,
  optional: (leaf) => ({
    schema: S.optional(leaf.schema as never),
    values: [...leaf.values, undefined],
  }),
  defaulted: (leaf) => ({
    schema: S.optional(leaf.schema as never, leaf.values[0] as never),
    values: leaf.values,
  }),
  nullable: (leaf) => ({
    schema: S.nullable(leaf.schema as never),
    values: [...leaf.values, null],
  }),
  "nullable-defaulted": (leaf) => ({
    schema: S.nullable(leaf.schema as never, leaf.values[0] as never),
    values: leaf.values,
  }),
  nullish: (leaf) => ({
    schema: S.nullish(leaf.schema as never),
    values: [...leaf.values, null, undefined],
  }),
  array: (leaf) => ({
    schema: S.array(leaf.schema as never),
    values: [[], leaf.values, [leaf.values[0]]],
    list: true,
  }),
  "optional-array": (leaf) => ({
    schema: S.optional(S.array(leaf.schema as never)),
    values: [leaf.values, [], undefined],
    list: true,
  }),
  "nullable-array": (leaf) => ({
    schema: S.nullable(S.array(leaf.schema as never)),
    values: [leaf.values, [], null],
    list: true,
  }),
  "optional-nullable": (leaf) => ({
    schema: S.optional(S.nullable(leaf.schema as never)),
    values: [...leaf.values, null, undefined],
  }),
  tuple: (leaf) => ({
    schema: S.schema([leaf.schema, leaf.schema] as never),
    values: [[leaf.values[0], leaf.values[leaf.values.length - 1]]],
    list: true,
  }),
};

// Entry lists a client could send for the one field the cross declares. Values
// an encode would never produce are the point: a key sent twice, a file where
// text belongs, the empty File an unchosen file input submits.
const WIRE: [string, unknown][][] = [
  [],
  [["a", ""]],
  [["a", "x"]],
  [["a", "42"]],
  [["a", "on"]],
  [["a", "false"]],
  [["a", "0"]],
  [["a", "null"]],
  [["a", " "]],
  [["a", file("up.txt", "hi")]],
  [["a", new File([], "", { type: "application/octet-stream" })]],
  [
    ["a", "x"],
    ["a", "y"],
  ],
  [
    ["a", "x"],
    ["a", file("up.txt", "hi")],
  ],
  [["b", "x"]],
];

const form = (entries: [string, unknown][]): FormData => {
  const formData = new FormData();
  for (const [key, value] of entries) formData.append(key, value as string);
  return formData;
};

// Fields the codec reads but cannot write, which the author hears about when
// the encoder is compiled and not before.
const ONE_WAY: Record<string, string> = {
  "*/union-text":
    "the union rules reject `string | number -> string` on the way out, where the codec is not consulted at all",
  "*/union-entry": "the same, for `string | File`",
  "bare/trimmed":
    "a trimmed bare string still says nothing about a blank entry, which the decoder rejects as ambiguous where the encoder has no blank to write",
  "bare/string-bare":
    "a bare required string says nothing about a blank entry, which the decoder rejects as ambiguous where the encoder has no blank to write",
};

// Values the wire cannot carry back, and why.
const KNOWN: Record<string, string> = {
  "nullish/* <- null":
    "a form has one way to say nothing, so a field declaring both sentinels reads it as the weaker one",
  "optional-nullable/* <- null": "the same, spelled as two wrappers",
  "optional/null <- null": "the same, from the other side",
  "defaulted/boolean <- false":
    "an unchecked box sends nothing, so a default of `true` states what the wire never says and reads back as itself",
  "nullable/void <- null": "the same, from the other side",
  "nullable-defaulted/boolean <- false": "the same, with `null` for the sentinel",
  "*/string-bare <- ''":
    "a bare string says nothing about a blank entry, so a wrapper reads one as the absence it declares",
  "optional-array/* <- undefined":
    "no entries is the empty list, so the absence a wrapper declares reads back as `[]`",
  "nullable-array/* <- null": "the same, with `null` for the absence",
};

const MUTATES: Record<string, string> = {};

const wireText = (entries: [string, unknown][]): string =>
  entries.length ? entries.map(([key, value]) => `${key}=${show(value)}`).join("&") : "nothing";

const known = catalog({
  ONE_WAY: { label: "one-way fields", cases: ONE_WAY },
  KNOWN: { label: "pairs the wire cannot tell apart", cases: KNOWN },
  MUTATES: { label: "encodes that write into their input", cases: MUTATES },
});
const { findings } = known;
// Every field that works, for one schema of them all: a field on its own can't
// show a name the compiler hands out twice, a declaration hoisted after the
// code that reads it, or a read that answers another field's entry.
const together: Record<string, unknown> = {};
// One entry each, so every field is read with its neighbours supplied.
const wire: [string, unknown][] = [];
let checked = 0;
let wires = 0;
let rejected = 0;

for (const [wrapperName, wrap] of Object.entries(WRAPPERS)) {
  for (const [leafName, leaf] of Object.entries(LEAVES)) {
    const id = `${wrapperName}/${leafName}`;
    let field: Leaf;
    try {
      field = wrap(leaf);
    } catch {
      // A wrapper the public API refuses to build is not this codec's business.
      continue;
    }
    const schema = S.formData.with(S.to, S.schema({ a: field.schema as never }) as never);
    const decode = compile(() => S.decodeOrThrow(schema));
    const encode = compile(() => S.encodeOrThrow(schema));
    const valid = compile(() => S.isOutput(schema));

    for (const [direction, result] of [
      ["decode", decode],
      ["encode", encode],
    ] as const) {
      if (result.crash) {
        findings.push(`${id}: ${direction} compile crashed - ${result.crash}`);
      }
    }
    if (!decode.rejected !== !encode.rejected) {
      const shape = decode.rejected ? "encodes but does not decode" : "decodes but does not encode";
      known.miss("ONE_WAY", wrapperName, leafName, `${id}: ${shape} - ${decode.rejected ?? encode.rejected}`);
    } else if (!decode.rejected) {
      known.hold("ONE_WAY", wrapperName, leafName, id);
    }
    if (decode.rejected || encode.rejected || decode.crash || encode.crash) {
      rejected += 1;
      continue;
    }
    const key = `${wrapperName}_${leafName}`.replace(/-/g, "_");
    together[key] = field.schema;
    wire.push([key, field.list ? "x" : "on"]);

    // What a client can send, checked against the schema's own output type:
    // a decode either rejects an entry list or reads it as a value the schema
    // says it produces. Nothing here is a value an encode could have written.
    for (const entries of WIRE) {
      let read: unknown;
      try {
        read = (decode.fn as (form: FormData) => unknown)(form(entries));
      } catch (error) {
        if (!(error instanceof S.Error)) {
          findings.push(
            `${id} <- ${wireText(entries)}: decode threw ${(error as Error).constructor.name} - ${(error as Error).message.split("\n")[0]}`,
          );
        }
        continue;
      }
      wires += 1;
      if (!(valid.fn as (value: unknown) => boolean)(read)) {
        findings.push(
          `${id} <- ${wireText(entries)}: read as ${show(read)}, which the schema's own output type rejects`,
        );
      }
    }

    let mutated = false;
    for (const value of field.values) {
      const printed = show(value);
      const key = `${id} <- ${printed}`;
      checked += 1;
      const before = copy(value);
      let back: unknown;
      try {
        back = (decode.fn as (form: FormData) => { a: unknown })(
          (encode.fn as (value: unknown) => FormData)({ a: value }),
        ).a;
      } catch (error) {
        known.miss("KNOWN", wrapperName, leafName, `${key}: round-trip threw - ${(error as Error).message.split("\n")[0]}`, printed);
        continue;
      }
      if (!same(before, value)) {
        mutated = true;
        known.miss("MUTATES", wrapperName, leafName, `${key}: the encode wrote into its input, leaving ${show(value)}`);
      }
      if (same(before, back)) {
        known.hold("KNOWN", wrapperName, leafName, key, printed);
      } else {
        known.miss("KNOWN", wrapperName, leafName, `${key}: read back as ${show(back)}`, printed);
      }
    }
    if (!mutated) {
      known.hold("MUTATES", wrapperName, leafName, id);
    }
  }
}

const combined = S.formData.with(S.to, S.schema(together as never) as never);
const combinedDecode = compile(() => S.decodeOrThrow(combined));
for (const [direction, result] of [
  ["decode", combinedDecode],
  ["encode", compile(() => S.encodeOrThrow(combined))],
] as const) {
  if (result.crash) {
    findings.push(`all ${Object.keys(together).length} fields in one schema: ${direction} - ${result.crash}`);
  } else if (result.rejected) {
    findings.push(
      `all ${Object.keys(together).length} fields in one schema: ${direction} rejected it - ${result.rejected}`,
    );
  }
}
if (combinedDecode.fn) {
  for (const [label, entries] of [
    ["nothing", []],
    ["one entry each", wire],
  ] as const) {
    try {
      (combinedDecode.fn as (form: FormData) => unknown)(form(entries as [string, unknown][]));
    } catch (error) {
      if (!(error instanceof S.Error)) {
        findings.push(
          `all fields in one schema <- ${label}: decode threw ${(error as Error).constructor.name} - ${(error as Error).message.split("\n")[0]}`,
        );
      }
    }
  }
}

known.finish(
  `${checked} round-trips and ${wires} entry lists read over ${Object.keys(WRAPPERS).length}x${Object.keys(LEAVES).length} fields (${rejected} rejected in both directions), ${Object.keys(together).length} of them compiled together`,
);
