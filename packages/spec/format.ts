// The spec format, defined *as a Sury schema* — the harness INFRASTRUCTURE.
//
// This half of the harness runs on a PUBLISHED sury (`sury-published` =
// npm:sury@<pinned>), not the in-development source. That keeps the CLI stable
// while Sury's internals are refactored: defining the spec format, validating
// specs, and emitting spec.schema.json never break just because the working
// tree's core is mid-change. Golden *execution* (harness.ts) uses the dev source
// instead, so goldens still track your changes.
//
// Design rules encoded here (see the `spec` skill):
//   - Closed world: every object is `.with(S.strict)` -> `additionalProperties:false`.
//   - Exhaustive dimensions: every dimension and every operation is *required*;
//     absence is not allowed. A dimension is either its real value or `{_skip}`.
//   - `_` prefix is the reserved harness namespace (currently just `_skip`).
//   - Every exported TS type is INFERRED from its schema via `S.Output<typeof x>`
//     — the schema is the single source of truth, never hand-duplicated.
//   - Every schema/field carries a `.with(S.meta, {description})` so
//     spec.schema.json (consumed by yaml-language-server for hover/autocomplete,
//     and by AI authors) is self-documenting — never rely on this SKILL.md alone.
//   - Descriptions are added via DIRECT `.with(S.meta, {...})` chains, never
//     through a generic wrapper function. A generic `desc<T extends
//     Schema<unknown,unknown>>(schema: T, description: string)` helper was
//     tried and reliably collapsed `S.Output<typeof specSchema>` to `unknown`
//     at THIS schema's nesting depth (reproduced in isolation; direct chaining
//     at the exact same depth was unaffected) — presumably the same complexity
//     cliff the "14-member union...costly to instantiate" note elsewhere in
//     this codebase refers to. Don't reintroduce a generic description helper
//     without re-verifying against the full, real specSchema, not a toy schema.
import * as S from "sury-published";

// A dimension that isn't asserted must say so explicitly, with a reason.
// Reasons are conventionally an enum (`parser-only`, `lossy`, `not-applicable`)
// or `todo(#…)` for a not-yet-built dimension; the CLI lints the reason string.
export const skip = S.schema({
  _skip: S.string.with(S.meta, {
    description:
      "Why this dimension is not asserted. An enum reason (parser-only, " +
      "serializer-only, lossy, not-applicable) or todo(#…) for a not-yet-built " +
      "dimension — never a bare/unexplained skip.",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "Explicit opt-out for a dimension that isn't asserted for this spec." });
export type Skip = S.Output<typeof skip>;

// Generic over the input schema so the inferred Output type isn't widened away
// (a non-generic `S.Schema<unknown, unknown>` parameter would collapse it).
const orSkip = <T extends S.Schema<unknown, unknown>>(schema: T) =>
  S.union([schema, skip]);

// A single runnable example: an input plus either an expected `output` or an
// expected `error` message. `bench` is reserved for the (not-yet-wired) perf
// dimension. Every code field holds source text, valid in both languages.
const inputDescription =
  'Source text for the input value, e.g. \'"hello"\' or "42". Written by hand; ' +
  "`pnpm spec update` derives output/error from it.";
const benchDescription = "Reserved for the not-yet-wired performance dimension.";
const exampleOutput = S.schema({
  input: S.string.with(S.meta, { description: inputDescription }),
  output: S.string.with(S.meta, {
    description: "Source text for the expected output value. Filled by `pnpm spec update`.",
  }),
  bench: S.optional(S.boolean).with(S.meta, { description: benchDescription }),
}).with(S.strict);
const exampleError = S.schema({
  input: S.string.with(S.meta, { description: inputDescription }),
  error: S.string.with(S.meta, { description: "Expected error message. Filled by `pnpm spec update`." }),
  bench: S.optional(S.boolean).with(S.meta, { description: benchDescription }),
}).with(S.strict);
const example = S.union([exampleOutput, exampleError]).with(S.meta, {
  description: "One named, runnable case: an input plus its expected output or error.",
});
export type Example = S.Output<typeof example>;

// One compiled operation over the schema. `expression` is the codegen golden
// (`.toString()` of the compiled function); examples are addressed by name so
// their identity survives insertion/removal (unlike array indices).
const operation = S.schema({
  expression: orSkip(S.string).with(S.meta, {
    description:
      "Codegen golden: `.toString()` of the compiled parser/decoder/encoder function. Filled by `pnpm spec update`.",
  }),
  examples: S.record(example).with(S.meta, {
    description: "Named example cases for this operation, keyed by a short descriptive name (e.g. `valid`, `invalid-type`).",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "A full operation block: the compiled codegen plus its runnable examples." });

// An operation is either a full block, or the literal `identity` shorthand for
// Sury's pass-through compile. harness.identityViolations enforces this both
// ways: an op that compiles to the pass-through must say `identity`, and
// `identity` must actually compile to it.
const operationOrIdentity = S.union(["identity", operation]).with(S.meta, {
  description:
    "Either the bare literal `identity` (this operation compiles to Sury's " +
    "pass-through — never `_skip: identity`) or a full operation block.",
});
export type Operation = S.Output<typeof operationOrIdentity>;

const operations = S.schema({
  parse: operationOrIdentity.with(S.meta, {
    description: "unknown → Output, with validation (the untrusted-input direction).",
  }),
  decode: operationOrIdentity.with(S.meta, {
    description: "Input → Output, no top-level type narrowing (input is already typed).",
  }),
  encode: operationOrIdentity.with(S.meta, { description: "Output → Input (the reverse direction)." }),
})
  .with(S.strict)
  .with(S.meta, { description: "The three compiled directions through the schema: parse, decode, encode." });

// The `ts` (JS `.with`-chain) surface: source plus every surface-specific
// dimension — type strings, TS-instantiation count, per-surface bundle cost.
// A future `res` (ReScript) surface sits alongside this with its own, smaller
// shape (no `input`, since ReScript's `S.t<'value>` has no separate Input type
// parameter; no `instantiations`, a TS/attest-only concept).
const ts = S.schema({
  schema: S.string.with(S.meta, {
    description:
      'The schema under test, as JS `.with`-chain source, e.g. `S.string.with(S.min, 3)`. ' +
      'Authored by hand; this is the one thing "spec new"/"spec update" never overwrite.',
  }),
  input: orSkip(S.string).with(S.meta, {
    description: "S.Input<schema>, printed as a TS type string (e.g. `string`). Filled by `pnpm spec update` via vendored TS introspection.",
  }),
  output: orSkip(S.string).with(S.meta, {
    description: "S.Output<schema>, printed as a TS type string. Filled by `pnpm spec update` via vendored TS introspection.",
  }),
  instantiations: orSkip(S.number).with(S.meta, {
    description:
      "Type-instantiation count contributed by declaring the schema and extracting its Output/Input types. Filled by `pnpm spec update`.",
  }),
  bundleBytes: orSkip(S.number).with(S.meta, {
    description: "Tree-shaken, minified+gzipped byte size of `S.parser(schema)` bundled against the dev source. Filled by `pnpm spec update`.",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "The JS `.with`-chain surface: source plus every surface-specific dimension." });

// The full spec. One file = one schema's complete contract.
export const specSchema = S.schema({
  ts,
  jsonSchema: orSkip(S.schema({ input: S.json, output: S.json }).with(S.strict)).with(S.meta, {
    description: "S.toJSONSchema(schema) for both directions. Filled by `pnpm spec update`.",
  }),
  operations,
})
  .with(S.strict)
  .with(S.meta, {
    description:
      "A Sury spec: one schema's complete, machine-checked contract — type, JSON Schema, and per-operation codegen + examples. See the `spec` skill.",
  });
export type Spec = S.Output<typeof specSchema>;

export type OpName = keyof Spec["operations"];

// Ordered dimension keys — the canonical key order for `spec fmt`.
export const KEY_ORDER: (keyof Spec)[] = ["ts", "jsonSchema", "operations"];
export const TS_KEY_ORDER: (keyof Spec["ts"])[] = [
  "schema",
  "input",
  "output",
  "instantiations",
  "bundleBytes",
];
export const OP_ORDER: OpName[] = ["parse", "decode", "encode"];

export const isSkip = (v: unknown): v is Skip =>
  v !== null && typeof v === "object" && "_skip" in (v as object);

// Run a spec object through Sury's own parser (published S).
export const validate = (
  obj: unknown,
): { ok: true } | { ok: false; error: string } => {
  try {
    S.parser(specSchema)(obj);
    return { ok: true };
  } catch (e) {
    return { ok: false, error: (e as Error).message };
  }
};

// The canonical spec.schema.json text emitted from the format schema.
export const schemaJson = (): string =>
  JSON.stringify(S.toJSONSchema(specSchema), null, 2) + "\n";
