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
import * as S from "sury-published";

// A dimension that isn't asserted must say so explicitly, with a reason.
// Reasons are conventionally an enum (`parser-only`, `lossy`, `not-applicable`)
// or `todo(#…)` for a not-yet-built dimension; the CLI lints the reason string.
export const skip = S.schema({ _skip: S.string }).with(S.strict);
export type Skip = S.Output<typeof skip>;

// Generic over the input schema so the inferred Output type isn't widened away
// (a non-generic `S.Schema<unknown, unknown>` parameter would collapse it).
const orSkip = <T extends S.Schema<unknown, unknown>>(schema: T) =>
  S.union([schema, skip]);

// A single runnable example: an input plus either an expected `output` or an
// expected `error` message. `bench` is reserved for the (not-yet-wired) perf
// dimension. Every code field holds source text, valid in both languages.
const exampleOutput = S.schema({
  input: S.string,
  output: S.string,
  bench: S.optional(S.boolean),
}).with(S.strict);
const exampleError = S.schema({
  input: S.string,
  error: S.string,
  bench: S.optional(S.boolean),
}).with(S.strict);
const example = S.union([exampleOutput, exampleError]);
export type Example = S.Output<typeof example>;

// One compiled operation over the schema. `expression` is the codegen golden
// (`.toString()` of the compiled function); examples are addressed by name so
// their identity survives insertion/removal (unlike array indices).
const operation = S.schema({
  expression: orSkip(S.string),
  examples: S.record(example),
}).with(S.strict);

// An operation is either a full block, or the literal `identity` shorthand for
// Sury's pass-through compile. harness.identityViolations enforces this both
// ways: an op that compiles to the pass-through must say `identity`, and
// `identity` must actually compile to it.
const operationOrIdentity = S.union(["identity", operation]);
export type Operation = S.Output<typeof operationOrIdentity>;

const operations = S.schema({
  parse: operationOrIdentity,
  decode: operationOrIdentity,
  encode: operationOrIdentity,
}).with(S.strict);

// The `ts` (JS `.with`-chain) surface: source plus every surface-specific
// dimension — type strings, TS-instantiation count, per-surface bundle cost.
// A future `res` (ReScript) surface sits alongside this with its own, smaller
// shape (no `input`, since ReScript's `S.t<'value>` has no separate Input type
// parameter; no `instantiations`, a TS/attest-only concept).
const ts = S.schema({
  schema: S.string,
  input: orSkip(S.string),
  output: orSkip(S.string),
  instantiations: orSkip(S.number),
  bundleBytes: orSkip(S.number),
}).with(S.strict);

// The full spec. One file = one schema's complete contract.
export const specSchema = S.schema({
  ts,
  jsonSchema: orSkip(S.schema({ input: S.json, output: S.json }).with(S.strict)),
  operations,
}).with(S.strict);
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
