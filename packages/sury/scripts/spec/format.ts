// The spec format, defined *as a Sury schema*.
//
// This is deliberate dogfooding: Sury's own test format is described with Sury,
// so `spec check` validates specs using Sury's parser and error messages, and
// `spec.schema.json` (consumed by editors and AI authors) is emitted from this
// schema via `S.toJSONSchema`. If the format is wrong, Sury's own test-suite
// setup breaks first.
//
// Design rules encoded here (see CONTRIBUTING "Specs"):
//   - Closed world: every object is `.with(S.strict)` -> `additionalProperties:false`.
//   - Exhaustive dimensions: every dimension and every operation is *required*;
//     absence is not allowed. A dimension is either its real value or `{_skip}`.
//   - `_` prefix is the reserved harness namespace (currently just `_skip`).
import * as S from "../../src/S.js";

// ---- authored spec shape (mirrors the Sury schema below) ------------------

export type Skip = { _skip: string };
export type Example = { input: string; bench?: boolean } & (
  | { output: string }
  | { error: string }
);
export type Operation =
  | Skip
  | { expression: string | Skip; examples: Record<string, Example | Skip> };
export type Spec = {
  schema: { res: string; ts: string };
  types: Skip | { ts: string };
  jsonSchema: Skip | { input: unknown; output: unknown };
  instantiations: Skip | number;
  bundleBytes: Skip | number;
  properties: Skip | unknown;
  operations: { parse: Operation; decode: Operation; encode: Operation };
};

export type OpName = "parse" | "decode" | "encode";

// ---- Sury schema for the format -------------------------------------------

// A dimension that isn't asserted must say so explicitly, with a reason.
// Reasons are conventionally an enum (`parser-only`, `lossy`, `not-applicable`)
// or `todo(#…)` for a not-yet-built dimension; the CLI lints the reason string.
export const skip = S.schema({ _skip: S.string }).with(S.strict);
const orSkip = (schema: S.Schema<unknown, unknown>) => S.union([schema, skip]);

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

// One compiled operation over the schema. `expression` is the codegen golden
// (`.toString()` of the compiled function); examples are addressed by name so
// their identity survives insertion/removal (unlike array indices).
const operation = S.schema({
  expression: orSkip(S.string),
  examples: S.record(example),
}).with(S.strict);

// The three directions through a schema. Each is required; a schema that does
// not support one (e.g. a one-way transform has no `encode`) says `{_skip}`.
const operations = S.schema({
  parse: orSkip(operation),
  decode: orSkip(operation),
  encode: orSkip(operation),
}).with(S.strict);

// The full spec. One file = one schema's complete contract.
export const specSchema = S.schema({
  // The schema under test, as source text in both surfaces. Identical strings
  // are allowed (e.g. `S.string`); only `ts` is evaluated by the harness today.
  schema: S.schema({ res: S.string, ts: S.string }).with(S.strict),
  // Static, value-independent dimensions.
  types: orSkip(S.schema({ ts: S.string }).with(S.strict)),
  jsonSchema: orSkip(S.schema({ input: S.json, output: S.json }).with(S.strict)),
  instantiations: orSkip(S.number),
  bundleBytes: orSkip(S.number),
  // Property-based testing dimension — scoped out for now (see CONTRIBUTING).
  properties: orSkip(S.json),
  // Value-driven dimension: codegen goldens + runnable examples per direction.
  operations,
}).with(S.strict);

// Ordered dimension keys — the canonical key order for `spec fmt`.
export const KEY_ORDER: (keyof Spec)[] = [
  "schema",
  "types",
  "jsonSchema",
  "instantiations",
  "bundleBytes",
  "properties",
  "operations",
];
export const OP_ORDER: OpName[] = ["parse", "decode", "encode"];

// Maps an operation name to the Sury builder that compiles that direction.
// Used by the harness to (re)compute expression goldens.
export const OP_BUILDER: Record<OpName, (schema: any) => (input: any) => any> = {
  parse: S.parser,
  decode: S.decoder,
  encode: S.encoder,
};

export const isSkip = (v: unknown): v is Skip =>
  v !== null && typeof v === "object" && "_skip" in (v as object);
