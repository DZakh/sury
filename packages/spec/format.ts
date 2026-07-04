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
import * as S from "sury-published";

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

export const skip = S.schema({ _skip: S.string }).with(S.strict);
const orSkip = (schema: S.Schema<unknown, unknown>) => S.union([schema, skip]);

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

const operation = S.schema({
  expression: orSkip(S.string),
  examples: S.record(example),
}).with(S.strict);

const operations = S.schema({
  parse: orSkip(operation),
  decode: orSkip(operation),
  encode: orSkip(operation),
}).with(S.strict);

// The full spec. One file = one schema's complete contract.
export const specSchema = S.schema({
  schema: S.schema({ res: S.string, ts: S.string }).with(S.strict),
  types: orSkip(S.schema({ ts: S.string }).with(S.strict)),
  jsonSchema: orSkip(S.schema({ input: S.json, output: S.json }).with(S.strict)),
  instantiations: orSkip(S.number),
  bundleBytes: orSkip(S.number),
  properties: orSkip(S.json),
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
