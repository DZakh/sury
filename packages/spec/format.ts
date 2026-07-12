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

// A dimension that isn't asserted must say so explicitly, with a reason:
// one of SKIP_REASONS, or `todo(#…)` for a not-yet-built dimension. The CLI
// lints the reason string (harness.ts's isValidSkipReason) and the schema
// description below is derived from the same array, so there's exactly one
// list to update.
export const SKIP_REASONS = ["parser-only", "serializer-only", "lossy", "not-applicable"] as const;
export const skip = S.schema({
  _skip: S.string.with(S.meta, {
    description: `Why unasserted: ${SKIP_REASONS.join(" | ")} | todo(#…).`,
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "Explicit opt-out for an unasserted dimension." });
export type Skip = S.Output<typeof skip>;

// Generic over the input schema so the inferred Output type isn't widened away
// (a non-generic `S.Schema<unknown, unknown>` parameter would collapse it).
const orSkip = <T extends S.Schema<unknown, unknown>>(schema: T) =>
  S.union([schema, skip]);

// `bench` is reserved for the not-yet-wired performance dimension.
const inputDescription =
  'Source text for the input, e.g. \'"hello"\'. Hand-written; `spec check --write` fills output/error.';
const benchDescription = "Reserved for the not-yet-wired performance dimension.";
const exampleOutput = S.schema({
  input: S.string.with(S.meta, { description: inputDescription }),
  output: S.string.with(S.meta, {
    description: "Expected output source text. Filled by `spec check --write`.",
  }),
  bench: S.optional(S.boolean).with(S.meta, { description: benchDescription }),
}).with(S.strict);
const exampleError = S.schema({
  input: S.string.with(S.meta, { description: inputDescription }),
  error: S.string.with(S.meta, { description: "Expected error message. Filled by `spec check --write`." }),
  bench: S.optional(S.boolean).with(S.meta, { description: benchDescription }),
}).with(S.strict);
const example = S.union([exampleOutput, exampleError]).with(S.meta, {
  description: "A named example: input plus expected output or error.",
});
export type Example = S.Output<typeof example>;

// Examples are addressed by name, not array index, so identity survives
// insertion/removal.
const operation = S.schema({
  expression: orSkip(S.string).with(S.meta, {
    description: "Compiled function source (`.toString()`). Filled by `spec check --write`.",
  }),
  examples: S.record(example).with(S.meta, {
    description: "Named example cases, keyed by a short name (e.g. `valid`, `invalid-type`).",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "Compiled codegen plus its runnable examples." });

// An operation is either a full block or a literal shorthand:
// - `identity` — Sury's pass-through compile.
// - `eq-to-parse` (decode/encode only) — compiles to exactly the same code as
//   the spec's `parse` op, so the expression and examples live there.
// harness.identityViolations enforces the shorthands both ways: an op that
// compiles to a shorthand's meaning must use it, and the shorthand must
// actually hold.
const operationOrIdentity = S.union(["identity", operation]).with(S.meta, {
  description: "`identity` if this compiles to Sury's pass-through, else a full operation block.",
});
const operationOrShorthand = S.union(["identity", "eq-to-parse", operation]).with(S.meta, {
  description:
    "`identity` if this compiles to Sury's pass-through, `eq-to-parse` if it compiles to the same code as `parse`, else a full operation block.",
});
export type Operation = S.Output<typeof operationOrShorthand>;

const operations = S.schema({
  parse: operationOrIdentity.with(S.meta, {
    description: "unknown → Output, with validation (the untrusted-input direction).",
  }),
  decode: operationOrShorthand.with(S.meta, {
    description: "Input → Output, no top-level type narrowing (input is already typed).",
  }),
  encode: operationOrShorthand.with(S.meta, { description: "Output → Input (the reverse direction)." }),
})
  .with(S.strict)
  .with(S.meta, { description: "The three compiled directions through the schema: parse, decode, encode." });

// A future `res` (ReScript) surface would sit alongside this with its own,
// smaller shape (no `input`, since ReScript's `S.t<'value>` has no separate
// Input type parameter; no `instantiations`, a TS/attest-only concept).
const ts = S.schema({
  schema: S.string.with(S.meta, {
    description:
      "The schema under test, as JS `.with`-chain source (e.g. `S.string.with(S.min, 3)`). " +
      "You write this by hand; `spec check --write` never touches it.",
  }),
  aliases: S.optional(S.array(S.string)).with(S.meta, {
    description:
      "Alternate `.with`-chain sources that must produce a schema equivalent to `schema` — " +
      "same ts.input/ts.output, jsonSchema, and operations. Checked live (not separately " +
      "snapshotted) by `spec check`. You write these by hand.",
  }),
  input: orSkip(S.string).with(S.meta, {
    description: "`S.Input<typeof schema>` as a TS type string. Filled by `spec check --write`.",
  }),
  output: orSkip(S.string).with(S.meta, {
    description: "`S.Output<typeof schema>` as a TS type string. Filled by `spec check --write`.",
  }),
  instantiations: orSkip(S.number).with(S.meta, {
    description: "TS type-instantiation cost of this schema. Filled by `spec check --write`.",
  }),
  bundleBytes: orSkip(S.number).with(S.meta, {
    description: "Minified+gzipped bundle size of `schema` itself. Filled by `spec check --write`.",
  }),
})
  .with(S.strict)
  .with(S.meta, {
    description: "The JS `.with`-chain surface: the schema itself, plus its inferred types, instantiation cost, and bundle size.",
  });

export const specSchema = S.schema({
  ts,
  jsonSchema: S.schema({ input: S.string, output: S.string }).with(S.strict).with(S.meta, {
    description:
      "S.toJSONSchema(schema) for both directions, as a one-line source-text string (same " +
      "formatting as example values) — or (per direction) the message S.toJSONSchema threw " +
      "if that direction can't be represented (e.g. a bigint/symbol field). Filled by " +
      "`spec check --write`.",
  }),
  operations,
})
  .with(S.strict)
  .with(S.meta, {
    description: "One schema's complete, machine-checked contract. See the `spec` skill.",
  });
export type Spec = S.Output<typeof specSchema>;

export type OpName = keyof Spec["operations"];

// Ordered dimension keys — the canonical key order for `spec format`. Built via
// `Record<keyof T, true>` (not a plain array literal) so adding a field to
// `ts`/`operations`/`specSchema` without updating the matching order here is a
// compile error, not a silently-out-of-order key at serialize time.
const keyOrder = <T,>(order: Record<keyof T, true>) => Object.keys(order) as (keyof T)[];
export const KEY_ORDER = keyOrder<Spec>({ ts: true, jsonSchema: true, operations: true });
export const TS_KEY_ORDER = keyOrder<Spec["ts"]>({
  schema: true,
  aliases: true,
  input: true,
  output: true,
  instantiations: true,
  bundleBytes: true,
});
export const OP_ORDER = keyOrder<Spec["operations"]>({ parse: true, decode: true, encode: true });

export const isSkip = (v: unknown): v is Skip =>
  v !== null && typeof v === "object" && "_skip" in (v as object);

// Parse, don't validate: return the parsed Spec itself, not just a pass/fail
// flag, so callers work from the value Sury actually confirmed matches the
// schema instead of re-trusting the raw input.
export const validate = (
  obj: unknown,
): { ok: true; value: Spec } | { ok: false; error: string } => {
  try {
    return { ok: true, value: S.parser(specSchema)(obj) };
  } catch (e) {
    return { ok: false, error: (e as Error).message };
  }
};

export const schemaJson = (): string =>
  JSON.stringify(S.toJSONSchema(specSchema), null, 2) + "\n";
