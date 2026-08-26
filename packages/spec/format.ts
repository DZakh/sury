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

const inputDescription =
  'Source text for the input, e.g. \'"hello"\'. Hand-written; `spec check --write` fills output/error.';
const benchDescription =
  "When true, this example is measured as its own perf target instead of being aggregated with the rest of its outcome.";
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
const operationExpression = S.schema({
  // Declared, not refreshed: an async operation is compiled by a different
  // builder and returns a Promise — a different API for every consumer — so
  // `--write` never adds or removes the marker in place. A schema that turns
  // async fails the check instead of quietly rewriting the spec to say so.
  // Absent means sync; `false` is never written, so there's one spelling per state.
  isAsync: S.optional(S.schema(true)).with(S.meta, {
    description:
      "`true` if this direction is async (built with S.asyncParser/asyncDecoder/asyncEncoder, " +
      "examples awaited). Written when the block is first created; `spec check` errors when it " +
      "disagrees with the schema. Omit when sync.",
  }),
  expression: orSkip(S.string).with(S.meta, {
    description: "Compiled function source (`.toString()`). Filled by `spec check --write`.",
  }),
  examples: S.record(example).with(S.meta, {
    description: "Named example cases, keyed by a short name (e.g. `valid`, `invalid-type`).",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "Compiled codegen plus its runnable examples." });
export type OperationExpression = S.Output<typeof operationExpression>;

// The operation analogue of a thrown `jsonSchema` string: some conversions are
// rejected when the operation is compiled (an unsupported or ambiguous `.to`),
// so there's no `expression` to record — only the creation-time message. Kept
// as its own block (not a `_skip`) because that message, and its suggested
// rewrites, are product surface to be ratcheted like codegen. Recorded per
// direction, like jsonSchema — the two directions can throw different messages.
const operationCreationError = S.schema({
  creationError: S.string.with(S.meta, {
    description:
      "The Sury error thrown when this operation is compiled (a conversion rejected at " +
      "operation creation). Filled by `spec check --write`.",
  }),
})
  .with(S.strict)
  .with(S.meta, {
    description: "An operation that can't be compiled — the creation-time error, ratcheted like codegen.",
  });
export type CreationError = S.Output<typeof operationCreationError>;

// An operation is either a full block or a literal shorthand:
// - `identity` — Sury's pass-through compile.
// - `eq-to-parse` (decode/encode only) — compiles to exactly the same code as
//   the spec's `parse` op, so the expression and examples live there; or is
//   rejected at creation with parse's exact message.
// - a `{creationError}` block — rejected at operation creation with a message
//   of its own.
// harness.identityViolations enforces the shorthands both ways: an op that
// compiles to a shorthand's meaning must use it, and the shorthand must
// actually hold.
const operationOrIdentity = S.union(["identity", operationExpression, operationCreationError]).with(S.meta, {
  description:
    "`identity` if this compiles to Sury's pass-through, a `{creationError}` block if rejected at operation creation, else a full operation block.",
});
const operationOrShorthand = S.union(["identity", "eq-to-parse", operationExpression, operationCreationError]).with(
  S.meta,
  {
    description:
      "`identity` if this compiles to Sury's pass-through, `eq-to-parse` if it compiles to the same code as `parse`, a `{creationError}` block if rejected at operation creation, else a full operation block.",
  },
);
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
      "The schema under test, as JS `.with`-chain source (e.g. `S.string.with(S.minLength, 3)`). " +
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
})
  .with(S.strict)
  .with(S.meta, {
    description: "The JS `.with`-chain surface: the schema itself, plus its inferred types and instantiation cost.",
  });

const zodOverwrite = S.schema({
  schema: S.string.with(S.meta, {
    description: "Zod (v4) schema whose type differs from ts on ≥1 side, e.g. `z.object({...})`.",
  }),
  divergence: S.string.with(S.meta, {
    description: "How the Zod type differs from Sury's, and why. Hand-written.",
  }),
  input: S.optional(S.string).with(S.meta, {
    description: "Zod's input type, only if it diverges from ts.input (filled by `--write`); omit when equal.",
  }),
  output: S.optional(S.string).with(S.meta, {
    description: "Zod's output type, only if it diverges from ts.output (filled by `--write`); omit when equal.",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "Zod equivalent that infers a different type than Sury; divergent side(s) recorded." });
export type ZodOverwrite = S.Output<typeof zodOverwrite>;

// Cross-library equivalent, checked live like `ts.aliases` (no golden). A
// required dimension: each spec declares a real Zod equivalent or an explicit
// `zod: { _skip }`. Only inferred types are asserted — codegen, JSON Schema,
// errors, coercion diverge by design.
const vs = S.schema({
  zod: S.union([S.string, zodOverwrite, skip]).with(S.meta, {
    description:
      "Equivalent Zod (v4) schema. Bare string: inferred types must equal ts.input/ts.output. Object " +
      "`{schema,divergence,input?,output?}`: differs from ts — divergent side recorded, matching side omitted. " +
      "`_skip` if Zod can't express it.",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "Cross-library equivalents, type-checked against this spec." });

export const JSON_SCHEMA_TARGETS = ["draft-2020-12", "openapi-3.0"] as const;
export type JsonSchemaTargetName = (typeof JSON_SCHEMA_TARGETS)[number];

const jsonSchemaDocument = S.schema({
  input: S.string.with(S.meta, {
    description: "S.toJSONSchema(schema), as source text, or its conversion error.",
  }),
  fromInputType: S.optional(S.string).with(S.meta, {
    description:
      "The type inferred by S.fromJSONSchema(input), only when it differs from ts.input; omit when equal or when input is a conversion error.",
  }),
  output: S.string.with(S.meta, {
    description: "S.toJSONSchema(S.reverse(schema)), as source text, or its conversion error.",
  }),
  fromOutputType: S.optional(S.string).with(S.meta, {
    description:
      "The type inferred by S.fromJSONSchema(output), only when it differs from ts.output; omit when equal or when output is a conversion error.",
  }),
})
  .with(S.strict)
  .with(S.meta, {
    description: "One dialect's toJSONSchema documents for both directions.",
  });
export type JsonSchemaDocument = S.Output<typeof jsonSchemaDocument>;

const jsonSchemaTargets = S.schema({
  "draft-2020-12": S.optional(jsonSchemaDocument).with(S.meta, {
    description:
      "Filled only when this dialect's document (ignoring $schema) differs from the default no-options emit.",
  }),
  "openapi-3.0": S.optional(jsonSchemaDocument).with(S.meta, {
    description:
      "Filled only when this dialect's document (ignoring $schema) differs from the default no-options emit.",
  }),
})
  .with(S.strict)
  .with(S.meta, {
    description:
      "Dialect-gated toJSONSchema output, omitted entirely when every listed target matches the default.",
  });
export type JsonSchemaTargets = S.Output<typeof jsonSchemaTargets>;

export const specSchema = S.schema({
  ts,
  jsonSchema: S.schema({
    input: S.string.with(S.meta, {
      description: "S.toJSONSchema(schema), as source text, or its conversion error.",
    }),
    fromInputType: S.optional(S.string).with(S.meta, {
      description:
        "The type inferred by S.fromJSONSchema(input), only when it differs from ts.input; omit when equal or when input is a conversion error.",
    }),
    output: S.string.with(S.meta, {
      description: "S.toJSONSchema(S.reverse(schema)), as source text, or its conversion error.",
    }),
    fromOutputType: S.optional(S.string).with(S.meta, {
      description:
        "The type inferred by S.fromJSONSchema(output), only when it differs from ts.output; omit when equal or when output is a conversion error.",
    }),
    targets: S.optional(jsonSchemaTargets).with(S.meta, {
      description:
        "Per-dialect documents, only for targets whose emit differs from the default draft-07 (no $schema) document. Filled by `spec check --write`.",
    }),
  })
    .with(S.strict)
    .with(S.meta, {
      description:
        "S.toJSONSchema(schema) for both directions, as one-line source text, plus any divergent " +
        "type inferred by S.fromJSONSchema for each generated document. Matching types are " +
        "omitted; if a direction can't be represented, no round-trip type is recorded. " +
        "Filled by `spec check --write`.",
    }),
  vs,
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
export const KEY_ORDER = keyOrder<Spec>({ ts: true, jsonSchema: true, vs: true, operations: true });
export const VS_KEY_ORDER = keyOrder<Spec["vs"]>({ zod: true });
export const VS_ZOD_KEY_ORDER = keyOrder<ZodOverwrite>({ schema: true, divergence: true, input: true, output: true });
export const TS_KEY_ORDER = keyOrder<Spec["ts"]>({
  schema: true,
  aliases: true,
  input: true,
  output: true,
  instantiations: true,
});
export const OP_ORDER = keyOrder<Spec["operations"]>({ parse: true, decode: true, encode: true });
export const OP_BLOCK_KEY_ORDER = keyOrder<OperationExpression>({
  isAsync: true,
  expression: true,
  examples: true,
});
export const JSON_SCHEMA_DOCUMENT_KEY_ORDER = keyOrder<JsonSchemaDocument>({
  input: true,
  fromInputType: true,
  output: true,
  fromOutputType: true,
});
export const JSON_SCHEMA_TARGET_KEY_ORDER = keyOrder<JsonSchemaTargets>({
  "draft-2020-12": true,
  "openapi-3.0": true,
});
export const JSON_SCHEMA_KEY_ORDER = keyOrder<Spec["jsonSchema"]>({
  input: true,
  fromInputType: true,
  output: true,
  fromOutputType: true,
  targets: true,
});

export const isSkip = (v: unknown): v is Skip => S.is(skip, v);

// The overwrite form of `vs.zod` — distinguished from a bare string (Zod
// source) and from `{_skip}` by carrying its own `schema` key.
export const isZodOverwrite = (v: unknown): v is ZodOverwrite => S.is(zodOverwrite, v);

// The creation-error operation block — distinguished from an `{expression,
// examples}` block and the string shorthands by carrying `creationError`.
export const isCreationError = (v: unknown): v is CreationError => S.is(operationCreationError, v);

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

// ---- bundleSize.yaml -------------------------------------------------------

// The whole-package bundle-size ratchet: one gzipped-byte row per public
// export of the dev entry, plus `total` for the whole entry. A second format
// alongside `specSchema`, not a spec dimension — bundle cost is a property of
// the package's export surface, and a per-schema number measures only the
// exports that schema reaches plus the author's own source literal.
//
// Every field is derived (bundleSize.ts measures them), so unlike specs there
// is no hand-authored part and no emitted JSON Schema — no author for
// yaml-language-server to help. It's still schema-validated so a hand-edited
// or truncated file fails with a pointed message instead of a whole-file diff.
export const bundleSizeSchema = S.schema({
  total: S.number.with(S.meta, {
    description: "Minified+gzipped size of the whole entry (`export * from \"sury\"`) — the anchor row.",
  }),
  exports: S.record(S.number).with(S.meta, {
    description: "Minified+gzipped size of each public export bundled in isolation, keyed by export name.",
  }),
})
  .with(S.strict)
  .with(S.meta, {
    description: "Bundle size of the package's export surface. Filled by `spec check --write`.",
  });
export type BundleSize = S.Output<typeof bundleSizeSchema>;

export const BUNDLE_SIZE_KEY_ORDER = keyOrder<BundleSize>({ total: true, exports: true });

export const validateBundleSize = (
  obj: unknown,
): { ok: true; value: BundleSize } | { ok: false; error: string } => {
  try {
    return { ok: true, value: S.parser(bundleSizeSchema)(obj) };
  } catch (e) {
    return { ok: false, error: (e as Error).message };
  }
};

// ---- scenarios.yaml --------------------------------------------------------

// A spec times the library's inner surface (create, compile, compiled
// operation); a scenario times a whole call the way a consumer writes it, so
// the dispatch around the compiled operation — invisible to every per-spec
// phase — is inside the measurement. Perf never stores a number, so scenarios
// have no goldens and no `--write`; `spec check` executes each one instead.
export const scenarioSchema = S.schema({
  prepare: S.optional(S.string).with(S.meta, {
    description:
      "Statements run once per library version before measuring, with `S` in scope; their bindings are in scope for `run`. Build the schema and the input here — only `run` is timed.",
  }),
  run: S.string.with(S.meta, {
    description:
      "The expression to measure, evaluated in `prepare`'s scope. Must not throw: timing a throw measures error construction.",
  }),
})
  .with(S.strict)
  .with(S.meta, { description: "One measured consumer-level call." });
export type Scenario = S.Output<typeof scenarioSchema>;

export const scenariosSchema = S.record(scenarioSchema).with(S.meta, {
  description:
    "Consumer-level performance scenarios, keyed by id, measured by `spec check --perf` alongside the specs.",
});
export type Scenarios = S.Output<typeof scenariosSchema>;

export const validateScenarios = (
  obj: unknown,
): { ok: true; value: Scenarios } | { ok: false; error: string } => {
  try {
    return { ok: true, value: S.parser(scenariosSchema)(obj) };
  } catch (e) {
    return { ok: false, error: (e as Error).message };
  }
};

export const scenariosSchemaJson = (): string =>
  JSON.stringify(S.toJSONSchema(scenariosSchema), null, 2) + "\n";
