# Sury Architecture

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `Sury.res.mjs` ships to browsers. Reuse helpers (`B.refine`, `B.applyInputRefiner`, `B.applyOutputRefiner`) over duplicated codegen.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime ships per-schema, library ships once).

## Input vs Output

A schema has an Input type and an Output type. They differ when the schema or any nested item has a transformation.

```ts
S.string                                          // string → string
S.schema({ foo: S.string.with(S.to, S.number) })  // {foo:string} → {foo:number}
```

Schema modifiers (`.with(S.refine, …)`, etc.) apply to the **output** type. `inputRefiner` and `refiner` are stored separately so `S.reverse` can swap them. Every schema must be reversible (Input→Output ↔ Output→Input) unless explicitly opted out. Modifiers like `name` and built-in refinements apply to both sides.

## Decode pipeline

Decoder takes a single schema, Input → Output. Schemas joined by `.to` form one fused transformation pipeline.

Per-schema execution order:

1. **decoder** — narrow input to schema's Input type (may skip to Output if no `inputRefiner`).
2. **inputRefiner** — user validations on Input.
3. **decoder** — Input → Output (e.g. decode nested fields).
4. **refiner** — user validations on Output.
5. If `.to`: **parser** (custom Output → `.to` Input) OR **encoder** (default Output → `.to` Input) + recurse into `.to.decoder`.

`S.reverse` swaps `inputRefiner ↔ refiner`, `parser ↔ serializer`, and reverses the `.to` chain.

## Refiner ownership

The parse loop (around `expected.decoder(~input)`) applies `inputRefiner`/`refiner` **only for primitive decoders** — ones whose result has `isOutput !== Some(true)`. It pushes them as checks via `B.refine`.

**Advanced decoders** (`objectDecoder`, `arrayDecoder`, tuple, union, recursive — anything that sets `isOutput = Some(true)`) own refiner application themselves, because:
- They know the optimal injection points: `inputRefiner` on the *raw input* before field decoding (short-circuits before allocation); `refiner` on the *assembled output*.
- The generic fallback would force materializing an output val even when nothing else needs it.

Use the two shared helpers — `B.applyInputRefiner(~val, ~schema)` and `B.applyOutputRefiner(~val, ~schema)` — at the right insertion points. Split into two fns (not `~which`) so the bundler DCEs whichever is unused. **A new advanced decoder that skips either silently drops user `S.refine`s.**

Async output refiner must run inside `.then()` on the resolved value, never on the Promise wrapper.

## Async

Any transformation may be async. Continue the chain via `.then()`. For nested items (object fields, array items), aggregate with `Promise.all()`.

## Val

A `val` is the compile-time view of a runtime value at one point in the generated code.

Core fields:
- `schema` — actual type at this point
- `expected` — schema to build decoder for
- `var()` — variable name in generated code
- `inline` — inline expression form
- `path` — location in input (for errors)

Transformation chain (relative to `.prev`):
- `prev` — previous val in the chain
- `code` — codegen from `.prev` to this val
- `validation` — type-check condition from `.prev` (distinct from user refiners)

Helpers:
- `B.refine` — clones a val to attach checks, keeping the var-allocation link.
- `skipTo` — abort parse after current decoder. Prefer `val.expected`; aim to remove `skipTo`.
