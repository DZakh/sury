# Sury

A ReScript schema validation library that compiles schemas into optimized JavaScript functions via `eval`.

## Project Structure

- `packages/sury/src/Sury.res` — main source (single large file, ~7000 lines)
- `packages/sury/src/S.resi` — public API interface
- `packages/sury/src/JSONSchema.res` — JSON Schema type definitions
- `packages/sury/tests/` — AVA test files (`.res` compiled to `.res.mjs`)
- `packages/sury/failing-tests.snapshot.txt` — baseline of known failing tests

## Build & Test

```sh
pnpm install
cd packages/sury
pnpm rescript build --dev   # compile ReScript
pnpm exec ava               # run all tests
pnpm exec ava tests/S_refine_test.res.mjs  # run a specific test file
```

## Architecture: Code Generation Pipeline

Sury compiles schemas into JavaScript functions at runtime. The pipeline has three main concepts:

### Val (`Sury.res:552`)

A `val` is the compile-time representation of a value flowing through generated code. It is NOT the runtime value — it's metadata the code generator uses to emit JavaScript.

Key fields:
- `var: unit => string` — produces the JS variable name (e.g., `"v0"`)
- `inline: string` — inline JS expression for the value
- `schema: internal` — the schema of the input data
- `expected: internal` — the schema we're parsing into
- `codeFromPrev: string` — accumulated code from the previous step in the val chain
- `prev: option<val>` — linked list back to the previous val
- `isInput/isOutput: option<bool>` — flags marking the val's position in the schema chain
- `path: Path.t` — JSON path for error reporting

Vals form a linked list via `prev`. Each decoder/transform step produces a new val pointing back to its input val.

### Schema (`internal` type, `Sury.res:440`)

An internal schema has:
- `decoder: builder` — code generator for parsing input into this schema's type
- `encoder: option<encoder>` — code generator for encoding from this schema's type
- `parser/serializer: option<builder>` — custom transform steps (from `S.transform`)
- `refiner: option<(~input: val) => string>` — output validation (runs after decoder)
- `inputRefiner: option<(~input: val) => string>` — input validation (runs before decoder, exists on reversed schemas)
- `to: option<internal>` — next schema in an `S.to` chain

### The `parse` function (`Sury.res:1968`)

The central code generation loop. Walks a schema chain, calling decoders and transforms:

```
while (!isOutput || expected.to exists) {
  if (async)        → wrap in .then(), recurse
  if (isOutput)     → run parser/transform to next schema in `to` chain
  else              → try encoder, then decoder
                      if decoder didn't set isOutput:
                        finalize: isInput=true, isOutput=true
                        apply refinements here
}
```

Three branches:
1. **Async** — wraps in Promise.then, delegates to recursive parse
2. **Parser-transform** (`isOutput && expected.to`) — val is finalized output of current schema, but there's a `to` chain. Runs parser to produce input for the next schema. No refinement application here — refinements already fired when the val was finalized.
3. **Decoder** — the main path. Runs encoder (if available), then decoder. If the decoder didn't set `isOutput`, the parse loop finalizes the val and applies refinements.

### `B.merge` (`Sury.res:1272`)

Walks the val linked list and concatenates generated code into a single JS string. Handles:
- Type validation code (typeof checks)
- Variable allocations
- `codeFromPrev` from each val

`merge` is pure string assembly — it does NOT apply refinements or read schema fields.

### Refinements

Refinements are code generators (`(~input: val) => string`) that emit inline JS validation checks.

Two kinds on a schema:
- `refiner` — validates the **output** (decoded value). Set by `S.refine` and built-in validators like `S.min`, `S.email`.
- `inputRefiner` — validates the **input** (pre-decoded value). Created by `S.reverse` which swaps refiner <-> inputRefiner.

**Where refinements are applied:**

Refinements are applied in `parse` after decoder finalization via `B.applyRefiner`:
- `refiner` code is appended to `outputVal.codeFromPrev`
- `inputRefiner` code is appended to `inputVal.codeFromPrev`

For decoders that set `isOutput` themselves (like `unionDecoder` with `toPerCase`), the decoder propagates the refiner into sub-schemas so recursive `parse` calls handle application.

### Union Decoder (`Sury.res:3332`)

The most complex decoder. Handles discriminated unions, type-based dispatch, try/catch fallbacks.

Key paths:
- **Standard union** — each branch is parsed recursively via `parse`. The recursive call handles refinement application for each member.
- **`toPerCase`** — when the union has `S.to(target)` without a custom parser, each member schema gets `mut.to = Some(target)` and the union-level refiner is propagated into each member. Each member's recursive `parse` applies the propagated refiner after the member's decoder, before the `to` coercion.
- **Wider union** — input is already a union of compatible type; decoder returns input unchanged.

### `S.reverse` (`Sury.res:2140`)

Creates a reversed schema by walking the chain and swapping:
- `parser` <-> `serializer`
- `refiner` <-> `inputRefiner`
- `default` <-> `fromDefault`

## Conventions

- Schema internals use `Obj.magic` for fast truthiness checks on optional fields
- `B.embed(value)` stores a runtime value in the `embeded` array and returns `e[N]` reference for generated JS
- `B.next` creates a new val with a transform (different inline expression)
- `B.refine` creates a new val with same value but different schema/expected metadata
- `updateOutput` walks to the output end of a schema chain and mutates a copy
