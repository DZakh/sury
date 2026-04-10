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

## Test Guidelines

- Use Wallaby.js for test results, errors, and debugging when available
- Leverage runtime values and coverage data when debugging tests
- Fall back to terminal only if Wallaby isn't available

When debugging test failures:

1. Analyze failing tests with Wallaby and identify the cause of the failure.
2. Use Wallaby's covered files to find relevant implementation files or narrow your search.
3. Use Wallaby's runtime values tool and coverage tool to support your reasoning.
4. Suggest and explain a code fix that will resolve the failure.
5. After the fix, use Wallaby's reported test state to confirm that the test now passes.
6. If the test still fails, continue iterating with updated Wallaby data until it passes.
7. If a snapshot update is needed, use Wallaby's snapshot tools for it.

When responding:

- Explain your reasoning step by step.
- Use runtime and coverage data directly to justify your conclusions.

# Architecture

## Schema Input and Output Types

A schema represents two types: Input and Output.

### Same Input and Output

```typescript
S.string
// Input: string
// Output: string
```

### Different Input and Output

```typescript
S.schema({
  foo: S.string.with(S.to, S.number)
})
// Input: { foo: string }
// Output: { foo: number }
```

The input and output differ because nested items have transformations, even though the schema itself does not have a `.to` property.

## Modifying a Schema

When modifying a schema, the modification applies to the output type.

```typescript
S.schema({
  foo: S.string.with(S.to, S.number)
}).with(S.refine, () => {...})
```

Since this schema does not have `.to`, `inputRefiner` and `refiner` must be stored separately to support `S.reverse`. Every schema should be reversible from Input->Output to Output->Input, unless explicitly prevented.

For modifications like `name` or built-in refinements that do not affect nested items, they apply to both input and output without differentiation.

## Val (`Sury.res:552`)

The `val` represents a value at a specific point in time during compilation. Each `val` reflects a specific value type at that moment. It is NOT the runtime value — it's metadata the code generator uses to emit JavaScript.

Key properties:

- `schema` — the actual type of the value at this point
- `expected` — the schema to build decoder for
- `var` — returns the variable name in generated code (e.g., `"v0"`)
- `inline` — the value as an inline code expression
- `path` — current location in the data structure (for error messages)
- `isInput/isOutput` — flags marking the val's position in the schema chain

Transformation tracking (relative to `.prev`):

- `prev` — the previous val in the chain, indicating where this value originated from
- `codeFromPrev` — generated code describing the transformation from `.prev` to this val
- `validation` — type check condition from `.prev` (e.g., `typeof x === "string"`). Different from custom refiners.

This design allows tracing back through the transformation history, where each step records what code was generated and what validations were applied to get from the previous state to the current one.

- `B.refine` allows to modify the value, by cloning, while keeping the var allocation link.
- `B.next` creates a new val with a transform (different inline expression).
- `skipTo` is used to abort the parse after finishing current decoder. Ideally to get rid of it and use `val.expected` instead.

## Schema (`internal` type, `Sury.res:440`)

An internal schema has:

- `decoder: builder` — code generator for parsing input into this schema's type
- `encoder: option<encoder>` — code generator for encoding from this schema's type
- `parser/serializer: option<builder>` — custom transform steps (from `S.transform`)
- `refiner: option<(~input: val) => string>` — output validation (runs after decoder)
- `inputRefiner: option<(~input: val) => string>` — input validation (runs before decoder, exists on reversed schemas)
- `to: option<internal>` — next schema in an `S.to` chain

## Schema Properties and Execution Order

Schema properties are executed in the following order:

1. **decoder** — If input val differs from the schema, decode it to the schema's input type. May skip directly to schema output if there is no inputRefiner.

2. **inputRefiner** — Custom validations on the input part of the schema value.

3. **decoder** — Decodes input to output for the current schema. Typically required to decode nested items such as object fields.

4. **refiner** — Custom validations on the output part of the schema value.

### If Schema Has `.to` Property

5. **parser** — Custom transformation logic to the `.to` schema. The serializer is the reverse of parser.

### If There Is No Parser

5. **encoder** — Transformation logic from the current schema's output to the `.to` schema's input.

6. **.to.decoder** — Starts the cycle from the beginning with the `.to` schema.

## The `parse` Function (`Sury.res:1968`)

The central code generation loop. Walks a schema chain, calling decoders and transforms:

```
while (!isOutput || expected.to exists) {
  if (async)        -> wrap in .then(), recurse
  if (isOutput)     -> run parser/transform to next schema in `to` chain
  else              -> try encoder, then decoder
                       if decoder didn't set isOutput:
                         finalize: isInput=true, isOutput=true
                         apply refinements here (B.applyRefiner)
}
```

Three branches:

1. **Async** — wraps in Promise.then, delegates to recursive parse
2. **Parser-transform** (`isOutput && expected.to`) — val is finalized output of current schema, but there's a `to` chain. Runs parser to produce input for the next schema. No refinement application here — refinements already fired when the val was finalized.
3. **Decoder** — the main path. Runs encoder (if available), then decoder. If the decoder didn't set `isOutput`, the parse loop finalizes the val and applies refinements via `B.applyRefiner`.

## `B.merge` (`Sury.res:1272`)

Walks the val linked list and concatenates generated code into a single JS string. Handles:

- Type validation code (typeof checks)
- Variable allocations
- `codeFromPrev` from each val

`merge` is pure string assembly — it does NOT apply refinements or read schema fields.

## Refinements

Refinements are code generators (`(~input: val) => string`) that emit inline JS validation checks.

Two kinds on a schema:

- `refiner` — validates the **output** (decoded value). Set by `S.refine` and built-in validators like `S.min`, `S.email`.
- `inputRefiner` — validates the **input** (pre-decoded value). Created by `S.reverse` which swaps refiner <-> inputRefiner.

**Where refinements are applied:**

Refinements are applied in `parse` after decoder finalization via `B.applyRefiner`:

- `refiner` code is appended to `outputVal.codeFromPrev`
- `inputRefiner` code is appended to `inputVal.codeFromPrev`

For decoders that set `isOutput` themselves (like `unionDecoder` with `toPerCase`), the decoder propagates the refiner into sub-schemas so recursive `parse` calls handle application.

## Union Decoder (`Sury.res:3332`)

The most complex decoder. Handles discriminated unions, type-based dispatch, try/catch fallbacks.

Key paths:

- **Standard union** — each branch is parsed recursively via `parse`. The recursive call handles refinement application for each member.
- **`toPerCase`** — when the union has `S.to(target)` without a custom parser, each member schema gets `mut.to = Some(target)` and the union-level refiner is propagated into each member. Each member's recursive `parse` applies the propagated refiner after the member's decoder, before the `to` coercion.
- **Wider union** — input is already a union of compatible type; decoder returns input unchanged.

## `S.reverse` (`Sury.res:2140`)

Creates a reversed schema by walking the chain and swapping:

- `parser` <-> `serializer`
- `refiner` <-> `inputRefiner`
- `default` <-> `fromDefault`

## Async Support

Every transformation may return an async value. To continue the transformation chain:

1. Append `.then()` and continue the logic in the callback function.
2. For nested items (e.g., object fields, array items), create a promise that collects all inner items with `Promise.all()`.

## Conventions

- Schema internals use `Obj.magic` for fast truthiness checks on optional fields
- `B.embed(value)` stores a runtime value in the `embeded` array and returns `e[N]` reference for generated JS
- `updateOutput` walks to the output end of a schema chain and mutates a copy
