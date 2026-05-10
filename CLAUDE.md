# Sury Architecture

## Primary Goals

Sury optimizes for three goals — in this order, when they conflict:

1. **DX** — public API and error messages must be intuitive. Schema authors and consumers should not need to know how the compiler works.
2. **High performance** — generated code is the hot path. Avoid redundant work (extra variables, unused allocations, double validation) and prefer inlining over indirection.
3. **Minimal bundle size** — every byte of `Sury.res.mjs` ships to the browser. Prefer reusable helpers (`B.refine`, `B.applyRefiners`, etc.) over duplicated codegen branches; prefer one well-named primitive over several near-duplicates.

When in doubt: pick the option that produces the shortest *generated* code at runtime AND the shortest *library* code at compile time. If those conflict, runtime wins (it ships per-schema, library code ships once).

## Schema Input and Output Types

A schema represents two types: Input and Output.

### Example 1: Same Input and Output

```typescript
S.string
// Input: string
// Output: string
```

### Example 2: Different Input and Output

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

Since this schema does not have `.to`, `inputRefiner` and `refiner` must be stored separately to support `S.reverse`. Every schema should be reversible from Input→Output to Output→Input, unless explicitly prevented.

For modifications like `name` or built-in refinements that do not affect nested items, they apply to both input and output without differentiation.

## Decode Function

The decode function is created from a single schema and transforms the schema's Input to Output. When multiple schemas are joined by the `.to` property, they are automatically combined into a single transformation pipeline.

## Schema Properties and Execution Order

Schema properties are executed in the following order:

1. **decoder** - If input val differs from the schema, decode it to the schema's input type. May skip directly to schema output if there is no inputRefiner.

2. **inputRefiner** - Custom validations on the input part of the schema value.

3. **decoder** - Decodes input to output for the current schema. Typically required to decode nested items such as object fields.

4. **refiner** - Custom validations on the output part of the schema value.

### If Schema Has `.to` Property

5. **parser** - Custom transformation logic to the `.to` schema. The serializer is the reverse of parser.

### If There Is No Parser

5. **encoder** - Transformation logic from the current schema's output to the `.to` schema's input.

6. **.to.decoder** - Starts the cycle from the beginning with the `.to` schema.

### Refiner Application — Who Is Responsible

The parse loop (in `parse`, around the `expected.decoder(~input)` call) handles refiner application **only for primitive decoders** — those that return a val with `isOutput !== Some(true)`. For these, the loop checks `expected.inputRefiner` / `expected.refiner` and pushes them as checks via `B.refine`.

**Advanced decoders that mark their output `isOutput = Some(true)` (i.e., decoders that produce an internally-transformed value, such as `objectDecoder`, `arrayDecoder`, tuple, union, recursive) own refiner application themselves.** The generic post-decoder fallback is intentionally skipped because:

- The advanced decoder knows the optimal injection points: `inputRefiner` runs on the *original input* before field/item decoding (so a failure short-circuits before any allocation); `refiner` runs on the *assembled output* (so it observes the transformed shape).
- Applying refiners generically after the fact would force the decoder to emit a materialized output value even when nothing else needed it, costing bundle size and runtime allocation.

A shared `B.applyRefiners(~val, ~schema, ~which=#Input | #Output)` helper exists so each advanced decoder can call it without duplicating the `hasInputRefiner` / `hasRefiner` boilerplate. If you implement a new advanced decoder, you **must** call this helper at both insertion points; otherwise user-supplied `S.refine` is silently dropped.

## Reversal with S.reverse

`S.reverse` swaps:

- `inputRefiner` ↔ `refiner`
- `parser` ↔ `serializer`
- Reverses the `.to` chain direction

## Async Support

Every transformation may return an async value. To continue the transformation chain:

1. Append `.then()` and continue the logic in the callback function.
2. For nested items (e.g., object fields, array items), create a promise that collects all inner items with `Promise.all()`.

## Val

The `val` represents a value at a specific point in time during compilation. Each `val` reflects a specific value type at that moment.

Key properties:

- `schema` - The actual type of the value at this point
- `expected` - The schema to build decoder for
- `var` - Returns the variable name in generated code
- `inline` - The value as an inline code expression
- `path` - Current location in the data structure (for error messages)

Transformation tracking (relative to `.prev`):

- `prev` - The previous val in the chain, indicating where this value originated from
- `code` - Generated code describing the transformation from `.prev` to this val
- `validation` - Type check condition from `.prev` (e.g., `typeof x === "string"`). Different from custom refiners.

This design allows tracing back through the transformation history, where each step records what code was generated and what validations were applied to get from the previous state to the current one.

- `B.refine` allows to modify the value, by cloning, while keeping the var allocation link.

- `skipTo` is used to abort the parse after finishing current decoder. Ideally to get rid of it and use `val.expected` instead.
