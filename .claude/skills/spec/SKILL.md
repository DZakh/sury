---
name: spec
description: Add or edit a Sury test spec — packages/sury/specs/*.yaml, one declarative file capturing a schema's full contract (type, JSON Schema, and per-operation codegen + input→output|error examples). Use when writing/updating a spec, adding an example, or touching the spec harness in packages/spec.
---

# Sury specs

One `specs/<id>.yaml` = one schema's full contract. **You author the schema and example _inputs_; the harness computes every golden.** Never hand-write a golden.

Run commands from the repo root (`pnpm spec …`).

## Workflow

```
pnpm spec new <id>       # scaffold — every dimension starts as _skip: todo
# edit specs/<id>.yaml: fill schema.res/ts and example inputs
pnpm spec update [id]    # execute schema → fill expression, jsonSchema, example results
pnpm spec check  [id]    # gate: format-valid, canonical, skips well-formed, goldens fresh
```

`[id]` is optional for `update`/`check`/`fmt`/`gen` — omit it to process every spec; `new` requires one.
`pnpm test` regenerates the hidden test files and runs them (behavior + types).
To add a case: add a named entry under an op's `examples` with just `input`, then `pnpm spec update`.

## Rules (these are enforced)

- **Never type a golden by hand.** `expression`, `jsonSchema`, and each example's
  `output`/`error` are written by `pnpm spec update` from the live schema. You only
  own `schema.*` and example `input`s.
- **Exhaustive.** Every dimension and every operation (`parse`/`decode`/`encode`)
  must be present. Not asserting one? Set `_skip: <reason>` — reason is an enum
  (`identity`, `parser-only`, `serializer-only`, `lossy`, `not-applicable`) or
  `todo(#…)`. A bare/unexplained skip is rejected.
- **Identity ops.** An operation that compiles to Sury's pass-through must be
  `_skip: identity` (not a full op block), and `_skip: identity` is verified to
  actually be identity. `update`/`check` error either way.
- **Two surfaces.** `schema.res` (ReScript, e.g. `S.string->S.min(3)`) and
  `schema.ts` (JS `.with`, e.g. `S.string.with(S.min, 3)`). Identical is fine
  (`S.string`). Only `ts` is executed today.
- **Closed world.** Unknown keys are rejected; `_`-prefixed keys are the reserved
  harness namespace (`_skip`). Never edit `tests/generated/` or `spec.schema.json` by hand.

## Format

```yaml
# yaml-language-server: $schema=./spec.schema.json
schema:
  res: S.string                 # ReScript surface
  ts: S.string                  # JS surface (executed)
types: { ts: S.Schema<string, string> }
jsonSchema: { input: {...}, output: {...} }   # filled by update
instantiations: { _skip: todo(#instantiations-dimension) }
bundleBytes:    { _skip: todo(#bundle-dimension) }
operations:
  parse:                        # parse=unknown→out, decode=in→out, encode=out→in
    expression: ""              # filled by update
    examples:
      valid:   { input: '"hi"' }          # you write input; update adds output/error
      bad:     { input: "42" }
  decode: { _skip: identity }   # identity op → skip (not a full block)
  encode: { _skip: identity }
```

Every code field is source text (`'"hi"'` is the string `"hi"`, `42` is a number).
For `encode`, input is an Output value and output an Input value (the type flips).

## Dimensions

| Field | Golden source |
|---|---|
| `operations.<op>.expression` | `.toString()` of `S.parser`/`S.decoder`/`S.encoder` |
| `operations.<op>.examples` | running the op on each input |
| `jsonSchema.input` / `.output` | `S.toJSONSchema(schema)` / `…(S.reverse(schema))` |
| `types.ts` | `expectTypeOf` under Vitest typecheck |
| `instantiations`, `bundleBytes` | planned — leave as `_skip: todo` |

## Layout

- `packages/sury/specs/<id>.yaml` — authored spec (published with the `sury` package)
- `packages/sury/specs/spec.schema.json` — emitted from the format schema (`pnpm spec schema`)
- `packages/spec/` — the spec CLI (its own workspace package). **Don't touch these files when working on Sury itself** — it's the test harness, not part of the library.
- `packages/sury/tests/generated/*.gen_test.ts` — gitignored; regenerated before `pnpm test`
- `packages/sury/tests/spec_test.ts` + `__snapshots__/` — harness tests + generated-output snapshot
