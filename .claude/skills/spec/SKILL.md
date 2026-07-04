---
name: spec
description: Add or edit a Sury test spec — packages/sury/specs/*.yaml, one declarative file capturing a schema's full contract (type, JSON Schema, and per-operation codegen + input→output|error examples). Use when writing/updating a spec, adding an example, or touching the spec harness in packages/spec.
---

# Sury specs

One `specs/<id>.yaml` = one schema's full contract. **You author the schema and example _inputs_; the harness computes every golden.** Never hand-write a golden.

Run all commands from `packages/sury`.

## Workflow

```
pnpm spec new <id>       # scaffold — every dimension starts as _skip: todo
# edit specs/<id>.yaml: fill schema.res/ts and example inputs
pnpm spec update <id>    # execute schema → fill expression, jsonSchema, example results
pnpm spec check          # gate: format-valid, canonical, skips well-formed, goldens fresh
```

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
properties:     { _skip: todo(#pbt-dimension) }        # PBT not built yet
operations:
  parse:                        # parse=unknown→out, decode=in→out, encode=out→in
    expression: ""              # filled by update
    examples:
      valid:   { input: '"hi"' }          # you write input; update adds output/error
      bad:     { input: "42" }
  decode: { _skip: identity }   # or a full op block
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
| `instantiations`, `bundleBytes`, `properties` | planned — leave as `_skip: todo` |

## Layout

- `packages/sury/specs/<id>.yaml` — authored spec (published with the `sury` package)
- `packages/sury/specs/spec.schema.json` — emitted from the format schema (`pnpm spec schema`)
- `packages/spec/` — the CLI, its own workspace package (run via `tsx`). It uses sury **twice**:
  - `format.ts` — spec format defined **as a Sury schema**, on **published** sury (`sury-published`). Stable infra: validation + `spec.schema.json`, so the CLI doesn't break while core is refactored.
  - `harness.ts` — golden execution on the **dev source** (`../sury/src/S.js`), so goldens track your changes. Canonicalize + generate.
  - `cli.ts` — `check|fmt|gen|update|new|schema`.
- `packages/sury/tests/generated/*.gen_test.ts` — gitignored; regenerated before `pnpm test`
- `packages/sury/tests/spec_test.ts` + `__snapshots__/` — harness tests + generated-output snapshot
