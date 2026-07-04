---
name: spec
description: Add or edit a Sury test spec — packages/sury/specs/*.yaml, one declarative file capturing a schema's full contract (type, JSON Schema, and per-operation codegen + input→output|error examples). Use when writing/updating a spec, adding an example, or touching the spec harness in packages/spec.
---

# Sury specs

One `specs/<id>.yaml` = one schema's full contract. **You author the schema and example _inputs_; the harness computes every golden.** Never hand-write a golden.

Run commands from the repo root (`pnpm spec …`).

## Workflow

```
pnpm spec new --id <id> --ts <schema>   # scaffold — jsonSchema + operations auto-derived from --ts
# edit specs/<id>.yaml: fill `ts.input`/`ts.output`, add example inputs under each op's `examples`
pnpm spec update [id]                    # execute schema → fill expression/jsonSchema/example results
pnpm spec check  [id]                    # gate: format-valid, canonical, skips well-formed, goldens fresh
```

`new` requires both `--id` and `--ts` (e.g. `pnpm spec new --id string.min --ts "S.string.with(S.min, 3)"`);
it immediately derives `jsonSchema` and `operations` from the given schema — identity ops collapse to
the bare literal `identity` automatically. Only `ts.input`/`ts.output` (still need the TS type strings
spelled out by hand) and example inputs are left to fill in. `[id]` is optional for
`update`/`check`/`fmt`/`gen` — omit it to process every spec.
`pnpm test` regenerates the hidden test files and runs them (behavior + types).
To add a case: add a named entry under an op's `examples` with just `input`, then `pnpm spec update`.

## Rules (these are enforced)

- **Never type a golden by hand.** `expression`, `jsonSchema`, and each example's
  `output`/`error` are written by `pnpm spec update` from the live schema. You only
  own `ts.schema`, `ts.input`/`ts.output`, and example `input`s.
- **Exhaustive.** Every dimension and every operation (`parse`/`decode`/`encode`)
  must be present. Not asserting one? Set `_skip: <reason>` — reason is an enum
  (`parser-only`, `serializer-only`, `lossy`, `not-applicable`) or `todo(#…)`.
  A bare/unexplained skip is rejected.
- **Identity ops.** An operation that compiles to Sury's pass-through must be
  the bare literal `identity` (not a full op block, not `_skip: identity`), and
  `identity` is verified to actually compile to a pass-through. `update`/`check`
  error either way.
- **Single surface (for now).** `ts.schema` is JS `.with`-chain source (e.g.
  `S.string.with(S.min, 3)`), executed directly. A future `res` (ReScript)
  surface sits alongside `ts` with its own shape.
- **Closed world.** Unknown keys are rejected; `_`-prefixed keys are the reserved
  harness namespace (`_skip`). Never edit `tests/generated/` or `spec.schema.json` by hand.

## Format

```yaml
# yaml-language-server: $schema=./spec.schema.json
ts:                              # the JS `.with`-chain surface (executed)
  schema: S.string
  input: string                  # S.Input<schema>, as a type string — you write this
  output: string                 # S.Output<schema>, as a type string — you write this
  instantiations: { _skip: todo(#instantiations-dimension) }
  bundleBytes:    { _skip: todo(#bundle-dimension) }
jsonSchema: { input: {...}, output: {...} }   # filled by update
operations:
  parse:                        # parse=unknown→out, decode=in→out, encode=out→in
    expression: ""              # filled by update
    examples:
      valid:   { input: '"hi"' }          # you write input; update adds output/error
      bad:     { input: "42" }
  decode: identity               # bare literal — this op compiles to Sury's pass-through
  encode: identity
```

Every code field is source text (`'"hi"'` is the string `"hi"`, `42` is a number).
For `encode`, input is an Output value and output an Input value (the type flips).

## Dimensions

| Field | Golden source |
|---|---|
| `operations.<op>.expression` | `.toString()` of `S.parser`/`S.decoder`/`S.encoder` |
| `operations.<op>.examples` | running the op on each input |
| `jsonSchema.input` / `.output` | `S.toJSONSchema(schema)` / `…(S.reverse(schema))` |
| `ts.output` / `ts.input` | `expectTypeOf<S.Output<schema>>`/`<S.Input<schema>>` under Vitest typecheck — you author the expected type string, `spec new`/`update` don't derive it |
| `ts.instantiations`, `ts.bundleBytes` | planned — leave as `_skip: todo` |

## Layout

- `packages/sury/specs/<id>.yaml` — authored spec (published with the `sury` package)
- `packages/sury/specs/spec.schema.json` — emitted from the format schema (`pnpm spec schema`)
- `packages/spec/` — the spec CLI (its own workspace package). **Don't touch these files when working on Sury itself** — it's the test harness, not part of the library.
- `packages/sury/tests/generated/*.gen_test.ts` — gitignored; regenerated before `pnpm test`
- `packages/sury/tests/spec_test.ts` + `__snapshots__/` — harness tests + generated-output snapshot

## Improving the harness

The harness should be strict and guide the author. If, while writing a spec, you
hit something it *should* have caught or guided better — a missing check, a weak
error, a strictness gap that let a bad spec through — **don't just work around it.**
Add a bullet under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
