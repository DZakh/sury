---
name: spec
description: Add or edit a Sury test spec — packages/sury/specs/*.yaml, one declarative file capturing a schema's full contract (type, JSON Schema, and per-operation codegen + input→output|error examples). Use when writing/updating a spec, adding an example, or touching the spec harness in packages/spec.
---

# Sury specs

One `specs/<id>.yaml` = one schema's full contract. **You author the schema and example _inputs_; the harness computes every golden.** Never hand-write a golden.

Run commands from the repo root (`pnpm spec …`).

## Workflow

```bash
pnpm spec new --id <id> --ts <schema>   # scaffold — everything is auto-derived from --ts
# edit specs/<id>.yaml: add example inputs under each op's `examples`
pnpm spec check --write [id]             # (re)derive everything from the live schema and persist it
pnpm spec check         [id]             # gate: format-valid, canonical, skips well-formed, goldens fresh
```

`new` requires both `--id` and `--ts` (e.g. `pnpm spec new --id string-min --ts "S.string.with(S.min, 3)"`).
`new`/`check --write` derive every dimension except example `input`s, which you write by hand — see
"How types/instantiations/bundle size are derived" below for `ts.*`. `[id]` is optional for
`check`/`format`; omit it to process every spec.

To add a case: add a named entry under an op's `examples` with just `input`, then `pnpm spec check --write`.

## Rules (these are enforced)

- **Never type a golden by hand.** Every field except `ts.schema` and example `input`s is written by
  `spec new`/`spec check --write` from the live schema — including `ts.input`/`ts.output`/
  `ts.instantiations`/`ts.bundleBytes`, not just `expression`/`jsonSchema`/example results.
- **Exhaustive.** Every dimension and every operation (`parse`/`decode`/`encode`)
  must be present. Not asserting one? Set `_skip: <reason>` — reason is an enum
  (`parser-only`, `serializer-only`, `lossy`, `not-applicable`) or `todo(#…)`.
  A bare/unexplained skip is rejected.
- **Identity ops.** An operation that compiles to Sury's pass-through must be
  the bare literal `identity` (not a full op block, not `_skip: identity`), and
  `identity` is verified to actually compile to a pass-through. `check` errors
  either way (and `--write` refuses to touch the file until it's resolved).
- **Single surface (for now).** `ts.schema` is JS `.with`-chain source (e.g.
  `S.string.with(S.min, 3)`), executed directly. A future `res` (ReScript)
  surface sits alongside `ts` with its own shape.
- **Closed world.** Unknown keys are rejected; `_`-prefixed keys are the reserved
  harness namespace (`_skip`). Never edit `spec.schema.json` by hand (`pnpm spec schema` emits it).

## Format

```yaml
# yaml-language-server: $schema=./spec.schema.json
ts:                              # the JS `.with`-chain surface (executed)
  schema: S.string
  input: string                  # S.Input<schema>, as a type string — filled by check --write
  output: string                 # S.Output<schema>, as a type string — filled by check --write
  instantiations: 226            # type-instantiation count — filled by check --write
  bundleBytes: 3765               # tree-shaken, minified+gzipped bytes for S.parser(schema) — filled by check --write
jsonSchema: { input: {...}, output: {...} }   # filled by check --write
operations:
  parse:                        # parse=unknown→out, decode=in→out, encode=out→in
    expression: ""              # filled by check --write
    examples:
      valid:   { input: '"hi"' }          # you write input; check --write adds output/error
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
| `ts.output` / `ts.input` | vendored TS introspection (`checker.typeToString`); freshness verified by re-deriving and diffing in `spec check`/`spec_test.ts` — no generated `expectTypeOf` code needed |
| `ts.instantiations` | vendored TS introspection (`program.getInstantiationCount()`, diffed against a baseline) |
| `ts.bundleBytes` | vendored esbuild measurement: bundle+minify+gzip `S.parser(schema)`, tree-shaken against the dev source |

## How types/instantiations/bundle size are derived

- `ts.input`/`ts.output`/`ts.instantiations` — `packages/spec/introspect.ts`: a vendored
  `@typescript/vfs` environment (not `@ark/attest` — same underlying mechanism, without attest's slow
  whole-project assertion scan). Declares the schema, extracts `S.Output<>`/`S.Input<>`, reads
  `checker.typeToString()` and `program.getInstantiationCount()` diffed against a bare-import baseline.
- `ts.bundleBytes` — `packages/spec/bundleSize.ts`: bundles `S.parser(schema)` with esbuild against the
  dev source, minifies, gzips.

Both run on every `spec new`/`spec check --write` — no separate benchmark command.

## Layout

- `packages/sury/specs/<id>.yaml` — authored spec (published with the `sury` package)
- `packages/sury/specs/spec.schema.json` — emitted from the format schema (`pnpm spec schema`)
- `packages/spec/` — the spec CLI (its own workspace package). **Don't touch these files when working on Sury itself** — it's the test harness, not part of the library.
- `packages/sury/tests/spec_test.ts` — the single, committed, hand-written test that dynamically
  exercises every spec at run time (no per-spec generated files)
- `packages/sury/tests/spec_errors_test.ts` — snapshots `checkSpec`'s guiding error messages
  (the same function `spec check` calls) against deliberately-broken mutations of a real spec

## Improving the harness

The harness should be strict and guide the author. If, while writing a spec, you
hit something it *should* have caught or guided better — a missing check, a weak
error, a strictness gap that let a bad spec through — **don't just work around it.**
Add a bullet under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
