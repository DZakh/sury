---
name: spec
description: Add or edit a Sury test spec — packages/sury/specs/*.yaml, one declarative file capturing a schema's full contract (type, JSON Schema, and per-operation codegen + input→output|error examples). Use when writing/updating a spec, adding an example, or touching the spec harness in packages/spec.
---

# Sury specs

One `specs/<id>.yaml` = one schema's full contract. **You author the schema and example _inputs_; the harness computes every golden.** Never hand-write a golden.

Run commands from the repo root (`pnpm spec …`).

## Workflow

```
pnpm spec new --id <id> --ts <schema>   # scaffold — everything is auto-derived from --ts
# edit specs/<id>.yaml: add example inputs under each op's `examples`
pnpm spec update [id]                    # re-derive everything from the live schema (one unified step)
pnpm spec check  [id]                    # gate: format-valid, canonical, skips well-formed, goldens fresh
```

`new` requires both `--id` and `--ts` (e.g. `pnpm spec new --id string.min --ts "S.string.with(S.min, 3)"`).
A single `pnpm spec new`/`pnpm spec update` derives **everything** the harness knows how to derive:
`jsonSchema`, `operations` (identity ops collapse to the bare literal `identity` automatically),
`ts.input`/`ts.output`/`ts.instantiations` (vendored TypeScript introspection,
`packages/spec/introspect.ts`) and `ts.bundleBytes` (vendored esbuild measurement,
`packages/spec/bundleSize.ts`) — see "How types/instantiations/bundle size are derived" below. Only
example inputs are left to fill in by hand. `[id]` is optional for `update`/`check`/`fmt` — omit it to
process every spec.
There is no code-generation step. `pnpm test` runs `packages/sury/tests/spec_test.ts`, a single
committed, hand-written Vitest file that dynamically loops over every spec at run time and calls
straight into the harness — so example execution and every dimension's freshness are exercised, and
covered, by a real Vitest run without ever materializing a per-spec test file.
To add a case: add a named entry under an op's `examples` with just `input`, then `pnpm spec update`.

## Rules (these are enforced)

- **Never type a golden by hand.** Every field except `ts.schema` and example `input`s is written by
  `pnpm spec update`/`spec new` from the live schema — including `ts.input`/`ts.output`/
  `ts.instantiations`/`ts.bundleBytes`, not just `expression`/`jsonSchema`/example results.
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
  harness namespace (`_skip`). Never edit `spec.schema.json` by hand (`pnpm spec schema` emits it).

## Format

```yaml
# yaml-language-server: $schema=./spec.schema.json
ts:                              # the JS `.with`-chain surface (executed)
  schema: S.string
  input: string                  # S.Input<schema>, as a type string — filled by update
  output: string                 # S.Output<schema>, as a type string — filled by update
  instantiations: 226            # type-instantiation count — filled by update
  bundleBytes: 3765               # tree-shaken, minified+gzipped bytes for S.parser(schema) — filled by update
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
| `ts.output` / `ts.input` | vendored TS introspection (`checker.typeToString`); freshness verified by re-deriving and diffing in `spec check`/`spec_test.ts` — no generated `expectTypeOf` code needed |
| `ts.instantiations` | vendored TS introspection (`program.getInstantiationCount()`, diffed against a baseline) |
| `ts.bundleBytes` | vendored esbuild measurement: bundle+minify+gzip `S.parser(schema)`, tree-shaken against the dev source |

## How types/instantiations/bundle size are derived

`ts.input`/`ts.output`/`ts.instantiations` are computed by `packages/spec/introspect.ts`, a small
vendored TypeScript introspection — **not** `@ark/attest`. It uses `@typescript/vfs` (the tech behind
the TS Playground) to spin up one isolated virtual environment, memoized for the process, then for
each schema: declares it plus `type __Output = S.Output<typeof __schema>`/`__Input` in a virtual file,
runs `program.getSemanticDiagnostics()` to force checking, reads `checker.typeToString()` for the type
strings, and reads the real `program.getInstantiationCount()` (diffed against a bare-import baseline)
for the instantiation count. `@ark/attest` uses this exact same mechanism internally for its own
`instantiations` benchmarks (`tests/types.bench.ts`, still a separate project-health benchmark unrelated
to specs) — what makes attest itself slow (~15s) is a separate, unrelated whole-project scan (`setup()`'s
`analyzeProjectAssertions()`) for pre-written, hardcoded-expected-value assertions, which this harness has
no use for. Vendoring just the isolated-environment logic measures ~1s cold, ~50-200ms warm per additional
schema in the same process.

`ts.bundleBytes` is computed by `packages/spec/bundleSize.ts`, which bundles a tiny `S.parser(schema)`
entry with esbuild (aliasing the bare `sury` specifier to the dev source), minifies, and gzips — the
same technique the project's own former `tests/bundle.bench.ts` project-health benchmark used (a
handful of fixed scenarios against a committed snapshot with its own CI gate; removed in favor of this
per-spec derivation). Unlike the TS-introspection environment,
each `deriveBundleBytes` call is an independent esbuild child-process build with no shared state, so
concurrent specs' bundle measurements run genuinely in parallel via `Promise.all`. `recomputeGoldens`
kicks off the bundle-size build *before* the synchronous TS work so the two overlap within a single
spec too — both are fast enough (low single-digit seconds even cold) to run on every `spec
update`/`spec new`, no separate command needed.

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
