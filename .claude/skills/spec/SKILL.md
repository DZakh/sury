---
name: spec
description: Develop Sury with the spec CLI. Use whenever changing Sury core logic (packages/sury/src) — specs snapshot codegen, bundle size, and type-cost metrics that every change must keep or improve — and when adding/editing packages/sury/specs/*.yaml.
---

# Sury specs

One `specs/<id>.yaml` = one schema's contract: type, JSON Schema, per-operation codegen + examples. You author the schema, any aliases, and example *inputs*; `pnpm spec` derives every golden — **never hand-write one**.

## Workflow

```bash
pnpm spec new --id <id> --ts "S.string.with(S.min, 3)"  # scaffold
# edit specs/<id>.yaml: add example inputs under each op's `examples`
pnpm spec check --write [id]   # (re)derive goldens
pnpm spec check [id]           # gate; omit [id] for all specs
```

Add a case: named entry with just `input` under an op's `examples`, then `check --write`.
Follow the CLI's error messages. Unassertable ops take `_skip: <reason>`; pass-through ops must be the bare literal `identity`; a decode/encode that compiles to the same code as `parse` must be the bare literal `eq-to-parse` (its expression and examples live on `parse`).

Examples must cover every edge case found while investigating the schema — boundary values, IEEE-754 oddities (`-0`, `NaN`, `Infinity`), coercion corners, each generated-check branch. Findings from a bug report/review go into `examples`, not test files or commit messages.

## Aliases

`ts.aliases`: optional alternate `.with`-chain sources that must behave identically to `ts.schema` (e.g. a shorthand spelling of the same schema). `spec check` verifies matching `ts.input`/`ts.output`, `jsonSchema`, and operation codegen live — no separate goldens or examples.

## Cross-library (`vs`)

`vs.zod`: required Zod v4 equivalent, e.g. `vs: { zod: z.string().min(3) }`. `spec check` asserts its inferred (`~standard`) input/output types equal `ts.input`/`ts.output` — live, no golden, types only.

Three forms:
- **Bare string** — `zod: z.string().min(3)`. Zod's inferred types must equal `ts.input`/`ts.output`.
- **Overwrite** — `zod: { schema, divergence, input?, output? }`. For a schema Zod *can* express but whose inferred type intentionally differs from Sury's on at least one side (e.g. `S.merge` keeps insertion order where Zod groups optionals last). `divergence` is a required hand-written note saying exactly what differs and why. `input`/`output` are checked independently: record the side that diverges (write the key — `spec check --write` fills it as a golden), **omit** the side that matches ts. `check` rejects a recorded side that actually matches (omit it) and an omitted side that actually diverges (record it); omitting both means no divergence — use the bare string form.
- **Skip** — `zod: { _skip: <reason> }`. Only when Zod can't express the schema at all.

## Specs are a metrics ratchet

Goldens snapshot key metrics: generated code, `ts.bundleBytes`, `ts.instantiations`, inferred types. After core-logic changes, run `pnpm spec check --write` and **review the golden diff as the deliverable** — every metric should improve or stay flat. A regression is a design smell; if unavoidable, call it out in the commit/PR.

## Layout

- `packages/sury/specs/*.yaml` — specs; published as machine-checked documentation.
- `packages/spec/` — the spec CLI. Don't touch it for Sury-itself work; gaps go under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
