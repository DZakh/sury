---
name: spec
description: Develop Sury with the spec CLI. Use whenever changing Sury core logic (packages/sury/src) — specs snapshot codegen, bundle size, and type-cost metrics that every change must keep or improve — and when adding/editing packages/sury/specs/*.yaml.
---

# Sury specs

One `specs/<id>.yaml` = one schema's full contract: type, JSON Schema, and per-operation codegen + examples. You author only the schema and example *inputs*; `pnpm spec` (from repo root) derives every golden — **never hand-write one**.

## Workflow

```bash
pnpm spec new --id <id> --ts "S.string.with(S.min, 3)"  # scaffold
# edit specs/<id>.yaml: add example inputs under each op's `examples`
pnpm spec check --write [id]   # (re)derive goldens from the live schema
pnpm spec check [id]           # gate; omit [id] for all specs
```

To add a case: add a named entry with just `input` under an op's `examples`, then `check --write`.
The CLI strictly enforces the format — follow its error messages. Ops you can't assert take `_skip: <reason>`; ops that compile to a pass-through must be the bare literal `identity`, and a decode/encode that compiles to exactly the same code as `parse` must be the bare literal `validated` (its expression and examples live on `parse`).

Examples must include every edge case you discover while working on or investigating the schema — boundary values, IEEE-754 oddities (`-0`, `NaN`, `Infinity`), coercion corner cases, values that exercise each branch of the generated check. If an edge case surfaced in a bug report, review, or investigation, it goes into `examples` before the work is done — the spec is where such findings are pinned, not test files or commit messages.

## Specs are a metrics ratchet

Goldens snapshot the library's key metrics per schema: generated code (`expression`), `ts.bundleBytes`, `ts.instantiations`, inferred types. When changing core logic, run `pnpm spec check --write` and **review the golden diff as the deliverable**: every metric should improve or stay flat. A regression is a design smell — rework the change; if genuinely impossible, call it out explicitly in the commit/PR.

## Layout

- `packages/sury/specs/*.yaml` — specs; published with the package as machine-checked documentation.
- `packages/spec/` — the spec CLI. Don't touch it when working on Sury itself; if it should have caught or guided something better, add a bullet under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
