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
pnpm spec check [id]           # gate
```

Add a case: a named entry with just `input` under an op's `examples`, then `check --write`. Follow the CLI's error messages — the op shorthands (`identity`, `eq-to-parse`) and `_skip` reasons are enforced with the fix in the message.

Examples must cover every edge case found while investigating the schema — boundary values, IEEE-754 oddities (`-0`, `NaN`, `Infinity`), coercion corners, each generated-check branch. Findings from a bug report/review go into `examples`, not test files or commit messages.

## Aliases

`ts.aliases`: optional alternate `.with`-chain sources that must behave identically to `ts.schema` (e.g. a shorthand spelling of the same schema) — checked live, with no goldens or examples of their own.

## Cross-library (`vs`)

`vs.zod`: required Zod v4 equivalent, e.g. `vs: { zod: z.string().min(3) }`, asserted against `ts.input`/`ts.output` — types only, live, no golden. Use the `{ schema, divergence, input?, output? }` form when Zod's inferred type intentionally differs (e.g. `S.merge` keeps insertion order where Zod groups optionals last): `divergence` is a hand-written note on what differs and why, and only the diverging side is recorded. `{ _skip: <reason> }` when Zod can't express the schema.

## Specs are a metrics ratchet

Goldens snapshot generated code, `ts.instantiations`, inferred types, and per-export bundle size (`specs/bundleSize.yaml`). After core-logic changes run `pnpm spec check --write`: it prints every metric that moved, ranked by percentage — **that summary is the deliverable**. Each should improve or stay flat; a regression is a design smell, so call it out in the commit/PR when it's unavoidable.

`check` also reports a relative performance delta (schema creation, creation+compilation, every example) against the library built from a git ref. Nothing is stored and it never fails the run. Use `--perf=skip` for the tight loop, `--perf=only` to measure alone, `[id…]` to narrow. **Ignore anything at or below the printed noise floor** — that's what the run could fabricate from nothing.

## Layout

- `packages/sury/specs/*.yaml` — specs, plus `bundleSize.yaml`; published as machine-checked documentation.
- `packages/spec/` — the spec CLI. Don't touch it for Sury-itself work; gaps go under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
