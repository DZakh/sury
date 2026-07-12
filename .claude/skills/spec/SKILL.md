---
name: spec
description: Develop Sury with the spec CLI. Use whenever changing Sury core logic (packages/sury/src) — specs snapshot codegen, bundle size, type-cost, and runtime-perf (instruction-count) metrics that every change must keep or improve — and when adding/editing packages/sury/specs/*.yaml.
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
Follow the CLI's error messages. Unassertable ops take `_skip: <reason>`; pass-through ops must be the bare literal `identity`.

Examples must cover every edge case found while investigating the schema — boundary values, IEEE-754 oddities (`-0`, `NaN`, `Infinity`), coercion corners, each generated-check branch. Findings from a bug report/review go into `examples`, not test files or commit messages.

## Aliases

`ts.aliases`: optional alternate `.with`-chain sources that must behave identically to `ts.schema` (e.g. a shorthand spelling of the same schema). `spec check` verifies matching `ts.input`/`ts.output`, `jsonSchema`, and operation codegen live — no separate goldens or examples.

## Specs are a metrics ratchet

Goldens snapshot key metrics: generated code, `ts.bundleBytes`, `ts.instantiations`, inferred types, and runtime perf (`ts.createPerf`, `operations.<op>.compilePerf`). After core-logic changes, run `pnpm spec check --write` and **review the golden diff as the deliverable** — every metric should improve or stay flat. A regression is a design smell; if unavoidable, call it out in the commit/PR.

## Perf goldens

`ts.createPerf` (build the schema once) and `operations.<op>.compilePerf` (compile that operation once) are **retired-instruction counts** measured under Valgrind — deterministic where wall-clock ops/sec isn't, so they can be a golden. Each is measured in its own short-lived worker (`packages/spec/perf.ts` → `native/perf-worker.mjs`), warm and fenced by a tiny Callgrind addon (`native/callgrind.c`, built on `pnpm install`). `spec check` runs the perf pass on **every** run; a full sweep of all specs is ~20s.

- **Requirements:** valgrind on `PATH` + the built addon. Where either is missing, `spec check` prints `perf: skipped (…)` and leaves perf goldens untouched — the rest of the check still gates.
- **Node-pinned:** counts are exact for one Node/V8 build (pinned in `package.json`'s `engines` and the CI perf job) but shift across Node versions — like `bundleBytes`, a Node bump re-baselines them. Run agent sessions on the pinned Node so local numbers match CI. A ±1% band absorbs toolchain wobble; a real codegen change re-records exactly.
- **`not-measured`:** a not-yet-baselined field. Read-only `spec check` doesn't gate it; `check --write` (with valgrind) upgrades it to a real number, which then gates. `spec new` scaffolds it — baseline with `check --write` on the pinned Node.

## Layout

- `packages/sury/specs/*.yaml` — specs; published as machine-checked documentation.
- `packages/spec/` — the spec CLI. Don't touch it for Sury-itself work; gaps go under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
