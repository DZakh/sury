---
name: spec
description: Develop Sury with the spec CLI. Use whenever changing Sury core logic (packages/sury/src) — specs snapshot codegen, bundle size, and type-cost metrics that every change must keep or improve — and when adding/editing packages/sury/specs/*.yaml.
---

# Sury specs

One `specs/<id>.yaml` = one schema's contract: type, JSON Schema, per-operation codegen + examples. You author the schema, `ts.aliases`, `vs.zod`, and example *inputs*. **Never hand-write a golden** — `pnpm spec` derives every one.

```bash
pnpm spec new --id <id> --ts "S.string.with(S.minLength, 3)"  # scaffold
pnpm spec check --write [id]   # (re)derive goldens
pnpm spec check [id]           # gate
```

Add a case: a named entry with just `input` under an op's `examples`, then `--write`. Follow the CLI's error messages — op shorthands (`identity`, `eq-to-parse`), `_skip` reasons, and the `vs.zod` divergence form all report the fix in the message.

**Examples are where findings live.** Cover every edge case the schema turns up — boundary values, IEEE-754 oddities (`-0`, `NaN`, `Infinity`), coercion corners, each generated-check branch. A bug report or review finding becomes an example, not a test file and not a commit message.

## Metrics ratchet

Goldens snapshot generated code, `ts.instantiations`, inferred types, and per-export bundle size (`bundleSize.yaml`). After core-logic changes run `pnpm spec check --write`: it prints every metric that moved, ranked — **that summary is the deliverable**. Each should improve or stay flat; call out an unavoidable regression in the commit/PR.

`check` also reports a performance delta against the library built from a git ref. Nothing is stored, and it never fails the run. `--perf=skip` for the tight loop, `--perf=only` to measure alone, `[id…]` to narrow. **Ignore anything at or below the printed noise floor** — that's what the run could fabricate from nothing.

## Scenarios

`specs/scenarios.yaml` measures a call the way a consumer writes it, so the dispatch *around* a compiled operation is inside the timing — invisible to every per-spec phase. Add one when a change targets that layer.

```yaml
is:
  prepare: |
    const schema = S.schema({ id: S.string })
    const data = { id: "u1" }
  run: S.inputValidator(schema, data)
```

`prepare` is optional, runs once per library version, and its bindings are in scope for `run`; only `run` is timed. No goldens, so no `--write` — but `check` executes each one, so a typo fails the gate. Ids share the `[id…]` namespace with specs.

## Layout

- `packages/sury/specs/*.yaml` — specs, plus `bundleSize.yaml` and `scenarios.yaml`; published as machine-checked documentation.
- `packages/spec/` — the CLI. Don't touch it for Sury-itself work; log gaps under **Spec Harness Suggestions** in `CONTRIBUTING.md`.
