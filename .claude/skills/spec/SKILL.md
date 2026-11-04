---
name: spec
description: The Sury spec CLI. Use for any change under packages/sury/src and for adding or editing packages/sury/specs/*.yaml - specs snapshot generated code, bundle size and type cost.
---

# Specs

`packages/sury/specs/<id>.yaml` is one schema's contract: type, JSON Schema, and
per operation its generated code and examples. You write the schema,
`ts.aliases`, `vs.zod` and example inputs (a named entry with just `input` under
an operation's `examples`). `--write` derives everything else.

```bash
pnpm spec new --id <id> --ts "S.string.with(S.minLength, 3)"
pnpm spec check --write [id…]   # derive goldens, print what moved
pnpm spec check [id…]           # the gate
pnpm spec schema                # regenerate the specs' JSON Schemas
```

- **Follow the CLI's messages.** Each format rule reports its own fix. If a
  message is the problem, add a bullet under Spec Harness Suggestions in
  `CONTRIBUTING.md`.
- **Examples are where findings live**: boundary values, `-0`, `NaN`,
  `Infinity`, coercion corners, every branch of the generated checks. A wrong
  answer recorded on purpose gets a `FIXME:` beside it.
- **The metric summary `--write` prints is the deliverable.** Each metric
  improves or stays flat, or the commit and PR name the regression. Perf compares
  against a git ref and never fails: ignore anything at or below the printed
  noise floor. `--perf=skip` for the tight loop, `--perf=only` to measure.
- `specs/scenarios.yaml` times a call as a consumer writes it (`prepare` runs
  once, `run` is timed). Add one when a change targets the work around a
  compiled operation. No goldens; `check` runs each one.
- Leave `packages/spec/` (the CLI) alone while working on Sury.
