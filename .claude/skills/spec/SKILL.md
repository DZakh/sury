---
name: spec
description: Develop Sury with the spec CLI. Use whenever changing Sury core logic (packages/sury/src) - specs snapshot codegen, bundle size, and type-cost metrics that every change must keep or improve - and when adding/editing packages/sury/specs/*.yaml.
---

# Sury specs

One `packages/sury/specs/<id>.yaml` = one schema's contract: its type, its JSON
Schema, and per operation its generated code and examples. You write the schema,
`ts.aliases`, `vs.zod` and example *inputs*. **Never hand-write a golden** -
`pnpm spec` derives every one.

```bash
pnpm spec new --id <id> --ts "S.string.with(S.minLength, 3)"   # scaffold
pnpm spec check --write [id…]   # (re)derive goldens, print what moved
pnpm spec check [id…]           # gate
pnpm spec schema                # regenerate the specs' JSON Schema
```

Add a case by writing a named entry with just `input` under an operation's
`examples`, then `--write`.

**Follow the CLI's messages.** Every rule the format has reports its own fix
where it is broken - the operation shorthands, the `_skip` reasons, what to do
when a verifier reads an example differently from `parse`. Do what the message
says rather than working around it; if the message itself is the problem, that
is a bullet under **Spec Harness Suggestions** in `CONTRIBUTING.md`.

**Examples are where findings live.** Cover the edges the schema turns up -
boundary values, IEEE-754 oddities (`-0`, `NaN`, `Infinity`), coercion corners,
each branch of the generated checks. A bug report or a review finding becomes an
example here, not a test file and not a commit message. When the example records
behaviour that is really a bug, say so in a `FIXME:` comment beside it.

## Metrics ratchet

Goldens snapshot generated code, `ts.instantiations`, inferred types and per
export bundle size (`bundleSize.yaml`). After a change under `packages/sury/src`
run `pnpm spec check --write`: it prints every metric that moved, ranked -
**that summary is the deliverable**. Each should improve or stay flat; call out
an unavoidable regression in the commit and the PR.

`check` also reports a performance delta against the library built from a git
ref. Nothing is stored and it never fails the run. `--perf=skip` for the tight
loop, `--perf=only` to measure alone, `[id…]` to narrow. **Ignore anything at or
below the printed noise floor** - that is what the run could fabricate from
nothing.

## Scenarios

`packages/sury/specs/scenarios.yaml` times a call the way a consumer writes it,
so the work around a compiled operation is measured too. Add one when a change
targets that layer.

```yaml
is:
  prepare: |
    const schema = S.schema({ id: S.string })
    const data = { id: "u1" }
  run: S.isInput(schema)(data)
```

`prepare` is optional, runs once per library version, and its bindings are in
scope for `run`; only `run` is timed. No goldens, so no `--write` - but `check`
runs each one, so a typo fails the gate. Ids share the `[id…]` namespace with
specs.

## Layout

- `packages/sury/specs/*.yaml` - the specs, plus `bundleSize.yaml`,
  `scenarios.yaml` and the generated `spec.schema.json` your editor reads for
  hover and completion. They ship as machine-checked documentation.
- `packages/spec/` - the CLI itself. Leave it alone while working on Sury.
