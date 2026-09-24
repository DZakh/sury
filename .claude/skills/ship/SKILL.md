---
name: ship
description: Take a Sury change from idea to a production-ready PR - build it to the end, verify, iterate on simplifications and measured alternatives, clean up, sync main, run the local CI gate, and write the PR body. Use for any implementation task in this repo, and whenever the ask is "build, verify, reflect, improve, clean up, make it prod ready".
---

# Ship a change

The standing goal for every change here is the same: **build it all, verify
each step, then iterate reviewing, measuring and simplifying until it is
production ready.** Do not stop at a plan, a first working version, or a
passing test. Stop when another round finds nothing worth changing.

## 1. Build to the end

Implement the whole thing, including docs (`docs/`, READMEs), `S.res` bindings,
`index.d.ts` types and the specs. Code under `packages/sury/src` goes through the
`spec` skill: `pnpm spec check --write` after every meaningful step, and read
the metric summary it prints.

## 2. Verify

- `pnpm spec check` for the touched specs, then all of them.
- The fuzzers for the area you touched (`fuzz` skill).
- `pnpm --filter=sury test` when anything under `src/` or `S.res` moved.
- Every bug you hit on the way lands as a spec example before you fix it.

## 3. Reflect and iterate (at least two rounds)

Each round, write down for yourself:

- **Is any of it hacky?** (CLAUDE.md goal 2.) A special case, a flag threaded
  through to fix one caller, a parallel walk that rebuilds something the core
  already knows, state smuggled through a schema. Replace it with the concept
  the core already has, even when that means a bigger rewrite.
- **What can go?** Dead branches, helpers with one caller, fields nobody reads,
  a second way to express the same thing.
- **What else could it be?** Name at least one real alternative and measure it
  (bundle, generated code, perf above the noise floor). Keep the numbers, the
  PR reports them, including the ones you rejected.
- **Is the positive flow as tight as it can be?** Extra vars, allocations,
  double validation, indirection in the generated code.

Implement what wins, re-verify, repeat.

## 4. Clean up

- Comments: delete any that restate code; keep only a non-obvious why.
- Prose everywhere (docs, comments, errors, commit, PR): no em dash, an example
  over a paragraph, what the user does rather than what the library does inside.
- Delete every `FIXME:` that has stopped being true.
- `pnpm lint:deadcode`.
- Squash-worthy history is fine; say "merge with squash" in the PR when
  intermediate commits undo each other.

## 5. Sync with main

`git fetch origin main && git merge origin/main`. Conflicts are almost always in
generated files. Never resolve those by hand, regenerate them on the merged tree:

| file | regenerate with |
|---|---|
| spec goldens, `specs/bundleSize.yaml` | `pnpm spec check --write` |
| `packages/benchmarks/goldens/`, `docs/benchmarks/` | `pnpm benchmarks --write` |
| `*.res.mjs` | `pnpm --filter=sury build` |
| `specs/spec.schema.json` | `pnpm spec schema` |

Anything that moves bundle size needs `pnpm benchmarks --write` too, or CI fails
on the benchmark goldens with a green `spec check` behind it.

## 6. Local CI gate

Mirror `.github/workflows/ci.yml`; if it changed, follow it rather than this
list. From the repo root:

```bash
pnpm lint:deadcode
(cd packages/sury && pnpm build && ../../scripts/assert-no-drift.sh '*.res.mjs' \
  && pnpm typecheck && pnpm coverage)
(cd packages/sury && pnpm fuzz:escfree && pnpm fuzz:union --seed=1 && pnpm fuzz:schema \
  && pnpm fuzz:formdata && pnpm fuzz:content)
(cd packages/sury/artifacts && npx --yes jsr@0.14.3 publish --dry-run --allow-dirty)
pnpm spec check
pnpm benchmarks
pnpm compliance
pnpm protobuf:compliance && pnpm protobuf:conformance
(cd packages/e2e && pnpm rescript && pnpm test)   # needs the ppx the session hook builds
```

Report anything you could not run and why. Never skip a failing step.

## 7. PR

Open one only when the task asks for it. One PR per root cause. Title: a plain
sentence of what a user gets ("Give every engine a stack, not just V8"), no
`feat:` prefix. Body, dropping sections that do not apply:

```md
Fixes #N. / Follow-up to #N. (one line on the problem)

## What changes
Before/after example of what a user writes and gets. A table when there are
several cases.

## Release notes
- **Fixed:** / **Behaviour change:** / **Faster:** one line each. Bold the breaking ones.

## Cause
(fixes only) The one root cause, and where the answer should have come from.

## Alternatives measured
| alternative | result | kept? |

## Measurements
Bundle total gz before -> after, notable exports. Generated code deltas.
Perf rows above the noise floor, with the floor.

## Tests
Which specs/examples pin it, and that each fails without the change.

## Verification
What ran locally from the gate, and what could not run here.
```
