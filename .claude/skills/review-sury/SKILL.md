---
name: review-sury
description: Review a Sury branch, PR or diff against this repo's goals - is it the right long-term architecture, is anything hacky, what are the alternatives - plus layering, tree-shaking, spec coverage, metrics and prose rules. Use for "review <branch/PR>", "is this the right solution", "reflect on this change".
---

# Review a Sury change

Get the diff (`git diff origin/main...HEAD`, or the PR's diff) and the PR body.
Read the changed files in full, not only the hunks. Judge in CLAUDE.md's goal
order: DX, not hacky, positive flow, negative flow, creation, bundle size.

## Architecture first

- **Is it the right solution long term?** Name the question the change answers
  and where in the core that answer is supposed to come from. If it answers it
  somewhere else, that is the finding, whatever the numbers say.
- **Alternatives.** Name at least one concrete alternative design. Where it is
  cheap, prototype it and measure (`pnpm spec check --write` prints bundle,
  generated code and perf deltas) rather than arguing.
- **What can be deleted?** New concepts, fields, helpers or flags that an
  existing one covers.

## Then the rules

- Layering: imports only point down the chain in CLAUDE.md; `base.ts` imports
  nothing; operations don't reach `standard.ts`.
- Tree-shaking: `// @__NO_SIDE_EFFECTS__` on every new pure public factory and on
  no operation; no factory published through an alias.
- Schema fields spelled out; bit flags as literals; `B_`-prefixed flat helpers.
- Every schema reversible unless opted out.
- Every behaviour change and every bug found has a spec example. Goldens are
  derived, never hand-edited. Stale `FIXME:`s removed.
- Bundle movement also regenerated `packages/benchmarks/goldens/`.
- Fuzzers for the touched area were run (`fuzz` skill), and new findings are
  fixed or listed in `knownBugs.ts` with a spec.
- Comments and prose: no restating comments, no em dash, examples over
  paragraphs, `docs/` written from the user's side.

## Output

Findings ranked most severe first. For each: file:line, what goes wrong with a
concrete input (ideally the spec example that would show it), and the fix. Keep
architecture findings separate from nits. End with a verdict: ship, ship after
fixes, or rethink, and why.

When asked to apply the review, fix every finding through the `fix` and `ship`
skills rather than patching in place.
