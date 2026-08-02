# Contributing to Sury

Rules only. How the compiler *currently works* belongs in a comment next to the
code it constrains — this file can't be kept honest against a refactor.

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `bundleSize.yaml` measures what ships.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime
ships per-schema, library ships once).

## Use the spec skill

Every change under `packages/sury/src` goes through it. Specs snapshot generated
code, bundle size and type-cost; the printed metric summary is the deliverable.
Never hand-write a golden.

Findings from a bug report or review go into a spec's `examples`, never into a
test file or a commit message.

## Layering

```
base → builder → primitives → parse → union → composites → factory
     → modifiers → refinements → operations → advanced/* → jsapi → jsonschema
```

- Only type-only imports may point "up"; `operations → jsonschema` is the one
  real exception.
- `base.ts` takes **no** outgoing imports. A constant two modules recognise by
  name lives there rather than with its schema.
- `src/advanced/` is one file per schema nothing else builds on; a schema other
  modules build on stays in the core.
- `src/entry.ts` is the single public entry. Add a `$res_*` export *only* for an
  API with no public-JS equivalent; where ReScript differs only in argument
  shape, bind the public export in `S.res` and adapt there.
- `S.res` is the only ReScript module, and reaches the runtime through the
  package's own `"."` export so both languages share one instance.

## Writing code

- Keep helpers flat and `B_`-prefixed so each shakes individually.
- Prefer `const f = () => {}` over `function` — measurably smaller minified.
- Inline intrinsics (`a | b`, `typeof x`) rather than wrapping them in helpers.
- Runtime field names on hot objects stay short: property names survive
  minification, so every character ships.
- Write bit-flag literals, not named `const`s — esbuild won't inline them, so the
  name costs bytes at every use. Document the values in a comment.
- Every schema must be reversible (Input ↔ Output) unless explicitly opted out.
- Name anything esbuild emits `index.*`. `S.*` belongs to the ReScript compiler,
  which overwrites whatever sits where its output lands.

## Comments

- Default: no comment.
- Write one only for a non-obvious *why* — a hidden constraint, a subtle
  invariant, a bug workaround, behavior that would surprise a reader.
- Never restate the code. Delete existing comments that fail this test, even in
  code you're only editing.
- An invariant that binds *another* module goes on the definition both sides
  reach, so the person about to break it is looking at it.
- Repo-wide, not just `packages/spec`.

## Tree-shaking

- Every public pure factory carries `// @__NO_SIDE_EFFECTS__` on the line above
  its declaration — except exports whose point *is* the effect (`assert`, `is`,
  `safe`, `safeAsync`, `global`, `enableStandardJSONSchema`,
  `$res_assertAsyncOrThrow`, `$res_setExnId`).
- **Never publish a factory through an alias** (`export const object = schemaObject`):
  the annotation counts only on the declaration that *is* the function. Re-export
  instead — `export { schemaObject as object } from "./factory"`.
- `schema.with(S.meta, …)` is a method call on an opaque receiver and can never
  be dropped; the functional `S.meta(schema, …)` is equivalent and does shake.
- `package.json`'s `sideEffects` is a list, not `false`: the ReScript entries
  carry a top-level call registering the exception identity, and a blanket
  `false` drops it while keeping the bindings, after which
  `try { … } catch { S.Raised }` stops matching.

`tests/treeShaking_test.ts` guards the first two; `bundleSize.yaml` can't.

## Changing the union compiler

Which member a value dispatches to is invisible in a golden until someone writes
the spec for exactly that permutation, so before *and* after any change to
`src/union.ts`:

```bash
pnpm --filter=sury fuzz:union --ref=HEAD   # --seed=N to widen the search
```

It sorts differences into `acceptance` / `exception-kind` (a behavior change —
exits non-zero) and `reasons` / `message` (error detail, for you to accept or
reject).

`CODEC_SPEC.md` is the normative statement of what conversions are legal.
