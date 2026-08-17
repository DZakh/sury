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

**Every issue found — bug report, review finding, or one you hit yourself —
lands as a spec that reproduces it, and stays as the regression test.** No spec,
not fixed. Add `examples` to the spec that covers the schema, or a new
`specs/<id>.yaml` when none does. A test file is for what the format genuinely
can't express (a packaging or tsconfig-level failure); say so in the commit.
Never a commit message alone.

`spec check` decides what a spec may carry, comments included — it is the rule,
so there isn't one here. The one thing it can't see is a `FIXME:` that has
stopped being true; delete those yourself.

## Layering

```
base → builder → primitives → parse → union → composites → factory
     → modifiers → refinements → operations → advanced/* → jsonschema → entry
```

- Only type-only imports may point "up"; `operations → jsonschema` is the one
  real exception.
- `base.ts` takes **no** outgoing imports. A constant two modules recognise by
  name lives there rather than with its schema.
- `src/advanced/` is one file per schema nothing else builds on; a schema other
  modules build on stays in the core.
- `src/entry.ts` is the single public entry, and the only module allowed to both
  re-export and declare: a public name that exists purely to adapt a core
  primitive to its documented argument shape is declared there, since nothing
  else may import it. Anything a second module needs belongs in the core.
- Add a `$`-prefixed export *only* for an API with no public-JS equivalent;
  where ReScript differs only in argument shape, bind the public export in
  `S.res` and adapt there.
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
- Repo-wide, not just `packages/sury`.
- The files in `artifact_test.ts`'s `FILES` ship — they land in a consumer's
  `node_modules` and editor hover. Comments there answer what the API does; a
  rule for whoever maintains it goes where only we read it — this file, or the
  test that enforces it.

## JSON Schema types

The dialect interfaces in `src/types/jsonschema.d.ts` are duplicated on purpose:
they mirror frozen specs, and a flat interface is what makes a hover, completion
and error name the dialect instead of expanding an intersection. Don't collapse
them into `extends`, `Omit` or mapped types.

`src/types/json.d.ts` holds `JSON` and the `FromJSONSchema` inference engine. Its
`Flatten` duplicates `index.d.ts`'s on purpose — a non-exported type can't cross
a file, and exporting one would add `S.Flatten` to the public API. The engine's
dispatch order mirrors the runtime chain in `src/jsonschema.ts` and they move
together.

Each must stay assignable to the wide `JSONSchema` — that is what lets a
`toJSONSchema` result feed `fromJSONSchema` or `extendJSONSchema` uncast — so a
keyword added to one belongs on `JSONSchema` too, and in the other two spellings
of the keyword set (`JSONSchemaT` in `src/jsonschema.ts`, `JSONSchema.res`).

## Tree-shaking

- Every public pure factory carries `// @__NO_SIDE_EFFECTS__` on the line above
  its declaration — except exports whose point *is* the effect (`assert`, `is`,
  `safe`, `safeAsync`, `global`, `enableStandardJSONSchema`,
  `$assertAsyncOrThrow`, `$setExnId`).
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

`CODEC_SPEC.md` is the normative statement of what conversions are legal,
built-in and custom alike.
