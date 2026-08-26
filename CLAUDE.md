# Contributing to Sury

Rules only. How the compiler *currently works* belongs in a comment next to the
code it constrains — this file can't be kept honest against a refactor.

Process (playbooks, TDD, review, comments, spawn) is pstack. Verification is
the spec skill. Hot-path TypeScript unsafety is pstack typescript-best-practices.

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `bundleSize.yaml` measures what ships.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime
ships per-schema, library ships once).

## Spec

Every change under `packages/sury/src` goes through the spec skill. Never
hand-write a golden. `spec check` decides what a spec may carry. Delete a
`FIXME:` that has stopped being true.

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
- Every schema must be reversible (Input ↔ Output) unless explicitly opted out.
- Name anything esbuild emits `index.*`. `S.*` belongs to the ReScript compiler,
  which overwrites whatever sits where its output lands.

Comments: default none; keep only a non-obvious why. Repo-wide, including files
you only edit.

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
the spec for exactly that permutation. `pnpm --filter=sury fuzz:union` compares
the compiler to a sequential try of each variant's own parser/encoder (grouping
is codegen, not semantics). It exits non-zero on `acceptance` /
`exception-kind`; `reasons` / `message` are error detail. `--ref` is an optional
changelog against a git commit, not the gate. `--seed=N` widens the search.

`CODEC_SPEC.md` is the normative statement of what conversions are legal,
built-in and custom alike.
