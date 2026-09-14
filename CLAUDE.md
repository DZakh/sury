# Contributing to Sury

Rules only. How the compiler *currently works* belongs in a comment next to the
code it constrains - this file can't be kept honest against a refactor.

## Goals (priority order on conflict)

1. **DX** - intuitive public API and error messages.
2. **Performance** - generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** - `bundleSize.yaml` measures what ships.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime
ships per-schema, library ships once).

## Use the spec skill

Every change under `packages/sury/src` goes through it. Specs snapshot generated
code, bundle size and type-cost; the printed metric summary is the deliverable.
Never hand-write a golden.

**Every issue found - bug report, review finding, or one you hit yourself -
lands as a spec that reproduces it, and stays as the regression test.** No spec,
not fixed. Add `examples` to the spec that covers the schema, or a new
`specs/<id>.yaml` when none does. A test file is for what the format genuinely
can't express (a packaging or tsconfig-level failure); say so in the commit.
Never a commit message alone.

`spec check` decides what a spec may carry, comments included - it is the rule,
so there isn't one here. The one thing it can't see is a `FIXME:` that has
stopped being true; delete those yourself.

## Layering

```
base → builder → primitives → parse → union → composites → factory
     → modifiers → refinements → eq → operations → standard → advanced/* → jsonschema → entry
```

- Only type-only imports may point "up".
- `operations.ts` holds the operation surface and must stay free of top-level
  side effects; `standard.ts` holds the schema-prototype interop getters
  (`toString`, `~standard`), which ARE top-level side effects. A bundle that
  reaches a module carries its top-level statements, so an operation must not
  reach the Standard Schema machinery - that is the whole reason they are two
  modules.
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

- Schema fields (`Internal`) are spelled out. Consumers `console.log` schemas, and short names (`pr`, `ir`, `rf`, `bd`) are what they see. Never shorten a field on a schema.
- Val and other compile-only objects never leave the compiler; those names may stay short.
- Keep helpers flat and `B_`-prefixed so each shakes individually.
- Prefer `const f = () => {}` over `function` - measurably smaller minified.
- Inline intrinsics (`a | b`, `typeof x`) rather than wrapping them in helpers.
- Write bit-flag literals, not named `const`s - esbuild won't inline them, so the name costs bytes at every use. Document the values in a comment.
- Every schema must be reversible (Input ↔ Output) unless explicitly opted out.
- Name anything esbuild emits `index.*`. `S.*` belongs to the ReScript compiler, which overwrites whatever sits where its output lands.

## Comments

- Default: no comment.
- Write one only for a non-obvious *why* - a hidden constraint, a subtle
  invariant, a bug workaround, behavior that would surprise a reader.
- Never restate the code. Delete existing comments that fail this test, even in
  code you're only editing.
- An invariant that binds *another* module goes on the definition both sides
  reach, so the person about to break it is looking at it.
- Repo-wide, not just `packages/sury`.
- The files in `artifact_test.ts`'s `FILES` ship - they land in a consumer's
  `node_modules` and editor hover. Comments there answer what the API does; a
  rule for whoever maintains it goes where only we read it - this file, or the
  test that enforces it.

## Prose

Everything written down: `docs/`, the READMEs, comments, commit messages, PR
bodies, error messages.

- No em dash. A hyphen, a comma or a full stop says it.
- An example over a paragraph. Show the schema and what it reads back; write a
  sentence only for what the example can't show.
- Write what a user does, not what the library does inside. A type they never
  name, a var the compiler allocates, the shape of a hook - none of that belongs
  in `docs/`. The exception is a rule that changes what they write: why a target
  is rejected, what a blank entry means.
- The same three apply to a comment, minus the last: a comment is where the
  implementation detail goes.

## JSON Schema types

The dialect interfaces in `src/types/jsonschema.d.ts` are duplicated on purpose:
they mirror frozen specs, and a flat interface is what makes a hover, completion
and error name the dialect instead of expanding an intersection. Don't collapse
them into `extends`, `Omit` or mapped types.

`src/types/json.d.ts` holds `JSON` and the `FromJSONSchema` inference engine. Its
`Flatten` duplicates `index.d.ts`'s on purpose - a non-exported type can't cross
a file, and exporting one would add `S.Flatten` to the public API. The engine's
dispatch order mirrors the runtime chain in `src/jsonschema.ts` and they move
together.

Each must stay assignable to the wide `JSONSchema` - that is what lets a
`toJSONSchema` result feed `fromJSONSchemaOrThrow` or `extendJSONSchema` uncast - so a
keyword added to one belongs on `JSONSchema` too, and in the other two spellings
of the keyword set (`JSONSchemaT` in `src/jsonschema.ts`, `JSONSchema.res`).

## Tree-shaking

- Every public pure factory carries `// @__NO_SIDE_EFFECTS__` on the line above
  its declaration - except exports whose point *is* the effect: every operation
  (`parse*`, `decode*`, `encode*`, `make*`, `is*`, `assert*`, the `$`-prefixed
  ReScript ones - their immediate call forms validate, and an annotated call
  whose result is discarded gets dropped), `global`, `enableStandardJSONSchema`,
  `$setExnId`. `tests/treeShaking_test.ts`'s `EFFECTFUL` is the list. No
  operation is annotated, whatever an individual one could prove about itself.
- **Never publish a factory through an alias** (`export const object = schemaObject`):
  the annotation counts only on the declaration that *is* the function. Re-export
  instead - `export { schemaObject as object } from "./factory"`.
- `schema.with(S.meta, ...)` is a method call on an opaque receiver and can never
  be dropped; the functional `S.meta(schema, ...)` is equivalent and does shake.
- `package.json`'s `sideEffects` is a list, not `false`: the ReScript entries
  carry a top-level call registering the exception identity, and a blanket
  `false` drops it while keeping the bindings, after which
  `try { ... } catch { S.Raised }` stops matching.

`tests/treeShaking_test.ts` guards the first two; `bundleSize.yaml` can't.

## Changing the union compiler

Which member a value dispatches to is invisible in a golden until someone writes
the spec for exactly that permutation. `pnpm --filter=sury fuzz:union` compares
the compiler to a sequential try of each variant's own parser/encoder (grouping
is codegen, not semantics). It exits non-zero on `acceptance` /
`exception-kind`; `reasons` / `message` are error detail. `--ref` is an optional
changelog against a git commit, not the gate. `--seed=N` widens the search.

## Changing the equality compiler

A spec pins the comparator's code and its answers for the values that spec
writes down, and says nothing about the branch no spec reaches.
`pnpm --filter=sury fuzz:eq` samples values out of generated schemas and holds
the answers to the properties an equivalence has whatever the emit chose: a
value equals a separately built copy of itself, `eq(a,b)` is `eq(b,a)`, equal to
the same value means equal to each other, the answer matches a schema-blind
structural walk, `isEqualInput(schema)` is `isEqualOutput(reverse(schema))`, and
two inputs the Input side calls equal decode to two outputs the Output side
calls equal. Cases known not to hold are listed in the script with the reason
written by hand, and the run fails on an unlisted one *and* on a listed one that
has started to hold. `--seeds=N` widens the search and `--cases=N` deepens each
stream; reach for the first, since the grammar branches on every draw and a
sweep of short streams covers what one long stream does not. A case it turns up
becomes a spec.

The harness asks the same question from the other end: every example pair it
already compares for itself goes to `isEqual*` too, and a disagreement with its
own oracle is a finding. So the whole spec corpus is the comparator's test
suite, and neither side is the one being trusted.

## Changing the form codec

A field's reading depends on its wrapper as much as its type, and a spec covers
one schema. `pnpm --filter=sury fuzz:formdata` crosses every wrapper with every
leaf and checks four properties: a field works in both directions or is rejected
in both, an encode does not write into the value it was handed, `decode(encode(v))`
is `v`, and every entry list a client could send is either rejected or read as a
value the schema's own output type accepts - the half a round-trip can't reach,
since a repeated key or a file where text belongs is nothing an encode would
produce. It then compiles every field that works into one schema, which is where
a name handed out twice or a declaration hoisted after its reader shows.

Each property has a list in the script of the cases known not to hold, keyed by
what the run prints, and the reason is written by hand - the run fails on an
unexplained one *and* on a listed one that has started to hold. It is exhaustive,
so there is no seed. A case it turns up becomes a spec, or a test where an
example can't hold it.

`CODEC_SPEC.md` is the normative statement of what conversions are legal,
built-in and custom alike; `CONTENT_CODEC_SPEC.md` covers the carrier/format
pairs where two built-in readings exist (pack/unpack).
