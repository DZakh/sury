# Sury Architecture

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `S.mjs` (bundled from `src/entry.ts`) ships to browsers. Reuse helpers (`B_refine`, `B_markOutput`) over duplicated codegen.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime ships per-schema, library ships once).

## Layout

The implementation lives in `packages/sury/src/*.ts` — plain TypeScript, layered acyclically (types → schema → builder → primitives → parse → composites → operations → formats → factory → refinements → jsapi → jsonschema; only type-only imports may point "up"). `src/entry.ts` is the single public entry: it re-exports the curated JS/TS API under its documented names, creates the eager PURE-annotated schema constants, and exposes a minimal ReScript-binding surface (`$res_*`-named exports, only for APIs with no public-JS equivalent — `$res` because ReScript externals reject `~` in names). `scripts/pack.ts` bundles it to the gitignored `src/S.mjs` (`pnpm build:entry`); the checked-in `src/S.d.mts` → `S.d.ts` provides its types, and the publish step additionally emits a CJS `S.js` into the artifact for the require condition. `S.res` is the one ReScript module: public types plus `@module("sury") external` bindings resolved through the package's own "." export, so ReScript and JS share a single runtime instance. Where the ReScript API differs from JS only in argument shape, S.res binds the public JS export and adapts in ReScript (`refine`, `to`, `decoder`, `tuple1/2/3`, `parseOrThrow`, …) rather than adding a `$res_*` export. The former `module B` is flattened to `B_`-prefixed top-level functions (and `Literal.parse` → `Literal_parse`, etc.) so bundlers tree-shake each helper individually; keep new helpers flat for the same reason, and PURE-annotate any top-level call initializer. Prefer `const name = () => {}` arrows over `function` declarations (measurably smaller minified; `noopOperation` and the `this`-based `_var` family are the deliberate exceptions), and inline former ReScript intrinsics (`a | b`, `typeof x`) rather than wrapping them in helpers. `val`/`check`/`bGlobal` runtime field names stay short (`cp`, `hd`, `vc`, …) — property names survive minification, so every character ships.

## Comments

- Default: no comment.
- Write one only for a non-obvious *why* — a hidden constraint, a subtle invariant, a bug workaround, or behavior that would surprise a reader.
- Never write one that just restates the code.
- Delete any existing comment that fails this test, even in code you're only editing, not authoring.
- Repo-wide, not just `packages/spec`.

## Input vs Output

A schema has an Input type and an Output type. They differ when the schema or any nested item has a transformation.

```ts
S.string                                          // string → string
S.schema({ foo: S.string.with(S.to, S.number) })  // {foo:string} → {foo:number}
```

Schema modifiers (`.with(S.refine, …)`, etc.) apply to the **output** type. `inputRefiner` and `refiner` are stored separately so `S.reverse` can swap them. Every schema must be reversible (Input→Output ↔ Output→Input) unless explicitly opted out. Modifiers like `name` and built-in refinements apply to both sides.

## Decode pipeline

Decoder takes a single schema, Input → Output. Schemas joined by `.to` form one fused transformation pipeline.

Per-schema execution order:

1. **decoder** — narrow input to schema's Input type.
2. **inputRefiner** — user validations on the typed Input (pre-transform).
3. **decoder** — Input → Output (e.g. decode nested fields).
4. **refiner** — user validations on the assembled Output.
5. If `.to`: **parser** (custom Output → `.to` Input) OR **encoder** (default Output → `.to` Input) + recurse into `.to.decoder`.

`S.reverse` swaps `inputRefiner ↔ refiner`, `parser ↔ serializer`, and reverses the `.to` chain.

## Refiner ownership

The parse loop applies refiners **only for primitive decoders** (result has `isOutput !== Some(true)`). **Advanced decoders** (object, array, tuple, union, recursive — anything that sets `isOutput = Some(true)`) own refiner application themselves, so input checks land on the pre-transform val and output checks on the assembled output.

Use `B_markOutput(val, valInput)`:
- Pushes input-refiner checks onto `valInput.checks` (emits at pre-transform slot).
- Wraps `val` via `B_refine` with output-refiner checks (observes assembled output).
- Sets `isOutput = Some(true)` on the result.
- When `valInput.prev` is None, input checks fold into the output wrap so emit has a `prev.var()`.

For primitives, `val === valInput`. For advanced decoders, `valInput` is the pre-transform input and `val` is the assembled output. **Skipping this call silently drops user `S.refine`s.**

Async output refiner must run inside `.then()` on the resolved value, never on the Promise wrapper.

## Async

Any transformation may be async. Continue the chain via `.then()`. For nested items (object fields, array items), aggregate with `Promise.all()`.

## Val

A `val` is the compile-time view of a runtime value at one point in the generated code.

Core fields:
- `schema` — actual type at this point. **Invariant: an output val's `schema` describes the value it actually holds** — build it from item-output schemas, never from the pre-transform `expected`, and never overwrite it on an `isOutput` val. The next `.to` segment decodes from it, so a stale schema double-decodes or skips decoding (#284).
- `expected` — schema to build decoder for
- `var()` — variable name in generated code (allocates lazily; reuse when the value is referenced more than once)
- `inline` — inline expression form
- `path` — location in input (for errors)
- `isOutput` — `Some(true)` once refiners have been applied (see Refiner ownership)

Deferred emission: generated code is a `Code` rope (`string | Code[] | Val`) where a val stands in as its own hole — the single `B_joinCode` in `compileDecoder` resolves it to `codeFromPrev` + `checksCode` + `hoistedDecls`, so all three stay writable after the val's segment was merged. Joining IS freezing: resolving a val's marker (or a mid-compile join into a closure body — `.then`, `Promise.all`, loop bodies) sets `finalized`. A caller that branches on `B_isEmptyCode` and then discards a tree must join it first so a late fill falls back to an inline re-read instead of being dropped.

Transformation chain (relative to `.prev`):
- `prev` — previous val in the chain
- `codeFromPrev` — code that produces this val from `.prev`. **A val owns the declaration of its own value here** (`let v=…;`, appended via `B_addCode` — legal even after the segment was merged); a non-empty `codeFromPrev` makes the val non-hoistable in `merge`, so a union discriminant can never be lifted above a `let` it reads (the `str->to(option(int))` bug class).
- `hoistedDecls` — `let` declarations hoisted *onto this val* by a descendant that can't own them (a field read on its parent object, a loop accumulator before its `for`). Use `B_hoistDecl(owner, decl)` — legal any time before the final join. The join emits them right after this val's checks.
- `finalized` — set by the join once a val's slots were read into a string. A late materialization on a finalized val re-reads inline instead of writing to a slot that can no longer emit (the old #240 class; now only reachable across real scope boundaries).
- `checks` — `array<check>`; both type-narrows and user refiners live here. A check whose `fail === B.failInvalidType` is a type-narrow and **doubles as a union dispatch discriminant**.

Helpers:
- `B_next` — new val one step down the transform chain (sets `hasTransform`).
- `B_refine` — clones a val to attach `checks`, keeping the var-allocation link.
- `B_hoistDecl(owner, decl)` — attach a `let` declaration to an owner val (prev/parent/self) whose segment dominates and outlives the materialized value.
- `B_markOutput` — applies `inputRefiner`/`refiner` and sets `isOutput` (see Refiner ownership).
- `B_merge` — walks the `.prev` chain into a `Code` rope of val markers (checks stringify into `checksCode`; `codeFromPrev`/`hoistedDecls` resolve at the join). With `~hoistCond` (union codegen) it lifts type-narrow checks into a dispatch condition; a val with non-empty `codeFromPrev` is kept non-hoistable so its decl stays with the check.
- `B_joinCode` — the single stringification point; seals every val it resolves.

