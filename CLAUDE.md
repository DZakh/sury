# Sury Architecture

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `S.mjs` (bundled from `src/entry.ts`) ships to browsers. Reuse helpers (`B_refine`, `B_markOutput`) over duplicated codegen.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime ships per-schema, library ships once).

## Layout

The implementation lives in `packages/sury/src/*.ts` — plain TypeScript, layered acyclically (types → schema → builder → primitives → parse → union → composites → operations → formats → factory → refinements → jsapi → jsonschema; only type-only imports may point "up"). `src/entry.ts` is the single public entry: it re-exports the curated JS/TS API under its documented names, creates the eager PURE-annotated schema constants, and exposes a minimal ReScript-binding surface (`$res_*`-named exports, only for APIs with no public-JS equivalent — `$res` because ReScript externals reject `~` in names). `scripts/pack.ts` bundles it to the gitignored `src/S.mjs` (`pnpm build:entry`); the checked-in `src/S.d.mts` → `S.d.ts` provides its types, and the publish step additionally emits a CJS `S.js` into the artifact for the require condition. `S.res` is the one ReScript module: public types plus `@module("sury") external` bindings resolved through the package's own "." export, so ReScript and JS share a single runtime instance. Where the ReScript API differs from JS only in argument shape, S.res binds the public JS export and adapts in ReScript (`refine`, `to`, `decoder`, `tuple1/2/3`, `parseOrThrow`, …) rather than adding a `$res_*` export. The former `module B` is flattened to `B_`-prefixed top-level functions (and `Literal.parse` → `Literal_parse`, etc.) so bundlers tree-shake each helper individually; keep new helpers flat for the same reason, and PURE-annotate any top-level call initializer. Prefer `const name = () => {}` arrows over `function` declarations (measurably smaller minified; `noopOperation` and the `this`-based `_var` family are the deliberate exceptions), and inline former ReScript intrinsics (`a | b`, `typeof x`) rather than wrapping them in helpers. `val`/`check`/`bGlobal` runtime field names stay short (`cp`, `hd`, `vc`, …) — property names survive minification, so every character ships.

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

## Union

`src/union.ts` owns `S.union` end to end: the factory, `unionDecoder`, and
`unionEncoder`. `CODEC_SPEC.md` is the normative statement of *what* it does
(the four conversion rules, the rejections, universal fallback); this section is
*how*.

`unionDecoder` runs three passes over the variants:

1. **Forward — group.** Rejections that need only types fire first (rule 2's
   partial match; rules 3/4 via `unionResolve` when the union carries its own
   `.to`). Then each variant joins a group keyed by its tag: a group owns one
   shared type narrow (`unionNarrowSchema`), parsed once, and every variant in it
   branches from that narrowed val. Merging a variant into an earlier group is
   legal only while no case in between could accept the same values
   (`maskAt[j] & group.m`). A creation error from `parse` propagates — a variant
   is never dropped, and that's the spec's whole-operation rejection.
2. **Reverse — suffix masks.** `suffix[i]` = what any later group accepts, in
   O(n) integer ops. It's the only lookahead the emit needs.
3. **Forward — emit.** `unionEmitChain` stitches `{cond, body}` alternatives into
   one fallback chain. A body that can't throw (`unionCanThrow`) or whose failure
   nothing later could recover from becomes a plain `if(cond){body}`; otherwise
   it runs inside `try{…}catch(eN){ <rest of the chain> }`, and the final `else`
   raises the aggregated union error with the caught `eN`s.

Acceptance masks are read off the narrow the attempt actually emitted
(`unionAcceptMask`), not off the variant's own tag — a JSON string offered to
`S.bigint` accepts *strings*. Only **hoistable** narrows count: a check the
dispatch can't lift stays in the body and constrains nothing about which values
reach the case. `unionWiden` closes the mask over object/instance, which the
`typeof` narrows don't separate. A `union`-tagged variant has no `typeof`
discriminant of its own, so the dispatch would have to assume it might accept
anything and learn otherwise by catching its failure; `unionNestedMask` reads the
tags a nested union accepts *when it compiles to nothing but a type test*, and
returns 0 — keeping the conservative source mask — the moment a member would get
code of its own.

A union whose every branch is a pass-through emits its narrow as one **check** on
the output val rather than an `if(!cond){fail}` statement. That's the library's
standard check shape (shorter), and it keeps the narrow hoistable, so an
enclosing union lifts it into the dispatch instead of reaching the next variant
through a thrown exception. The check pins `self` as its expected schema on a val
of its own: the decoder's tail overwrites `e` with the `.to` target and rebuilds
`s` from the variants' outputs, either of which would otherwise rename the error
to a schema the value was never matched against.

Two internal shapes bypass the user-facing rules, both marked
`Internal.perVariant`: a possibly-absent dict read (`V | undefined`, from
`valGet`) converts per variant with a member that has no decoder dropping out,
and the JSON encoder's per-object-field mapping pairs source and target variants
by position.

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

Transformation chain (relative to `.prev`):
- `prev` — previous val in the chain
- `codeFromPrev` — statements that produce this val from `.prev`. **A val owns the declaration of its own value here** (`let v=…;`); a non-empty `codeFromPrev` makes the val non-hoistable in `merge`, so a union discriminant can never be lifted above a `let` it reads (the `str->to(option(int))` bug class).
- `hoistedDecls` — `let` declarations hoisted *onto this val* by a descendant whose own segment was already emitted, so the decl must live on a still-open owner that outlives it (a field read on its parent object, a loop accumulator before its `for`). Use `B_hoistDecl(owner, decl)` — it never mutates an unrelated val behind a callback. `merge` emits them right after this val's checks.
- `finalized` — set by `merge` once a val's code is emitted. A late cached-bond materialization checks `parent.finalized` and re-reads inline instead of hoisting a now-undroppable decl (#240).
- `checks` — `array<check>`; both type-narrows and user refiners live here. A check whose `fail === B.failInvalidType` is a type-narrow and **doubles as a union dispatch discriminant**.

Helpers:
- `B_next` — new val one step down the transform chain (sets `hasTransform`).
- `B_refine` — clones a val to attach `checks`, keeping the var-allocation link.
- `B_hoistDecl(owner, decl)` — attach a `let` declaration to a still-open owner val (prev/parent/self) whose segment dominates and outlives the materialized value.
- `B_markOutput` — applies `inputRefiner`/`refiner` and sets `isOutput` (see Refiner ownership).
- `B_merge` — walks the `.prev` chain into a code string. With `~out` (union codegen) it lifts type-narrow checks into a `HoistCond` — both as the dispatch condition and as the rejecting form a `try`-wrapped case needs; a val with non-empty `codeFromPrev` is kept non-hoistable so its decl stays with the check.

