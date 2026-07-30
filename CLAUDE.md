# Sury Architecture

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `S.mjs` (bundled from `src/entry.ts`) ships to browsers. Reuse helpers (`B_refine`, `B_markOutput`) over duplicated codegen.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime ships per-schema, library ships once).

## Layout

The implementation lives in `packages/sury/src/*.ts` — plain TypeScript, layered acyclically (base → builder → primitives → parse → union → composites → factory → modifiers → refinements → operations → advanced/\* → jsapi → jsonschema; only type-only imports may point "up", and `operations → jsonschema` is the one that does). `base.ts` is the bottom: the data model (`Internal`, `Val`, `Check`), the schema object and its prototype, tags, flags and paths, plus the `Builder`/`Encoder` aliases — merged into one module because they are mutually dependent by nature, and it has **no outgoing edge at all**, so anything may reach it. The split by concern is what each name says: `factory.ts` builds schemas, `modifiers.ts` takes a schema and returns a changed one (refine, transform, metadata, object modes, defaults), `refinements.ts` layers checks (and the string formats, which are the same idea with a canned predicate), `operations.ts` compiles a schema into a callable. `src/advanced/` holds one file per schema that nothing else builds on — `json`, `recursive`, `compactColumns`, `uint8Array`, `date`, `list` — so a schema with a decoder of its own is read on its own; `union` is *not* among them (six modules build on it, so it stays in the core). Two identity constants live in base rather than with their schema, because the modules that recognise them by name can't import them without a cycle: `defsPath` (`S.recursive`'s `$ref` prefix) and `jsonName`. `src/entry.ts` is the single public entry: it re-exports the curated JS/TS API under its documented names, creates the eager PURE-annotated schema constants, and exposes a minimal ReScript-binding surface (`$res_*`-named exports, only for APIs with no public-JS equivalent — `$res` because ReScript externals reject `~` in names). `scripts/pack.ts` bundles it to the gitignored `src/S.mjs` (`pnpm build:entry`); the checked-in `src/S.d.mts` → `S.d.ts` provides its types, and the publish step additionally emits a CJS `S.js` into the artifact for the require condition. `S.res` is the one ReScript module: public types plus `@module("sury") external` bindings resolved through the package's own "." export, so ReScript and JS share a single runtime instance. Where the ReScript API differs from JS only in argument shape, S.res binds the public JS export and adapts in ReScript (`refine`, `to`, `decoder`, `tuple1/2/3`, `parseOrThrow`, …) rather than adding a `$res_*` export. The former `module B` is flattened to `B_`-prefixed top-level functions (and `Literal.parse` → `Literal_parse`, etc.) so bundlers tree-shake each helper individually; keep new helpers flat for the same reason, and PURE-annotate any top-level call initializer. Prefer `const name = () => {}` arrows over `function` declarations (measurably smaller minified; `noopOperation` and the `this`-based `_var` family are the deliberate exceptions), and inline former ReScript intrinsics (`a | b`, `typeof x`) rather than wrapping them in helpers. `val`/`check`/`bGlobal` runtime field names stay short (`cp`, `hd`, `vc`, …) — property names survive minification, so every character ships.

## Tree-shaking

Two different questions, with two different answers.

*How much of Sury does a consumer's bundle carry?* `bundleSize.yaml` measures
that, one row per public export. It is bounded from below by the eager
`Object.defineProperty(schemaPrototype, "~standard", …)` in `operations.ts`: an
unconditional top-level mutation no bundler can drop, whose getter reaches
`getDecoder` → `compileDecoder` → `parse` → the whole builder. That is why every
row starts at ~3.5 kB even for `S.unknown`. Making Standard Schema opt-in the
way `enableStandardJSONSchema` already is would cut a schema-only import to
~0.7 kB; it is a breaking API change, so it hasn't been made.

*How much of the **consumer's own** schema code survives?* That's what
`@__NO_SIDE_EFFECTS__` on every public factory buys: without it a shared
`schemas.ts` is a wall of unanalyzable calls, and importing one schema from it
retains all of them plus everything they reach. Rules:

- Every public export that is a pure factory carries `// @__NO_SIDE_EFFECTS__`
  on the line above its declaration. The exceptions are the exports whose whole
  point is the effect — `assert`, `is`, `safe`, `safeAsync`, `global`,
  `enableStandardJSONSchema`, `$res_assertAsyncOrThrow`, `$res_setExnId`.
- Never publish a factory through an alias (`export const object = schemaObject`).
  The annotation counts only on the declaration that *is* the function; an alias
  makes the public name a variable holding one, and the annotation is lost.
  Re-export instead (`export { schemaObject as object } from "./factory"`).
- `tests/treeShaking_test.ts` asserts both of these against the emitted
  `src/S.mjs`. `bundleSize.yaml` cannot: it measures with esbuild, which honors
  `@__NO_SIDE_EFFECTS__` only within a single file, so the annotations are
  invisible to it (Rollup ≥ 4 and Rolldown do honor them across the package
  boundary; that's where the win lands).
- `schema.with(S.meta, …)` is a method call on an opaque receiver — no bundler
  can drop it. The functional spelling `S.meta(schema, …)` is equivalent and
  does shake.
- `package.json`'s `sideEffects` lists `S.res.mjs`/`S.res.js` rather than being
  `false`: those carry a top-level `$res_setExnId(Exn)` that registers the
  ReScript exception identity, and a blanket `false` lets a bundler drop it
  while keeping the bindings around it — `try { … } catch { S.Raised }` then
  stops matching. Everything else in the package is side-effect-free.

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

`unionDecoder` runs four stages, each a named function taking the previous one's
output:

0. **`unionNormalize`.** Facts about the source that every later stage reads: the
   mask of runtime tags it can produce (`unionMask` mode 2, which resolves a
   recursive root `$ref` to its definition), whether a variant spells the source
   `const` out exactly, and whether the union carries `fromDefault`. Rejections
   needing only types fire around here — rule 2's partial match, and rules 3/4
   via `unionResolve` when the union carries its own `.to`.
1. **`unionAnalyze`.** One `UnionMember` record per variant, all of it integers
   and small tuples: the tags it accepts (`m`), whether it produces anything
   (`o`), its effect class, its grouping key `k` (the tag; class *identity* for
   instances — never `class.name`, which collides after minification), its
   specificity tier `p`, its route `r` (the tag family its mask must fit inside),
   and its discriminator `d` — the first literal field, as a `[key, value]` pair.
   `unionTraits` is the one bounded structural walk behind the effect class;
   refs, nested unions, functions and custom parsers stop it, so a recursive
   schema terminates without eager expansion.
2. **`unionPlan`.** Members become an ordered list of groups. A group owns one
   shared type narrow (`unionNarrowSchema`), parsed once, with every member
   branching from that narrowed val. Two structures drive it, both sparse arrays
   indexed by route: `active` holds the bucket a route is currently filling, and
   `priority` the first bucket it ever had, so a high-specificity member (an
   instance against `object`, an exact `NaN` against `number`) can still reach it
   after an intervening member closed `active`. Within a bucket, members sit in
   tiers by `p`, and the flattening pass emits tier 0 before 1 before 2 — that,
   not source order, is what puts an instance ahead of an earlier generic object.
   A bucket stops accepting members the moment one in between could accept the
   same values. A creation error from `parse` propagates — a variant is never
   dropped, and that's the spec's whole-operation rejection.

   A reverse pass over the finished plan then sets each group's fallback bit:
   `laterMask` is what any later group accepts, in O(n) integer ops, and a
   per-route set of discriminator values proves distinct exact discriminants
   disjoint without comparing every group with every later one.
3. **`unionEmit`.** Each group compiles to a `{c, b, f}` case — condition, body,
   flags — and `unionEmitChain` stitches them into one fallback chain. A body
   that cannot raise, or whose failure nothing later could recover from, becomes
   a plain `if(cond){body}`; otherwise it runs inside `try{…}catch(x){…}` and its
   failure is recorded. Whether a stretch of emitted code can raise is read off
   `g.t` by bracketing the emission, never by inspecting the string: `e[N](…)` is
   the accessor for *every* embed.

The narrow is hoisted into the dispatch condition even for a member that can fall
through. A value the narrow rejects could never have been accepted by that
member, so `if(cond)` reaches the next member exactly as catching would, without
re-emitting the narrow as a statement. Two consequences worth knowing: adjacent
cases whose conditions are *textually* identical share one test (the second is
spliced into the first's still-open block), and a case behind a condition the
previous one already accepted outright is dropped as unreachable.

That hoisting also fixes which reasons an aggregated union error carries, which
used to depend on where codegen happened to put a narrow. The rule is now
uniform: **a member ruled out by its type narrow or discriminant contributes no
reason** — the `Expected A | B | C, received X` line already says that much — and
a member that ran and failed deeper contributes its reason. The chain always ends
in the aggregated error, never in a bare inner one that names no member.
`specs/union2-error-aggregation.yaml` pins both halves.

Because a group's shared narrow stands in for its members' own type checks, "a
decoder's own type narrow is exactly `typeCheckCond` for its tag" is a
cross-module invariant: an object mode that skipped `!Array.isArray` because it
rebuilds its value anyway would widen what the case accepts past what its mask
claims.

Acceptance masks describe the narrow the attempt actually emitted, not the
variant's own tag — a JSON string offered to `S.bigint` accepts *strings*, which
is why a member reached only by coercion is assumed to consume the source's
string. Only **hoistable** narrows count: a check the dispatch can't lift stays
in the body and constrains nothing about which values reach the case.
`unionWiden` closes the mask over object/instance, which the `typeof` narrows
don't separate.

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

Which member a value dispatches to is only visible in a golden once someone has
written the spec for exactly that permutation of members, so before and after any
change to analysis, planning or emission, run the differential harness against
the commit you started from:

```bash
pnpm --filter=sury fuzz:union --ref=HEAD   # then --seed=N to widen the search
```

It builds both revisions, drives seeded random unions through each, and sorts the
differences into `acceptance` / `exception-kind` (a behavior change — it exits
non-zero) and `reasons` / `message` (error detail, for you to accept or reject).
Anything it finds belongs in a spec's `examples`, not in a commit message.

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

