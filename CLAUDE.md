# Sury Architecture

## Goals (priority order on conflict)

1. **DX** — intuitive public API and error messages.
2. **Performance** — generated code is the hot path; avoid extra vars, allocations, double validation; inline over indirect.
3. **Bundle size** — `S.mjs` (bundled from `src/entry.ts`) ships to browsers. Reuse helpers (`B_refine`, `B_markOutput`) over duplicated codegen.

Tiebreaker: shortest *generated* code wins over shortest *library* code (runtime ships per-schema, library ships once).

## Layout

`packages/sury/src/*.ts`, plain TypeScript, layered acyclically:

```
base → builder → primitives → parse → union → composites → factory
     → modifiers → refinements → operations → advanced/* → jsapi → jsonschema
```

Only type-only imports may point "up"; `operations → jsonschema` is the one real
exception. Each name says what it holds: `factory` builds schemas, `modifiers`
takes a schema and returns a changed one (refine, transform, metadata, object
modes, defaults), `refinements` layers checks (string formats are the same idea
with a canned predicate), `operations` compiles a schema into a callable.

Rules that are easy to break:

- **`base.ts` has no outgoing imports** — the data model (`Internal`, `Val`,
  `Check`), the schema prototype, tags, flags, paths, `Builder`/`Encoder`.
  Anything may reach it. `defsPath` (`S.recursive`'s `$ref` prefix) and
  `jsonName` live here too, because the modules that recognise them by name
  can't import their schema without a cycle.
- **`src/advanced/`** is one file per schema nothing else builds on (`json`,
  `recursive`, `compactColumns`, `uint8Array`, `date`, `list`). `union` is *not*
  there — six modules build on it.
- **`src/entry.ts` is the single public entry.** Add `$res_*` exports *only* for
  APIs with no public-JS equivalent (`$res` because ReScript externals reject
  `~`); where ReScript differs only in argument shape, `S.res` binds the public
  export and adapts (`refine`, `to`, `decoder`, `tuple1/2/3`, `parseOrThrow`, …).
- **`S.res` is the only ReScript module** — `@module("sury") external` bindings
  resolved through the package's own `"."` export, so ReScript and JS share one
  runtime instance.
- `scripts/pack.ts` bundles to the gitignored `src/S.mjs` (`pnpm build:entry`);
  checked-in `src/S.d.mts` provides its types.

Conventions, all for bundle size:

- Keep helpers **flat and `B_`-prefixed** (`B_refine`, `Literal_parse`) so each
  shakes individually. PURE-annotate any top-level call initializer.
- Prefer `const f = () => {}` over `function` (measurably smaller minified).
  `noopOperation` and the `this`-based `_var` family are the deliberate
  exceptions.
- Inline intrinsics (`a | b`, `typeof x`) rather than wrapping them in helpers.
- `val`/`check`/`bGlobal` field names stay short (`cp`, `hd`, `vc`, …) —
  property names survive minification, so every character ships.
- esbuild does **not** inline module-level `const` numbers, so a named bit flag
  costs bytes every time it is used. Document the values in a comment and write
  the literal (see `UnionCase.f`, `unionEffect*`).

## Tree-shaking

*How much of Sury does a consumer carry?* `bundleSize.yaml`, one row per public
export. Bounded below by the eager
`Object.defineProperty(schemaPrototype, "~standard", …)` in `operations.ts` — an
unconditional top-level mutation no bundler can drop, whose getter reaches
`getDecoder` → `compileDecoder` → `parse` → the whole builder. Hence ~3.5 kB
even for `S.unknown`. Making Standard Schema opt-in like
`enableStandardJSONSchema` would cut a schema-only import to ~0.7 kB; it is
breaking, so it hasn't been done.

*How much of the **consumer's own** schema code survives?* That's what
`@__NO_SIDE_EFFECTS__` buys — without it, importing one schema from a shared
`schemas.ts` retains all of them:

- Every public pure factory carries `// @__NO_SIDE_EFFECTS__` on the line above
  its declaration. Exceptions are the exports whose point *is* the effect:
  `assert`, `is`, `safe`, `safeAsync`, `global`, `enableStandardJSONSchema`,
  `$res_assertAsyncOrThrow`, `$res_setExnId`.
- **Never publish a factory through an alias** (`export const object = schemaObject`)
  — the annotation counts only on the declaration that *is* the function.
  Re-export instead: `export { schemaObject as object } from "./factory"`.
- `tests/treeShaking_test.ts` asserts both against the emitted `S.mjs`.
  `bundleSize.yaml` can't: esbuild honors the annotation only within one file
  (Rollup ≥ 4 and Rolldown honor it across the package boundary — that's where
  the win lands).
- `schema.with(S.meta, …)` is a method call on an opaque receiver and can never
  be dropped. The functional `S.meta(schema, …)` is equivalent and does shake.
- `package.json`'s `sideEffects` lists `S.res.mjs`/`S.res.js` rather than being
  `false`: they carry a top-level `$res_setExnId(Exn)`, and a blanket `false`
  lets a bundler drop it while keeping the bindings, after which
  `try { … } catch { S.Raised }` stops matching.

## Comments

- Default: no comment.
- Write one only for a non-obvious *why* — a hidden constraint, a subtle
  invariant, a bug workaround, behavior that would surprise a reader.
- Never restate the code. Delete existing comments that fail this test, even in
  code you're only editing.
- Repo-wide, not just `packages/spec`.

## Input vs Output

Input and Output differ when the schema or any nested item transforms.

```ts
S.string                                          // string → string
S.schema({ foo: S.string.with(S.to, S.number) })  // {foo:string} → {foo:number}
```

Modifiers (`.with(S.refine, …)`) apply to the **output** type. `inputRefiner`
and `refiner` are stored separately so `S.reverse` can swap them. Every schema
must be reversible unless explicitly opted out. `name` and built-in refinements
apply to both sides.

## Decode pipeline

Decoder takes one schema, Input → Output. Schemas joined by `.to` form one fused
pipeline. Per-schema order:

1. **decoder** — narrow input to the Input type.
2. **inputRefiner** — user validations on the typed Input.
3. **decoder** — Input → Output (e.g. nested fields).
4. **refiner** — user validations on the assembled Output.
5. If `.to`: **parser** (custom) or **encoder** (default) + recurse into
   `.to.decoder`.

`S.reverse` swaps `inputRefiner ↔ refiner`, `parser ↔ serializer`, and reverses
the `.to` chain.

## Refiner ownership

The parse loop applies refiners **only for primitive decoders** (result has
`isOutput !== Some(true)`). **Advanced decoders** (object, array, tuple, union,
recursive — anything setting `isOutput = Some(true)`) apply them themselves, via
`B_markOutput(val, valInput)`:

- input-refiner checks go onto `valInput.checks` (pre-transform slot);
- `val` is wrapped via `B_refine` with output-refiner checks (assembled output);
- `isOutput = Some(true)` on the result;
- with `valInput.prev` None, input checks fold into the output wrap so emit has
  a `prev.var()`.

For primitives `val === valInput`. **Skipping this call silently drops user
`S.refine`s.** An async output refiner must run inside `.then()` on the resolved
value, never on the Promise wrapper.

## Async

Any transformation may be async; continue the chain via `.then()`. Aggregate
nested items (object fields, array items) with `Promise.all()`.

## Val

The compile-time view of a runtime value at one point in the generated code.
`expected` is the schema to build a decoder for, `inline` the expression form,
`var()` the lazily allocated variable name (reuse it when the value is
referenced more than once), `path` the location in the input for errors,
`isOutput` whether refiners have run, `prev` the previous val in the transform
chain. The rest carry invariants:

- `schema` — the actual type here. **An output val's `schema` must describe the
  value it actually holds.** Build it from item-output schemas, never from the
  pre-transform `expected`, and never overwrite it on an `isOutput` val — the
  next `.to` segment decodes from it, so a stale schema double-decodes or skips
  decoding (#284).
- `codeFromPrev` — statements producing this val from `prev`. **A val owns the
  declaration of its own value here** (`let v=…;`); non-empty `codeFromPrev`
  makes the val non-hoistable in `merge`, so a union discriminant can never be
  lifted above a `let` it reads (the `str->to(option(int))` bug class).
- `hoistedDecls` — `let` declarations hoisted *onto this val* by a descendant
  whose own segment was already emitted, so the decl lives on a still-open owner
  that outlives it (a field read on its parent, a loop accumulator before its
  `for`). Use `B_hoistDecl(owner, decl)`; `merge` emits them after this val's
  checks.
- `finalized` — set by `merge` once the val's code is emitted. A late cached-bond
  materialization checks `parent.finalized` and re-reads inline instead of
  hoisting a now-undroppable decl (#240).
- `checks` — type-narrows and user refiners. A check whose
  `fail === B.failInvalidType` is a type-narrow and **doubles as a union
  dispatch discriminant**.

Helpers: `B_next` (one step down the chain, sets `hasTransform`), `B_refine`
(clone to attach checks, keeping the var-allocation link), `B_hoistDecl`,
`B_markOutput`, `B_merge` (walks `.prev` into a code string; with `~out` it lifts
type-narrow checks into a `HoistCond`, keeping a val with non-empty
`codeFromPrev` non-hoistable so its decl stays with the check).

## Union

`src/union.ts` owns `S.union` end to end. `CODEC_SPEC.md` is the normative
*what* (four conversion rules, rejections, universal fallback); this is the
*how*. `unionDecoder` runs four stages, each a named function taking the
previous one's output:

1. **`unionNormalize`** — facts about the source every later stage reads: the
   tags it can produce (`unionMask` mode 2 resolves a recursive root `$ref`),
   whether a variant spells the source `const` out exactly, whether the union
   carries `fromDefault`. Type-only rejections fire here (rule 2's partial match;
   rules 3/4 via `unionResolve` when the union has its own `.to`).
2. **`unionAnalyze`** — one `UnionMember` per variant, all integers and small
   tuples: accepted tags `m`, produces-anything `o`, effect class `e`, grouping
   key `k`, specificity tier `p`, route `r`, discriminator `d`. `unionTraits` is
   the one bounded structural walk behind `e`; refs, nested unions, functions and
   custom parsers stop it, so a recursive schema terminates without eager
   expansion.
3. **`unionPlan`** — members become an ordered list of groups, each owning one
   shared narrow (`unionNarrowSchema`) parsed once; a group stops accepting
   members once one in between could accept the same values. Ordering is by
   specificity tier, *not* source order — that is what puts an instance ahead of
   an earlier generic `object`, and an exact `NaN` ahead of `number`. The
   `priority` route map exists so such a member can still reach a bucket after an
   intervening member closed it. A creation error from `parse` propagates — a
   variant is never dropped (the spec's whole-operation rejection).
4. **`unionEmit`** — each group becomes a `{c, b, f}` case; `unionEmitChain`
   stitches them into one fallback chain.

Invariants worth knowing before touching any of it:

- **Grouping key for instances is the class *identity*, never `class.name`** —
  names collide after minification.
- **A decoder's own type narrow must be exactly `typeCheckCond` for its tag.**
  This is cross-module: a group's shared narrow stands in for its members' type
  checks, so an object mode that skipped `!Array.isArray` because it rebuilds the
  value anyway would widen what the case accepts past what its mask claims.
- **Whether emitted code can raise is read off `g.t` by bracketing the
  emission**, never by inspecting the string — `e[N](…)` is the accessor for
  *every* embed.
- **Narrows are hoisted into the dispatch condition even for a member that can
  fall through.** A value the narrow rejects could never have been accepted by
  that member, so `if(cond)` reaches the next member exactly as catching would.
  Consequences: adjacent cases with *textually* identical conditions share one
  test, and a case behind a condition the previous one already accepted outright
  is dropped as unreachable.
- **Error reasons follow one rule:** a member ruled out by its type narrow or
  discriminant contributes *no* reason (the `Expected A | B | C, received X` line
  already says that), a member that ran and failed deeper contributes its
  reason, and the chain always ends in the aggregated error rather than a bare
  inner one naming no member. `specs/union2-error-aggregation.yaml` pins this.
- **Acceptance masks describe the narrow actually emitted, not the variant's own
  tag** — a JSON string offered to `S.bigint` accepts *strings*. Only hoistable
  narrows count; a check the dispatch can't lift constrains nothing.
- A union whose every branch is a pass-through emits its narrow as one **check**
  rather than an `if(!cond){fail}` statement — shorter, and it stays hoistable so
  an enclosing union can lift it. The check pins `self` as its expected schema on
  a val of its own, because the decoder's tail overwrites `e` and rebuilds `s`,
  either of which would rename the error to a schema the value was never matched
  against.
- Two internal shapes bypass the user-facing rules, both marked
  `Internal.perVariant`: a possibly-absent dict read (`V | undefined` from
  `valGet`), and the JSON encoder's per-object-field mapping, which pairs source
  and target variants by position.

Which member a value dispatches to is invisible in a golden until someone writes
the spec for exactly that permutation, so run the differential harness against
the commit you started from before and after any change to analysis, planning or
emission:

```bash
pnpm --filter=sury fuzz:union --ref=HEAD   # --seed=N to widen the search
```

It builds both revisions, drives seeded random unions through each, and sorts
differences into `acceptance` / `exception-kind` (a behavior change — exits
non-zero) and `reasons` / `message` (error detail, for you to accept or reject).
Anything it finds belongs in a spec's `examples`, not a commit message.
