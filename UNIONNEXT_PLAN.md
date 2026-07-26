# unionNext — final one-shot rewrite plan

Implementation plan for a from-scratch union factory (`unionNext`) that behaves
exactly as `CODEC_NEXT_SPEC.md` describes. Built side-by-side with the existing
`S.union`; nothing existing changes behavior until a later switchover PR
replaces `unionFactory` with the new implementation and rewrites the
`codec-*.yaml` goldens.

Non-functional targets, in the repo's priority order:

1. **DX** — every spec rejection happens at operation creation with the
   spec's "Invalid operation … say what you mean" suggestions; runtime
   failures aggregate per-case errors under one union error.
2. **Performance** — creation does one resolution pass with integer masks (no
   Sets, no string-keyed dictionaries, no codegen-exception probing);
   generated code is a flat dispatch chain whose exact shape is **decided
   empirically** (see "Measure & iterate" below), not by intuition.
3. **Bundle** — one new flat module reusing `B_*` helpers; net size is
   measured at switchover when `unionDecoder`/`unionEncoder` + the five
   `unionIs*`/`unionCan*` helpers (~350 lines of `composites.ts`) get deleted.

## Settled semantics: universal case fallback

**Any failure of a case — discriminant miss, refinement failure, or any
decoding error inside the case body — passes the value to the next case.**
Only when no case remains does the union throw, and that error aggregates the
individual case errors (`unionErrors`, as today). This is the uniform rule for
plain validation unions and for every conversion rule below; it subsumes the
old design's separation of "type-narrow → dispatch cond" vs "refinement →
committed body".

Consequences:

- A case's *selection condition* may absorb **all** of its cond-expressible
  checks (type narrows *and* refinements): since failure means "try the next
  case" anyway, `if(typeof i==="string"&&i.length>=3){...}else if(...)` is
  both the semantics and the fastest emission. This also fixes the two
  known bugs from IDEAS.md (fused type+refinement error, and same-type cases
  with different refinements losing dispatch).
- Case bodies that can genuinely throw (embedded `BigInt(...)`, custom
  `S.to` coders, nested object field validation) get fallback routing —
  `try{}catch(eN){ <next case> }` or a cond rewrite where the failure is
  check-shaped — **only when a later case could still accept the value**.
- **Compiled-away fallback:** when acceptance analysis proves no later case
  can accept a value that entered this case (disjoint tag masks, disjoint
  literal discriminants — the discriminated-union shape), the fall-through
  is dead: reaching the final `else` would just re-raise this case's error
  inside the aggregate. Emit a direct throw with the precise per-case error
  instead (`Failed at ["a"]: Expected string, received 42`). The accepted
  input set is identical; the error is sharper and the code smaller. This
  preserves the current `union5-discriminated` golden shape and keeps happy
  paths free of try/catch.

## Why from scratch, structurally

The current `unionDecoder` cannot express the spec because of three design
choices, each of which the rewrite removes rather than patches:

1. **It discovers dispatch by running codegen and catching.** Variant
   viability is probed by calling `parse` per variant inside `try/catch`
   (`getArrItemsCode`, the `typeValidationOutput` probe). A creation-time
   `unsupported_decode` becomes a dropped variant, a per-value throwing
   branch, or an always-throwing operation — the exact three salvage modes
   the spec forbids. unionNext resolves *before* any codegen; `parse` is only
   invoked on variants the plan already accepted, and builder-time errors
   propagate out of operation creation untouched.
2. **Semantics and codegen are fused.** Which variant wins is an emergent
   property of `byKey`/`byDiscriminant` string accumulation, so ordering bugs
   (`codec-json-union2`'s dead `S.string` member, the `else if` re-testing its
   own else) are unfixable locally. unionNext computes a semantic plan first
   (ordered attempts per variant, with acceptance domains), then a grouping
   pass that is *provably* behavior-preserving (spec: "grouping is codegen,
   not semantics").
3. **Fallback is try/catch-shaped and partial.** `catch(e0){}` swallows
   everything a branch throws, not just the type miss it falls back from
   (`codec-string-union2-transformed` FIXME), while refinement failures in
   hoisted cases *don't* fall back at all. Universal fallback (above) is the
   single rule replacing both behaviors.

## Architecture: three phases

```
unionNext(items)          — schema creation: flatten, dedupe, wire decoder/encoder
UN_resolve(...)           — operation creation: rules 1–4 → Plan | throw
UN_emit(plan, input)      — codegen: Plan → {pre, cond, body} branches → Val
```

New module `packages/sury/src/unionnext.ts`, flat `UN_`-prefixed top-level
arrows (tree-shaking convention), importing only from `builder`, `types`,
`tags`, `flags`, `path`, `schema`, `parse`. No imports from `composites.ts`
(that's the module being replaced); the two needed pieces that currently live
there (`typeCheckCond` is already in `parse.ts`; object-literal discriminant
hoisting via `B_hoistChildChecks` is in `builder.ts`) are reachable without it.

### Phase 1 — creation: `unionNext(items: Internal[]): Internal`

- 0 items → panic; 1 item → the item itself (as today).
- **Flattening** per spec: a nested union spreads only when it is fully
  transparent — `to === U && parser === U && format === U && refiner === U &&
  inputRefiner === U` (today only `to` is checked; the missing conditions are
  the `codec-union-nested-refined-union` fix). An opaque union stays as one
  variant and type-matches by reference only.
- Dedupe by reference in the same single pass (array scan, no `Set` — variant
  counts are small and `Set` costs allocation plus megamorphic iteration).
- Wire `decoder = UN_decoder`, `encoder = UN_encoder`, keep `has` exactly as
  today (its consumers — `isOptional`, reverse, jsonschema — stay untouched).
- No other precomputation: resolution work belongs to operation creation,
  which `getDecoder` already caches per schema pair, so creation stays O(n)
  with zero closures.

### Phase 2 — resolution

All spec checks live here, running against **derived** types: `UN_decoder`
receives `input.s` (derived source) and `input.e` (the union); `UN_encoder`
receives the union-typed `input` and the `target`.

#### Type identity (spec "same type")

```ts
UN_sameType = (a, b) =>
  a === b ||
  (a.type === b.type &&
    !flagUnsafeHas(tagFlags[a.type], tagFlagRef | tagFlagUnion) && // opaque ⇒ reference only
    a.class === b.class &&      // instances
    a.format === b.format);     // int32 vs number, json-string vs string
```

`S.json` is `refTag` ⇒ opaque ⇒ matches only itself. `S.unknown` matches only
`unknown` (plain tag equality). Matching sides per spec: a source variant
matches by `getOutputSchema(variant)`, a target variant by the variant itself
(its input). A variant whose matching side is `never` is skipped everywhere:
no exception triggering, no coverage counting, no emitted branch on the side
where it is unreachable (`never.with(S.to, X)` as target), but a reachable
`X.with(S.to, never)` arm still compiles to its explicit rejection branch.

#### Acceptance model (drives rule 2/3, grouping legality, and fallback elision)

Two integer masks over the existing `tagFlags` bits — all coverage,
partial-match, and can-a-later-case-accept questions become integer ops:

- `UN_producibleMask(schema)`: runtime tags the source's values can carry.
  Own tag for concrete schemas; OR of variants for transparent unions; for
  `S.json` the JSON value tags (string|number|boolean|null|object|array);
  `unknown` ⇒ 0 (see below).
- `UN_offerMask(variantSchema)`: which *foreign* runtime tags the built-in
  decode into this variant is offered (gap filling). A small static table
  mirroring today's decoders: `number|int32 ← string`, `bigint ← string`,
  `boolean ← string`, `string ← number|boolean|bigint`, non-string literals ←
  string (via `literalDecoder`'s string branch), `null ↔ undefined`, etc.
  This table is the **only new routing knowledge** in the design — the actual
  coercion code still comes from the existing decoder/encoder composition —
  and every entry is pinned by a codec spec.
  - Source-sensitive representations stay with the source, where they live
    today: for an encoder-bearing source (`S.json`), the representation of a
    non-producible target tag is defined by that encoder (bigint ⇒ string,
    undefined ⇒ null). Resolution asks a tiny per-source hook
    (`UN_repMask(source, variantTag)`; default = `UN_offerMask ∩
    producible(source)`, json overrides) instead of probing codegen. This is
    what keeps JSON numbers out of `BigInt` while JSON strings reach it.
- A case's acceptance is refined past the mask by literal consts (exact
  values) and marked *approximate* when refinements or nested structure make
  it a strict subset of its tags — approximate acceptance can never justify
  eliding a later case's fallback, only disjointness can.

#### The four rules as plan constructors

Let `M` = non-never target variants with `UN_sameType(source, variant)`.

- **Rule 1 (non-union → non-union)** — not unionNext's job; the parse loop
  already composes `source.encoder → target.decoder`. unionNext only ever
  delegates to it per variant.
- **Rule 2 (non-union → union)** — in `UN_decoder` when `input.s` is not a
  transparent union:
  - `0 < |M| < |variants∖never|` ⇒ throw `invalid_operation` with the spec's
    two suggested rewrites in the message.
  - `M = all` ⇒ ordered attempts, each variant's pipeline starting from the
    as-is source value (transformed variants run their own `.to`; plain ones
    pass through). Universal fallback chains the attempts.
  - `M = ∅` ⇒ gap decoding: each variant accepts (a) values of its own tag
    when that tag ∈ producible(source) — as-is, validate only — and (b)
    values of its `UN_repMask` tags — via built-in decode. Attempts in
    definition order with universal fallback (this yields
    `codec-number-union2-int32`: int32 range-check first, `""+i` next; and
    `codec-json-union2`: literal, then BigInt attempt, then string
    catch-all). `unknown` source: producible = 0, so every variant is pure
    gap — as-is for its own tag *plus* offered decodes — reproducing
    `codec-unknown-union2` ("123" → 123n).
  - Reachability: a non-never variant with own-tag ∉ producible and
    `UN_repMask = 0` has no consuming path ⇒ the whole operation throws
    `unsupported_decode` ("Can't decode X to Y…"). No dropping, no per-value
    throw branch, no always-throwing op — fixes the three
    `codec-bool-union2*` / `codec-union2-string-unsupported` rows.
- **Rule 3 (union → non-union)** — in `UN_encoder`: match each source
  variant's output type against the target. Partial ⇒ `invalid_operation`
  with the two rewrites. Otherwise **rewrite**: return a val whose expected
  schema is the source union with `variant.to(target)` appended per variant
  (the `unionPerVariantVal` trick, kept — it makes rule 3 literally the
  rewrite the error message suggests), then let `UN_decoder` compile the
  dispatch. Any variant pair without a built-in decoder rejects the whole
  operation at this point, because `parse` on an accepted plan no longer runs
  under a salvage catch.
- **Rule 4 (union → union)** — in `UN_encoder` when the target is a
  (transparent) union: build the matching at plan time — for each source
  variant (by output type), the *first* same-type target variant in
  definition order; unmatched `null`/`undefined` bridge to the opposite
  nullish variant even if that one already has a same-type match (runtime
  same-type wins; the bridge branch only exists where there is no same-type
  source). Coverage must hold both ways over non-never variants, else
  `invalid_operation` naming the uncovered variant. Then rewrite per source
  variant: matched plain target ⇒ pass-through (type check only — exhaustive
  validation is preserved because every branch still carries its check and
  the final `else` throws); matched transformed target ⇒ append that
  variant's `.to` pipeline; bridge ⇒ const swap (`i=null` / `i=void 0`).
  Resolution is a pure function of (source variants by output, target
  variants by input); `reverse` maps those two sets into each other, so
  compiling the reversed schema yields the mirrored plan — the spec's
  "reversing doesn't re-run the checks" holds observably, and both directions
  get pinned by specs.

The plan is an array of monomorphic records (single hidden class, fields
always initialized in canonical order, integer/enum fields — no
`string | string[]` unions like today's `byDiscriminant`):

```ts
type UN_Attempt = {
  v: Internal;   // variant (with .to appended by rules 3/4 rewrites)
  m: number;     // acceptance mask: own-tag bit(s) + offered rep bits
  k: number;     // dispatch class bit (typeof group) or 0 for opaque
  c: unknown;    // literal const, U otherwise (exact acceptance)
  x: boolean;    // acceptance is approximate (refined/structured subset)
};
```

### Phase 3 — codegen: `UN_emit`

Branch IR anticipated by the `B_isHoistable` comment — `{pre, cond, body}`:

- `pre` — producer statements a discriminant reads (`let v0=+i;`), emitted
  *before* the cond inside the owning branch of the outer chain, never
  hoisted across it. This dissolves the `str->to(option(int))` bug class by
  construction instead of guarding it with `B_isHoistable`.
- `cond` — the case selection condition: type narrows *plus* every
  cond-expressible check of the case (universal fallback makes refinements
  selection criteria, not committed asserts), built from `typeCheckCond`
  atoms, literal `===` conds, and lifted `Check.c` conds (reusing
  `B_merge(~hoistCond)` on the case's val chain, now without the
  type-narrow-only partition).
- `body` — the case pipeline merged from its val chain (`parse` on a val
  with `e = variant`, `s` = tag-narrow of the group, `u = true`), including
  object-literal discriminant hoisting via `B_hoistChildChecks` so
  discriminated-union codegen keeps its current `i["kind"]==="a"` shape.

Emission rules:

1. **Grouping pass (legality-checked).** Walk attempts in definition order;
   an attempt may merge into an earlier group with the same dispatch class
   iff no attempt between them intersects its acceptance (mask intersection,
   literal consts exact, approximate acceptance counts as intersecting).
   Same-tag literals fuse into one
   `typeof i==="string"&&(i==="a"||i==="b")` cond; an illegal hoist (string
   catch-all past bigint-from-string) stays in its definition slot, and when
   the same `typeof` is tested in more than one slot it is materialized once
   into a var and reused (spec's "repeated typeof is reused from a var").
2. **Fallback routing (universal).** A non-final attempt whose failure
   points are all `Check`s folds them into its selection cond — the failing
   value simply doesn't select the case. Bodies with embedded throwing code
   get `try{}catch(eN){ <route to next candidate> }`; the catch never
   swallows terminally — when no candidate remains it feeds `eN` into the
   aggregated union error.
3. **Fallback elision.** When the acceptance masks prove no later case
   intersects this case's accepted set, emit the body's failures as direct
   throws with their precise errors (no try/catch, no re-dispatch) — the
   discriminated-union fast shape. Elision is decided per failure point from
   the plan, never by probing.
4. **Exhaustive close.** Final `else` throws the aggregated union error via
   one embedded fail fn (`expected` = the union, collected `eN` case errors
   as `unionErrors`) — same UX as today, one embed slot.
5. **Refiners.** `B_markOutput` semantics per CLAUDE.md: the union's
   `inputRefiner` checks attach once to the pre-dispatch val; the `refiner`
   wraps the *joined* output val once after the chain (not per-case as the
   current `appendUnionRefiners` FIXME does — one emit site, smaller code).
   Async: if any branch is async the joined val is async and output checks
   run inside `.then` on the resolved value, never on the Promise.
6. **Async join** as today: mark output async when any branch is; sync
   branches' results unify through `Promise.resolve` at the join only when
   needed.
7. **Static shortcuts** (replacing today's tier-1/self-decode special
   cases): source is the union itself with no transforms ⇒ identity; source
   is a literal const ⇒ resolve the winner statically at creation and emit
   only the plausible attempt prefix (constant folding — strictly better than
   today's reorder hack); target union already validated (`io && s === e`) ⇒
   identity.

Deliberately ported behaviors (each gets a spec case so the port is pinned):
`fromDefault` skipping the undefined variant; issue #150 nested-option
ordering (`BS_PRIVATE_NESTED_SOME_NONE` priority); NaN-before-number and
instance/array-before-object priority ordering; `noValidation` interaction
(the literal-in-union `noValidation` dispatch hole from IDEAS.md is rejected
at creation rather than silently miscompiled).

## Measure & iterate (how "better" gets decided)

Implementation shape questions are settled by measurement, not argument. The
loop, run after each candidate lands:

```bash
pnpm spec check --write            # codegen goldens + instantiations + whole-package bundle size
pnpm --filter=sury build:entry     # rebuild S.mjs for the bench import
pnpm --filter=sury bench           # vitest bench (tests/*.bench.ts)
```

**Temporary bench file** `packages/sury/tests/unionnext.bench.ts` (same
harness as `sury.bench.ts`, imports `../src/S.mjs`), covering both sides of
the cost model:

- *Creation/compile (cold):* `unionNext([...])` factory alone;
  `S.parser(makeUnion())` for — 2-variant primitive union, 5-variant
  discriminated object union, literal-heavy enum-like union (10 literals),
  rule 2 (`json → union`), rule 3 (`union → json`), rule 4
  (`optional → nullable`) conversions. Each with an `S.union` baseline where
  the old implementation supports the shape.
- *Runtime (hot):* compiled parse hitting first / middle / last case;
  fallback-heavy input (value that fails N-1 cases before matching);
  miss (aggregated error path); discriminated object dispatch; the two
  existing `sury.bench.ts` union benches replicated on unionNext.

Candidate axes to A/B through the loop (each is a localized emission or
resolution strategy, so variants are cheap to swap):

- **D1 — fallback mechanism:** checks folded into selection conds vs
  `try/catch` routing for borderline cases (e.g. cheap-but-many refinement
  conds); measures runtime on fallback-heavy inputs + bundle delta.
- **D2 — dispatch skeleton:** flat `if/else if` on `typeof` conds vs a
  materialized `typeof` var vs `switch(typeof i)`; measures hot dispatch and
  generated-code size across 2/5/10-variant unions.
- **D3 — literal fusion shape:** `i==="a"||i==="b"` chains vs `switch` on
  the value for enum-like unions past ~N literals; find N empirically.
- **D4 — resolution representation:** integer-mask plan records vs direct
  schema rescans per question; measures cold compile time (`create+parse`)
  and factory allocation.
- **D5 — fallback elision aggressiveness:** always-aggregate vs
  direct-throw-when-provably-terminal; measures error-path cost and codegen
  size on discriminated unions (goldens make the DX difference reviewable).

Decision rule per axis, in repo priority order: (1) DX — goldens must stay
readable and errors precise (spec check diff is the review artifact); (2)
generated-operation runtime, then cold compile time; (3) whole-package
bundle size from `bundleSize.yaml`. Iterate until no axis has a strictly
better candidate; record the losing variants' numbers in the PR description
so the choices are auditable. The temporary bench file is deleted at the end
of the loop; any bench that proved decision-relevant graduates into
`sury.bench.ts`'s union section instead.

## V8 / perf tactics (creation path)

- Every `Val` keeps the canonical field order (reuse `B_next`/`B_refine`/
  `B_scope` exclusively — no new val shapes).
- Plan records monomorphic (one hidden class), attempts stored in plain
  arrays indexed by integers; tag sets are ints (`tagFlags`), so partial
  match, coverage, grouping legality, and fallback elision are `&`/`===` on
  Smis.
- No string-keyed accumulation objects in the emit loop (today's
  `byKey`/`byDiscriminant` go dictionary-mode); string building stays `+`
  (V8 rope strings), conds reuse the memoized `typeofCond` closures.
- No `try/catch` around `parse` at creation (removes hidden exception-path
  cost and the salvage semantics in one move); resolution never calls
  `parse` for skipped/never variants at all, where today every variant pays
  a `B_scope` + narrow + probe.

## Wiring

- `src/unionnext.ts` — the module (factory + resolver + emit).
- `src/entry.ts` — `export { unionNext } from ...` plus a `js_unionNext` in
  `jsapi.ts` mapping raw definitions through `definitionToSchema` (same shape
  as `js_union`).
- `src/S.d.mts`/`S.d.ts` — `unionNext` typed identically to `union`.
- `packages/sury/tests/unionnext.bench.ts` — temporary, deleted after the
  iteration loop (winners graduate to `sury.bench.ts`).
- No `S.res` binding yet (JS-first experimental surface); no changes to
  `optionFactory`/`js_optional`/`js_nullable` until switchover.

## Spec coverage (the deliverable gate)

New `specs/codecnext-*.yaml` authored with `pnpm spec new`, written with
explicit `S.unionNext([...])` spellings (optional/nullable spelled as
`S.unionNext([X, undefined])` / `[X, null]` since the option factories still
build old unions):

1. One spec per row of `CODEC_NEXT_SPEC.md`'s behavior-change table (15 rows)
   asserting the *new* expected outcome, including `creationError` goldens
   for every rejection with the suggested-rewrite text.
2. Mirrors of the 16 already-conformant codec specs, proving parity or better
   codegen (the tracked FIXME codegen bugs — dead `else if` re-test,
   catch-all swallow — must be gone in the unionNext goldens).
3. Plain-union parity specs mirroring `union2`, `union5`,
   `union5-discriminated` — generated code must match or beat the current
   goldens where semantics are unchanged (this is the "generated operation
   most optimal" ratchet), plus **universal-fallback specs** that pin the new
   semantics: refined same-type cases falling through
   (`unionNext([string.with(min,3), number, string])`), decode-error
   fall-through, and a discriminated union showing elided fallback keeps the
   precise per-case error.
4. Both directions everywhere `eq-to-parse` doesn't hold, pinning the
   reverse-symmetry claim of rule 4.

`pnpm spec check --write` after implementation; the metrics summary
(instantiations, codegen, bundle) is part of the commit message.
`bundleSize.yaml` will show a temporary whole-package increase while both
unions ship — called out as expected, netted out at switchover.

## One-shot execution order

1. `unionnext.ts`: identity + masks + offer table (`UN_sameType`,
   `UN_producibleMask`, `UN_offerMask`, `UN_repMask`).
2. Resolver: rule 2 plan builder, rules 3/4 rewrites, all rejection
   messages, acceptance/approximation marking.
3. Emit: `{pre, cond, body}` chain, grouping pass, universal fallback with
   elision, refiner/async join, static shortcuts.
4. Factory + wiring (`entry.ts`, `jsapi.ts`, `S.d.mts`).
5. Author specs (table rows first — they define done), `pnpm spec check
   --write`, iterate until every expected behavior and rejection matches the
   spec doc.
6. Add `tests/unionnext.bench.ts`; run the measure-&-iterate loop over axes
   D1–D5 until no candidate improves; fold winners in, delete the temp
   bench, graduate the useful cases into `sury.bench.ts`.
7. Ported-behavior specs + parity specs, final metrics review, commit with
   the spec-check summary and the bench comparison table.

## Remaining open questions (recommendation first)

1. **Offer/rep table vs probing.** Recommended: the static table (this
   plan). Alternative — deriving representations by symbolically running
   source encoders — avoids the table but reintroduces codegen-time
   discovery, the root cause being removed. The table is ~10 entries and
   fully spec-pinned.
2. **Mixed old/new unions in one pipeline** (old `S.optional` source into a
   `unionNext` target). Recommended: out of scope; old `unionEncoder`
   behavior applies until switchover, and new specs use pure-unionNext
   spellings. Documenting this beats half-supporting it.
3. **Public name.** `S.unionNext` as a temporary documented-as-experimental
   export, removed at switchover when `S.union` adopts the implementation.
