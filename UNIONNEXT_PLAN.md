# unionNext — final one-shot rewrite plan

Implementation plan for a from-scratch union factory (`unionNext`) that behaves
exactly as `CODEC_NEXT_SPEC.md` describes. Delivered in two stages inside one
effort: **Stage 1** builds `unionNext` side-by-side with `S.union` and iterates
until the `codecnext-*` specs meet the spec doc; **Stage 2** replaces the
`S.union` implementation with it, re-derives the existing goldens, and makes
the whole test surface pass (updating tests whose expectations encoded the old
rules).

Non-functional targets, in the repo's priority order:

1. **DX** — every spec rejection happens at operation creation with the
   spec's "Invalid operation … say what you mean" suggestions; runtime
   failures aggregate per-case errors under one union error.
2. **Performance** — resolution and emission are fused into two linear scans
   over primitive integer arrays (no plan-object IR, no Sets, no
   string-keyed dictionaries, no codegen-exception probing); generated-code
   shape is **decided empirically** (see "Measure & iterate"), not by
   intuition.
3. **Bundle** — one new flat module reusing `B_*` helpers; Stage 2 deletes
   `unionDecoder`/`unionEncoder` + the five `unionIs*`/`unionCan*` helpers
   (~350 lines of `composites.ts`), where the net size win is measured.

## Settled semantics: universal case fallback

**Any failure of a case — discriminant miss, refinement failure, or any
decoding error inside the case body — passes the value to the next case.**
Only when no case remains does the union throw, and that error aggregates the
individual case errors (`unionErrors`, as today). This is the uniform rule for
plain validation unions and for every conversion rule below.

Consequences:

- A case's *selection condition* may absorb **all** of its cond-expressible
  checks (type narrows *and* refinements): since failure means "try the next
  case" anyway, `if(typeof i==="string"&&i.length>=3){...}else if(...)` is
  both the semantics and the fastest emission. This also fixes the two known
  bugs from IDEAS.md (fused type+refinement error, and same-type cases with
  different refinements losing dispatch).
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
   the spec forbids. unionNext decides viability from masks *before* calling
   `parse`; builder-time errors propagate out of operation creation
   untouched.
2. **Semantics and codegen are fused with no semantic layer at all.** Which
   variant wins is an emergent property of `byKey`/`byDiscriminant` string
   accumulation, so ordering bugs (`codec-json-union2`'s dead `S.string`
   member, the `else if` re-testing its own else) are unfixable locally.
   unionNext fuses *phases* for speed (below) but keeps the semantic
   decisions expressed as mask predicates that exist independently of any
   emitted string.
3. **Fallback is try/catch-shaped and partial.** `catch(e0){}` swallows
   everything a branch throws, not just the type miss it falls back from
   (`codec-string-union2-transformed` FIXME), while refinement failures in
   hoisted cases *don't* fall back at all. Universal fallback (above) is the
   single rule replacing both behaviors.

## Architecture: fused resolve+emit

Per your call: resolution and emission are **fused** rather than staged
through a plan-object IR — with one structural concession, a reverse
*pre-scan* that only computes integers. Full single-pass fusion is impossible
in principle: grouping legality and fallback elision for case *i* depend on
whether any case *after i* can accept the same values, and the spec's
rejections must be decided from the whole variant list. The cheapest complete
lookahead is a reverse scan accumulating suffix masks — O(n) integer ops, no
allocation beyond a few parallel arrays.

```
unionNext(items)                     — creation: flatten, dedupe, wire decoder/encoder
UN_decoder / UN_encoder:
  pass 1 (forward)  — mask-only rejections (partial match, rule 4 coverage),
                      then per-case attempt vals via plain parse();
                      creation errors propagate uncaught
  pass 2 (reverse)  — ints only: suffix acceptance masks
  pass 3 (forward)  — emit: grouping, fallback, elision decided inline
```

- **Pass 1 (forward)** — rejections that need only tags/formats (partial
  match, rule 4 coverage) fire first as pure mask predicates, before any
  codegen. Then each surviving case's attempt val(s) are built with plain
  `parse()` (`s = source`, `e = case`) — the parse loop runs the source
  encoder then the case decoder (the encoder-as-oracle rule below), and an
  `unsupported_decode` thrown here propagates uncaught: the whole-op
  rejection. Alongside each attempt, store into parallel primitive arrays
  (structure-of-arrays; PACKED_SMI — no plan-object allocation):
  - `mask[i]` — acceptance mask, read off the attempt's emitted type
    narrows (own-tag and/or representation-tag bits),
  - `kind[i]` — dispatch class bit, plus an approximate-acceptance flag
    (refined/structured cases whose accepted set is a strict subset of
    their tags — approximation blocks elision, never enables it),
  - literal consts in one parallel `consts` array for exact comparisons.
- **Pass 2 (reverse, trivial)** — `suffix[i] = mask[i+1] | suffix[i+1]`:
  what any *later* case accepts — the lookahead grouping and elision need,
  in O(n) integer ops.
- **Pass 3 (forward emit)** — stitches the attempt vals into `{pre, cond,
  body}` branches, making the former "plan" decisions inline from the
  arrays: group-merge legality (`mask[i] & interveningMask`), fallback
  elision (`mask[i] & suffix[i]`), literal fusion (consts array).

Readability cost is real and contained: the passes are small named
functions, the mask predicates carry the rule names, and the *readable*
statement of the semantics lives in `CODEC_NEXT_SPEC.md` + the codec specs —
which is where behavior is reviewed anyway.

### Phase 1 — creation: `unionNext(items: Internal[]): Internal`

- 0 items → panic; 1 item → the item itself (as today).
- **Flattening** per spec: a nested union spreads only when it is fully
  transparent — `to === U && parser === U && format === U && refiner === U &&
  inputRefiner === U` (today only `to` is checked; the missing conditions are
  the `codec-union-nested-refined-union` fix). An opaque union stays as one
  variant and type-matches by reference only.
- Dedupe by reference in the same single pass (array scan, no `Set`).
- Wire `decoder = UN_decoder`, `encoder = UN_encoder`, keep `has` exactly as
  today (its consumers — `isOptional`, reverse, jsonschema — stay untouched).
- No other precomputation: scan/emit work belongs to operation creation,
  which `getDecoder` already caches per schema pair, so creation stays O(n)
  with zero closures.

### Type identity (spec "same type")

```ts
UN_sameType = (a, b) =>
  a === b ||
  (a.type === b.type &&
    !flagUnsafeHas(tagFlags[a.type], tagFlagRef | tagFlagUnion) && // opaque ⇒ reference only
    a.class === b.class &&      // instances
    a.format === b.format);     // int32 vs number, json-string vs string
```

`S.json` is `refTag` ⇒ opaque ⇒ matches only itself. `S.unknown` matches only
`unknown`. A source variant matches by `getOutputSchema(variant)`, a target
variant by the variant itself (its input). A variant whose matching side is
`never` is skipped everywhere: no exception triggering, no coverage counting,
no emitted branch where it is unreachable (`never.with(S.to, X)` as target);
a reachable `X.with(S.to, never)` arm still compiles to its explicit
rejection branch.

### Gap-fill routing: the source encoder is the representation oracle

**Settled (your call): no static table.** The general rule — *if the encoder
fails, fall to the next case; if it passes, continue with the decoder* —
turns out to map 1:1 onto machinery that already exists. An attempt for case
`C` is one ordinary `parse()` of a val with `s = source, e = C`: the parse
loop already runs `source.encoder(input, C)` first when the source has one,
then `C.decoder`. unionNext adds only ordering, cond extraction, and
fallback stitching around plain `parse` calls.

Worked example — `S.json.with(S.to, S.union([S.bigint, S.string]))`.
`jsonEncoderFn` today (formats.ts):

```
target tag                       jsonEncoderFn emits
──────────────────────────────   ─────────────────────────────────────────────
string|number|boolean|null       narrow to that tag  (as-is: no re-typing)
undefined|nan                    nullLiteral().to(target)   (null is the rep)
array / object                   isArray / object narrow, items become json
union|ref                        input unchanged (the union drives per-case)
else (bigint, instance, …)       string().to(target)        (string is the rep)
```

So per case, at runtime:

```
value        bigint case: typeof i==="string" → BigInt(i)     string case: typeof i==="string"
"123"        cond ✓ → 123n                                    (not reached)
"abc"        cond ✓ → BigInt throws → NEXT CASE               cond ✓ → "abc"
123          cond ✗ (encoder narrow failed) → NEXT CASE       cond ✗ → no case → throw
true         cond ✗ → NEXT CASE                               cond ✗ → no case → throw
```

The decisive property: `bigintDecoder` *can* decode numbers (`BigInt(123)` —
rule 1 uses it for `S.number.to(S.bigint)`), yet JSON `123` must throw. No
table row encodes that — `jsonEncoderFn` does, by emitting a string narrow
for the bigint target. "Encoder failed" is simply that narrow failing: at
runtime it's a cond miss routing to the next case (universal fallback); at
creation, an `unsupported_decode` thrown while building an attempt
propagates uncaught and rejects the whole operation (the spec's
variant-undecodable rule — `boolean → union([string, symbol])` dies on the
symbol attempt). A case with no buildable attempt at all rejects the same
way, automatically — no reachability bookkeeping needed.

**Why this is future-proof:** adding or changing a coercion means editing
the *source schema's encoder* — the same single place rule 1 already reads —
and every union conversion picks it up. Nothing to keep in sync, which was
the table's fatal flaw.

**`S.unknown` needs the same oracle.** This is what reconciles two goldens
that otherwise contradict:

- `union2.yaml` (`S.union([S.string, S.number])`, parse) must stay pure
  validation — `42` must not become `"42"`.
- `codec-unknown-union2` (`S.unknown.with(S.to, S.union([S.bigint,
  S.string]))`) must coerce `"123" → 123n` "as in codec-json-union2".

Bare decoders-from-unknown only emit typeof narrows, so with no oracle the
second golden is unreachable; a naive "offer everything" oracle breaks the
first. The resolution: unknown uses the *JSON representation model* —
JSON-native tags (string, number, boolean, null, object, array) are as-is
only; non-JSON types (bigint, instance, …) additionally get their
string/null representation attempt. Concretely, a case gets up to two entry
paths: **as-is** (own tag ∈ producible(source), pure narrow + case pipeline)
and **representation** (the shared rep encoder, extracted from
`jsonEncoderFn`'s dispatch). For json only one path ever exists (its encoder
picks); for unknown a bigint case gets both (`typeof i==="bigint"` pass, and
`typeof i==="string"` → BigInt attempt); for single-tag sources the source
tag itself is the only representation, offered directly to the case decoder.

Implementation caution: the rep encoder is **not** installed as
`unknown.encoder` — the shared `unknown` constant heads every compiled
pipeline, so a global encoder would fire on all `s.encoder` checks in the
parse loop and perturb non-union codegen. unionNext invokes the shared rep
helper internally when `source.type === unknownTag`. `jsonEncoderFn` gets
audited during implementation to confirm it rejects wrong-representation
inputs at the granularity the rule needs (the codecnext specs pin every
cell of the tables above).

One user-visible consequence to be aware of: `S.parser(schema)` compiles as
`unknown.to(schema)` (`js_parser = getDecoder(unknown, ...)`), so plain
parsing of a union containing a non-JSON type inherits rule 2's coercion —
`S.parser(S.union([S.bigint, S.string]))("123")` yields `123n`. That is
exactly what `codec-unknown-union2`'s expected golden asserts (its parse op
*is* this chain), and JSON-native unions like `union2` are untouched — but
it's a semantics change for plain parsers of bigint/instance-bearing unions,
worth an explicit spec case and a line in the docs update.

### The four rules as scan predicates

Let `M` = non-never target variants with `UN_sameType(source, variant)`.

- **Rule 1 (non-union → non-union)** — not unionNext's job; the parse loop
  already composes `source.encoder → target.decoder`. unionNext only ever
  delegates to it per variant.
- **Rule 2 (non-union → union)** — in `UN_decoder` when `input.s` is not a
  transparent union:
  - `0 < |M| < |variants∖never|` ⇒ `invalid_operation` with the spec's two
    suggested rewrites in the message.
  - `M = all` ⇒ ordered attempts from the as-is source value (transformed
    variants run their own `.to`); universal fallback chains them.
  - `M = ∅` ⇒ gap decoding: each case gets its entry paths per the
    encoder-as-oracle rule — as-is when its tag ∈ producible(source),
    representation via the source's encoder otherwise (or additionally, for
    `unknown`). Definition order + universal fallback (yields
    `codec-number-union2-int32`: int32 range-check then `""+i`;
    `codec-json-union2`: literal, BigInt attempt, string catch-all;
    `codec-unknown-union2`: "123" → 123n via the unknown rep oracle).
  - Reachability is automatic: a case whose every attempt throws
    `unsupported_decode` at creation rejects the whole operation (the
    throw propagates — there is no catch). No dropping, no per-value throw
    branch, no always-throwing op — the three `codec-bool-union2*` /
    `codec-union2-string-unsupported` rows.
- **Rule 3 (union → non-union)** — in `UN_encoder`: match each source
  variant's output type against the target. Partial ⇒ `invalid_operation`
  with the two rewrites. Otherwise **rewrite**: source union with
  `variant.to(target)` appended per variant (the `unionPerVariantVal` trick,
  kept — rule 3 literally becomes the rewrite its error suggests), then
  `UN_decoder` compiles the dispatch. A variant pair without a built-in
  decoder rejects the whole operation here, because accepted plans run
  `parse` with no salvage catch.
- **Rule 4 (union → union)** — in `UN_encoder` when the target is a
  transparent union: for each source variant (by output type), the *first*
  same-type target variant in definition order; unmatched `null`/`undefined`
  bridge to the opposite nullish variant even if that one already has a
  same-type match (runtime same-type wins; the bridge branch exists only
  where there is no same-type source). Coverage must hold both ways over
  non-never variants, else `invalid_operation` naming the uncovered variant.
  Rewrite per source variant: matched plain target ⇒ pass-through (type
  check only — exhaustiveness preserved by the final `else`); matched
  transformed target ⇒ append that variant's `.to` pipeline; bridge ⇒ const
  swap (`i=null` / `i=void 0`). Matching is a pure function of (source
  variants by output, target variants by input); `reverse` maps those sets
  into each other, so compiling the reversed schema yields the mirrored
  plan — the spec's "reversing doesn't re-run the checks" holds observably,
  pinned by specs in both directions.

### Emit (forward walk)

Branch IR anticipated by the `B_isHoistable` comment — `{pre, cond, body}`:

- `pre` — producer statements a discriminant reads (`let v0=+i;`), emitted
  *before* the cond inside the owning branch of the outer chain, never
  hoisted across it. Dissolves the `str->to(option(int))` bug class by
  construction.
- `cond` — the case selection condition: type narrows *plus* every
  cond-expressible check of the case (universal fallback makes refinements
  selection criteria, not committed asserts), built from `typeCheckCond`
  atoms, literal `===` conds, and lifted `Check.c` conds (reusing
  `B_merge(~hoistCond)` without the type-narrow-only partition).
- `body` — the case pipeline merged from its val chain (`parse` on a val
  with `e = variant`, `s` = tag-narrow of the group, `u = true`), including
  object-literal discriminant hoisting via `B_hoistChildChecks` so
  discriminated-union codegen keeps its `i["kind"]==="a"` shape.

Emission rules (decisions read straight off the scan arrays):

1. **Grouping.** A case merges into an earlier same-class group iff no case
   between them intersects its acceptance (mask `&`, literal consts exact,
   approximate acceptance counts as intersecting). Same-tag literals fuse
   into `typeof i==="string"&&(i==="a"||i==="b")`; an illegal hoist (string
   catch-all past bigint-from-string) stays in its definition slot; a
   `typeof` tested in more than one slot materializes once into a var.
2. **Fallback routing (universal).** A non-final case whose failure points
   are all `Check`s folds them into its selection cond. Bodies with embedded
   throwing code get `try{}catch(eN){ <next candidate> }`; a terminal catch
   feeds `eN` into the aggregated union error, never swallows.
3. **Fallback elision.** `mask[i] & suffix[i] === 0` ⇒ no later case can
   accept ⇒ body failures emit as direct throws with precise errors (the
   discriminated-union fast shape). Decided per failure point from the
   arrays, never by probing.
4. **Exhaustive close.** Final `else` throws the aggregated union error via
   one embedded fail fn (`expected` = the union, collected `eN` case errors
   as `unionErrors`).
5. **Refiners.** Per CLAUDE.md `B_markOutput` semantics: the union's
   `inputRefiner` checks attach once to the pre-dispatch val; `refiner`
   wraps the *joined* output val once after the chain (not per-case as the
   current `appendUnionRefiners` FIXME does). Async output checks run inside
   `.then` on the resolved value, never on the Promise.
6. **Async join** as today: joined val async when any branch is; sync
   results unify through `Promise.resolve` only when needed.
7. **Static shortcuts** (replacing tier-1/self-decode special cases): source
   is the union itself with no transforms ⇒ identity; literal-const source ⇒
   resolve the winner statically and emit only the plausible attempt prefix;
   already-validated target (`io && s === e`) ⇒ identity.

Deliberately ported behaviors (each pinned by a spec case): `fromDefault`
skipping the undefined variant; issue #150 nested-option ordering
(`BS_PRIVATE_NESTED_SOME_NONE` priority); NaN-before-number and
instance/array-before-object priority; `noValidation` interaction (the
literal-in-union `noValidation` dispatch hole from IDEAS.md is rejected at
creation rather than silently miscompiled).

## Measure & iterate (how "better" gets decided)

Implementation shape questions are settled by measurement. The loop, run
after each candidate lands:

```bash
pnpm spec check --write            # codegen goldens + instantiations + whole-package bundle size
pnpm --filter=sury build:entry     # rebuild S.mjs for the bench import
pnpm --filter=sury bench           # vitest bench (tests/*.bench.ts)
```

**Temporary bench file** `packages/sury/tests/unionnext.bench.ts` (same
harness as `sury.bench.ts`, imports `../src/S.mjs`):

- *Creation/compile (cold):* `unionNext([...])` factory alone;
  `S.parser(makeUnion())` for — 2-variant primitive union, 5-variant
  discriminated object union, literal-heavy enum-like union (10 literals),
  rule 2 (`json → union`), rule 3 (`union → json`), rule 4
  (`optional → nullable`) conversions. Each with an `S.union` baseline where
  the old implementation supports the shape.
- *Runtime (hot):* compiled parse hitting first / middle / last case;
  fallback-heavy input (fails N-1 cases before matching); miss (aggregated
  error path); discriminated object dispatch; the two existing
  `sury.bench.ts` union benches replicated on unionNext.

Candidate axes to A/B through the loop:

- **D1 — fallback mechanism:** checks folded into selection conds vs
  `try/catch` routing for borderline cases; runtime on fallback-heavy inputs
  + bundle delta.
- **D2 — dispatch skeleton:** flat `if/else if` on `typeof` conds vs a
  materialized `typeof` var vs `switch(typeof i)`; hot dispatch + code size
  across 2/5/10-variant unions.
- **D3 — literal fusion shape:** `i==="a"||i==="b"` chains vs `switch` on
  the value past ~N literals; find N empirically.
- **D4 — scan representation:** parallel Smi arrays vs direct schema rescans
  per question; cold compile (`create+parse`) and factory allocation.
- **D5 — fallback elision aggressiveness:** always-aggregate vs
  direct-throw-when-provably-terminal; error-path cost + codegen size on
  discriminated unions (goldens make the DX difference reviewable).

Decision rule per axis, in repo priority order: (1) DX — goldens readable,
errors precise (spec check diff is the review artifact); (2)
generated-operation runtime, then cold compile time; (3) whole-package
bundle size from `bundleSize.yaml`. Iterate until no axis has a strictly
better candidate; record losing variants' numbers in the PR description. The
temporary bench file is deleted at the end; decision-relevant benches
graduate into `sury.bench.ts`'s union section.

## V8 / perf tactics

- Every `Val` keeps the canonical field order (`B_next`/`B_refine`/`B_scope`
  exclusively — no new val shapes).
- Scan data as parallel primitive arrays (PACKED_SMI), decisions as `&`/
  `===` on Smis; no plan-object allocation at all.
- No string-keyed accumulation objects in the emit loop (today's
  `byKey`/`byDiscriminant` go dictionary-mode); string building stays `+`
  (rope strings); conds reuse memoized `typeofCond` closures.
- No `try/catch` around `parse` at creation; `parse` never runs for
  skipped/never variants, where today every variant pays a `B_scope` +
  narrow + probe.

## Stage 1 — unionNext until codecnext meets the spec

Wiring:

- `src/unionnext.ts` — the module (factory + fused scan/emit).
- `src/entry.ts` — `export { unionNext }`; `js_unionNext` in `jsapi.ts`
  mapping raw definitions through `definitionToSchema` (same shape as
  `js_union`); `S.d.mts`/`S.d.ts` typing identical to `union`.
  `S.unionNext` is explicitly temporary — it exists only until Stage 2.
- `packages/sury/tests/unionnext.bench.ts` — temporary (deleted at loop
  end).
- No `S.res` binding; no changes to `optionFactory`/`js_optional`/
  `js_nullable` yet. Mixed old/new unions in one pipeline are out of scope
  during Stage 1 (old `unionEncoder` behavior applies; new specs use
  pure-unionNext spellings) — the question dissolves at Stage 2.

Spec gate — new `specs/codecnext-*.yaml` authored with `pnpm spec new`,
using explicit `S.unionNext([...])` spellings (optional/nullable spelled
`S.unionNext([X, undefined])` / `[X, null]`):

1. One spec per row of `CODEC_NEXT_SPEC.md`'s behavior-change table (15
   rows), including `creationError` goldens with the suggested-rewrite text.
2. Mirrors of the 16 already-conformant codec specs, proving parity or
   better codegen (the tracked FIXME bugs — dead `else if` re-test,
   catch-all swallow — must be gone).
3. Plain-union parity specs mirroring `union2`, `union5`,
   `union5-discriminated` — match or beat current goldens where semantics
   are unchanged — plus **universal-fallback specs**: refined same-type
   cases falling through (`unionNext([string.with(min,3), number,
   string])`), decode-error fall-through, and a discriminated union showing
   elided fallback keeps the precise per-case error.
4. Both directions wherever `eq-to-parse` doesn't hold (rule 4 reverse
   symmetry).

Stage 1 exit: every codecnext golden matches the spec doc, bench loop done.
`bundleSize.yaml` shows a temporary whole-package increase while both unions
ship — called out as expected in the Stage 1 commit.

## Stage 2 — switchover: `S.union` becomes unionNext

1. Point `unionFactory` at the unionNext implementation so every internal
   builder routes through it: `js_union`, `js_optional`, `js_nullable`,
   `nullish`, `enum`, `optionFactory`/`nestedOption`, `valGet`'s dict-key
   option, the `S.json` def, `operations.ts`'s output-union rebuilds.
2. Delete the old cluster: `unionDecoder`, `unionEncoder`,
   `unionIsSelfDecodeNoop`, `unionIsWiderSchema`, `unionGetToPerCase`,
   `unionCanDispatchPerVariant`, `unionIsPriority` (and `unionToKey` if the
   new emit doesn't reuse it). This is where the bundle-size win lands.
3. Re-derive all goldens: `pnpm spec check --write` — the 15
   behavior-change rows change per the spec table (their `FIXME: Codec next
   expects:` notes come off), the 16 conformant specs must stay flat or
   improve, all non-codec specs (`union2`, `union5`, `union5-discriminated`,
   `object*`, option/nullable-touching specs) must stay flat or improve.
4. Full gates green: `pnpm test` (the 107 ReScript test files via vitest —
   `S_union_test.res`, `S_option_test.res`, `S_nullable_test.res`,
   `S_to_test.res`, `S_union_optionInObject_test.res`,
   `S_union_nestedOptionAndVariant_test.res` are the likely hot spots),
   `pnpm --filter=e2e test` (genType/ppx), `pnpm compliance` (JSON-Schema
   suite), `pnpm lint:deadcode`. Tests whose expectations encoded old rules
   (dropped-variant salvage, non-fallback refinements, partial-match
   acceptance) are updated to the spec semantics — each update cites the
   spec rule it follows.
5. Fold `codecnext-*` back: rows that mirror an existing `codec-*` spec are
   deleted in favor of the (now-correct) original; net-new coverage
   (universal fallback, creation errors) is renamed into `codec-*`.
   `docs/*-usage.md` union/conversion sections update per the spec doc's
   "this file replaces it once the implementation lands".
6. Remove the temporary `S.unionNext`/`js_unionNext` export and d.ts entry;
   final metrics summary (instantiations, codegen, bundle — now net of the
   deleted old cluster) goes in the commit/PR.

## Execution order

1. `unionnext.ts`: identity + masks (`UN_sameType`, `UN_producibleMask`);
   extract the shared representation encoder from `jsonEncoderFn`'s dispatch
   and audit that it rejects wrong-representation inputs at the granularity
   the oracle rule needs; wire the unknown-source rep path (internal helper,
   NOT `unknown.encoder`).
2. Pass 1: mask-only rejections (rules 2–4 partial/coverage), per-case
   attempt vals via plain `parse()`, acceptance masks off the emitted
   narrows, approximation flags.
3. Pass 2 + Pass 3: suffix masks; `{pre, cond, body}` emit, grouping,
   universal fallback with elision, refiner/async join, static shortcuts.
4. Factory + Stage 1 wiring.
5. Author codecnext specs (behavior-change table first — it defines done),
   including the plain-parser coercion consequence case
   (`S.parser(S.union([S.bigint, S.string]))("123") → 123n`);
   `pnpm spec check --write`; iterate to conformance.
6. Bench loop over D1–D5; fold winners; delete temp bench.
7. Stage 2: switchover steps 1–6 above.

## Settled decisions

- **Fallback**: universal — discriminant, refinement, or any decoding
  failure passes the value to the next case; provably-terminal fallbacks
  compile to direct precise throws.
- **Gap-fill routing**: no static table — the source encoder is the
  representation oracle (encoder failed → next case; encoder passed →
  continue with decoder); `unknown` reuses the JSON representation model
  via a shared internal helper.
- **Rollout**: Stage 1 ships `S.unionNext` until the codecnext specs meet
  the spec doc; Stage 2 replaces `S.union`'s implementation, deletes the
  old cluster, re-derives goldens, greens the full test surface, and
  removes the temporary export.
- **Implementation shape** (dispatch skeleton, literal fusion, elision
  aggressiveness, …): decided by the measure-&-iterate loop, not upfront.
