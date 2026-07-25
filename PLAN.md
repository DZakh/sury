# Union `.to` rules — one-go rewrite plan

> Working document for the `claude/codec-specs-s-to-rules-*` branch. Delete before merge.
> Replaces the 7-phase iterative plan: each phase mutates a site of `composites.ts` that is
> entangled with the others (activeKey feeds the skip loop, byKey feeds `getArrItemsCode`,
> `unionEncoder` branches on `unionIsWiderSchema`), so every intermediate state is broken.
> Instead: fix the contract, encode it as specs/tests, land the implementation as one change
> verified by the spec ratchet.
>
> Every "today" claim below is **verified empirically** against the built `src/S.mjs`
> (probes in the session scratchpad), not read off the source.

## 0. The contract

### 0.1 Match key

`matchKey(schema) = (tag, class-reference, ref-identity)`.

- **No `format`.** `int32` and `number` share a key. (Verified counterexample:
  `S.int32 → S.union([S.int32, S.number])` works today; with format in the key the plain
  `number` variant becomes an uncovered leftover and a working schema starts erroring. Worse,
  `S.int32 → S.union([S.string, S.number])` would drop to the coercion regime where *target
  order* silently decides `5` vs `"5"`.) `format`/`const` keep their existing job: **ordering
  inside a same-key group** (const-refined, then format-refined, then catch-all).
- **No `const`** either, same reason — the const-priority reorder (composites.ts:975) already
  handles it, at the right granularity.
- **`class` by reference**, unlike `unionToKey`. Two distinct classes both named `Box` must not
  pair. (Today they do, producing a *runtime* "Expected Box | string" failure; by-reference
  turns it into a creation-time error.)
- `unionToKey` (composites.ts:507) is untouched: it stays the *runtime dispatch* key
  (typeof-discriminable, class by name). Match key is the *compile-time pairing* key. Two keys,
  two jobs — do not conflate them.

Source variants are keyed by their **output** schema (`getOutputSchema`); target variants by
their own **input** type. A non-union counts as a one-variant side.

**Exempt from coverage counting:** `never`-keyed target variants; source variants whose output
is `never`; `unknown`/`union`/`ref`/`json`-keyed target variants (catch-alls — reachable from
any source, so never "missed"); the `undefined` variant skipped under `fromDefault`
(composites.ts:1003). A source variant `S.never.with(S.to, X)` is a normal source variant with
`matchKey(X)` — it **provides** coverage for target `X` while never matching at runtime. That is
the whole never-recipe, and it already works mechanically today (verified: output type of
`S.union([S.string, S.never.with(S.to, S.number)])` is `string | number`; encoding `5` throws
"Expected never").

### 0.2 Destination ranks

Each source variant resolves to a destination by the first applicable rank:

- **R1 — same key.** The group of target variants sharing its key, tried in group order.
- **R2 — nullish bridge** (*the union special case*). Source variant is a nullish const, no R1
  group exists, and the opposite nullish const is a target → that variant, **exclusively**.
  Total, lossless, symmetric. This is today's tier 2, kept as a rule.
- **R3 — coercion.** No key match: every target group is compiled and tried in target order
  (today's tier 3).

### 0.3 Coverage, per arity

| Arity | Regime |
|---|---|
| **Rule 2** — one source → union target | R1 (all-or-error) → R2 (exclusive) → R3 |
| **Rule 3** — union source → one target | symmetric: all variants' output keys equal the target key → per-variant pass-through; some but not all → error; none → R3 per variant |
| **Rule 4** — union → union | R1 pairing + R2 for leftovers. **No R3.** Any uncovered variant on either side → error |

**Coverage rule (R1 regime).** If any source variant resolves at R1, every non-exempt target key
must be the destination of some source variant. Many source variants → one target key is fine;
one source variant cannot cover two target keys (it produces one value). R2 resolutions carry no
coverage obligation — see the wart in §0.5.

**Why rule 4 has no R3:** with a union on both sides, "try every target for every source" is a
combinatorial guess. It is what produces today's unreadable flagship codegen (nested
`try{BigInt(i)}catch{+i}catch{i==="null"}`) and it is why the flagship conversion becomes an
error. With one side singular the trial set is small and legible, so R3 survives there.

### 0.4 The error is the feature

Invalid operation = `panic` (plain `Error`, schema.ts:56), at operation-creation time. Message
renders both derived shapes (`toExpression`), which variants matched, which didn't, and the
rewrite template for the *actual* failure mode:

- uncovered **target** variant `T` → `S.never.with(S.to, <T>)` added to the source union
  (or `S.union([<source>, S.never.with(S.to, <T>)])` when the source is singular);
- uncovered **source** variant `s` → `<s>.with(S.to, <T>)`, one line per unmatched target key.

### 0.5 Known wart, accepted

R1 demands coverage, R2 does not. So `S.schema(null) → S.union([S.string, undefined])` bridges
silently (R2), while the near-identical `S.schema(null) → S.union([S.schema(null), S.string])`
errors (R1 + uncovered `string`). This is the price of keeping today's tier-2 behavior, which
is a deliberate decision (§0.6 D1). The strict alternative — R2 also requires coverage, making
row 4 an error with the never-recipe as migration — is a one-line change to the resolver if the
wart ever bites.

### 0.6 Decisions

- **D1 — resolved (user).** The `string ↔ null` codec **keeps** its coercion:
  `S.schema(null).with(S.to, S.string)` → `"null"` and `S.string.with(S.to, S.schema(null))`
  parses `"null"` stay as they are. No primitives change. The earlier recommendation to delete
  the nullish stringified-const branches is **rejected** — unnecessary once the union gets R2,
  and it would have killed a working codec. The union special case is R2, ranked above R3.
- **D2 — resolved (user).** `S.unit → S.union([null, undefined])` is a **compile error**
  (1 of 2 target keys matched → partial). Its price, stated plainly: any *exact match plus
  leftover nullish target* now errors, which includes widening into an optional —
  `S.string.with(S.to, S.optional(S.string))` errors where today it "works". That is fine, and
  arguably a fix: today's encode side of exactly that schema turns `undefined` into the **string
  `"undefined"`** (verified), i.e. today's widening silently corrupts. No nullish exemption is
  possible — it would un-error D2 itself, since the two cases have the same shape.
- **D3 — keep.** Explicit const→const `.to` stays. It needs **no work**: `literalDecoder`'s
  mismatched-const branch already emits both directions (verified: `null → void 0`,
  `undefined → null`). **Phase 3 (rule 1) is therefore already implemented** — it collapses to
  "spec it, don't build it", and R2's codegen reuses this exact path (`try{i=void 0}`).
- **D4 — revised.** Class by reference: **yes**. Format in the key: **no** (§0.1).

## 1. Behavior matrix — every row verified against today

| # | Conversion | Today (verified) | New | Mechanism |
|---|---|---|---|---|
| 1 | `S.string → union([string, number])` | `"123"` → `"123"` (identity wins) | **error** | R1 partial |
| 2 | `S.string → union([string.to(number), string])` | — | works | R1 group, refined first |
| 3 | `S.bool → union([string, float])` | `true` → `"true"` | unchanged | R3 |
| 4 | `S.schema(null) → optional(string)` | `null` → `undefined`, string branch not compiled | unchanged | **R2** |
| 5 | `S.schema(undefined) → union([null, undefined])` | `undefined` → `undefined` | **error** (D2) | R1 partial |
| 6 | `union([bigint, number, null]) → union([string, undefined])` (flagship) | works, 5-branch nested codegen | **error** | rule 4, no R3 |
| 6b | migration: `union([bigint.to(string), number.to(string), null])` → same target | — | works | R1 ×2 + R2 leftover |
| 7 | `union([string, number]) → string` | `5` → `"5"` | **error** | rule 3 partial |
| 7b | migration: `union([string, number.to(string)]) → string` | works | unchanged | rule 3 all-match |
| 8 | `union(["a","b"]) → string` | works | unchanged | rule 3 all-match |
| 9 | `optional(string) → string` | `undefined` → **`"undefined"`** (string!) | **error** | rule 3 partial |
| 10 | `union([bigint, number]) → string` | both `""+i` | unchanged | rule 3, none → R3 |
| 11 | `S.int32 → union([int32, number])` | `5` → `5` | unchanged | R1 group (needs format **out** of key) |
| 12 | `instance(Box) → union([instance(OtherBox-same-name), string])` | **runtime** "Expected Box \| string" | creation-time error | key by reference |
| 13 | `union([string, never.to(number)])` | output type `string \| number`; encode `5` throws | unchanged | never-recipe |
| 14 | `S.string → optional(string)` | parse ok; **encode `undefined` → `"undefined"`** | **error** | D2's price |

Rows that stay valid become specs; rows that become errors are tests (specs can't express
creation-time throws) asserting the full message including both rewrite templates.

## 2. Architecture

**Matching core** (composites.ts, beside `unionToKey`):
- `matchKeyEq(a, b)` — tuple compare, no string building.
- `resolveConversion(sourceVariants, targetVariants, arity)` → per-source-variant destination
  (`group(targets)` | `bridge(target)` | `coerceAll`) or panic. Direction-agnostic, so
  decode-of-schema ≡ encode-of-reverse by construction. Rules 2/3/4 are the three arities of
  this one function.
- Group ordering made explicit (const-refined → format-refined → catch-all) instead of emergent
  from byKey insertion order.
- The invalid-operation renderer (§0.4).

**`unionFactory`** (composites.ts:1247): implement the flatten TODO — spread a nested union only
when it carries no `format`/`parser`/`to`/`refiner`/`inputRefiner` (today only `to` is checked,
so spreading silently drops a nested union's refiner). Gives the flattening rule everywhere and
normalizes what the resolver sees.

**`unionDecoder`** (composites.ts:622): replace the activeKey block (649-679) with the resolver
verdict; the nullish-bridge arm (672-678) becomes R2, driven by the resolver rather than by
"tier 1 was empty". Delete the `unionIsWiderSchema` self-decode acceptance (635-637). "All
match" keeps `activeKey = sourceKey`, "none" keeps `""`, so the skip loop at 1001 and everything
downstream — const reorder, byKey grouping, `unionIsPriority`, `appendUnionRefiners`, the whole
`getArrItemsCode` machine — is untouched.

**`unionEncoder`** (composites.ts:606): drop the `unionIsWiderSchema` branch; take the rule-3/4
verdict; dispatch via `unionPerVariantVal`. Lift `unionCanDispatchPerVariant`'s
transformed-variant bail-outs (581-586) — the migrations *are* transformed variants (rows 6b,
7b). Keep the ref/json bail-outs. Deepest work item: dispatch narrow comes from the variant's
**input** tag, pairing from its **output** key.

**Primitives:** no change (D1, D3).

**Error channel:** no marker needed. `panic` throws a plain `Error` and `getOrRethrow` rethrows
non-SuryErrors, so the per-variant catch at composites.ts:779 already lets it escape. The one
swallow-everything site is `catch (_)` at composites.ts:1090 — make it rethrow non-SuryErrors.
Other compile-path catches (parse.ts:98,120; operations.ts:196,505; jsonschema.ts:215;
jsapi.ts:42) are all `getOrRethrow`-based; verified safe by inspection, to be confirmed by the
error-message tests.

**Deletions:** `unionIsWiderSchema`, the tier-keyed bridge arm. Net `S.mjs` should shrink (tier
logic and widening checks go; the resolver is creation-time-only) — but it still ships, so hold
the result to the `bundleBytes` ratchet rather than to the assumption.

## 3. Execution order (one change)

1. **Docs first — the contract of record.** Rewrite docs/js-usage.md §"Decoding into / out of a
   union" (873-912) as §0. The three-tier prose and the flagship worked example are the *old*
   spec; the flagship becomes the invalid-operation showcase with its real error text and
   migration. (The branch premise is "specs match the S.to rules in js-docs" — today those rules
   exist nowhere in the repo, so the docs are step 1, not step 8.)
2. **Red first.** Specs for the valid rows (2,3,4,8,10,11,13 + 6b,7b) via `pnpm spec new`;
   tests in S_to_test.res:889-1055 rewritten for the error rows (1,5,6,7,9,12,14), asserting the
   full message.
3. Matching core + `unionFactory` flatten + error renderer.
4. `unionDecoder` rewiring + `catch(_)` fix.
5. `unionEncoder` rewiring + bail-out lift.
6. **Internal-consumer audit.** Full suite, then specifically:
   `optionFactory`/`nestedOption`/`nestedNone`/`fromDefault`, `S.nullish`, recursive/ref paths,
   async variants. These build unions with nullish variants and reverse them — the most likely
   sources of new partial-verdict panics. Fix machinery with explicit variants; never by
   weakening a rule.
7. `pnpm spec check --write`; **review the golden diff as the deliverable** — codegen,
   `instantiations`, `bundleBytes` flat-or-better; call out anything unavoidable.

## 4. Breaking changes (release notes; repo has no CHANGELOG file)

- **Identity-wins is an error.** A typed source meeting a union with a mix of same-key and
  other-key variants no longer silently picks the same-key variant (row 1).
- **Union widening is an error**, including widening into an optional (rows 9, 14) — migrate with
  a `S.never.with(S.to, …)` source variant. Note this *removes a corruption*: widening currently
  encodes `undefined` to the string `"undefined"`.
- **Union→union requires key coverage** (no coercion guessing): the flagship
  `union([bigint, number, null]) → union([string, undefined])` is now a creation-time error whose
  message contains its own migration (row 6b).
- **Nullish is no longer interchangeable by accident** (row 5): `undefined → null | undefined`
  errors instead of quietly keeping `undefined`.
- **Distinct classes with the same name no longer pair** — a runtime failure becomes a
  creation-time one (row 12).
- Unchanged and explicitly preserved: the `string ↔ null` codec, the nullish bridge inside
  unions (row 4), explicit const→const `.to`, and `int32`/`number` grouping (row 11).

## 5. Risks

| Risk | Why | Mitigation |
|---|---|---|
| option/nullish/default internals hit new panics | they build nullish-variant unions and reverse them; D2 makes those shapes strict | step 6 audit; fix machinery, not rules |
| transformed-variant per-variant dispatch | the lifted bail-outs existed for a reason; reversed chains change variant input tags | rows 2/6b/7b/13 as specs, incl. async |
| the §0.5 wart surfaces in real code | R1 demands coverage, R2 doesn't | documented; strict variant is a one-liner |
| coverage-exempt catch-all targets | `unknown`-keyed target variants next to an R1 match are exempt from counting yet not compiled | pin with the existing unknown-source test (S_to_test.res:1057) + a new exact-match-plus-unknown case |
| bundle ratchet | resolver ships in `S.mjs` | spec gate; reuse `toExpression`, keep the renderer lean |
