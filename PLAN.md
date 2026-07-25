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

`matchKey(schema) = (tag, format, class-reference, ref-identity)`.

- **`format` is part of the key.** `int32 ≠ number`, `port ≠ number`, `email ≠ string`,
  `date-time ≠ string`, `json ≠ string`, `compactColumns ≠ array`. A format-refined schema and
  its base type never pair, in either direction. This is what makes
  `S.int32 → S.union([S.string, S.number])` produce `"5"`: no key matches, so the coercion
  regime applies and target order decides.
- **`class` by reference**, unlike `unionToKey`. Two distinct classes both named `Box` must not
  pair. (Today they do, producing a *runtime* "Expected Box | string" failure; by-reference
  turns it into a creation-time error.)
- **`const` is not in the key.** Consts of the same tag share a key and form one group; the
  existing const-priority reorder (composites.ts:975) orders them inside it.
- `unionToKey` (composites.ts:507) is untouched: it stays the *runtime dispatch* key
  (typeof-discriminable, class by name, format-blind). Match key is the *compile-time pairing*
  key. Two keys, two jobs — do not conflate them.

Source variants are keyed by their **output** schema (`getOutputSchema`); target variants by
their own **input** type. A non-union counts as a one-variant side.

**Exempt from coverage counting:** `never`-keyed target variants; source variants whose output is
`never`; `unknown`/`union`/`ref`/`json`-keyed target variants (catch-alls — reachable from any
source, so never "missed"); the `undefined` variant skipped under `fromDefault`
(composites.ts:1003). A source variant `S.never.with(S.to, X)` is a normal source variant with
`matchKey(X)` — it **provides** coverage for target `X` while never matching at runtime. That is
the whole never-recipe, and it already works mechanically today (verified: output type of
`S.union([S.string, S.never.with(S.to, S.number)])` is `string | number`; encoding `5` throws
"Expected never").

### 0.2 Destination of a source variant

Ranked, first hit wins:

1. **Equal key** — the group of target variants with the same match key. Ordered: const-refined
   first (existing reorder), then target order.
2. **Nullish pair** — the source variant is a nullish const and no equal-key target exists: the
   opposite nullish const target.
3. **No destination.**

The ranking matters: without it, `union([null, undefined]) → union([undefined, null])` — a noop
today (verified) — would see each source variant match two distinct target keys and break.

A nullish pair is a **match like any other**: it carries the same coverage obligation as an
equal-key match (§0.3). It is not a coverage-exempt escape hatch.

### 0.3 Verdict — one rule, all arities

Let every non-exempt source variant seek a destination (§0.2).

| Condition | Verdict |
|---|---|
| every source variant has a destination **and** every non-exempt target key is some variant's destination | **matched** — per-variant dispatch into destinations |
| **no** source variant has a destination | **coercion** — compile every target group, try in target order (today's tier 3) |
| anything else — some matched and some not, or a target key left uncovered | **invalid operation** |

One source variant covers exactly one target *key* (it produces one value); many source variants
may share a destination key. Multiple targets under the *same* key are a group, not a conflict.

**Arity exception:** the coercion verdict requires one side to be a single variant. Union → union
with no matches is an **invalid operation**, not a guess. With unions on both sides, "try every
target for every source" is combinatorial — it is what produces today's unreadable flagship
codegen (nested `try{BigInt(i)}catch{+i}catch{i==="null"}`), and removing it is the point.

### 0.4 The error is the feature

Invalid operation = `panic` (plain `Error`, schema.ts:56), at operation-creation time. The message
renders both derived shapes (`toExpression`), which variants matched, which didn't, and the
rewrite template for the *actual* failure mode:

- uncovered **target** variant `T` → `S.never.with(S.to, <T>)` added to the source union
  (or `S.union([<source>, S.never.with(S.to, <T>)])` when the source is singular);
- unmatched **source** variant `s` → `<s>.with(S.to, <T>)`, one line per unmatched target key.

### 0.5 Decisions

- **D1 — resolved (user).** The `string ↔ null` codec keeps its coercion; no primitives change.
  It stays reachable inside unions through the coercion verdict whenever the target has no
  nullish variant: `S.schema(null) → S.union([S.string, S.number])` → `"null"` (verified
  unchanged). The earlier recommendation to delete the nullish stringified-const branches is
  **rejected**.
- **D2 — resolved (user).** `S.schema(undefined) → S.union([null, undefined])` is a **compile
  error**. Its price, stated plainly: any exact match with a leftover nullish target now errors,
  which includes widening into an optional — `S.string.with(S.to, S.optional(S.string))`. That is
  arguably a fix, not a loss: today's encode side of exactly that schema turns `undefined` into
  the **string `"undefined"`** (verified), i.e. today's widening silently corrupts. No nullish
  exemption is possible — it would un-error D2 itself, since both cases have the same shape.
- **D3 — resolved (user).** The nullish bridge is a match, not an exemption, so
  `S.schema(null) → S.union([S.string, undefined])` now **fails** like
  `S.schema(null) → S.union([S.schema(null), S.string])` — the leftover `string` is uncovered.
  This removes the last asymmetry in the rule set: coverage applies to every match uniformly.
- **D4 — keep.** Explicit const→const `.to` needs **no work**: `literalDecoder`'s
  mismatched-const branch already emits both directions (verified: `null → void 0`,
  `undefined → null`). **Phase 3 (rule 1) is already implemented** — spec it, don't build it.
  The nullish-pair codegen reuses this exact path (`try{i=void 0}`).
- **D5 — resolved (user).** `format` is in the key (§0.1). Consequence, accepted: a target union
  holding both a format and its base type is now an error (rows 11, 17), because one variant
  subsumes the other — `email | string` is `string` with extra steps, `int32 | number` is
  `number`. Today those compile to a try-then-swallow (`try{e[0].test(i)||e[1](i);}catch(e0){}` —
  an empty catch discarding the format check, verified); the new rule replaces dead weight with a
  message telling the author to drop the redundant variant.

## 1. Behavior matrix — every row verified against today

| # | Conversion | Today (verified) | New | Why |
|---|---|---|---|---|
| 1 | `string → union([string, number])` | `"123"` → `"123"` (identity wins) | **error** | `number` uncovered |
| 2 | `string → union([string.to(number), string])` | — | works | one group, target order |
| 3 | `bool → union([string, float])` | `true` → `"true"` | unchanged | no match → coercion |
| 4 | `schema(null) → optional(string)` | `null` → `undefined` | **error** (D3) | `string` uncovered |
| 5 | `schema(undefined) → union([null, undefined])` | `undefined` → `undefined` | **error** (D2) | `null` uncovered |
| 6 | `union([bigint, number, null]) → union([string, undefined])` (flagship) | works, 5-branch nested codegen | **error** | bigint/number unmatched, `string` uncovered |
| 6b | migration: `union([bigint.to(string), number.to(string), null])` → same | — | works | 2 equal-key + 1 nullish pair, covered |
| 7 | `union([string, number]) → string` | `5` → `"5"` | **error** | `number` unmatched |
| 7b | migration: `union([string, number.to(string)]) → string` | works | unchanged | all matched |
| 8 | `union(["a","b"]) → string` | works | unchanged | all matched |
| 9 | `optional(string) → string` | `undefined` → **`"undefined"`** (string!) | **error** | `undefined` unmatched |
| 10 | `union([bigint, number]) → string` | both `""+i` | unchanged | no match → coercion |
| 11 | `int32 → union([int32, number])` | `5` → `5` | **error** (D5) | plain `number` uncovered/subsumed |
| 11b | `int32 → union([string, number])` | `5` → `5` | **`"5"`** (D5) | no match → coercion → target order |
| 11c | `port → union([string, number])` | `8080` → `8080` | **`"8080"`** (D5) | same as 11b |
| 12 | `instance(Box) → union([instance(OtherBox same name), string])` | **runtime** "Expected Box \| string" | creation-time error | key by reference; every branch fails to compile |
| 13 | `union([string, never.to(number)])` | output `string \| number`; encode `5` throws | unchanged | never-recipe |
| 14 | `string → optional(string)` | parse ok; **encode `undefined` → `"undefined"`** | **error** | D2's price |
| 15 | `schema(null) → union([string, number])` | `null` → `"null"` | unchanged | no nullish target → coercion (D1) |
| 16 | `union([null, undefined]) → union([undefined, null])` | noop | unchanged | equal-key ranked above nullish pair |
| 17 | `string → union([email, string])` | works via `try{…}catch{}` | **error** (D5) | `email` uncovered/subsumed |
| 18 | `union([email, uuid]) → string` | works | unchanged | no match → coercion → identity |
| 19 | `isoDateTime → union([string, date])` | passes through as string | unchanged | no match → coercion → string first |

Rows that stay valid become specs; rows that become errors are tests (specs can't express
creation-time throws) asserting the full message including the rewrite template.

## 2. Architecture

**Matching core** (composites.ts, beside `unionToKey`):
- `matchKeyEq(a, b)` — tuple compare, no string building.
- `resolveConversion(sourceVariants, targetVariants, sourceIsUnion, targetIsUnion)` → per-source
  destination (`group(targets)` | `coerceAll`) or panic. Direction-agnostic, so
  decode-of-schema ≡ encode-of-reverse by construction. Rules 2/3/4 are three arities of one
  function; the only arity-dependent branch is §0.3's coercion exception.
- Group ordering stays const-refined-then-target-order. Format ordering is **moot now** — with
  format in the key a group is format-homogeneous, so the docs' "const/format-refined first"
  reduces to const only.
- The invalid-operation renderer (§0.4).

**`unionFactory`** (composites.ts:1247): implement the flatten TODO — spread a nested union only
when it carries no `format`/`parser`/`to`/`refiner`/`inputRefiner` (today only `to` is checked, so
spreading silently drops a nested union's refiner). Gives the flattening rule everywhere and
normalizes what the resolver sees.

**`unionDecoder`** (composites.ts:622): replace the activeKey block (649-679) with the resolver
verdict; the nullish-bridge arm (672-678) becomes a rank-2 destination, driven by the resolver
rather than by "tier 1 was empty". Delete the `unionIsWiderSchema` self-decode acceptance
(635-637). "Matched" keeps `activeKey = <destination key>`, "coercion" keeps `""`, so the skip
loop at 1001 and everything downstream — const reorder, byKey grouping, `unionIsPriority`,
`appendUnionRefiners`, the whole `getArrItemsCode` machine — is untouched.

**`unionEncoder`** (composites.ts:606): drop the `unionIsWiderSchema` branch; take the resolver
verdict; dispatch via `unionPerVariantVal`. Lift `unionCanDispatchPerVariant`'s
transformed-variant bail-outs (581-586) — the migrations *are* transformed variants (rows 6b,
7b). Keep the ref/json bail-outs. Deepest work item: dispatch narrow comes from the variant's
**input** tag, pairing from its **output** key.

**Primitives:** no change (D1, D4).

**Error channel:** no marker needed. `panic` throws a plain `Error` and `getOrRethrow` rethrows
non-SuryErrors, so the per-variant catch at composites.ts:779 already lets it escape. The one
swallow-everything site is `catch (_)` at composites.ts:1090 — make it rethrow non-SuryErrors.
Other compile-path catches (parse.ts:98,120; operations.ts:196,505; jsonschema.ts:215;
jsapi.ts:42) are all `getOrRethrow`-based; verified safe by inspection, confirmed by the
error-message tests.

**Deletions:** `unionIsWiderSchema`, the tier-keyed bridge arm. Net `S.mjs` should shrink (tier
logic and widening checks go; the resolver is creation-time-only) — but it still ships, so hold
the result to the `bundleBytes` ratchet rather than to the assumption.

## 3. Execution order (one change)

1. **Docs first — the contract of record.** Rewrite docs/js-usage.md §"Decoding into / out of a
   union" (873-912) as §0. The three-tier prose and the flagship worked example are the *old*
   spec; the flagship becomes the invalid-operation showcase with its real error text and
   migration. (The branch premise is "specs match the S.to rules in js-docs" — today those rules
   exist nowhere in the repo, so the docs are step 1, not cleanup.)
2. **Red first.** Specs for the valid rows (2,3,8,10,11b,11c,13,15,16,18,19 + 6b,7b) via
   `pnpm spec new`; tests in S_to_test.res:889-1055 rewritten for the error rows
   (1,4,5,6,7,9,11,12,14,17), asserting the full message.
3. Matching core + `unionFactory` flatten + error renderer.
4. `unionDecoder` rewiring + `catch(_)` fix.
5. `unionEncoder` rewiring + bail-out lift.
6. **Internal-consumer audit.** Full suite, then specifically:
   `optionFactory`/`nestedOption`/`nestedNone`/`fromDefault`, `S.nullish`, `S.json`/`jsonString`
   (format `json` is now a distinct key), `compactColumns`, recursive/ref paths, async variants.
   These build unions with nullish or format-carrying variants and reverse them — the most likely
   sources of new panics. Fix machinery with explicit variants; never by weakening a rule.
7. `pnpm spec check --write`; **review the golden diff as the deliverable** — codegen,
   `instantiations`, `bundleBytes` flat-or-better; call out anything unavoidable.

## 4. Breaking changes (release notes; repo has no CHANGELOG file)

- **Identity-wins is an error.** A typed source meeting a union with a mix of matching and
  non-matching variants no longer silently picks the matching one (row 1).
- **Union widening is an error**, including widening into an optional (rows 9, 14) — migrate with
  a `S.never.with(S.to, …)` source variant. This *removes a corruption*: widening currently
  encodes `undefined` to the string `"undefined"`.
- **Union→union requires coverage** (no coercion guessing): the flagship
  `union([bigint, number, null]) → union([string, undefined])` is now a creation-time error whose
  message contains its own migration (row 6b).
- **Nullish no longer bridges silently past uncovered variants** (rows 4, 5): both
  `null → string | undefined` and `undefined → null | undefined` are errors. The bridge itself
  survives where the rest of the union is covered (row 6b).
- **`format` is a distinct type for matching** (D5): a union holding both a format and its base
  type is an error (rows 11, 17), and a format-refined source now coerces by target order rather
  than snapping to its base type — `S.int32 → union([S.string, S.number])` yields `"5"`, not `5`
  (rows 11b, 11c).
- **Distinct classes with the same name no longer pair** — a runtime failure becomes a
  creation-time one (row 12).
- Unchanged and explicitly preserved: the `string ↔ null` codec including inside unions with no
  nullish target (row 15), explicit const→const `.to`, format-refined sources passing through a
  same-format group, and nullish identity conversions (row 16).

## 5. Risks

| Risk | Why | Mitigation |
|---|---|---|
| option/nullish/default internals hit new panics | they build nullish-variant unions and reverse them; D2/D3 make those shapes strict | step 6 audit; fix machinery, not rules |
| format-in-key blast radius wider than `int32` | `port`, `email`, `uuid`, `cuid`, `url`, `date-time`, `json`, `compactColumns` all become distinct keys — `S.json`/`jsonString` and compact columns are the load-bearing ones | enumerate format carriers in step 6; rows 11/11b/11c/17/18/19 as specs |
| transformed-variant per-variant dispatch | the lifted bail-outs existed for a reason; reversed chains change variant input tags | rows 2/6b/7b/13 as specs, incl. async |
| coverage-exempt catch-all targets | `unknown`-keyed target variants next to a match are exempt from counting yet not compiled | pin with the existing unknown-source test (S_to_test.res:1057) + an exact-match-plus-unknown case |
| bundle ratchet | resolver ships in `S.mjs` | spec gate; reuse `toExpression`, keep the renderer lean |
