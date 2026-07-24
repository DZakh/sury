# Union `.to` rules — one-go rewrite plan

> Working document for the `claude/codec-specs-s-to-rules-*` branch. Delete before merge.
> Replaces the 7-phase iterative plan with a single coordinated change: the phases each
> mutate a site of `composites.ts` that is entangled with the others (activeKey feeds the
> skip loop, byKey feeds `getArrItemsCode`, `unionEncoder` branches on `unionIsWiderSchema`),
> so every intermediate state is broken. Instead: fix the contract first, encode it as
> specs/tests, then land the implementation as one change verified by the spec ratchet.

## 0. The contract — rules stated precisely

**Match key.** Every schema has a match key: `(tag, format, class-reference, ref-target)`.
Compared as a tuple (not a string): `class` by reference identity, recursive refs by ref
identity. This is distinct from `unionToKey` (composites.ts:507), which stays as-is —
`unionToKey` is the *runtime dispatch* key (typeof-discriminable, class by *name*);
the match key is the *compile-time pairing* key (finer: `int32 ≠ number`).

- Source variants are keyed by their **output** schema (`getOutputSchema(v)`).
- Target variants are keyed by their **own input-side** type.
- `never`-tagged variants are **exempt from needing** a counterpart on both sides, but a
  source variant `S.never.with(S.to, t)` still **provides** coverage for target `t`
  (its output key is `t`'s key). This is what makes the never-recipe work.

**Rule 1 — nullish coercion is built-in.** `undefined ↔ null` becomes an ordinary
primitive coercion in `literalDecoder` (primitives.ts:210), living next to the
stringified-const coercions, both directions. All union-level bridge machinery is deleted.

**Rule 2 — decode, typed source → target union.** Count non-never target variants whose
key equals the source key:
- **all** match → dispatch inside the group (today's activeKey behavior; const-matched
  variants first via the reorder at composites.ts:975, then target order);
- **none** match → compile every group; per-variant coercion decides (today's tier 3);
- **partial** → invalid operation at compile time. "Identity wins" is dead; the user
  opts in explicitly: `S.union([S.string.with(S.to, S.number), S.string])`.

**Rule 3 — encode, union source → typed target.** Symmetric on output keys: count source
variants whose *output* key equals the target key. All → per-variant identity dispatch
(replaces the `unionIsWiderSchema` pass-through); none → per-variant coercion; partial →
invalid operation. Requires lifting `unionCanDispatchPerVariant`'s bail-outs for
transformed variants (composites.ts:581-586) — the suggested rewrites *are* transformed
variants. Ref/json targets keep their dedicated handling.

**Rule 4 — union → union.** Bipartite coverage: every non-never source variant needs a
same-key target variant and vice versa. Leftover **nullish const** variants may bridge to
the opposite nullish const on the other side (reusing an already-matched one is allowed).
Covered → each source variant gets `.to` pointed at its same-key target group ("same type
wins" is free — bridges only exist where no same-key match exists); a bridged variant
compiles to a direct const→const mapping. Not covered → invalid operation.

**The error is the feature.** Invalid operation = `panic` (plain `Error`, schema.ts:56).
Message renders: both derived shapes (`toExpression`), the matched vs unmatched variants,
and both rewrite templates built from the actual schemas —
`x.with(S.to, target)` (opt into coercion) and `S.never.with(S.to, target)` (accept an
extra target variant). It must escape to operation creation, never become a runtime
branch failure (see §4, error channel).

## 1. Behavior matrix (the cases specs/tests must pin)

| # | Conversion | Verdict | Result |
|---|---|---|---|
| 1 | `S.string → S.union([S.bool, S.string])` | partial | **compile error** (was: identity wins) |
| 2 | `S.string → S.union([S.string.with(S.to, S.number), S.string])` (the canonical opt-in rewrite of row 1's intent) | all (keys all string) | group dispatch, const/format-refined variant first |
| 3 | `S.bool → S.union([S.string, S.float])` | none | tier-3-style per-group coercion, `""+i` wins |
| 4 | `S.schema(null) → S.optional(S.string)` | none | `null → undefined` via rule 1 (string group fails at compile → branch dropped by `staticBlockFailure`) — **depends on D1** |
| 5 | `S.unit → S.union([S.literal(null), S.unit])` | partial | **compile error** (was: tier-1 keeps undefined) — flips test S_to_test.res:1041 |
| 6 | `S.union([S.bigint, S.number, S.null]) → S.union([S.string, S.unit])` | rule 4 uncovered | **compile error** (the old flagship example becomes the error showcase) |
| 7 | `S.union([S.float, S.string]) → S.float` | partial (outputs: match+no-match) | **compile error**; fix: `S.union([S.float, S.string.with(S.to, S.float)])` |
| 8 | `S.union(["a","b"]) → S.string` | all | per-variant pass-through (replaces widening acceptance) |
| 9 | union→union with `S.never.with(S.to, t)` source variant | covered | decode: never branch always fails (other variants own the input); encode: `t → never` fails at runtime — correct, nothing maps back |
| 10 | `S.union([S.literal(null)…]) ↔ union([…S.unit])` leftovers | rule 4 nullish bridge | direct const→const |
| 11 | `S.int32 → S.union([S.float, …])` | int32 ≠ number key | none/partial per the rest — pins the format-in-key rule |
| 12 | instance sources/targets | class-reference key | `Set → union([Map, Set])`: all-in-group by class; two classes with the same `.name` must **not** match (unlike `unionToKey`) |

## 2. Open decisions — settle before writing code

**D1 (blocking case 4): implicit nullish↔string stringified coercion.** Today
`stringDecoderFn` turns a nullish literal into the string const `"null"`/`"undefined"`
(primitives.ts:115-124) and `literalDecoder` parses `"null" → null` back
(primitives.ts:223-242). If these survive, case 4 breaks: with a "none" verdict the
groups compile in target order, the string group succeeds (`null → "null"`) and wins over
the undefined group — `S.schema(null) → S.optional(S.string)` would yield `"null"`, not
`undefined`. **Recommendation: delete the nullish half of both stringified-const branches
(keep bool/number/bigint)** — this is the concrete meaning of "the general const bridge is
gone, only nullish bridges", it makes case 4 fall out of plain rule-2 compilation, and it
matches the changelog item that the flagship `"null" → null` conversion is now invalid.

**D2: strictness of rule 2 for nullish sources.** Case 5 makes
`undefined → [null, undefined]` a compile error (partial). This is the consistent reading
(null and undefined are *different* match keys — required for case 4 to be "none"), but it
flips an existing passing test and risks touching `S.nullish`/nested-option internals.
**Recommendation: keep strict**, and gate on the audit in §5 step 7 (option/nullable/
nullish/default machinery must not route through rule 2 with a nullish typed source; if it
does, fix the machinery with explicit variants, not the rule).

**D3: explicit const↔const.** `S.literal("a").with(S.to, S.literal(5))` — single-schema,
explicit. `literalDecoder`'s mismatched-const branch (primitives.ts:214-219) powers it and
also powers rule-4 nullish bridges. **Recommendation: keep it** — "explicit is opt-in";
only *union-implicit* const pairing disappears (via the coverage rule, not via
`literalDecoder`).

**D4: instance keys by class reference.** `unionToKey` uses `class.name` (needed for the
string-keyed byKey dispatch); the match tuple uses the class reference.
**Recommendation: yes** (case 12). Cost: the resolver compares tuples with a tiny
`matchKeyEq` instead of string equality.

## 3. Architecture of the change

**New matching core (composites.ts, near unionToKey):**
- `matchKeyEq(a, b)` — tuple compare on `(type, format, class, ref-identity)`.
- One resolver used by all three arities, direction-agnostic so decode-of-schema ≡
  encode-of-reverse by construction:
  `resolveConversion(sourceVariants /* keyed by output */, targetVariants /* keyed by input */)`
  → per-source-variant assignment (`sameKeyGroup(targets)` | `coerceAll` | `nullishBridge(t)`)
  or panic. Rule 2 = |S|=1 vs union; rule 3 = union vs |T|=1; rule 4 = union vs union
  with the coverage + bridge pass.
- Invalid-operation renderer: builds the message + both rewrite templates from the real
  schemas via `toExpression`, then `panic`s.

**`unionFactory` flatten TODO (composites.ts:1247):** spread a nested union only when it
carries no `format`/`parser`/`to`/`refiner`/`inputRefiner` (today only `to` is checked —
spreading currently drops a nested union's refiner). Gives the flattening rule everywhere
for free, including the shapes the resolver sees.

**`unionDecoder` (composites.ts:622):**
- Replace the activeKey block (649-679) with the rule-2 verdict; delete the nullish-bridge
  arm (672-678). "all" keeps `activeKey = sourceKey` so the skip loop at 1001 is untouched;
  "none" keeps `activeKey = ""`.
- Delete the `unionIsWiderSchema` self-decode acceptance (635-637); union-typed inputs go
  through per-variant dispatch under rule 4.
- Survivors, untouched: const-priority reorder (975-987), first-occurrence byKey grouping,
  `unionIsPriority` NaN/instance ordering, `fromDefault` undefined-skip (1003-1007 — the
  skipped variant must also be excluded from the verdict counts, it is effectively not a
  target), `appendUnionRefiners`, the whole `getArrItemsCode` machine.

**`unionEncoder` (composites.ts:606):** delete the `unionIsWiderSchema` branch; compute
rule-3/4 verdict; dispatch via `unionPerVariantVal`. Lift the transformed-variant
bail-outs in `unionCanDispatchPerVariant`; keep the ref/json bail-outs. This is the
deepest work item: per-variant dispatch must now handle variants with their own
`.to`/`parser` chains — dispatch narrow comes from the variant's input-side tag
(unchanged), pairing comes from the variant's output key (resolver). Do not conflate the
two keys.

**Error channel:** no marker needed. `panic` throws a plain `Error`; `getOrRethrow`
already rethrows non-SuryErrors, so the per-variant catch at composites.ts:778 is safe.
The one swallow-everything site is `catch (_)` at composites.ts:1090 — change it to
rethrow anything that is not a SuryError. Audit the remaining compile-path catches
(parse.ts:98,120; operations.ts:196,505; jsonschema.ts:215; jsapi.ts:42) — all
`getOrRethrow`-based, so panics escape to operation creation as required.

**Primitives (primitives.ts):** add `null ↔ undefined` to `literalDecoder` (rule 1);
apply D1 (delete nullish stringified branches in `stringDecoderFn` + `literalDecoder`'s
string-input side).

**Deletions:** `unionIsWiderSchema`, the bridge arm, tier-ordering special cases the
verdict supersedes. Net `S.mjs` should shrink: tier/bridge codegen goes away and the
resolver is creation-time-only code — but it still ships in the bundle, so hold the
result to the `bundleBytes` ratchet, not to assumption.

## 4. Execution order (inside the single change)

1. **Docs first — the contract of record.** Rewrite docs/js-usage.md §"Decoding into /
   out of a union" (873-912) to state §0's rules; the old worked example becomes the
   invalid-operation showcase with its error text and both rewrites. (The branch's whole
   premise is "specs match the S.to rules in js-docs" — today the docs still describe the
   three-tier algorithm, so the rules exist nowhere in the repo.)
2. **Red first.** Author the new codec specs (`pnpm spec new`, one yaml per matrix row
   that yields a *valid* schema: rows 2,3,4,8,9,10,11,12) and rewrite the tier tests in
   S_to_test.res:889-1055 for the error rows (1,5,6,7 — specs can't express
   creation-time throws; those live in tests, asserting the full message incl. both
   rewrite templates). All red against current behavior.
3. Primitives: rule 1 + D1.
4. Matching core + `unionFactory` flatten + error renderer.
5. `unionDecoder` rewiring + catch(_) fix.
6. `unionEncoder` rewiring + bail-out lift.
7. **Internal-consumer audit:** run the full suite; specifically chase
   option/nullable/nullish/default/nested-option (`optionFactory`, `nestedNone`,
   `fromDefault`) and recursive/ref and async-variant paths — these are the internal
   users of union conversion most likely to now hit partial-verdict panics.
8. `pnpm spec check --write` over everything; **review the golden diff as the
   deliverable** — codegen, `instantiations`, `bundleBytes` all flat-or-better; call out
   any unavoidable regression.

## 5. Breaking-changes list (for the release notes; repo has no CHANGELOG file)

- Identity-wins is gone: a typed source meeting a union with a *mix* of same-key and
  other-key variants is a compile-time error (was: silently picked the same-key variant).
- Union widening pass-through is gone: wider target unions are an error; migrate with a
  `S.never.with(S.to, extra)` source variant.
- The general const bridge is gone: implicit nullish↔string stringified coercion is
  removed (D1); `"null" → null` no longer parses implicitly. Only nullish consts bridge,
  and only inside rule-4 coverage.
- The flagship `S.union([S.bigint, S.number, null]) → S.union([S.string, undefined])`
  conversion is now a compile-time error whose message contains its own migration.
- `undefined ↔ null` now coerces as a built-in primitive coercion everywhere (rule 1).

## 6. Risk register

| Risk | Why | Mitigation |
|---|---|---|
| option/nullish/default internals hit new panics | they build unions with nullish variants and reverse them | step 7 audit; fix machinery with explicit variants, never by weakening rules |
| transformed-variant per-variant dispatch | bail-outs existed for a reason; reversed chains change variant input tags | pin with specs (matrix rows 2/7/9) incl. async variants; keep ref/json dedicated paths |
| dead-branch codegen from compile-failing groups | "none" verdict + D1 relies on `staticBlockFailure` dropping branches | assert generated code in specs (row 4 must compile to a bare const mapping) |
| bundle ratchet | resolver ships in S.mjs | spec check gate; reuse `toExpression`, keep renderer lean |
| same-name distinct classes | match must be by reference, dispatch by name | matrix row 12 test |
