# Ideas draft

## v11

### ideas

- A discriminated union re-validates every field of the variant the
  discriminant already selected, so `S.encoder` — which emits no checks at all
  for a plain object — emits a `typeof` per field for a union, and `decode`
  compiles to the same code as `parse` instead of skipping validation
  (`specs/jsonstring-union-encode.yaml`). Once `i["type"]==="user.renamed"`
  matches, no other variant can, so the remaining checks are neither dispatch
  nor (in the decode direction) wanted. They are load-bearing only for a
  union whose members are told apart by shape rather than by a discriminant.
- A union carrying a coerced field can't be encoded back through
  `S.jsonString`: for
  `S.union([S.schema({type: "user.created", id: S.bigint}), …])`, parse turns
  `"42"` into `42n`, but the encode direction checks `typeof id === "bigint"`
  against the value JSON.parse produced — a string — so nothing round-trips
  (`Failed at ["id"]: Expected bigint, received "42"`, pinned in the same
  spec). The union's per-variant path seems to lose the `.to` coercion the
  object path keeps; `S.schema({id: S.bigint})` alone round-trips fine. This
  also means README's headline `S.decoder(S.jsonString, eventSchema)` example
  does not run as written. Pre-existing, not from the jsonString work.
- `fromJSONSchema({const: {...}})` builds an instance-tagged literal compared by
  reference, so it rejects a structurally equal value — `S.parseOrThrow({bar:
  "baz"}, S.fromJSONSchema({const: {bar: "baz"}}))` fails with `Expected
  { bar: "baz"; }, received invalid { bar: "baz"; }`. `S.literal({bar: "baz"})`
  builds a structural object schema instead and compares deeply, which is the
  behavior the document means. Found by running fast-json-stringify's benchmark
  suite (`pnpm --filter=sury bench:fjs`), where it's the one case Sury can't
  serialize.
- `fromJSONSchema` on `{type: "string", format: "unsafe"}` (a fast-json-stringify
  extension meaning "skip escaping") produces a schema whose jsonString encode
  treats the value as JSON text and re-parses it, so a string containing quotes
  throws at runtime instead of being escaped. An unknown/unsupported `format`
  should fall back to a plain string rather than change the conversion.
- Follow a field's own forward chain before targeting `S.json`/`S.jsonString`.
  An object field declared `S.uint8Array.with(S.to, S.string)` fails to encode
  to `S.json` and `S.jsonString` with "Can't decode Uint8Array to JSON": the
  field piece retargets the field's *input* schema at json instead of first
  resolving the field's `.to` chain (Uint8Array -> string) and serializing its
  output. The same chain works top-level (`S.decoder(S.uint8Array, S.string)`)
  and wire-side-declared (`S.string.with(S.to, S.uint8Array)` + `S.encoder`).
  Fix idea: in `jsonDecoderFn`'s object-field mapping and `fieldPiece`, when
  `itemVal.s.to !== U`, parse the field's own chain first and retarget the
  resolved output at json/jsonString — mirroring what `updateOutput`/
  `perVariantTo` already does for union variants.
- Add `promise` type and `S.promise` (instead of async flag internally)
- Async output refiner runs on the Promise wrapper, not the resolved value.
  When a decoder result is async (e.g. a union with an async member) and the
  schema has a user output refiner, `B_markOutput` emits the checks against the
  Promise var instead of inside `.then()` on the resolved value. Fix must run
  the output checks inside the async continuation without adding the ~40 bytes
  per-schema the naive fix cost (B_markOutput is on every schema's hot path).

TODO:

Test null<> in ppx

```
// Test that refinement works correctly with reverse

S.reverse(S.schema({
  foo: S.string->S.to(S.number)
})->S.refine(value => value.foo > 0))
```

### TS operation functions

- rename `serializer` to reverse parser ?
- Make `foo->S.to(S.unknown)` stricter ??

- Add `S.to(from, target, parser, serializer)` instead of `S.transform`?
- Make built-in refinements not work with `unknown`. Use `S.to` (manually & automatically) to deside the type first
- Better inline empty recursive schema operations (union convert)
- Don't iterate over JSON value when it's `S.json` convert without parsing
- Add `S.date.with(S.migrationFrom, S.string, <optionalParser>)`.
- Allow to pass {} instead of S.schema({}) to S.array and other schemas

### Final release fixes

- Add `S.env` to support coercion for union items separately. Like `rescript-envsafe` used to do with `preprocess`
- Make `S.record` accept two args
- Update docs

### Numeric bounds follow-ups

- **Move bound checks off `refiner` into the decoder.** `S.gt`/`S.lt` build a
  refinement whose check duplicates what the decoder could emit from the
  bound fields directly, so `S.int32.with(S.gte, 5)` range-checks twice.
  Deriving them in `numberDecoder` fuses the two and drops a check per bound.
  Do it in its own PR, in this order — the risk and the safety net are the
  same piece:
    1. Merge the branch that renamed `S.min`/`S.max`. `pnpm fuzz:union` builds
       its baseline from a git ref, and every ref before that rename lacks
       `S.gte`/`S.minLength`, so the harness cannot build one today.
    2. First commit of the follow-up: run `fuzz:union --ref=<merge-base>` on an
       unchanged tree, to confirm the harness works against the new API.
    3. Then make the change and diff, so the gate actually gates.
  Three knock-ons to expect: `union.ts` decides a schema has refinements with
  `schema.refiner !== U`, which bounded schemas would stop setting;
  `parse.ts`'s reverse swaps `refiner`/`inputRefiner`, and a field carries no
  side; and bound checks would move to a fixed position relative to `pattern`
  and `refine`, changing which error surfaces when both fail. Messages survive
  only if the decoder-emitted check carries its own fail builder from
  `errorMessage[key]` — without that it reports `Expected int32` where the
  refinement reports the bound.

- **A narrowing bound should retract the check it supersedes.** Applying a
  bound that doesn't narrow is skipped outright, but in the other order the
  earlier check is already in the refiner chain and can't be pulled back, so
  `gte(1).gte(5)` runs `i>=1` and `i>=5` where only the second matters. The
  advertised JSON Schema is right either way — but this is *not* codegen only,
  which is the part worth fixing first: a superseded check keeps its own
  message, so which one a caller sees depends on which check fires.
  `S.string.with(S.maxLength, 5, "MAX").with(S.length, 3)` advertises
  `string.length == 3` and reports "MAX" for a 6-character string and the
  generic message for a 4-character one, both being equally "too long"
  (`specs/string-length-supersedes-maxLength-message.yaml`). Compounding it,
  `length()` writes its message under both `minLength` and `maxLength` while
  the check it attaches reads only `minLength` — dead in that order, and in the
  reverse order (`maxLength(5, "MAX").length(3, "EXACT")`) it overwrites the
  caller's "MAX" so the surviving `i.length<6` check reports "EXACT". Retracting
  the check retires both. ArkType
  reduces both orders to a single `number >= 5` node because its refinements
  intersect rather than append (`min: (l, r) => l.isStricterThan(r) ? l : r`),
  so it's reachable, but it needs `internalRefine` to be able to replace a
  check rather than only push one. Of the rest: Zod narrows the field but runs
  both checks (same as here), Valibot keeps both in the pipe with no
  narrowing, and TypeBox lets the later option win outright — so
  `Type.Number({minimum:5})` overridden by `{minimum:1}` accepts 3.

- **Narrow a numeric format's range check against the schema's own bounds.**
  `S.int32.with(S.gt, 5)` emits `i<=2147483647&&i>=-2147483648&&i%1===0` and
  then `i>5`, but `i>5` already implies the lower half; `S.lt` makes the upper
  half dead the same way, and `S.port` (`i>=0&&i<65536&&i%1===0`) has the
  identical redundancy. `numberDecoder` has `input.e` in hand and the bounds
  are native fields on it, so `int32FormatValidation` can drop whichever half
  the bound subsumes. Two costs: a value outside the format range but also
  outside the bound would report the bound's error rather than
  `Expected int32`, and `int32Check` would stop being a module-level const —
  the one place `primitives.ts` deliberately avoids a per-compile closure.

- **A range `fromJSONSchema` can't represent resolves two different silent
  ways.** `integer` maps onto int32, so a document whose bound falls outside
  that range has no faithful schema — and the two sides disagree about what to
  do. `{minimum: 3000000000}` collapses to `never` (`applyBound` reads the
  panic and gives up), rejecting the very values the document describes;
  `{maximum: 3000000000}` is dropped as non-narrowing, leaving a schema that
  rejects 2.5e9 and re-emits int32's edge as if the document had said it.
  Neither round-trips. The file already fails creation for keywords it cannot
  model rather than widening silently — an unrepresentable range wants the same
  answer, or a wider integer schema to land in. Pinned in
  `specs/jsonschema-int-{minimum,maximum}-above-int32.yaml`.

- **A bound is the only refinement that rewrites the schema's type
  expression.** So it's the only one that shows up when the *type* check is
  what failed: `S.string.with(S.minLength, 2)` reports
  `Expected string.length >= 2, received null`, where the same string carrying
  `S.pattern` or `S.refine` still reports `Expected string, received null`.
  The statement is true — `null` is not a string of length >= 2 — but it points
  at a length nothing got far enough to have, and which refinement was applied
  shouldn't decide how a wrong-type failure reads. The two checks are already
  separate throws with separate builders (`e[1]` vs `e[0]`), so a custom bound
  message correctly does *not* leak here; only the rendering does. Fixing it
  means `failInvalidType` rendering the bare type where the bound check renders
  the bounded one — which costs the `skipOverride` path a second caller.
  Pinned in `specs/string-minLength.yaml`.

- **Union headers enumerate bounds.** The same rewrite reaches the union
  header, which is built from member expressions and deduped on rendered text.
  Bounded members no longer render alike, so three string members that used to
  collapse to `string` now spell
  `string.length >= 5 | string | string.length <= 1`, and a non-string input
  gets all three back as the answer to what was wrong with it. Visible in
  `specs/union3-same-tag-effect-boundary.yaml`,
  `union3-same-tag-validation-group`, `union2-refined-literal-fallback` and
  `union-large-planner`. Three options, cheapest first: build the header from
  `inputExpression(member, true)` so it names the shapes and leaves the bounds
  to the per-member lines, which already carry them; or dedupe on the base
  rendering and re-add a bound only where it's what distinguishes two members;
  or keep the header and drop the `, received X` each sub-line repeats from it.
  The first restores every golden above to its pre-bounds text without losing
  detail, since the sub-lines are per-member already.

- **A hard-coded array length should build a tuple.** `S.array(S.string)`
  with `S.length(2)` describes exactly `[string, string]`, and with `S.empty`
  exactly `[]` — but both infer `string[]` and run a length check beside the
  array's own loop, where a tuple would carry the arity in its type and check
  it once. `S.tuple` already exists and already emits `i.length===n`, so this
  is `length`/`empty` on an array tag rewriting to it rather than refining,
  and the win is a truer inferred type more than codegen. Two things to settle:
  the bound is reversible today and a tuple rewrite has to stay so, and
  `length` applied to an already-bounded array (`minLength(1).length(2)`) has
  to pick one representation. Pinned in `specs/array-length.yaml` and
  `specs/array-empty.yaml`.

- **A bound that doesn't narrow takes its custom message down with it.**
  `gte(5).gte(1, "MY MESSAGE")` drops the second bound — correctly, there is no
  failure left for it to guard — but the message is the caller's own text and
  it vanishes with no log, no error, and a schema that builds. A caller who
  writes a message and never sees it has no way to learn why. Either carry it
  onto the bound that survived, or reject a message supplied to a bound that
  doesn't narrow at construction, the way a contradictory pair already is.
  Same on the length side. Pinned in `specs/number-gte-redundant.yaml` and
  `specs/string-length-redundant.yaml`.

### Known bugs left over from the validation refactor (`val.validation: array<validationCheck>`)

- **Union discriminant hoists refinement checks with `&&` instead of `;`.**
  Now that refinements are structured checks, the union item merge loop
  hoists all checks on a val via `andJoinChecks`, fusing type checks and
  refinement checks into one `&&`-joined condition with a single error throw.
  This causes two problems: (1) `typeof==="string"&&length===N` shares one
  error instead of separate type/refinement errors, and (2) same-type items
  with different refinements (e.g. `S.union([S.string->S.email, S.string->S.url])`)
  lose per-item error messages. Fix: split hoisted checks by `fail` reference —
  first group (type checks) → discriminant condition, remaining groups
  (refinement checks) → body code as `cond||fail;`. For same-type items with
  different refinements, use if/else if dispatch on the refinement cond instead
  of try/catch. Failing regression tests in `S_union_test.res`.
- **`noValidation` on a literal inside a union silently breaks dispatch.**
  `literalDecoder` short-circuits when `expectedSchema.noValidation` is set
  and emits no check at all, so there's nothing for the union discriminant
  hoister to lift — that case becomes a catch-all. Fix: either emit the
  equality check regardless of `noValidation` when the val ends up inside a
  union, or reject `S.noValidation` on a literal-in-union at schema
  construction time. Failing regression test: `S_noValidation_test.res ›
  Union dispatch still works when a case has noValidation`.
- **`err.received` is wrong for refine-chain vals on type failures.** Because
  `B.refine` sets `~schema=prev.expected`, `val.schema` on a refined val
  equals the target schema, and `failInvalidType` reads `val.schema` for
  `received`. So `err.received === err.expected` on a primitive type failure.
  User-visible reason text is unaffected (it uses `input->stringify`) but
  programmatic consumers reading `err.received` get the target schema instead
  of the source type. Fix: either have the fail function reach through
  `val.prev.schema` (with a comment on the invariant that validation-owning
  vals always have a prev) or stop mutating `val.schema` to the target in
  `refine` and walk the chain differently for "Expected X" messages.
  FIXME is tagged at `Sury.res:failInvalidType`.

### Pre-existing bugs surfaced by the TS-migration review (ported faithfully, fix separately)

- **`exclusiveMaximum` read as `exclusiveMinimum` in the max branches.** Both
  `toJSONSchema` and `fromJSONSchema` max handling read
  `jsonSchema.exclusiveMinimum` where they mean `exclusiveMaximum`
  (`packages/sury/src/jsonschema.ts`, the two max dispatch sites), so
  exclusive upper bounds round-trip incorrectly.
- **`S.merge` forces all keys of both objects into `required`.**
  `merge` (`packages/sury/src/entry.ts`) rebuilds the merged object with
  every property required, dropping optionality that either side declared.
- **`inlinedValueFromString` escapes only `"` and `\n`.**
  (`packages/sury/src/types.ts`) — other control characters (`\r`, `\t`,
  backslash itself) survive unescaped into generated code and error text.
- **ReDoS risk in `fromJSONSchema` patterns.** `new RegExp(jsonSchema.pattern)`
  compiles untrusted patterns directly; a hostile JSON Schema can supply a
  catastrophic-backtracking pattern.
- **Async output refiners run on the Promise wrapper.** Marked with a TODO in
  the source: an async transform followed by an output refiner can observe the
  pending Promise instead of the resolved value in some advanced-decoder
  paths.
- **Empty async dict returns a forever-pending Promise.**
  `S.record` with an async item schema and `{}` input never resolves
  (`Promise.all` aggregation is skipped for zero keys).
- **Loop guard message says 100 but triggers at 50.** The recursion guard in
  `packages/sury/src/parse.ts` throws "Loop count exceeded 100" behind a
  `> 50` check — align the number (and consider making the limit configurable).
- **`deepStrip`/`deepStrict` don't descend when a nested schema's
  `additionalItems` already matches the target mode.**
  `Object_setAdditionalItems` (`packages/sury/src/operations.ts`) early-returns
  the schema unchanged whenever `currentAdditionalItems === additionalItems`,
  which also skips the `deep` recursion into `items`/`properties` — so a
  nested object whose own mode already matches the top-level target, but whose
  children don't, is left un-recursed-into. Present verbatim in the original
  ReScript `Object.setAdditionalItems` (`Sury.res`), carried through the TS
  migration unchanged.
- **Homomorphic tuple-mapped types don't map variadic tuple elements.**
  `UnknownArrayToOutput`/`UnknownArrayToInput` (`packages/sury/index.d.ts`)
  guard on `number extends T["length"]` to distinguish tuples from plain
  arrays, but a variadic tuple like `[string, ...number[]]` also has
  `T["length"]` widened to `number`, so it falls into the "return as-is"
  branch instead of mapping each element through `UnknownToOutput`/
  `UnknownToInput`. Same guard existed in the original recursive
  `_RestToOutput`/`_RestToInput` accumulator types, so this isn't a regression
  from the homomorphic-type rewrite — just an existing gap now easier to spot
  in the simpler form.

## v11 initial

- Add `s.parseChild` to EffectContext ???
- Support arrays for `S.to`
- Remove fieldOr in favor of optionOr?
- Allow to pass custom error message via `.with`
- Make S.to extensible
- ~~Add S.Date (S.instanceof) and remove S.datetime~~ (S.date added; S.datetime kept for backward compat)
- Add refinement info to the tagged type

## v???

- `S.promise: S.t<'value> => S.t<promise<'value>>` and `S.await: S.t<promise<'value>> => S.t<'value>`
- Remove `S.deepStrict` and `S.deepStrip` in favor of `S.deep` (if it works)
- Make S.serializeToJsonString super fast
- Somehow determine whether transformed or not (including shape)
- Add JSDoc
- s.optional for object
- S.transform(s => {
  s.reverse(input => input) // Or s.asyncReverse(input => Promise.resolve(input))
  input => input
  }) // or asyncTransform // Maybe format ?
- Clean up Caml_option.some, Js_dict.get
- Github Action: Add linter checking that the generated files are up to date (?)
- Support optional fields (can have problems with serializing) (???)
- S.mutateWith/S.produceWith (aka immer) (???)
- Add S.function (?) (An alternative for external ???)

```

let trimContract: S.contract<string => string> = S.contract(s => {
s.fn(s.arg(0, S.string))
}, ~return=S.string)

```

- Use internal transform for trim
- Add schema input to the error ??? What about build errors?
- async serializing support
- Add S.promise
- S.create / S.validate
- Add S.codegen
- Rename S.inline to S.toRescriptCode + Codegen type + Codegen schema using type
- Make `error.reason` tree-shakeable
- S.toJSON/S.castToJson ???
- S.produce
- S.mutator
- Check only number of fields for strict object schema when fields are not optional (bad idea since it's not possible to create a good error message, so we still need to have the loop)

## `fromJSONSchema` type inference (landed — history below)

**Landed 2026-08-07 with a Sury-owned engine instead of the vendored one** the
research below recommends. The phase-0 measurements changed the calculus:
vendored `json-schema-to-ts` costs 3k–86k instantiations across the tiers and
dies at ~14 levels of literal nesting, while a ~130-line first-match resolver
in `ata-validator`'s deferral style — written against Sury's actual runtime
dispatch order, with the recursion-safe key-remap object split — measured
368/1,076/676 on the same tiers, handles 30-level nesting at 396, and keeps
recursive/mutual `$ref` under 1.1k. `FromJSONSchema<T>` +
`JSONSchemaResolve` live in `src/types/json.d.ts`; the dispatch-order invariant
is commented on both sides (`src/types/json.d.ts`, `src/jsonschema.ts`). Specs:
`fromjsonschema-object`, `fromjsonschema-recursive-ref`, plus the three
`jsonschema-*` ts goldens moving JSON→precise. Not modeled (deliberately, v1):
`not`, `if/then/else`, `default`-fold's input/output split, `oneOf`
exclusivity; all degrade to the runtime's wider static shape, never narrower.

### Follow-ups

- **Runtime `$defs`/`$ref` resolution** — the static type already resolves
  local pointers while the runtime parses a `$ref` as plain JSON; the FIXME in
  `specs/fromjsonschema-recursive-ref.yaml` pins the divergence. Recursive
  documents need the runtime's recursive-schema machinery (`S_recursive`), so
  size that first. Closing this deletes the one "type leads runtime" caveat.
- **Corpus-wide round-trip dimension (phase 3)** — derive a `fromJSONSchema`
  check in the spec harness from each spec's existing `jsonSchema.input`
  golden (~126 cases): pin the inferred type + instantiations next to the
  emitter's output so a runtime branch gaining support without a matching
  type branch shows up as a spec diff. Harness change → log under Spec
  Harness Suggestions in CONTRIBUTING.md per the spec skill's rule.
- **`default`-fold input/output split** — a non-required property with
  `default` is folded via `Option_getOr`, so it's optional on the input side
  but always present on the output side; the inferred type currently keeps it
  optional on both (sound, just wider). Needs `FromJSONSchema` split into
  per-side resolvers; measure the cost of doubling before committing.
- **Same-level `not` exclusion** — `{ enum: [...], not: { enum: [...] } }`
  could infer `Exclude<...>` cheaply. Only worth doing together with runtime
  structure (today `not` is an opaque refinement), and note upstream
  `json-schema-to-ts` gets the `allOf`-sibling variant wrong — pin whatever
  behavior lands in a spec.
- **`anyOf`/`oneOf` inside `type: "object"`** — the runtime drops them (TODO
  at the object branch in `src/jsonschema.ts`); the type chain mirrors that.
  When the runtime TODO lands, add the matching branch to `JSONSchemaResolve`
  in the same change — the dispatch-order comment binds the two.

## `fromJSONSchema` type inference (researched, parked — original notes)

`S.fromJSONSchema` returns `Schema<JSON, JSON>` — the described type isn't
inferred from the schema literal. Nothing in the Standard Schema ecosystem does
this (Zod v4's `z.fromJSONSchema` and `zod-from-json-schema` are runtime-only,
`@valibot/to-json-schema` is the reverse direction), so it would be a real
differentiator. Findings from the investigation, so it doesn't have to be redone:

- **Vendor, don't depend on, `json-schema-to-ts` v3.1.1 + `ts-algebra` v2.0.0**
  (MIT, ~6k lines of pure type-level code, ~26M downloads/week, the engine behind
  Fastify's type provider). Both are frozen — no release since Aug 2024, community
  PRs for `$defs` (#224) and tuples (#231) unreviewed — so a vendored copy can't
  fall behind, and depending on it would mean waiting forever for the fixes below.
  It parses a schema literal into a tagged meta-type IR, then resolves it; the IR
  is what makes `allOf` merging and `not` exclusion expressible.
- **Adaptations it needs**: `M.Any` → `S.JSON` (not `unknown`); add `prefixItems`
  and alias `$defs` → `definitions` (it is draft-07-shaped); reject Sury's
  unsupported keywords at compile time instead of ignoring them; drop its
  index-signature widening for `additionalProperties` alongside `properties`
  (Sury strips extras).
- **Recursive `$ref`** is a hard no upstream. `ata-validator`'s `index.d.ts`
  (~120 lines, MIT) solves it by threading a root `$defs` map through the
  recursion (`RootDefs`/`RefName`/`ResolveRef`) — worth grafting when the runtime
  learns to resolve `$defs`. Its engine is too shallow to vendor as a base:
  first-match dispatch, so sibling keywords are dropped (`allOf` next to
  `properties` silently ignores the latter) and `nullable` is missing from `Infer`.
- **Cost is the risk**: upstream re-recurses on `Omit<S, keyword>` per keyword, so
  instantiations grow multiplicatively; "type instantiation is excessively deep"
  is a known, unanswered issue there and its CI never ran on TS 5.x. Gate the drop
  on a stress schema through the spec harness before adapting anything.
- **Coverage enforcement**: derive a `fromJSONSchema` dimension in the spec harness
  from each spec's existing `jsonSchema.input` golden — round-tripping the whole
  corpus through `fromJSONSchema` pins the inferred type and its instantiation cost
  next to the emitter's output, so a runtime branch that gains support without a
  matching type branch shows up as a spec diff.

The dialect split landed here (wide `JSONSchema` in, per-target types out) is the
shape that work plugs into.

### Phase 0 result (2026-08-06): GO

Measured `json-schema-to-ts@3.1.1` + `ts-algebra@2.0.0` unmodified on TS 5.8.3
(`strict`, `skipLibCheck` off), with the exact `packages/spec/introspect.ts`
methodology (`@typescript/vfs`, `getInstantiationCount()` delta vs bare-import
baseline). Corpus context: existing specs' `ts.instantiations` are median ~5.2k,
max 12.4k; today's `fromJSONSchema` specs pin 254.

- Small object schema (5 props, formats, enum): **2,984** inst — same band as
  hand-written object specs (`object5-optional` is 3,610).
- Realistic API schema (~30 props, 3-level nesting, discriminated `anyOf`,
  tuples, dicts): **17,895** inst, 376ms warm check.
- Stress (10-branch union + `allOf` merge + `not` + `oneOf` + `if/then/else`
  with `parseNotKeyword`/`parseIfThenElseKeywords` on + 8-level nesting):
  **86,455** inst, 694ms, no errors.
- Wrapping the result in `Schema<T, T>` and reading `S.Output`/`S.Input` back
  (the phase-2 signature simulated) adds a flat **~3k** on any tier.
- **The only breaking dimension is nesting depth**: pure object nesting dies at
  ~14–20 levels ("Type instantiation is excessively deep") because the
  `Omit`-per-keyword recursion burns ~3 of TS's 100-depth budget per schema
  level. Width is fine: 100 union branches = 268k inst / 1.0s / ok, 100
  properties = 15k / ok. Realistic schemas express depth via `$ref` (out of
  scope anyway), so document the limit; the depth-only failure mode also means
  a depth-capped bailout to `S.JSON` is possible if ever needed.
- The shipped `.d.ts` passes `tsc --noEmit --strict --skipLibCheck false`
  (the `declarations_test.ts` gate) in 2.7s including the probe project.
- Correctness caveat found while measuring: `not` as an `allOf` *sibling* does
  not exclude even with `parseNotKeyword` (`allOf: [{enum: [a..e]}, {not:
  {enum: [b, d]}}]` infers all five) — exclusion only applies same-level.
  Pin whatever behavior the adaptation keeps in a spec.

**`$ref` is in scope for the types** (decided after the measurements below;
the runtime keeps producing `json` for `$ref` until it learns resolution —
the type side leads):

- Upstream v3.1.1 already resolves **non-recursive** `$ref` — `#/definitions/`,
  `#/$defs/` and the `references` option all infer correctly at ~6k inst.
  The earlier "PR #224 means no `$defs`" read was too pessimistic; only verify
  nested/edge placements after vendoring.
- **Recursive** `$ref` is where upstream hard-fails: "Type of property
  'children' circularly references itself in mapped type", property degrades
  to `any`, both via `references` and internal `definitions`.
- The `ata-validator` `RootDefs`/`RefName`/`ResolveRef` graft closes exactly
  that gap, measured on its shipped `index.d.ts` (546 lines, MIT): direct
  recursion (tree) **616 inst**, mutual recursion (expr/binop) **974 inst**,
  and the inferred types are usable — deep `next?.next?....` chains and
  discriminant narrowing typecheck clean, TS displays the cycle lazily.
  Its `InferObject` mapped-type deferral is the pattern the vendored engine's
  `$ref` branch must adopt; note it leaks `readonly` from `as const` (strip
  with `-readonly` during adaptation).

ArkType postmortem (checked 2026-08): `@ark/json-schema` built full literal
inference as direct recursive conditional types (accumulator + one
`Omit<schema, kw>` per keyword, piggybacking their constraint brands), then
deleted it before merge (PR #1159, commit 07a5215) over type-perf concerns —
shipped 0.0.x is runtime-only, returns `Type.Any`, no `$ref` even at runtime.
Confirms: don't hand-roll the direct-recursion style; the ts-algebra IR is the
only approach that has shipped. Also confirms upstream is still frozen: PRs
#224 (`$defs`) and #231 (tuples) remain unreviewed.

## Articles

- Write an article about creating an AI-friendly JS library (how the API design, type overloads like `S.is`/`S.assert` accepting both arg orders, and error messages make Sury easy for both humans and LLMs to use)

```

```
