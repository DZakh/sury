# Ideas draft

## v11

### ideas

- Bytes on the wire: make `S.uint8Array` encode as base64, and give the text
  conversion its own name. Today `S.uint8Array`'s string codec runs
  `TextEncoder`/`TextDecoder`, so any byte outside ASCII is silently destroyed —
  `S.encoder(S.schema({payload: S.uint8Array}), S.jsonString)` on
  `[137, 80, 78, 71]` (a PNG magic number) emits `"�PNG"`, which decodes back as
  `[239, 191, 189, 80, 78, 71]`, with no error at either end. The file's own
  first line already promises the intended contract ("a base64 string on the
  JSON side, bytes on ours"); the implementation just never matched it. The type
  can't say whether bytes are text or opaque, so the default has to be the total
  conversion — guessing base64 when the user meant text is instantly visible,
  guessing text when they meant binary corrupts data nobody re-reads.
  - `S.uint8Array` <-> string becomes base64. Feature-detect
    `Uint8Array.prototype.toBase64` / `Uint8Array.fromBase64` (TC39 stage 3;
    Bun has it, Node 22 does not) once at module init and embed the chosen
    helper, so generated code stays a single `e[N](i)` call with no per-schema
    branch. The fallback (`btoa`/`atob` over a latin1 bridge, or `Buffer`)
    allocates an intermediate string per value where the native path doesn't —
    worth a `scenarios.yaml` entry before byte fields appear in a benchmark.
  - Add `S.base64`, a string schema carrying the encoding, so the wire format is
    named on the wire side. It gets JSON Schema both ways:
    `{type: "string", contentEncoding: "base64"}` out of `toJSONSchema` and back
    through `fromJSONSchema`. `contentEncoding` is already in the keyword set
    (`src/types/jsonschema.d.ts`, `jsonschema.ts`, `JSONSchema.res`), and
    `toJSONSchema(S.uint8Array)` currently throws `Expected JSON, received
    Uint8Array` — so this closes a gap that exists whichever encoding wins.
  - Add `S.utf8` for bytes that really are text, converting against
    `S.uint8Array` via `TextEncoder`/`TextDecoder` — with
    `new TextDecoder("utf-8", {fatal: true})`, so invalid bytes throw instead of
    becoming U+FFFD. An error is recoverable; a replacement character isn't.
    That flag alone is the minimum fix if the default is left as it is.
  - Consider `base64url` too, but only as an explicit spelling — there is no
    reliable signal for "this value is going into a URL", and inferring one is
    the same class of guess that produced the current bug. The principled
    exception is a container that genuinely owns a URL-safe context (a
    query-string or path codec, the way `compactColumns` owns columns), which
    could default its byte fields to it.
  - Layering: `src/advanced/` is for schemas nothing else builds on, so if
    `uint8Array` builds on `S.base64` the shared piece belongs in the core, not
    beside it. Specs must round-trip non-ASCII bytes in both directions —
    ASCII-only fixtures are exactly what hid this. Breaking for anyone relying
    on text semantics, so it wants a CHANGELOG line; `tests/S_test.ts`'s
    `S.decoder(S.string, S.uint8Array, S.jsonString)("data") === '"data"'`
    becoming `'"ZGF0YQ=="'` is the whole behavior change in one assertion.
- Trusted union decode can leave a dead `let` behind: `valGet` builds a
  grandchild's inline string eagerly (`` `${parent.v()}${pathAppend}` ``,
  `composites.ts`), materializing the parent var even when the passthrough
  case never uses the child — `{let v0=i["VAL"];break}` in
  `S_union_test.res`'s issue-101 golden. Eliminating it means making
  field-val inline strings lazy, a cross-cutting builder change.
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
- **Every composite export carries the whole union compiler.** `S.array`,
  `S.dict`, `S.schema`, `S.tuple`, `S.shape` — none of which have union
  semantics — each retain all ~10.2 kB minified of `union.ts`, through two
  statically-reachable but usually-dead edges: `valGet`'s dict-missing-key
  wrap (`schema = option(s)`, `composites.ts`) and the `fieldOr` helper the
  object ctx builds eagerly (`makeFieldOr`, `factory.ts`). Both reach
  `optionFactory` -> `unionFactory`, which attaches `unionDecoder` and
  `unionEncoder` as fields and so drags planner, emitter and rewrite in as one
  clump. Cutting them is the largest per-export win measured anywhere in the
  library (esbuild min+gzip, against main): `array` 11088 -> 5580 B gzip
  (-50%), `dict` 11090 -> 5578, `schema` 11338 -> 6160, `shape` 12268 -> 7132,
  `tuple` 12385 -> 7220, `object` 13563 -> 7656 (needs both edges — cutting
  `valGet` alone leaves it at 13535).
  Neither edge yields to a mechanical fix. Splitting `valGet` into plain and
  option-wrapping variants was tried and does *not* work: `makeObjectVal`'s
  val template names `decoder: objectDecoder`, which `arrayDecoder` reaches,
  so the decoder graph is one strongly-connected component and `array` keeps
  the retention anyway. Breaking it needs late binding — a mutable slot like
  the one `enableStandardJSONSchema` already uses, or parameterizing the wrap
  — which trades a runtime indirection on the decoder path. The `fieldOr` edge
  wants the other treatment: implement field-with-default without a union at
  all (an `if(v===void 0){v=def}` decoder wrapper instead of
  `union([unit, item])` + default fold), which also shortens generated code.
  That subsumes "Remove fieldOr in favor of optionOr?" below.
  What is *not* worth doing: splitting `union.ts` into core + planner files.
  Measured as a dead end — a mechanical split drops zero bytes, since
  everything is reachable from the two field assignments in `unionFactory`
  (and two more in `unionRewrite`). Real severance needs a new light
  dispatcher for the 2-3-variant `T | undefined` shapes (est. 1.5-3 kB of new
  code, and any case it can't compile either panics or falls back to the
  planner, restoring the retention), and it only pays off for
  `optional`/`nullable`-only bundles — ceiling -4.9 kB gzip there. Do the two
  edges first; revisit the split only if that ceiling still matters.
- **`fromJSONSchema` builds its own schemas through the proxy ctx.** The
  object and tuple construction sites in `src/jsonschema.ts` go through
  `object(() => {})` / `tuple(s => ...)`, which drag `schemaObject`/
  `schemaTuple`'s proxy-ctx machinery — `proxifyShapedSchema`, `makeFieldOr`
  (and through it `optionFactory`), `shapedSerializer` — into every
  `fromJSONSchema` bundle. Swapping them for the `schemaFactory` /
  `definitionToSchema` the module already imports drops factory retention
  from 4694 to 598 B minified, -1588 B gzip on the export. Mechanical, but
  not free: a `definitionToSchema` tuple is strict and carries no shaped
  serializer, so the equivalence with `schemaTuple` for the
  identity-mapping case needs pinning in a spec before the swap.
  Two neighbours measured and deliberately left alone: the bounds machinery
  (`minimum`/`minLength`/`multipleOf` -> `refinements.ts`, ~1.7 kB gzip) is
  core-keyword payload, and a `fromJSONSchema` that drops those keywords is
  simply wrong; and mapping `true`/`{}` to `unknown` instead of `S.json`
  would save its own ~1 kB but stops rejecting non-JSON values, which is a
  semantics change to argue on its own merits, not a size fix.
  `toJSONSchema` needs nothing here — it already retains a disjoint half of
  `jsonschema.ts` (~130 B overlap) and none of refinements, factory or union.
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
  Half the groundwork is already done: `boundsRefiner` derives every check
  from the schema's own fields at codegen time rather than from a value each
  call closed over, so moving them is now relocating a field read rather than
  inventing one. The other half is the payoff — that refiner currently ships
  with every bound export, and folding it into the decoder every number
  consumer already carries is what wins those bytes back.
  Run `fuzz:union --ref=<merge-base>` before *and* after; the harness builds
  its baseline from a git ref, so confirm it works on an unchanged tree first
  and the gate actually gates.
  Three knock-ons to expect: `union.ts` decides a schema has refinements with
  `schema.refiner !== U`, which bounded schemas would stop setting;
  `parse.ts`'s reverse swaps `refiner`/`inputRefiner`, and a field carries no
  side; and bound checks would move to a fixed position relative to `pattern`
  and `refine`, changing which error surfaces when both fail. Messages survive
  only if the decoder-emitted check carries its own fail builder from
  `errorMessage[key]` — without that it reports `Expected int32` where the
  refinement reports the bound.

- **Narrow a numeric format's range check against the schema's own bounds.**
  `S.int32.with(S.gt, 5)` emits `i<=2147483647&&i>=-2147483648&&i%1===0` and
  then `i>5`, but `i>5` already implies the lower half; `S.lt` makes the upper
  half dead the same way, and `S.port` (`i>=0&&i<65536&&i%1===0`) has the
  identical redundancy. `numberDecoder` has `input.e` in hand and the bounds
  are native fields on it, so `int32FormatValidation` can drop whichever half
  the bound subsumes. The same read gives `S.integer`'s `i%1===0` away for
  free wherever a divisor is an integer multiple of 1 — `multipleOf(2)` on an
  integer schema already implies it. Two costs: a value outside the format
  range but also outside the bound would report the bound's error rather than
  `Expected int32`, and `int32Check` would stop being a module-level const —
  the one place `primitives.ts` deliberately avoids a per-compile closure.
  Do it with the item above, not before it: both rewrite the same emit.

- **A bound applied after a transform emits no check.**
  `S.string.with(S.trim).with(S.minLength, 5)` accepts `""`: `transform` sets
  the output tail to a copy of `unknown`, `updateBounds` writes
  `bounds`/`minLength` onto that tail, and `boundsRefiner` dispatches on the
  tail's `type` — `unknown` matches neither the length branch nor the numeric
  one, so it returns no checks at all. Before `boundsRefiner` the per-call
  closure emitted `i.length>4` regardless of type, so the check ran (though
  its expression rendered as the garbled `unknown <= undefined` — the write
  site targeting the tail while the bound helpers read the root predates this
  refactor, and the JSON Schema output loses the bound the same way). Fix
  candidates: have `boundsRefiner` pick the branch from which bound fields
  are set instead of from `type`, or make `updateBounds` refuse/forward when
  the tail carries no type to range over. Needs a spec for
  transform-then-bound in both directions; none exists today.

- **Type-less JSON Schema assertion keywords vanish on re-emit.**
  `fromJSONSchema({multipleOf: 2})` builds (the keyword joined the type-less
  `keywordTypes` "number" group) and validates correctly through the opaque
  `refine()` that group compiles to, but `toJSONSchema` of the result returns
  `{}` — the document silently widens on a round-trip, where the keyword used
  to be rejected loudly as unsupported. First settle what a type-less schema
  should even mean here: per spec `{multipleOf: 2}` constrains only numeric
  instances and accepts everything else, which is what the refine does — so
  the fix is on the emit side, carrying the original keywords through the
  opaque refinement onto the output document rather than changing validation.

- **Rewrite a zero length bound on an array to a real empty tuple at runtime.** The
  type-level half of "a hard-coded length is arity" is done, for the exact
  bound and the lower one alike: on an array `S.length(N)` infers the N-tuple,
  `S.minLength(N)` the N-tuple with an open tail, `S.nonEmpty`
  `[T, ...T[]]` and `S.length(0)` `[]`; on a string only `S.length(0)` reaches a type
  (`""`), since TypeScript can't count characters (`Sized`/`AtLeast`/`Repeat`
  in `index.d.ts`, pinned in the `array-`/`string-` length specs).
  The runtime deliberately still refines: a general tuple rewrite unrolls
  generated code O(N) where the loop is O(1), emits N copies of the item
  schema in JSON Schema, bypasses the `maybeMessage` machinery (tuple arity
  fails as `invalid_type`), and compiles decode/encode to `identity` where the
  refinement re-checks the length — each a behavior change to pin
  deliberately, not inherit. The N=0 case has none of the scaling problems
  and a strict win: `S.length(0)` on an array rewriting to `items: []` +
  `additionalItems: "strict"` drops the dead element loop from parse
  (`Array.isArray(i)&&i.length===0||e(i)`), turns decode/encode into
  identity, and makes the schema union-dispatchable by arity. Settle there
  whether JSON Schema keeps emitting the now-unreachable `items` schema, and
  what `minLength`/`maxLength` applied *after* the rewrite should do
  (compare against `items.length` and no-op/conflict, not add a redundant
  bound).


### Size bounds and the form-data family

`S.blob`/`S.file` and `S.minSize`/`S.maxSize`/`S.size` landed as the first step
of a form-data story. What they were built to make cheap, roughly in order:

- **Widen `S.minSize` to the other containers.** The runtime already accepts any
  instance whose prototype carries a `.size`, so `S.instance(Set)` and
  `S.instance(Map)` work today (`specs/set-minSize.yaml` is the coverage that
  proves it, and exists because a `Set` is the only `.size` carrier the spec
  harness can serialize). What's missing is schemas of their own: `S.set(item)`
  and `S.map(key, value)` would make the bounds discoverable rather than
  reachable only through `S.instance`.
- **Objects, under `minProperties`/`maxProperties`.** The one container whose
  size is neither `.length` nor `.size`: the check would be
  `Object.keys(i).length`, which allocates — worth a spec snapshot so the cost
  is visible before it ships. Unlike `minSize`, both keywords are native JSON
  Schema, so `jsonschema.ts` gains a real emit rather than the nothing that
  `minSize` maps to today.
- **File/Blob content codecs.** `S.file.with(S.to, S.string)` (via `.text()`)
  and `S.to(S.uint8Array)` (via `.arrayBuffer()`) are async in the decode
  direction and sync in the encode one (`new File([i], name)`), so they need
  `B_asyncVal` and the `flagAsync` guard that already makes a sync `S.decode`
  fail with `invalid_operation`. `advanced/uint8Array.ts` is the shape to copy.
  The payoff is `S.file.with(S.to, S.jsonString.with(S.to, configSchema))` —
  parse an upload into a typed value, and reverse it to *build* the upload.
- **`S.formData` as a codec, not a preprocessor.** A `FormData` field is
  `string | File`, so the per-field work is the existing string coercions plus
  `.get`/`.getAll` extraction; the object rebuild in `advanced/json.ts`
  (`jsonDecoderFn`, via `makeObjectVal`/`B_addObjectField`) is the pattern.
  Reversing it emits `new FormData()` + `append` per field, which is what makes
  this different from VineJS and every other form validator: one schema serves
  the request handler *and* the `fetch` body. `S.urlSearchParams` is the same
  code minus files, and `S.queryString` is to it what `S.jsonString` is to
  `S.json`.
- **The three HTML-form quirks**, once `S.formData` exists: a checkbox is absent
  when unchecked and `"on"` when checked (VineJS spells this `vine.accepted()`),
  an empty text input submits `""` rather than nothing, and repeated keys are
  how arrays arrive. The first wants a named `S.accepted`; the second belongs to
  the codec rather than a global flag, since it's a wire quirk; the third is
  `.getAll`. Bracket notation (`user[name]`) is deliberately out — VineJS leans
  on `qs` for it too.
- **`S.mime`** for uploads, next to the size bounds. Wants a JSON Schema emit
  (`contentMediaType`, and `format: "binary"` for the instances) — which is the
  point at which `minSize`/`maxSize` should be revisited, since neither has a
  keyword today and both are dropped from the emitted document.

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

## `fromJSONSchema` type inference follow-ups

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

## Articles

- Write an article about creating an AI-friendly JS library (how the API design, type overloads like `S.is`/`S.assert` accepting both arg orders, and error messages make Sury easy for both humans and LLMs to use)

```

```
