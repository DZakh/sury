# Ideas draft

## v11

### ideas

- **`jsonString` -> `Uint8Array`, and a native encoder under it.** Every real
  consumer of `S.jsonString` hands the result to something that wants bytes -
  `fetch`'s body, a socket write, `fs.write`, a Kafka producer - so the JS
  string it returns is an intermediate that exists only to be UTF-8 encoded a
  moment later. Two halves, and the second is what makes the first worth doing:
  - `S.jsonString.with(S.to, S.uint8Array)` (and the reverse) as a declared
    target, so the wire type is bytes and the codec owns the encoding. Today the
    same thing spells as two hops through `advanced/uint8Array.ts`. It is
    UTF-8, as `S.uint8Array <-> S.string` is, and must stay so - bytes in a
    *value* position are base64, and this is not one.
  - A native encoder for the aggregate: reuse one module-level `TextEncoder`
    (`encodeInto` into a caller-owned buffer where one is supplied) instead of
    building the whole JSON text and encoding it after. The interesting version
    doesn't materialize the string at all - the aggregate already emits a
    concat chain, and the constant pieces (`{"id":"`, `","at":"`) are known at
    codegen time, so they can be pre-encoded to byte arrays once per compiled
    operation and only the dynamic slices go through `encodeInto`. That is the
    same trick the escape-free format splice plays, one level lower.
  Measured, 100-row list to `Uint8Array` on node 22, the whole trip inside the
  timer - `JSON.stringify` + `Buffer.from` 18.4µs, today's `jsonString` +
  `Buffer.from` 23.7µs, a byte writer 6.6µs. So the prize is real (~2.6x over
  the current path), but it lives entirely in *how* the bytes are written: the
  same writer built the obvious way, one `buf.write` per piece with `""+n` for
  numbers, measured 24.4µs - slower than the string path it replaces. The 3.6x
  between those two is byte stores for the constant chunks, a manual itoa, and
  an inline char loop for short strings, and all three are things only a
  compiler can emit. Two shapes that look alike are worth ruling out first:
  `parts.join()` produces a flat string but costs more to build than the
  cheaper `Buffer.from` wins back (25.8µs), and `encodeInto` over a pooled
  buffer under the existing concat is only ~7% (22.1µs). Wants a
  `scenarios.yaml`/`bench:jsonstring` entry measuring end-to-end (stringify +
  encode), never stringify alone - that is the measurement that hides the
  flatten and made the string path look like it was already winning.
- Trusted union decode can leave a dead `let` behind: `valGet` builds a
  grandchild's inline string eagerly (`` `${parent.v()}${pathAppend}` ``,
  `composites.ts`), materializing the parent var even when the passthrough
  case never uses the child - `{let v0=i["VAL"];break}` in
  `S_union_test.res`'s issue-101 golden. Eliminating it means making
  field-val inline strings lazy, a cross-cutting builder change.
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

- Make `foo->S.to(S.unknown)` stricter ??
- Better inline empty recursive schema operations (union convert)
- Don't iterate over JSON value when it's `S.json` convert without parsing
- Add `S.date.with(S.migrationFrom, S.string, <optionalParser>)`.

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
  inventing one. The other half is the payoff - that refiner currently ships
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
  `errorMessage[key]` - without that it reports `Expected int32` where the
  refinement reports the bound.

- **Narrow a numeric format's range check against the schema's own bounds.**
  `S.int32.with(S.gt, 5)` emits `i<=2147483647&&i>=-2147483648&&i%1===0` and
  then `i>5`, but `i>5` already implies the lower half; `S.lt` makes the upper
  half dead the same way, and `S.port` (`i>=0&&i<65536&&i%1===0`) has the
  identical redundancy. `numberDecoder` has `input.e` in hand and the bounds
  are native fields on it, so `int32FormatValidation` can drop whichever half
  the bound subsumes. The same read gives `S.integer`'s `i%1===0` away for
  free wherever a divisor is an integer multiple of 1 - `multipleOf(2)` on an
  integer schema already implies it. Two costs: a value outside the format
  range but also outside the bound would report the bound's error rather than
  `Expected int32`, and `int32Check` would stop being a module-level const -
  the one place `primitives.ts` deliberately avoids a per-compile closure.
  Do it with the item above, not before it: both rewrite the same emit.

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
  refinement re-checks the length - each a behavior change to pin
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
  `Object.keys(i).length`, which allocates - worth a spec snapshot so the cost
  is visible before it ships. Unlike `minSize`, both keywords are native JSON
  Schema, so `jsonschema.ts` gains a real emit rather than the nothing that
  `minSize` maps to today.
- **File/Blob content codecs.** `S.file.with(S.to, S.string)` (via `.text()`)
  and `S.to(S.uint8Array)` (via `.arrayBuffer()`) are async in the decode
  direction and sync in the encode one (`new File([i], name)`), so they need
  `B_asyncVal` and the `flagAsync` guard that already makes a sync `S.decode`
  fail with `invalid_operation`. `advanced/uint8Array.ts` is the shape to copy.
  The payoff is `S.file.with(S.to, S.jsonString.with(S.to, configSchema))` -
  parse an upload into a typed value, and reverse it to *build* the upload.
- **`S.urlSearchParams` and `S.queryString`**, now that `S.formData` has shipped.
  The codec iterates the entry list and calls `append`, both of which
  `URLSearchParams` has, so the first is the same code minus files and the
  second is to it what `S.jsonString` is to `S.json`.
- **A `S.record` target for the same readers**, for a form whose keys aren't
  known ahead of time. `S.formData.with(S.to, S.record(S.string))` is rejected
  today: the codec takes the object path only when `additionalItems` is
  `"strip"`/`"strict"`, and a record's is the value schema, so the pair falls
  through to `Can't decode FormData -> { [key: string]: string; }. Define
  custom codec with S.to`. Two things
  to settle before it can be written:
  - **What a repeated key becomes.** The declared path answers `getAll` for a
    `S.array` field and `get` for every other, which a record has no field to
    ask. `Object.fromEntries(fd)` keeps the last value and loses the rest;
    `S.record(S.array(V))` keeps them but wraps the common case in a
    one-element array. A third reading - `getAll` where the value type is an
    array and `get` otherwise - matches the declared path exactly and is
    probably the one.
  - **Which value types can work.** Only ones a text wire can discriminate:
    `S.record(S.string)` and `S.record(S.number)` are fine, and
    `S.record(S.union([S.string, S.number]))` can't be - the union rules reject
    `string -> string | number` before the codec is consulted, since every
    entry satisfies the string arm.
- **A three-schema chain decodes but does not encode.** `S.decoder(S.file,
  S.jsonString, S.array(User))` works; the mirror
  `S.encoder(S.array(User), S.jsonString, S.file)` raises `Can't decode JSON
  string -> File`, and the wire-first spelling raises `Expected base64url,
  received {...}` on the base64url version of the same pair. Two arguments
  encode fine, and so does the nested schema the chain stands for
  (`S.encoder(S.file.with(S.to, S.jsonString.with(S.to, S.array(User))))`
  returns a `File`), so the feature is there and only the variadic form misses
  it - which is the spelling the `encoder` overloads advertise.
- **A union dispatch writes its result back into the slot it read**, so
  encoding a tuple whose slots need one mutates the caller's array:
  `S.schema([S.union([S.boolean, S.number])]).with(S.to, S.schema([S.string]))`
  leaves its input holding the converted value. Nothing else in the library
  does this - an array builds a `new Array`, an object a fresh literal - so it
  is the dispatch's destination that is wrong: an inlined property access
  rather than a var of its own. Pinned by a `FIXME:` in `formData_test.ts`,
  where `fuzz:formdata` found it; the fix belongs in the union compiler and
  moves every dispatching golden.
- **A union with a bare `string` arm can be read from a form but not written
  to one.** The codec's hook is consulted per target arm on the way in, so
  `S.union([S.string, S.number])` and `S.union([S.string, S.file])` decode; on
  the way out `appendValue` hands the whole union to one `-> string`
  conversion, and the union rules reject that as ambiguous whenever a `string`
  arm passes through untouched beside an arm that would have to convert.
  `S.union([S.boolean, S.number])`, with no such arm, writes today. Writing
  the rest needs a runtime dispatch per arm (`v instanceof Blob`, and
  each arm's own encoder for the rest), which is bundle the common case - a
  union of literals, which already works both ways - would carry for nothing.
  Listed in `fuzz:formdata`'s ONE_WAY, so the day it starts working the run
  says so.
- **`string -> string | undefined` is still rejected by the union rules**, so
  the env pattern can't read an optional string field - where the form codec
  converts the present arm itself. Pinned by
  `specs/dict-to-object-optional-string`.
- **Nested keys for `S.formData`, with no API to turn them on.** Nesting the
  schema is the switch: `S.schema({ user: S.schema({ city }) })` rejects the
  pair today, and instead should read `user[city]`. Brackets only - PHP
  invented the spelling, Rails, `qs` and Express read it, and a plain `<form>`
  can produce it without JS. Dot notation stays out: it is the newer JS-side
  convention, and accepting both means two `get` calls per leaf for a spelling
  no browser emits on its own. The point of driving the key off the schema is
  that none of `qs`'s hazards arrive with it - no depth or parameter limit, no
  `__proto__` filtering, no array-vs-object heuristic - because the shape is
  known before a document is read, and a flat schema never probes at all. If a
  second spelling is ever wanted on the wire out, it is a second constant
  (`S.formDataNested`), not a config object on the first.
- **`S.mime`** for uploads, next to the size bounds. Wants a JSON Schema emit
  (`contentMediaType`, and `format: "binary"` for the instances) - which is the
  point at which `minSize`/`maxSize` should be revisited, since neither has a
  keyword today and both are dropped from the emitted document.

### Custom codec follow-ups

- **The ReScript codec seam trusts more than the ReScript type proves.** A
  `~custom` coder's result compiles as a typed decode: the target's refiners
  run, its decoder does not, which is the same deal `S.decodeOrThrow` gives a caller
  who declares the input's schema, and it's why the surface costs nothing on a
  structural target (it skips a full walk plus the object rebuild, not just a
  `typeof`). Two carve-outs exist. Literals, since a type says `string` and
  never `the string "a"`: `B_conversion` routes a const-carrying target through
  the validating seam, the rule `compileDecoder` already states for its own
  typed input. And `S.any`, whose `t<'any>` unifies with whatever the coder
  returns, so `to` drops that whole pair to the junction. What's left is every
  constraint a ReScript type is too coarse to
  imply. `S.float` rejects `NaN` while ReScript's `float` includes it, so
  `S.string->S.to(S.float, ~custom={decode: Sync(_ => Float.Constants.nan), encode: Never})`
  returns `NaN` where the JS surface rejects it, and any `Obj.magic` upstream
  turns the tag itself into a claim rather than a proof. Tightening this inside
  the codec alone would make a coder stricter than `S.decodeOrThrow(~from=S.float)`,
  which accepts the same `NaN`, so both want one shared answer: a single
  predicate for "constraints a tag does not imply", consulted by the
  typed-decode entry and by `B_conversion`. Cheap interim step: route the
  number family through the validating seam the way literals already are, then
  measure what it costs.

- **A ReScript `Sync`/`Async` coder can't target a schema that already
  converts.** `s1->S.to(s2WithChain, ~custom={decode: Sync(fn), ...})` fails at
  creation with "The target already converts", because `codecs<'from, 'to>`
  types the coder against `t<'to>`, which is the chain's output, while the
  value has to be fed to the chain's input. The slots that place no coder are
  exempt - `Auto`, `Never`, and the `Pack`/`Unpack` readings
  (`tests/S_to_custom_test.res` carries `S.uint8Array->S.to(S.jsonString->S.to(S.string),
  ~custom={decode: Unpack, encode: Pack})`), the guard is the `outputSeam`
  branch of `to` in `src/entry.ts`. JS has no such limit: its `{decode,
  encode}` pair lands at the chain head and the whole chain runs after it. So
  the runtime is already there and only the ReScript type is missing:
  `t<'value>` names the output, so a chain's input type has no name to write,
  and `t` stays single-parameter by decision. The error message is the API,
  and chaining `.to` explicitly says exactly what the fused form would have
  meant.

- **A never-slot arm blocks the union's identity shortcut, so encoding a
  default is no longer free.** `unionDecoder` returns the input untouched when
  the source is the union itself and every variant is a noop; a never-slot arm
  fails that test because it carries a `parser` and a `.to`. Encode used to be
  `identity` for every defaulted schema and now dispatches:
  `optional-default` and `nullable-default` pay a `typeof` (+49% on the
  measured encode), `object-advanced` pays one per defaulted field, and
  `nullable-definition-or` pays a full validate-and-rebuild of its object
  (+528%) because a member with no literal discriminant is never compiled
  trusted. Treating a never-linked arm as absent is wrong in general: the
  arm's *input* type is still part of the union's, so a value only it could
  hold has to be rejected rather than passed through. Two sound pieces, both
  in `unionEmit` and both needing `fuzz:union` on either side: drop a dispatch
  check the declared source type already guarantees (compare the live members'
  acceptance masks against the source's - `getOr`'s default arm is a copy of
  the surviving item, so its mask adds nothing and the check falls out), and
  extend trusted case compilation past field-discriminated members, so a lone
  object member validates as little as a typed object does.

### Known bugs left over from the validation refactor (`val.validation: array<validationCheck>`)

- **`err.received` is `unknown` for refine-chain vals on type failures.**
  `S.parseOrThrow(S.string.with(S.minLength, 2))(1)` reports `expected: string` but
  `received: unknown` - `failInvalidType` reads the val's own schema, and a
  refined val's is the refinement's, not the source's. User-visible reason text
  is unaffected (it uses `input->stringify`), but programmatic consumers
  reading `err.received` get nothing usable where the unrefined
  `S.parseOrThrow(S.string)(1)` reports the input's type. Fix: have the fail function
  reach through `val.prev.schema` (with a comment on the invariant that
  validation-owning vals always have a prev).

### Pre-existing bugs surfaced by the TS-migration review (ported faithfully, fix separately)

- **`required` on an object schema is not what its name says, and no two
  producers agree.** `S.schema`, `S.object`, `S.shape` and `S.merge` set it to
  every declared key, optional or not (`S.schema({a: S.optional(S.string)}).required`
  is `["a"]`); `fromJSONSchemaOrThrow` alone filters to the non-optional keys, and the
  comment at that producer (`src/jsonschema.ts`) claims the others already do.
  Parse, inferred types and the emitted JSON Schema are all right
  (`specs/merge-optional.yaml`) - the emitter recomputes from the properties -
  so only the introspected field lies, and it is public: the `Schema` type
  publishes `required?: string[]` on the object variant. `S.merge` has two
  more docs drifts: it inherits `additionalItems` from its *first* argument
  where `docs/js-usage.md` says the second, and the docs say shared keys throw
  where `specs/merge-overwrite.yaml` pins that the second schema's field wins.
- **ReDoS risk in `fromJSONSchemaOrThrow` patterns.** `new RegExp(jsonSchema.pattern)`
  compiles untrusted patterns directly; a hostile JSON Schema can supply a
  catastrophic-backtracking pattern.
- **Homomorphic tuple-mapped types don't map variadic tuple elements.**
  `UnknownArrayToOutput`/`UnknownArrayToInput` (`packages/sury/index.d.ts`)
  guard on `number extends T["length"]` to distinguish tuples from plain
  arrays, but a variadic tuple like `[string, ...number[]]` also has
  `T["length"]` widened to `number`, so it falls into the "return as-is"
  branch instead of mapping each element through `UnknownToOutput`/
  `UnknownToInput`. Same guard existed in the original recursive
  `_RestToOutput`/`_RestToInput` accumulator types, so this isn't a regression
  from the homomorphic-type rewrite - just an existing gap now easier to spot
  in the simpler form.

### `toJSONSchema` drops refinements across a per-variant conversion

`S.json.with(S.to, S.array(S.optional(S.number.with(S.lte, 1))))` emits
`{items: {anyOf: [{type: "number"}, {type: "null"}]}}` - the item's
`maximum: 1` is gone. A variant converted through `.to(json)` (jsonDecoderFn's
`unionRewriteTo`, via `perVariantTo`) is described by the target's type, and the
source's refinements aren't carried onto it. The non-optional
`S.array(S.number.with(S.lte, 1))` keeps its `maximum`, so the loss is specific
to the per-variant path.

Validation is unaffected - the generated code enforces the bound in both
directions - so this is a fidelity gap in the emitted contract, not a hole:
a consumer handed the JSON Schema would accept `[2]` where the codec rejects it.

Pinned by `specs/codec-json-array-optional-bounded.yaml` (FIXME) and the
`toJSONSchema` case in `tests/S_toJSONSchema_test.res`. Surfaced by #376, whose
`undefined -> null` conversion made this shape describable at all - before it,
the whole schema emitted `{}`.

### String formats (follow-ups to the JSON Schema format vocabulary)

Scores below are against the JSON-Schema-Test-Suite `optional/format` corpus,
which is what `packages/sury/specs/<format>.yaml` examples are drawn from.

- `S.email` scores 13/21 - now the weakest format, and untouched pre-existing
  code. The suite wants RFC 5321 behavior where the current regex is the
  practical one Zod ships. Cheapest correctness win left in the vocabulary.
- Emit `pattern` for formats with no JSON Schema name. `cuid` currently vanishes
  in `toJSONSchema` - the denylist in the string branch drops it. Zod emits a
  regex `pattern` in that situation, which would let it survive a round trip
  through a JSON Schema consumer.
- `S.pattern` drops the regex flags when emitting JSON Schema, so
  `S.string.with(S.pattern, /^https:\/\//i)` accepts `HTTPS://` while emitting
  `pattern: "^https:\\/\\/"`, which a downstream validator reads
  case-sensitively and rejects. The emitted schema is stricter than the schema
  it describes. JSON Schema `pattern` has no flag syntax, so the fix is either
  to desugar `i` into the pattern source or to reject flagged regexes that
  cannot be represented.
- `fromJSONSchemaOrThrow` only reaches the format schemas through the
  `type === "string"` branch, so a bare `{"format": "date"}` - which is exactly
  how the JSON-Schema-Test-Suite and most real documents write it - converts to
  an unconstrained schema and validates nothing. Pre-existing (the same gate
  held for `email`/`uri`/`uuid`/`date-time` before the vocabulary landed), but
  it is now the main thing between the format work and real `fromJSONSchemaOrThrow`
  coverage: `packages/json-schema-test-suite` scores `optional/format/date.json`
  at 22/75 where the schemas themselves are 69/69 on the same strings. Faithful
  handling means a string-or-anything-else schema, since `format` is
  type-conditional - the same structural question the suite README raises for
  `maxLength` and `properties`.
- The ordering question behind the `S.uri.with(S.to, S.url)` encode bug is
  settled for the two instance codecs but not in general. A check emits against
  its val's *prev* var, so a val carrying its own transform expression is the
  wrong place to hang one - `date.ts` and `url.ts` both did, and both tested the
  instance rather than the string built from it. They wrap in `B_refine` now.
  Nothing stops the next codec from making the same mistake: the invariant lives
  in a comment on the two encoders rather than in the type or in `B_next`.
- Drop the `.test` from the decode path of a format-plus-codec pair such as
  `S.uri.with(S.to, S.url)` once `S.constructor` exists. Decode runs the URI
  regex *and* constructs the `URL`, which is two validations of one value, and
  under a constructor-shaped schema the construction is the validation - there
  is nothing left for the regex to add. Not done now because it cannot be scoped
  to `uri`: `decode` skips the type guard but keeps every refinement, uniformly
  (`string-minLength` decode still checks `i.length>1`, `ipv6` still `.test`s),
  so dropping it for one format alone makes that format the odd one out.
  The constraint to carry over: the two languages **cross**, so neither check
  subsumes the other. Over ~5.2k sampled forms, 2601 parse as `URL` but fail the
  RFC 3986 regex (`http://a.b `, `%zz`, backslashes, braces) and 181 pass the
  regex but make `new URL` throw (`http:`, `http://` - legal path-empty URIs the
  WHATWG parser refuses). So the construction guard cannot be dropped either,
  and whatever `S.constructor` validates has to be understood as WHATWG's
  language, not RFC 3986's - the schema's accepted set changes with it.
- `S.uriReference` and `S.iriReference` accept `1:b`. RFC 3986 §4.2 builds a
  relative-path reference on `segment-nz-nc` - a first segment with no colon in it,
  the colon being exactly what would make that segment read as a scheme.
  `uriPattern` uses full `pchar` for the rootless-path branch and makes the scheme
  group optional, so the reference forms inherit a first segment that admits `:`.
  Parameterizing that character class is *not* the fix: the same branch carries the
  path of a scheme-bearing URI, where a colon is legal and common - `urn:oasis:names:x`
  and `http:1:b` are valid URIs and therefore valid URI-references, and both would
  start failing. Doing it properly means spelling the reference form as the grammar
  does, `URI-reference = URI / relative-ref`, so the two paths stop being one branch.
  It only over-accepts, and the format suite has no case for it.
- IDNA validation for `S.hostname` / `S.idnHostname` (32/55 and 51/84). Both
  accept an `xn--` label on shape alone; rejecting one whose Punycode decodes to
  a character IDNA2008 disallows needs Punycode plus the Unicode
  derived-property tables (see TypeBox's `src/format/_idna.ts` / `_puny.ts` for
  the shape of it). This is a bundle-size decision rather than a code one, and
  the gap only ever over-accepts - no valid hostname is turned away. The cases
  are published as `known-gap-*` spec examples so they stay visible.

## v11 initial

- Add `s.parseChild` to EffectContext ???
- `S.to` with a raw definition crashes at compile time rather than at
  creation: `S.json.with(S.to, [S.string, S.number])` and
  `S.json.with(S.to, {a: S.string})` throw `loopInput.e.decoder is not a
  function`. Every container accepts inline definitions since rc.1, so `to`
  should either run `definitionToSchema` on its target or reject a
  non-schema target with a creation error.
- Remove fieldOr in favor of optionOr?

## v???

- `S.promise: S.t<'value> => S.t<promise<'value>>` and `S.await: S.t<promise<'value>> => S.t<'value>`
- Remove `S.deepStrict` and `S.deepStrip` in favor of `S.deep` (if it works)
- Somehow determine whether transformed or not (including shape)
- Add JSDoc
- s.optional for object
- S.mutateWith/S.produceWith (aka immer) (???)
- Add S.function (?) (An alternative for external ???)

```

let trimContract: S.contract<string => string> = S.contract(s => {
s.fn(s.arg(0, S.string))
}, ~return=S.string)

```

- Use internal transform for trim
- Add S.codegen
- Make `error.reason` tree-shakeable
- S.produce
- S.mutator
- Check only number of fields for strict object schema when fields are not optional (bad idea since it's not possible to create a good error message, so we still need to have the loop)

## `fromJSONSchemaOrThrow` type inference follow-ups

- **Corpus-wide round-trip dimension (phase 3)** - derive a `fromJSONSchemaOrThrow`
  check in the spec harness from each spec's existing `jsonSchema.input`
  golden (~126 cases): pin the inferred type + instantiations next to the
  emitter's output so a runtime branch gaining support without a matching
  type branch shows up as a spec diff. Harness change → log under Spec
  Harness Suggestions in CONTRIBUTING.md per the spec skill's rule.
- **`default`-fold input/output split** - a non-required property with
  `default` is folded via `Option_getOr`, so it's optional on the input side
  but always present on the output side; the inferred type currently keeps it
  optional on both (sound, just wider). Needs `FromJSONSchema` split into
  per-side resolvers; measure the cost of doubling before committing.
- **Same-level `not` exclusion** - `{ enum: [...], not: { enum: [...] } }`
  could infer `Exclude<...>` cheaply. Only worth doing together with runtime
  structure (today `not` is an opaque refinement), and note upstream
  `json-schema-to-ts` gets the `allOf`-sibling variant wrong - pin whatever
  behavior lands in a spec.

## Recursive protobuf messages

`S.protobuf` rejects a message that refers to itself, directly or through
another message. It costs four cases of the conformance suite
(`NestedMessage.corecursive` is `TestAllTypesProto3` itself, so the field is
left undeclared and rides through as an unknown field), and it is why
`Struct`, `Value` and `ListValue` - the three recursive well-known types - are
undeclared in `packages/protobuf-conformance/testMessages.ts`. Outside the
suite it is the shape of every tree and every linked list.

A spike got the encode direction compiling and found what the rest needs, so
this is a scoped piece of work rather than an open question:

- `inferType` (protobufField.ts) has no case for `refTag`, which is what a
  recursive schema is until it is resolved. One line: a ref can only be a
  message on the wire.
- `firstObject` has to dereference. The `$defs` map lives on the root schema
  alone, so it threads down through `compileMessage`. The decode direction is
  the awkward one: it reaches the object through `objectSchemaOf(input)`,
  which has no root to read `$defs` from.
- `compileMessage` ties the cycle by caching on the object it is compiling and
  allocating `raw`/`schema` before the field loop instead of after, so a field
  can refer to the message that contains it. This part worked as written.
- The encode path splices the *root* message's body inline rather than
  emitting it as a function, so a self-reference has no callee - the spike got
  as far as `ReferenceError: m0 is not defined`. A recursive root has to be
  emitted as a function and called.
- Not reached, and the reason this is not a small change: `message.schema` is
  the decode's output schema, and tying the cycle makes it a *cyclic object*.
  The rest of the library expects recursion spelled as `refTag` + `$defs` -
  the JSON Schema emit and the equality compiler both walk that schema and
  neither would terminate. Either the normalized side keeps the user's own ref
  schema for a recursive field, or the compiler learns to emit ref-shaped
  normalized schemas.

Unknown-field *retention* is the other conformance gap (two cases) and is not
this: it is a deliberate decision, documented under "Unknown fields" in
`docs/js-usage.md`.

## Articles

- Write an article about creating an AI-friendly JS library (how the API design, type overloads like `S.assertInputOrThrow` accepting both arg orders, and error messages make Sury easy for both humans and LLMs to use)

```

```
