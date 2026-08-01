# Ideas draft

## v11

### ideas

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
  `js_merge` (`packages/sury/src/jsapi.ts`) rebuilds the merged object with
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
- **`toJSONSchema` infinitely recurses on a string-format ↔ string conversion.**
  `S.email.with(S.to, S.string)`, `S.uuid.with(S.to, S.string)`,
  `S.cuid.with(S.to, S.string)`, `S.string.with(S.to, S.email)` and
  `S.env.with(S.to, S.string)` all throw `Maximum call stack size exceeded`
  (`codec-env-string` and `codec-env-string-min0` record that as their
  `jsonSchema` golden). `internalToJSONSchema` calls `encodeToJsonSchema` for
  any schema with a `.to`, which reverses, parses, and recurses on `output.s`
  (`packages/sury/src/jsonschema.ts`). When both sides share a type tag the
  decode is a no-op, so `output.s` is the *source* schema — still carrying the
  same `.to` — and the pair recurses forever. `S.int32`/`S.number` escapes only
  because the int32 branch rebuilds the val. Fix: recurse into
  `internalToJSONSchemaBase` rather than `internalToJSONSchema` from inside
  `encodeToJsonSchema` (an encode has already been attempted, so re-entering the
  encode path can't add precision), or bail when the parse made no progress.
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
  `UnknownArrayToOutput`/`UnknownArrayToInput` (`packages/sury/src/S.d.ts`)
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

## Articles

- Write an article about creating an AI-friendly JS library (how the API design, type overloads like `S.is`/`S.assert` accepting both arg orders, and error messages make Sury easy for both humans and LLMs to use)

```

```
