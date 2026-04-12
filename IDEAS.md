# Ideas draft

## Alpha.5

- Add `S.date` — standalone Date instance schema. Validates `instanceof Date` and rejects Invalid Date. Unlike `S.isoDateTime` (which validates ISO 8601 UTC strings) and `S.string->S.to(S.date)` (which decodes ISO strings into Date objects), `S.date` directly validates existing Date instances.
- Add `S.isoDateTime` and `S.enableIsoDateTime` — standalone string schema that validates ISO 8601 UTC datetime strings (no timezone offsets, arbitrary sub-second precision). Reuses the built-in string decoder; the regex lives inside `enableIsoDateTime` so it is tree-shaken from the bundle when unused. Replaces the removed `S.datetime` for the "validate an ISO string" use case — for string↔Date conversion use `S.string->S.to(S.date)`.
- Added `S.compactColumns` - transforms columnar data (`[[a1,a2], [b1,b2]]`) to/from row objects (`[{foo:a1,bar:b1}, {foo:a2,bar:b2}]`)
- TypeScript: Use `S.encoder(schema)` for encoding (replaces internal `reverseConvertOrThrow`)
- `S.compactColumns` type is `Schema<Output[][], Input[][]>`
- `S.toExpression` now shows proper type for `S.compactColumns` without `S.to` (e.g., `"string[][]"`)
- Changed `S.refine` from callback-based to boolean-returning. TS/JS: `(value) => boolean` with optional `{ error?: string, path?: string[] }` options. ReScript: `'value => bool` with optional `~error: string=?` and `~path: array<string>=?` labeled arguments. The check returns `true` when valid, `false` when invalid.
- TS/JS API: Renamed `S.asyncParserRefine` to `S.asyncDecoderAssert`. Removed `EffectCtx` (`s`) parameter — throw directly to signal failure instead of calling `s.fail()`
- TS API: Removed `S.transform` in favor of `S.to`
- Add `S.uint8Array` and `S.enableUint8Array`
- Updated `InvalidType` error code to include the received schema
- Updated internal representation of object schema - removed `items` fields. Updated internalt representation of tuple schema - `items` field is now an array of schemas instead of array of items. The `item` type is removed.
- Removed `Failed parsing/converting/asserting` when the error is at root
- Renamed `Failed parsing/converting/asserting at path` to `Failed at path`
- ReScript: Removed `schema` from `S.transform` and `S.refine` context
- ReScript:
  - `S.ErrorClass.constructor` -> `S.Error.make` - now accepts full error details and doesn't require `flag` parameter
  - `S.ErrorClass.t` -> `S.Error.class`
  - `S.ErrorClass.value` -> `S.Error.class`
  - Reworked error code and added `S.Error.classify` to turn error into a variant of all possible error codes
- All errors thrown in transform/refine are wrapped in `SuryError`
- TS: Updated `S.Error` type to use variants instead of code property
- ReScript: `S.null` -> `S.nullAsOption`
- Updated union conversion logic - it now always performs exhaustive validation
- Encoding to JSON now strips undefined fields

### TS

- `S.parseOrThrow` -> `S.parser(schema)(data)`
- `S.parseJsonOrThrow` -> `S.decoder(S.json, schema)(data)`
- `S.parseJsonStringOrThrow` -> `S.decoder(S.jsonString, schema)(data)`
- `S.parseAsyncOrThrow` -> `S.asyncParser(schema)(data)`
- `S.convertOrThrow` -> `S.decoder(schema)(data)`
- `S.convertToJsonOrThrow` -> `S.decoder(schema, S.json)(data)`
- `S.convertToJsonStringOrThrow` -> `S.decoder(schema, S.jsonString)(data)`
- `S.reverseConvertOrThrow` -> `S.encoder(schema)(data)`
- `S.reverseConvertToJsonOrThrow` -> `S.encoder(schema, S.json)(data)`
- `S.reverseConvertToJsonStringOrThrow` -> `S.encoder(schema, S.jsonString)(data)`
- `S.assertOrThrow` -> `S.assert(schema, data)`
- `S.compile` -> `S.decoder` or `S.encoder` or `S.parser`

## v11

### ideas

- Add `promise` type and `S.promise` (instead of async flag internally)

TODO:

Test null<> in ppx

```
// Test that refinement works correctly with reverse

S.reverse(S.schema({
  foo: S.string->S.to(S.number)
})->S.refine(value => value.foo > 0))
```

I left on cleaning up validation code and moving everything to their own decoder functions

- Make reverse a property on schema, so it's not shown when logging
- Keep current operationFn approach. Rename to makeOperation
- Use define property to be enumerable and simplify copy
- Add counter and set unique id to each schema
- Use the unique id to cache the operationFn (from/to) in the schema (partially solves garbage collection problem)
- Also cache reverse result
- makeParseOrThrow
- parseOrThrow(schema)(data) for ts api
- deprecate compile

### ReScript operation functions

| Before | After |
|---|---|
| `S.parseOrThrow(data, schema)` | `S.parseOrThrow(data, ~to=schema)` |
| `S.parseAsyncOrThrow(data, schema)` | `S.parseAsyncOrThrow(data, ~to=schema)` |
| `S.parseJsonOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=S.json, ~to=schema)` |
| `S.parseJsonStringOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=S.jsonString, ~to=schema)` |
| `S.assertOrThrow(data, schema)` | `S.assertOrThrow(data, ~to=schema)` |
| — | `S.assertAsyncOrThrow(data, ~to=schema)` |
| `S.reverseConvertOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.unknown)` |
| `S.reverseConvertToJsonOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.json)` |
| `S.reverseConvertToJsonStringOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.jsonString)` |
| `S.reverseConvertToJsonStringOrThrow(data, schema, ~space=2)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.jsonStringWithSpace(2))` |
| `S.convertOrThrow(data, schema)` | `S.decoder1(schema)(data)` |
| `S.compile(schema, ~input=Any, ~output=Value, ~mode=Sync, ~typeValidation=true)` | `S.parser(~to=schema)` |
| `S.compile(schema, ~input=Any, ~output=Value, ~mode=Async, ~typeValidation=true)` | `S.asyncParser(~to=schema)` |
| `S.compile(schema, ~input=Value, ~output=Unknown, ~mode=Sync, ~typeValidation=false)` | `S.decoder(~from=schema, ~to=S.unknown)` |
| `S.compile(schema, ~input=Value, ~output=Unknown, ~mode=Async, ~typeValidation=false)` | `S.asyncDecoder(~from=schema, ~to=S.unknown)` |

### TS operation functions

- rename `serializer` to reverse parser ?
- Make `foo->S.to(S.unknown)` stricter ??

- Add `S.to(from, target, parser, serializer)` instead of `S.transform`?
- Remove `s.fail` with `throw new Error`
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

```

```
