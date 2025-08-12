# Ideas draft

## Alpha.3

- `S.unknown.with(S.to, S.schema(true).with(S.noValidation, true))` - Support transformation to literals without validation, to be able to set the output value. (This is how assert works)
- Use ReScript v12
- Use unboxed variants for JSONSchema definitions

## v11

### ideas

- Add `promise` type and `S.promise` (instead of async flag internally)

```diff
const userSchema = S.schema({
  id: S.string,
  name: S.string
})

S.parseOrThrow(data, userSchema)

- S.parseJsonOrThrow(data, userSchema)
+ S.convertFromOrThrow(data, S.json, userSchema)

- S.parseJsonStringOrThrow(data, userSchema)
+ S.convertFromOrThrow(data, S.jsonString, userSchema)

- S.reverseConvertOrThrow(user, userSchema)
+ S.convertOrThrow(user, S.reverse(userSchema))

- S.reverseConvertToJsonOrThrow(user, userSchema)
+ S.convertFromOrThrow(user, userSchema, S.json)

- S.reverseConvertToJsonStringOrThrow(user, userSchema)
+ S.convertFromOrThrow(user, userSchema, S.jsonString)

- S.reverseConvertToJsonStringOrThrow(user, userSchema, 2)
+ S.convertFromOrThrow(user, userSchema, S.jsonStringWithSpace(2))

S.convertOrThrow(data, userSchema)

- S.convertToJsonOrThrow(data, userSchema)
+ S.convertOrThrow(data, userSchema.with(S.to, S.json))

- S.convertToJsonStringOrThrow(data, userSchema)
+ S.convertOrThrow(data, userSchema.with(S.to, S.jsonString))
```

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

## v11 initial

- Add `s.parseChild` to EffectContext ???
- Start using rescript v12 (Fix unboxed types in JSONSchema module)
- Support arrays for `S.to`
- Remove fieldOr in favor of optionOr?
- Allow to pass custom error message via `.with`
- Make S.to extensible
- Add S.Date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type

## v???

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
