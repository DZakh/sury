# Ideas draft

## Alpha.2

- Fix array validation during reverse parsing of nested arrays/tuples
- Tuple literals are not validated during convert operation

## v11

### ideas

- Optimise bundle size

- Add `promise` type and `S.promise` (instead of async flag internally)

- Add `S.parseFromOrThrow(fromSchema, toSchema)` and `convertFromOrThrow` + async.
  Deprecate `reverseConvert` and `parseJsonOrThrow`, `parseJsonStringOrThrow`, `convertToJsonOrThrow`, `convertToJsonStringOrThrow`, `reverseConvertOrThrow`, `reverseConvertToJsonOrThrow`, `reverseConvertToJsonStringOrThrow`

- rename `serializer` to reverse parser ?
- Make `foo->S.to(S.unknown)` stricter ??

- Add `S.to(from, target, parser, serializer)` instead of `S.transform`?
- Remove `s.fail` with `throw new Error`
- Make built-in refinements not work with `unknown`. Use `S.to` (manually & automatically) to deside the type first
- Better inline empty recursive schema operations (union convert)
- Don't iterate over JSON value when it's `S.json` convert without parsing
- Add `S.date.with(S.migrationFrom, S.string, <optionalParser>)`.

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
