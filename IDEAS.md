# Ideas draft

## v10

### Done

- Removed `S.to` in favor of `S.shape`
- S.dict now does `!Array.isArray(${inputVar})` check
- Fixed `S.coerce` TS type
- Move `~standard` to the top level. Remove `S.standard`, since now every schema you create implements Standard Schema
- Removed `S.classify` and make the schema type a variant itself
- Removed `literal` type and `Literal` module, since it's now a part of the schema type variant
- Removed `S.undefined` for TS API in favor of `S.schema(undefined)`
- (rescript) Renamed `S.nullable` to `S.nullish` to match js api
- (js/ts) Don't transform `null` to `undefined` for `S.nullish`
- Updated error massage to be shorter and cleaner
- Support coerce from any literal to another literal
- Temporary removed `S.inline`
- Improved error massage for recursive schema
- Support coerce for S.union, S.option and S.null
- Renamed `unknownKeys` to `additionalItems` and made it lowercase.
- Allow to set `S.tuple` to Strip mode (still strict by default)
- Changed `S.removeTypeValidation` to `S.noValidation`. It doesn't affect union discriminated optimisation anymore.
- Renamed `S.describe` to `S.description`. Removed previous `S.description` getter in favor of the `description` field on schema
- Renamed `S.deprecate` to `S.deprecated`. Removed previous `S.deprecation` getter in favor of the `deprecated` field on schema. Added `S.deprecated` to JS/TS API
- Renamed `S.name` to `S.toExpression` and `S.setName` to `S.name`. Also, it's now possible to get the name itself from the `name` property on schema.
- Renamed `RescriptSchemaError` to `SchemaError`
- (rescript) Renamed `S.Raised` exception to `S.SchemaError`
- (js/ts) Renamed `S.Error` to `S.SchemaError`
- (rescript) Removed `RescriptSchema` namespace
- Improve SchemaError display in console https://github.com/DZakh/rescript-schema/issues/106
- (rescript) Added `S.nullAsUnit`
- Remove `rescript-schema` mention from the code and use generic `Schema` instead

### Scope

- Support `@s.name` etc for ppx and fail on removed `@s.describe`
- Todo something with nullish and ppx
- Start using rescript v12
- Todo something with preprocess (remove Unknown tag for now, until coerce is not there yet)
- Integrate rescript-json-schema
- Add `.pipe` to Ts api (or `.with` & if first arg is string - it's a custom error message)
- Support unions and complex types for S.coerce
- Add S.toJSONSchema and S.fromJSONSchema
- Add S.meta like Zod v4 (Move S.example to rescript-schema)
- Remove number/string/array prefixes from refinements
- Update benchmark. Zod v4 and add TypeBox

## v???

- Make S.coerce extensible
- Add S.date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type
- Make S.serializeToJsonString super fast
- Make refinements change type from unknown to known
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
