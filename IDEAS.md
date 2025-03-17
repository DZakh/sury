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
- Updated error massage to look more like ArkType
- Support coerce from any literal to another literal
- Unknown keys union changed to lower case
- Removed `S.inline`
- Improved error massage for recursive schema
- Support coerce for S.union, S.option and S.null
- Renamed `unknownKeys` to `additionalItems`.
- Make `S.tuple` strip additional items by default
- Add `strict` and `strip` modes support for `S.tuple`

### Scope

- Rename `setName` to `name` (or `ref`) and move the getter to schema type
- Move description and deprecated to schema fields
- Start using rescript v12
- Remove namespace and integrate rescript-json-schema
- Todo something with preprocess
- Rename RescriptSchemaError to SchemaError
- Remove `Failed parsing at ...` for error messages
- Provide isOptional in Union tag
- Somehow determine whether transformed or not (including shape)
- Make refinements change type from unknown to known
- Add `.pipe` to Ts api
- Rename S.Raised to S.Error
- Support unions and complex types for S.coerce
- Make S.coerce extencible
- Move example to rescript-schema
- Add S.toJsonSchema and S.fromJsonSchema
- Add S.date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type
- Make S.serializeToJsonString super fast

## v???

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
