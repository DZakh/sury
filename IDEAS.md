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
- (js/ts) `S.nullable` and `S.nullish` don't transform `null` to undefined anymore FIXME: Do it only for `S.nullish` keep the transformation logic for `S.nullable`

### Scope

- Add `.pipe` to Ts api
- Rename S.Raised to S.Error
- Support unions and complex types for S.coerce
- Make S.coerce extencible
- Move description and deprecated to schema fields
- Move example to rescript-schema
- Add S.toJsonSchema and S.fromJsonSchema
- Add S.date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type
- S.transform(s => {
  s.reverse(input => input) // Or s.asyncReverse(input => Promise.resolve(input))
  input => input
  }) // or asyncTransform // Maybe format ?
- Make S.serializeToJsonString super fast
- s.optional for object

## v???

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
