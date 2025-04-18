# Ideas draft

## v10

### Done

- Removed `S.to` in favor of `S.shape`
- S.dict now does `!Array.isArray(${inputVar})` check
- Renamed `S.coerce` to `S.to`
- Fixed `S.to` TS type
- Move `~standard` to the top level. Remove `S.standard`, since now every schema you create implements Standard Schema
- Removed `S.classify` and make the schema type a variant itself
- Removed `literal` type and `Literal` module, since it's now a part of the schema type variant
- Removed `S.undefined` for TS API in favor of `S.schema(undefined)`
- (rescript) Renamed `S.nullable` to `S.nullish` to match js api
- (js/ts) Don't transform `null` to `undefined` for `S.nullish`
- Updated error massage to be shorter and cleaner
- Support `S.to` from any literal to another literal
- Temporary removed `S.inline`
- Improved error massage for recursive schema
- Support `S.to` for `S.union`, `S.option` and `S.null`
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
- Default `Input` generic to `unknown` instead of `Output`
  ```diff
  - export type Schema<Output, Input = Output>
  + export type Schema<Output, Input = unknown>
  ```
- Fix infered type of `S.merge` call (https://github.com/DZakh/rescript-schema/issues/107)
- (ts) Infer optional fields with `?` when using `S.schema` (https://github.com/DZakh/rescript-schema/issues/87)
- (ts) Add `.with` helper
- (ts) Allow to pass default value to `S.nullable`
- (rescript) Removed `InvalidUnion` error in favor of `InvalidType` with `unionErrors` field
- Improved union error message

### Scope

- Support `@s.name` etc for ppx and fail on removed `@s.describe`
- Todo something with nullish and ppx
- Todo something with preprocess (remove Unknown tag for now, until `S.to` is not there yet)
- Integrate rescript-json-schema
- Add S.toJSONSchema and S.fromJSONSchema
- Add S.meta like Zod v4 (Move S.example to rescript-schema)
- Update benchmark. Zod v4 and add TypeBox
- Support `S.to` from `unknown` and to `unknown`

## v11

- Start using rescript v12
- Remove number/string/array prefixes from refinements
- Support arrays for `S.to`
- Remove fieldOr in favor of optionOr?
- Make built-in refinements not work with `unknown`. Use `S.to` (manually & automatically) to deside the type first
- Allow to pass custom error message via `.with`
- Make S.to extensible
- Add S.date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type
- Make refinements change type from unknown to known

## v???

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
