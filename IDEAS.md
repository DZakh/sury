# Ideas draft

## v10

### rc.0

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
- Support `S.to` for `S.union`, `S.option` and `S.null`
- Support `S.to` for unknown schemas (transforms)
- Temporary removed `S.inline`
- Improved error massage for recursive schema
- Renamed `unknownKeys` to `additionalItems` and made it lowercase.
- Allow to set `S.tuple` to Strip mode (still strict by default)
- Changed `S.removeTypeValidation` to `S.noValidation`. It doesn't affect union discriminated optimisation anymore.
- Removed `S.describe` in favor of `S.meta`.
- Removed `S.description` getter in favor of the `description` field on schema
- Removed `S.deprecate` in favor of `S.meta`
- Removed `S.deprecation` getter in favor of the `deprecated` field on schema. Added `S.deprecated` to JS/TS API
- Removed `S.setName` in favor of `S.meta.
- Renamed `S.name` to `S.toExpression`. Also, it's now possible to get the name itself from the `name` property on schema.
- Renamed `RescriptSchemaError` class to `SuryError`
- (rescript) Renamed `S.Raised` exception to `S.Error`
- (rescript) Removed `Error` module. Access `message` and `reason` directly on `error` type. Also, added the `ErrorClass` module for advanced use-cases.
- (rescript) Removed `RescriptSchema` namespace
- Improve thrown error display in Node.js console https://github.com/DZakh/rescript-schema/issues/106
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
- Removed `S.preprocess` in favor of `S.to` and `S.transform`.
- Added `S.toJSONSchema`
- Renamed `S.stringMinLength` to `S.min`
- Renamed `S.stringMaxLength` to `S.max`
- Renamed `S.stringLength` to `S.length`
- Renamed `S.intMin` to `S.min`
- Renamed `S.intMax` to `S.max`
- Renamed `S.numberMin` to `S.min`
- Renamed `S.numberMax` to `S.max`
- Renamed `S.arrayMinLength` to `S.min`
- Renamed `S.arrayMaxLength` to `S.max`
- Renamed `S.arrayLength` to `S.length`
- (ppx) Improved error message for unsupported attributes
- (ppx) Added support for `@s.meta`, `@s.noValidation`, `@s.strict`, `@s.strip`, `@s.deepStrict` and `@s.deepStrip`
- (ppx) Removed `@s.nullable` in favor of `Nullable.t` support.

### rc.1

- (ts) Make TS type a tagged union
- (ts) Add types for `S.toJSONSchema` and `S.extendJSONSchema`
- Support for `example` metadata

### rc.2

- Move all ts files to S.d.ts as a source of truth (better IDE display on hover)
- Renamed `rescript-schema-ppx` to `sury-ppx`
- Add `nullable` to JSONSchema.res type
- Support `default` for `S.toJSONSchema`
- Renamed `S.setGlobalConfig` to `S.global`
- Improve TS Result type

### rc.3

- `S.to` support from literal to non-literal of the same type
- Fix union case with parsing schemas of the same type
- Export `UnknownToOutput` and `UnknownToInput` and add GenType support for S.schema type
- Fixed `S.toJSONSchema` error message with invalid object fields

### Scope

### Final release fixes

- Fix all tests marked as Failing
- Update docs
- Fix all fixme

## v10.1

- Add S.fromJSONSchema

## v11

- Make built-in refinements not work with `unknown`. Use `S.to` (manually & automatically) to deside the type first
- Start using rescript v12 (Fix unboxed types in JSONSchema module)
- Support arrays for `S.to`
- Remove fieldOr in favor of optionOr?
- Allow to pass custom error message via `.with`
- Make S.to extensible
- Add S.Date (S.instanceof) and remove S.datetime
- Add refinement info to the tagged type
- Replace `s.fail` with `throw new Error`

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
