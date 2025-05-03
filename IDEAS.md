# Ideas draft

## v10

### rc.3

- `S.to` support from literal to non-literal of the same type
- Fix union case with parsing schemas of the same type
- Export `UnknownToOutput` and `UnknownToInput` and add GenType support for S.schema type
- Fixed `S.toJSONSchema` error message with invalid object fields
- Display expected and received values in the same format
- Update validation to start with deeply checking object and literal fields. This might resolve in a less detail error path, but allows nested literals to act as union discriminators.
- Stopped validating literals in conversion mode
- Allowed reverse parsing for `S.object` with nested discriminants

### Scope

### Final release fixes

- Make `S.record` accept two args
- Remove `s.fail` in favor of `throw new Error`
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
