open Ava

module Common = {
  let value = Null.null
  let invalidValue = %raw(`123`)
  let any = %raw(`null`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.literal(Null.null)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalidTypeAny->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(%raw(`null`))->S.toUnknown,
          received: invalidTypeAny,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Fails to serialize invalid value", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalidValue->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({expected: S.literal(%raw(`null`))->S.toUnknown, received: invalidValue}),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==null){e[0](i)}return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return i}`)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, %raw(`null`))
  })
}
