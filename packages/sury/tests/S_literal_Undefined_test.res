open Ava

module Common = {
  let value = ()
  let invalidValue = %raw(`123`)
  let any = %raw(`undefined`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.literal()

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidTypeAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.literal(None)->S.castToUnknown, received: invalidTypeAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any)
  })

  test("Fails to serialize invalid value", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidValue->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({expected: S.literal(None)->S.castToUnknown, received: invalidValue}),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==void 0){e[0](i)}return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==void 0){e[0](i)}return i}`)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, %raw(`undefined`))
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, %raw(`undefined`))
  })
}
