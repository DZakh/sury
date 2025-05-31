open Ava

module Common = {
  let value = 123.
  let invalidValue = %raw(`444.`)
  let any = %raw(`123`)
  let invalidAny = %raw(`444`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.literal(123.)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.literal(123.)->S.toUnknown, received: 444.->Obj.magic}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidTypeAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.literal(123.)->S.toUnknown, received: invalidTypeAny}),
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

    t->U.assertThrows(
      () => invalidValue->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({expected: S.literal(123.)->S.toUnknown, received: invalidValue}),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==123){e[0](i)}return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==123){e[0](i)}return i}`)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.toUnknown)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, 123.)
  })
}

test("Formatting of negative number with a decimal point in an error message", t => {
  let schema = S.literal(-123.567)

  t->U.assertThrows(
    () => %raw(`"foo"`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.literal(-123.567)->S.toUnknown, received: "foo"->Obj.magic}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})
