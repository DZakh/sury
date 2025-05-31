open Ava

module Common = {
  let value = 123.
  let any = %raw(`123`)
  let invalidAny = %raw(`"Hello world!"`)
  let factory = () => S.float

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.castToUnknown, received: invalidAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="number"||Number.isNaN(i)){e[0](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse schema to S.float", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.castToUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, 123.3)
  })
}

test("Successfully parses number with a fractional part", t => {
  let schema = S.float

  t->Assert.deepEqual(%raw(`123.123`)->S.parseOrThrow(schema), 123.123, ())
})

test("Fails to parse NaN", t => {
  let schema = S.float

  t->U.assertThrows(
    () => %raw(`NaN`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`NaN`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})
