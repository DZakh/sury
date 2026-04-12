open Ava

module Common = {
  let value = 123.
  let any = %raw(`123`)
  let invalidAny = %raw(`"Hello world!"`)
  let factory = () => S.float

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected number, received "Hello world!"`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="number"&&!Number.isNaN(i)||e[0](i);return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse schema to S.float", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
    t->U.assertReverseReversesBack(schema)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, 123.3)
  })
}

test("Successfully parses number with a fractional part", t => {
  let schema = S.float

  t->Assert.deepEqual(%raw(`123.123`)->S.parseOrThrow(~to=schema), 123.123)
})

test("Fails to parse NaN", t => {
  let schema = S.float

  t->U.assertThrowsMessage(
    () => %raw(`NaN`)->S.parseOrThrow(~to=schema),
    `Expected number, received NaN`,
  )
})
