open Ava

module Common = {
  let value = %raw(`NaN`)
  let invalidValue = %raw(`123`)
  let any = %raw(`NaN`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.literal(%raw(`NaN`))

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected NaN, received "Hello world!"`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Fails to serialize invalid value", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidValue->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      `Expected NaN, received 123`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{Number.isNaN(i)||e[0](i);return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{Number.isNaN(i)||e[0](i);return i}`,
    )
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, %raw(`NaN`))
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, %raw(`NaN`))
  })
}
