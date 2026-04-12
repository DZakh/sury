open Ava

module Common = {
  let value = Null.null
  let invalidValue = %raw(`123`)
  let any = %raw(`null`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.literal(Null.null)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected null, received "Hello world!"`,
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
      `Expected null, received 123`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i===null||e[0](i);return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{i===null||e[0](i);return i}`)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, %raw(`null`))
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, %raw(`null`))
  })
}
