open Vitest

module Common = {
  let value = "ReScript is Great!"
  let invalidValue = "Hello world!"
  let any = %raw(`"ReScript is Great!"`)
  let invalidAny = %raw(`"Hello world!"`)
  let invalidTypeAny = %raw(`true`)
  let factory = () => S.literal("ReScript is Great!")

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected "ReScript is Great!", received "Hello world!"`,
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected "ReScript is Great!", received true`,
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
      `Expected "ReScript is Great!", received "Hello world!"`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{i==="ReScript is Great!"||e[0](i);return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{i==="ReScript is Great!"||e[0](i);return i}`,
    )
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, "ReScript is Great!")
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, "ReScript is Great!")
  })
}
