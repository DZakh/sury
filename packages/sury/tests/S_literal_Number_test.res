open Vitest

module Common = {
  let value = 123.
  let invalidValue = %raw(`444.`)
  let any = %raw(`123`)
  let invalidAny = %raw(`444`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.literal(123.)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertThrowsMessage(() => invalidAny->S.parseOrThrow(~to=schema), `Expected 123, received 444`)
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected 123, received "Hello world!"`,
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
      `Expected 123, received 444`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i===123||e[0](i);return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===123||e[0](i);return i}`)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, 123.)
  })
}

test("Formatting of negative number with a decimal point in an error message", t => {
  let schema = S.literal(-123.567)

  t->U.assertThrowsMessage(
    () => %raw(`"foo"`)->S.parseOrThrow(~to=schema),
    `Expected -123.567, received "foo"`,
  )
})
