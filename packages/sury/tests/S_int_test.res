open Vitest

module Common = {
  let value = 123
  let any = %raw(`123`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.int

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected int32, received 123.45`,
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
      `i=>{typeof i==="number"&&i<=2147483647&&i>=-2147483648&&i%1===0||e[0](i);return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
    t->U.assertReverseReversesBack(schema)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, 123)
  })
}

test("Fails to parse int when JSON is a number bigger than +2^31", t => {
  let schema = S.int

  t->U.assertThrowsMessage(
    () => %raw(`2147483648`)->S.parseOrThrow(~to=schema),
    `Expected int32, received 2147483648`,
  )
  t->Assert.deepEqual(%raw(`2147483647`)->S.parseOrThrow(~to=schema), 2147483647)
})

test("Fails to parse int when JSON is a number lower than -2^31", t => {
  let schema = S.int

  t->U.assertThrowsMessage(
    () => %raw(`-2147483649`)->S.parseOrThrow(~to=schema),
    `Expected int32, received -2147483649`,
  )
  t->Assert.deepEqual(%raw(`-2147483648`)->S.parseOrThrow(~to=schema), -2147483648)
})

test("Fails to parse NaN", t => {
  let schema = S.int

  t->U.assertThrowsMessage(
    () => %raw(`NaN`)->S.parseOrThrow(~to=schema),
    `Expected int32, received NaN`,
  )
})
