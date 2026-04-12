open Ava

module Common = {
  let value = true
  let any = %raw(`true`)
  let invalidAny = %raw(`"Hello world!"`)
  let factory = () => S.bool

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse ", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected boolean, received "Hello world!"`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{typeof i==="boolean"||e[0](i);return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
    t->U.assertReverseReversesBack(schema)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, true)
  })
}

test("Parses bool when JSON is true", t => {
  let schema = S.bool

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(~to=schema), true)
})

test("Parses bool when JSON is false", t => {
  let schema = S.bool

  t->Assert.deepEqual(JSON.Encode.bool(false)->S.parseOrThrow(~to=schema), false)
})
