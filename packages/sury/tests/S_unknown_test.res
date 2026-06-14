open Vitest

module Common = {
  let any = %raw(`"Hello world!"`)
  let factory = () => S.unknown

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), any)
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Parse)
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
    t->U.assertReverseParsesBack(schema, %raw(`new Blob()`))
  })
}

test("Is Unknown variant", t => {
  let schema = S.unknown
  switch schema {
  | Unknown(_) => t->Assert.is(true, true)
  | _ => t->Assert.fail("Expected Unknown")
  }
})
