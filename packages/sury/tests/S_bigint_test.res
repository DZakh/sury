open Ava

module Common = {
  let value = 123n
  let any = %raw(`123n`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.bigint

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.toUnknown, received: invalidAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to convert to Json", t => {
    let schema = factory()

    t->Assert.throws(
      () => value->S.convertToJsonOrThrow(schema),
      ~expectations={
        message: "Failed converting to JSON: The 'bigint' schema cannot be converted to JSON",
      },
      (),
    )
  })

  test("BigInt name", t => {
    let schema = factory()
    t->Assert.is(schema->S.toExpression, "bigint", ())
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(typeof i!=="bigint"){e[0](i)}return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, value)
  })
}
