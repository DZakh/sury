open Ava

module Common = {
  let value = 123
  let any = %raw(`123`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.int

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
      `i=>{if(typeof i!=="number"||i>2147483647||i<-2147483648||i%1!==0){e[0](i)}return i}`,
    )
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
    t->U.assertReverseParsesBack(schema, 123)
  })
}

test("Fails to parse int when JSON is a number bigger than +2^31", t => {
  let schema = S.int

  t->U.assertThrows(
    () => %raw(`2147483648`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`2147483648`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  t->Assert.deepEqual(%raw(`2147483647`)->S.parseOrThrow(schema), 2147483647, ())
})

test("Fails to parse int when JSON is a number lower than -2^31", t => {
  let schema = S.int

  t->U.assertThrows(
    () => %raw(`-2147483649`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`-2147483649`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  t->Assert.deepEqual(%raw(`-2147483648`)->S.parseOrThrow(schema), -2147483648, ())
})

test("Fails to parse NaN", t => {
  let schema = S.int

  t->U.assertThrows(
    () => %raw(`NaN`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`NaN`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})
