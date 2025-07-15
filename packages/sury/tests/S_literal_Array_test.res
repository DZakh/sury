open Ava

module Common = {
  let value = ("bar", true)
  let invalid = %raw(`123`)
  let factory = () => S.literal(("bar", true))

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.parseOrThrow(schema), value)
  })

  test("Fails to parse invalid", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalid->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(("bar", true))->S.castToUnknown,
          received: invalid,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), value->U.castAnyToUnknown)
  })

  test("Fails to serialize invalid", t => {
    let schema = factory()

    t->Assert.is(
      invalid->S.reverseConvertOrThrow(schema),
      invalid,
      ~message="Convert operation doesn't validate anything and assumes a valid input",
    )

    t->U.assertThrowsMessage(
      () => invalid->S.parseOrThrow(schema->S.reverse),
      `Failed parsing: Expected ["bar", true], received 123`,
    )
  })

  test("Fails to parse array like object", t => {
    let schema = factory()

    t->U.assertThrows(
      () => %raw(`{0: "bar",1:true}`)->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(("bar", true))->S.castToUnknown,
          received: %raw(`{0: "bar",1:true}`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse array with excess item", t => {
    let schema = factory()

    t->U.assertThrows(
      () => %raw(`["bar", true, false]`)->S.parseOrThrow(schema->S.strict),
      {
        code: InvalidType({
          expected: S.literal(("bar", true))->S.castToUnknown,
          received: %raw(`["bar", true, false]`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)||i.length!==2||i["0"]!=="bar"||i["1"]!==true){e[0](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, ("bar", true))
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, ("bar", true))
  })
}

module EmptyArray = {
  let value: array<string> = []
  let invalid = ["abc"]
  let factory = () => S.literal([])

  test("Successfully parses empty array literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.parseOrThrow(schema), value)
  })

  test("Ignores extra items in strip mode and prevents in strict (default)", t => {
    let schema = factory()

    t->Assert.deepEqual(invalid->S.parseOrThrow(schema->S.strip), [])

    t->U.assertThrows(
      () => invalid->S.parseOrThrow(schema->S.strict),
      {
        code: InvalidType({
          expected: S.literal([])->S.castToUnknown,
          received: invalid->U.castAnyToUnknown,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes empty array literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), value->U.castAnyToUnknown)
  })

  test("Serialize array with excess item in strict mode and it passes through", t => {
    let schema = factory()

    t->Assert.deepEqual(invalid->S.reverseConvertOrThrow(schema->S.strict), invalid->Obj.magic)
  })

  test("Compiled parse code snapshot of empty array literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)||i.length!==0){e[0](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of empty array literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse empty array literal schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, [])
  })

  test(
    "Succesfully uses reversed empty array literal schema for parsing back to initial value",
    t => {
      let schema = factory()
      t->U.assertReverseParsesBack(schema, value)
    },
  )
}
