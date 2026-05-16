open Ava

module Common = {
  let any = %raw(`true`)
  let factory = () => S.never

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(() => any->S.parseOrThrow(~to=schema), `Expected never, received true`)
  })

  test("Fails to serialize ", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => any->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      `Expected never, received true`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{e[0](i);return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{e[0](i);return i}`)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
    t->U.assertReverseReversesBack(schema)
  })
}

module ObjectField = {
  test("Fails to parse a object with Never field", t => {
    let schema = S.object(s =>
      {
        "key": s.field("key", S.string),
        "oldKey": s.field("oldKey", S.never),
      }
    )

    t->U.assertThrowsMessage(
      () => %raw(`{"key":"value"}`)->S.parseOrThrow(~to=schema),
      `Failed at ["oldKey"]: Expected never, received undefined`,
    )
  })

  test("Successfully parses a object with Never field when it's optional and not present", t => {
    let schema = S.object(s =>
      {
        "key": s.field("key", S.string),
        "oldKey": s.field(
          "oldKey",
          S.never
          ->S.option
          ->S.meta({description: "We stopped using the field from the v0.9.0 release"}),
        ),
      }
    )

    t->Assert.deepEqual(
      %raw(`{"key":"value"}`)->S.parseOrThrow(~to=schema),
      {
        "key": "value",
        "oldKey": None,
      },
    )
  })
}
