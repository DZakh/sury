open Ava

module Common = {
  let any = %raw(`true`)
  let factory = () => S.never

  test("Fails to ", t => {
    let schema = factory()

    t->U.assertThrows(
      () => any->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.never->S.castToUnknown, received: any}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to serialize ", t => {
    let schema = factory()

    t->U.assertThrows(
      () => any->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.castToUnknown, received: any}),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{e[0](i);return i}`)
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{e[0](i);return i}`)
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

    t->U.assertThrows(
      () => %raw(`{"key":"value"}`)->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.never->S.castToUnknown, received: %raw(`undefined`)}),
        operation: Parse,
        path: S.Path.fromArray(["oldKey"]),
      },
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
      %raw(`{"key":"value"}`)->S.parseOrThrow(schema),
      {
        "key": "value",
        "oldKey": None,
      },
    )
  })
}
