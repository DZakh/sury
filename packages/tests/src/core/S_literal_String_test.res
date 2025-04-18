open Ava

module Common = {
  let value = "ReScript is Great!"
  let invalidValue = "Hello world!"
  let any = %raw(`"ReScript is Great!"`)
  let invalidAny = %raw(`"Hello world!"`)
  let invalidTypeAny = %raw(`true`)
  let factory = () => S.literal("ReScript is Great!")

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal("ReScript is Great!")->S.toUnknown,
          received: "Hello world!"->Obj.magic,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidTypeAny->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal("ReScript is Great!")->S.toUnknown,
          received: invalidTypeAny,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Fails to serialize invalid value", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidValue->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal("ReScript is Great!")->S.toUnknown,
          received: "Hello world!"->Obj.magic,
        }),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(i!=="ReScript is Great!"){e[0](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(i!=="ReScript is Great!"){e[0](i)}return i}`,
    )
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, "ReScript is Great!")
  })
}
