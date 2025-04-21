open Ava

module Common = {
  let value = {"foo": "bar"}
  let invalid = %raw(`123`)
  let factory = () => S.literal({"foo": "bar"})

  %%raw(`
    export class NotPlainValue {
      constructor() {

        this.foo = "bar";
      }
    }
  `)

  @new
  external makeNotPlainValue: unit => {"foo": string} = "NotPlainValue"

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.parseOrThrow(schema), value, ())
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), value->U.castAnyToUnknown, ())
  })

  test("Fails to serialize invalid", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalid->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal("bar")->S.toUnknown,
          received: %raw(`undefined`),
        }),
        operation: ReverseConvert,
        path: S.Path.fromLocation("foo"),
      },
    )
  })

  test("Fails to parse null", t => {
    let schema = factory()

    t->U.assertThrows(
      () => %raw(`null`)->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(Dict.fromArray([("foo", "bar")]))->S.toUnknown,
          received: %raw(`null`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Can parse object instances, reduces it to normal object by default", t => {
    let schema = factory()

    t->Assert.deepEqual(makeNotPlainValue()->S.parseOrThrow(schema), {"foo": "bar"}, ())
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||i["foo"]!=="bar"){e[0](i)}return {"foo":i["foo"],}}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v0=i["foo"];if(v0!=="bar"){e[0](v0)}return i}`,
    )
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, {"foo": "bar"})
  })
}

module EmptyDict = {
  let value: dict<string> = Dict.make()
  let invalid = Dict.fromArray([("abc", "def")])
  let factory = () => S.literal(Dict.make())

  test("Successfully parses empty dict literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.parseOrThrow(schema), value, ())
  })

  test("Strips extra fields passed to empty dict literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(invalid->S.parseOrThrow(schema), Dict.make(), ())
  })

  test("Successfully serializes empty dict literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), value->U.castAnyToUnknown, ())
  })

  test("Ignores extra fields during conversion of empty object literal", t => {
    let schema = factory()

    t->Assert.is(invalid->S.reverseConvertOrThrow(schema), invalid->Obj.magic, ())
  })

  test("Compiled parse code snapshot of empty dict literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}return {}}`,
    )
    t->U.assertCompiledCode(
      ~schema=schema->S.strict,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)){e[1](i)}let v0;for(v0 in i){if(true){e[0](v0)}}return i}`,
    )
  })

  test("Compiled serialize code snapshot of empty dict literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return i}`)
  })

  test("Reverse empty dict literal schema to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test(
    "Succesfully uses reversed empty dict literal schema for parsing back to initial value",
    t => {
      let schema = factory()
      t->U.assertReverseParsesBack(schema, value)
    },
  )
}
