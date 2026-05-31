open Vitest

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

    t->Assert.deepEqual(value->S.parseOrThrow(~to=schema), value)
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), value->U.castAnyToUnknown)
  })

  test("Fails to serialize invalid", t => {
    let schema = factory()

    t->Assert.is(
      invalid->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      invalid,
      ~message=`Convert operation doesn't validate anything and assumes a valid input`,
    )

    t->U.assertThrowsMessage(
      () => invalid->S.parseOrThrow(~to=schema->S.reverse),
      `Expected { foo: "bar"; }, received 123`,
    )
  })

  test("Fails to parse null", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => %raw(`null`)->S.parseOrThrow(~to=schema),
      `Expected { foo: "bar"; }, received null`,
    )
  })

  test("Can parse object instances, reduces it to normal object by default", t => {
    let schema = factory()

    t->Assert.deepEqual(makeNotPlainValue()->S.parseOrThrow(~to=schema), {"foo": "bar"})
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[1](i);let v0=i["foo"];v0==="bar"||e[0](v0);return {"foo":v0,}}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
  })

  test("Reverse schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, %raw(`{foo: "bar"}`))
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

    t->Assert.deepEqual(value->S.parseOrThrow(~to=schema), value)
  })

  test("Strips extra fields passed to empty dict literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(invalid->S.parseOrThrow(~to=schema), Dict.make())
  })

  test("Successfully serializes empty dict literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), value->U.castAnyToUnknown)
  })

  test("Ignores extra fields during conversion of empty object literal", t => {
    let schema = factory()

    t->Assert.is(invalid->S.decodeOrThrow(~from=schema, ~to=S.unknown), invalid->Obj.magic)
  })

  test("Compiled parse code snapshot of empty dict literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[0](i);return {}}`,
    )
    t->U.assertCompiledCode(
      ~schema=schema->S.strict,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);let v0;for(v0 in i){if(true){e[0](v0)}}return i}`,
    )
  })

  test("Compiled serialize code snapshot of empty dict literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
  })

  test("Reverse empty dict literal schema to self", t => {
    let schema = factory()
    t->U.assertReverseReversesBack(schema)
    t->U.assertReverseParsesBack(schema, %raw(`{}`))
  })

  test(
    "Succesfully uses reversed empty dict literal schema for parsing back to initial value",
    t => {
      let schema = factory()
      t->U.assertReverseParsesBack(schema, value)
    },
  )
}
