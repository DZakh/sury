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

  test("Fails to parse invalid", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalid->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(Dict.fromArray([("foo", "bar")]))->S.toUnknown,
          received: invalid,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), value->U.castAnyToUnknown, ())
  })

  test("Fails to serialize invalid", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalid->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(Dict.fromArray([("foo", "bar")]))->S.toUnknown,
          received: invalid->U.castAnyToUnknown,
        }),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse null", t => {
    let schema = factory()

    t->U.assertRaised(
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

  test("Fails to parse value with excess fields", t => {
    let schema = factory()

    t->U.assertRaised(
      () => %raw(`{"foo": "bar","excess":true}`)->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(Dict.fromArray([("foo", "bar")]))->S.toUnknown,
          received: %raw(`{"foo": "bar","excess": true}`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Can parse non plain objects", t => {
    let schema = factory()

    t->Assert.deepEqual(makeNotPlainValue()->S.parseOrThrow(schema), makeNotPlainValue(), ())
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(i!==e[0]&&(typeof i!=="object"||!i||Object.keys(i).length!==1||i["foo"]!=="bar")){e[1](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(i!==e[0]&&(typeof i!=="object"||!i||Object.keys(i).length!==1||i["foo"]!=="bar")){e[1](i)}return i}`,
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

  test("Fails to parse empty dict literal schema with invalid type", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalid->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(Dict.make())->S.toUnknown,
          received: invalid->U.castAnyToUnknown,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes empty dict literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), value->U.castAnyToUnknown, ())
  })

  test("Fails to serialize empty dict literal schema with invalid value", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalid->S.reverseConvertOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(Dict.make())->S.toUnknown,
          received: invalid->U.castAnyToUnknown,
        }),
        operation: ReverseConvert,
        path: S.Path.empty,
      },
    )
  })

  test("Compiled parse code snapshot of empty dict literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(i!==e[0]&&(typeof i!=="object"||!i||Object.keys(i).length!==0)){e[1](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of empty dict literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(i!==e[0]&&(typeof i!=="object"||!i||Object.keys(i).length!==0)){e[1](i)}return i}`,
    )
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
