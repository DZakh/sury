open Ava

module Common = {
  let value = ("bar", true)
  let invalid = %raw(`123`)
  let factory = () => S.literal(("bar", true))

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse invalid", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalid->S.parseOrThrow(~to=schema),
      `Expected ["bar", true], received 123`,
    )
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
      ~message="Convert operation doesn't validate anything and assumes a valid input",
    )

    t->U.assertThrowsMessage(
      () => invalid->S.parseOrThrow(~to=schema->S.reverse),
      `Expected ["bar", true], received 123`,
    )
  })

  test("Fails to parse array like object", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => %raw(`{0: "bar",1:true}`)->S.parseOrThrow(~to=schema),
      `Expected ["bar", true], received { 0: "bar"; 1: true; }`,
    )
  })

  test("Fails to parse array with excess item", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => %raw(`["bar", true, false]`)->S.parseOrThrow(~to=schema->S.strict),
      `Expected ["bar", true], received ["bar", true, false]`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{Array.isArray(i)&&i.length===2||e[2](i);let v0=i["0"],v1=i["1"];v0==="bar"||e[0](v0);v1===true||e[1](v1);return i}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
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

    t->Assert.deepEqual(value->S.parseOrThrow(~to=schema), value)
  })

  test("Ignores extra items in strip mode and prevents in strict (default)", t => {
    let schema = factory()

    t->Assert.deepEqual(invalid->S.parseOrThrow(~to=schema->S.strip), [])

    t->U.assertThrowsMessage(
      () => invalid->S.parseOrThrow(~to=schema->S.strict),
      `Expected [], received ["abc"]`,
    )
  })

  test("Successfully serializes empty array literal schema", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), value->U.castAnyToUnknown)
  })

  test("Serialize array with excess item in strict mode and it passes through", t => {
    let schema = factory()

    t->Assert.deepEqual(invalid->S.decodeOrThrow(~from=schema->S.strict, ~to=S.unknown), invalid->Obj.magic)
  })

  test("Compiled parse code snapshot of empty array literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{Array.isArray(i)&&i.length===0||e[0](i);return i}`,
    )
  })

  test("Compiled serialize code snapshot of empty array literal schema", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
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
