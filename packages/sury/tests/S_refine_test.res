open Ava

test("Successfully refines on parsing", t => {
  let schema = S.int->S.refine(s => value =>
    if value < 0 {
      s.fail("Should be positive")
    })

  t->Assert.deepEqual(%raw(`12`)->S.parseOrThrow(schema), 12)
  t->U.assertThrows(
    () => %raw(`-12`)->S.parseOrThrow(schema),
    {
      code: OperationFailed("Should be positive"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails with custom path", t => {
  let schema = S.int->S.refine(s => value =>
    if value < 0 {
      s.fail(~path=S.Path.fromArray(["data", "myInt"]), "Should be positive")
    })

  t->U.assertThrows(
    () => %raw(`-12`)->S.parseOrThrow(schema),
    {
      code: OperationFailed("Should be positive"),
      operation: Parse,
      path: S.Path.fromArray(["data", "myInt"]),
    },
  )
})

test("Successfully refines on serializing", t => {
  let schema = S.int->S.refine(s => value =>
    if value < 0 {
      s.fail("Should be positive")
    })

  t->Assert.deepEqual(12->S.reverseConvertOrThrow(schema), %raw("12"))
  t->U.assertThrows(
    () => -12->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Should be positive"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Successfully parses simple object with empty refine", t => {
  let schema = S.object(s =>
    {
      "foo": s.field("foo", S.string),
      "bar": s.field("bar", S.bool),
    }
  )->S.refine(_ => _ => ())

  t->Assert.deepEqual(
    %raw(`{
      "foo": "string",
      "bar": true,
    }`)->S.parseOrThrow(schema),
    {
      "foo": "string",
      "bar": true,
    },
  )
})

test("Compiled parse code snapshot for simple object with refine", t => {
  let schema = S.object(s =>
    {
      "foo": s.field("foo", S.string),
      "bar": s.field("bar", S.bool),
    }
  )->S.refine(s => _ => s.fail("foo"))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"],v1=i["bar"];if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="boolean"){e[2](v1)}let v2={"foo":v0,"bar":v1,};e[3](v2);return v2}`,
  )
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.refine(s => value =>
    if value < 0 {
      s.fail("Should be positive")
    })
  t->Assert.not(schema->S.reverse, schema->S.castToUnknown)
  t->U.assertEqualSchemas(schema->S.reverse, S.int->S.castToUnknown)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.refine(s => value =>
    if value < 0 {
      s.fail("Should be positive")
    })
  t->U.assertReverseParsesBack(schema, 12)
})

// https://github.com/DZakh/rescript-schema/issues/79
module Issue79 = {
  test("Successfully parses", t => {
    let schema = S.object(s => s.field("myField", S.nullable(S.string)))->S.refine(_ => _ => ())
    let jsonString = `{"myField": "test"}`

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["myField"];if(!(typeof v0==="string"||v0===void 0||v0===null)){e[1](v0)}e[2](v0);return v0}`,
    )
    t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{let v0=i["myField"];e[0](v0);return v0}`)

    t->Assert.deepEqual(jsonString->S.parseJsonStringOrThrow(schema), Value("test"))
  })
}
