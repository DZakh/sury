open Ava

test("Supports String", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.string("Foo")

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data, ())
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data, ())
})

test("Supports Number", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.float(123.)

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data, ())
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data, ())
})

test("Supports Bool", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.bool(true)

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data, ())
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data, ())
})

test("Supports Null", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.null

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data, ())
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data, ())
})

test("Supports Array", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.array([JSON.Encode.string("foo"), JSON.Encode.null])

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data, ())
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data, ())
})

test("Supports Object", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.object(
    [("bar", JSON.Encode.string("foo")), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data, ())
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data, ())
})

test("Fails to parse Object field", t => {
  let schema = S.json(~validate=true)
  let data = JSON.Encode.object(
    [("bar", %raw(`undefined`)), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->U.assertRaised(
    () => data->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`undefined`), expected: schema->S.toUnknown}),
      operation: Parse,
      path: S.Path.fromLocation("bar"),
    },
  )
})

test("Fails to parse matrix field", t => {
  let schema = S.json(~validate=true)
  let data = %raw(`[1,[undefined]]`)

  t->U.assertRaised(
    () => data->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`undefined`), expected: schema->S.toUnknown}),
      operation: Parse,
      path: S.Path.fromArray(["1", "0"]),
    },
  )
})

test("Fails to parse NaN", t => {
  let schema = S.json(~validate=true)
  t->U.assertRaised(
    () => %raw(`NaN`)->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`NaN`), expected: schema->S.toUnknown}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse undefined", t => {
  let schema = S.json(~validate=true)
  t->U.assertRaised(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`undefined`), expected: schema->S.toUnknown}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.json(~validate=true)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{return e[0](i)}`)
})

test("Compiled parse code snapshot with validate=false", t => {
  let schema = S.json(~validate=false)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Parse)
})

test("Compiled serialize code snapshot", t => {
  let schema = S.json(~validate=true)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Reverse schema to S.json(~validate=false) with validate=true", t => {
  let schema = S.json(~validate=true)
  t->U.assertEqualSchemas(schema->S.reverse, S.json(~validate=false)->S.toUnknown)
})

test("Succesfully uses reversed schema with validate=true for parsing back to initial value", t => {
  let schema = S.json(~validate=true)
  t->U.assertReverseParsesBack(schema, %raw(`{"foo":"bar"}`))
})

test("Reverse schema to self with validate=false", t => {
  let schema = S.json(~validate=false)
  t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
})

test(
  "Succesfully uses reversed schema with validate=false for parsing back to initial value",
  t => {
    let schema = S.json(~validate=false)
    t->U.assertReverseParsesBack(schema, %raw(`{"foo":"bar"}`))
  },
)
