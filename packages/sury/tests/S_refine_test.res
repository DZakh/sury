open Ava

test("Successfully refines on parsing", t => {
  let schema = S.int->S.refine(value => value >= 0, ~error="Should be positive")

  t->Assert.deepEqual(%raw(`12`)->S.parseOrThrow(~to=schema), 12)
  t->U.assertThrowsMessage(() => %raw(`-12`)->S.parseOrThrow(~to=schema), `Should be positive`)
})

test("Fails with default error message", t => {
  let schema = S.int->S.refine(value => value >= 0)

  t->Assert.deepEqual(%raw(`12`)->S.parseOrThrow(~to=schema), 12)
  t->U.assertThrowsMessage(() => %raw(`-12`)->S.parseOrThrow(~to=schema), `Refinement failed`)
})

test("Fails with custom path", t => {
  let schema = S.int->S.refine(
    value => value >= 0,
    ~error="Should be positive",
    ~path=["confirm"],
  )

  t->U.assertThrowsMessage(
    () => %raw(`-12`)->S.parseOrThrow(~to=schema),
    `Failed at ["confirm"]: Should be positive`,
  )
})

test("Successfully refines on serializing", t => {
  let schema = S.int->S.refine(value => value >= 0, ~error="Should be positive")

  t->Assert.deepEqual(12->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw("12"))
  t->U.assertThrowsMessage(() => -12->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Should be positive`)
})

test("Successfully parses simple object with empty refine", t => {
  let schema = S.object(s =>
    {
      "foo": s.field("foo", S.string),
      "bar": s.field("bar", S.bool),
    }
  )->S.refine(_ => true)

  t->Assert.deepEqual(
    %raw(`{
      "foo": "string",
      "bar": true,
    }`)->S.parseOrThrow(~to=schema),
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
  )->S.refine(_ => false, ~error="foo")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["foo"],v1=i["bar"];typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);return {"foo":v0,"bar":v1,}}`,
  )
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.refine(value => value >= 0, ~error="Should be positive")
  t->Assert.not(schema->S.reverse, schema->S.castToUnknown)
  t->U.assertEqualSchemas(schema->S.reverse, S.int->S.castToUnknown)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.refine(value => value >= 0, ~error="Should be positive")
  t->U.assertReverseParsesBack(schema, 12)
})

// https://github.com/DZakh/rescript-schema/issues/79
module Issue79 = {
  test("Successfully parses", t => {
    let schema = S.object(s => s.field("myField", S.nullable(S.string)))->S.refine(_ => true)

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[1](i);let v0=i["myField"];if(!(typeof v0==="string"||v0===void 0||v0===null)){e[0](v0)}return v0}`,
    )
    t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{return i["myField"]}`)

    t->Assert.deepEqual(%raw(`{"myField": "test"}`)->S.parseOrThrow(~to=schema), Value("test"))
  })
}

test("Chaining refinements", t => {
  let schema = S.int
    ->S.refine(value => value > 0, ~error="Must be positive")
    ->S.refine(value => mod(value, 2) === 0, ~error="Must be even")

  t->Assert.deepEqual(%raw(`4`)->S.parseOrThrow(~to=schema), 4)
  t->U.assertThrowsMessage(() => %raw(`-2`)->S.parseOrThrow(~to=schema), `Must be positive`)
  t->U.assertThrowsMessage(() => %raw(`3`)->S.parseOrThrow(~to=schema), `Must be even`)
})
