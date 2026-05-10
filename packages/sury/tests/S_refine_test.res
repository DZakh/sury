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
    `i=>{if(typeof i!=="object"||!i){e[3](i)}let v0=i["foo"],v1=i["bar"],v2={"foo":v0,"bar":v1,};if(typeof v0!=="string"){e[0](v0)}if(typeof v1!=="boolean"){e[1](v1)}if(!e[2](v2)){e[4]()}return v2}`,
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
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["myField"];if(!(typeof v0==="string"||v0===void 0||v0===null)){e[1](v0)}if(!e[2](v0)){e[3]()}return v0}`,
    )
    t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{let v0=i["myField"];if(!e[0](v0)){e[1]()}return v0}`)

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

test(
  "Refiner application order: type-narrow first, then inputRefiner, then output refiner",
  t => {
    let schema =
      S.schema({"foo": S.string})
      ->S.refine(i => i["foo"] !== "rejectByInput", ~error="Input refine failure")
      ->S.reverse
      ->S.refine(i => i["foo"] !== "rejectByOutput", ~error="Output refine failure")

    // Type-narrow on `foo` runs before either refiner.
    t->U.assertThrowsMessage(
      () => %raw(`{"foo": 123}`)->S.parseOrThrow(~to=schema),
      `Failed at ["foo"]: Expected string, received 123`,
    )

    // Type-narrow passes; inputRefiner rejects.
    t->U.assertThrowsMessage(
      () => %raw(`{"foo": "rejectByInput"}`)->S.parseOrThrow(~to=schema),
      `Input refine failure`,
    )

    // Type-narrow + inputRefiner pass; output refiner rejects.
    t->U.assertThrowsMessage(
      () => %raw(`{"foo": "rejectByOutput"}`)->S.parseOrThrow(~to=schema),
      `Output refine failure`,
    )

    // All three pass.
    t->Assert.deepEqual(
      %raw(`{"foo": "ok"}`)->S.parseOrThrow(~to=schema),
      {"foo": "ok"},
    )
  },
)
