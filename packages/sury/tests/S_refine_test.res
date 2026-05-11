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

// Regression: with a transforming field, the inputRefiner must observe the
// pre-transform input value, not the post-transform output. Here the schema's
// foo field is string -> bigint, so after S.reverse the field decodes
// bigint -> string and the original output refiner becomes the inputRefiner.
// The predicate checks that foo is bigint — which is true of the Input but
// false of the post-decode Output. The check must be pushed onto the input
// val's checks slot so it emits before field decoding code (where the
// bigint -> string transform turns foo into a string).
test("inputRefiner observes pre-transform input on a reversed transforming schema", t => {
  let schema =
    S.schema({"foo": S.string->S.to(S.bigint)})
    ->S.refine(
      i => Js.typeof(i["foo"]) === "bigint",
      ~error="Input refine should get a correct input value",
    )
    ->S.reverse

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);e[1](i)||e[3](i);let v0=i["foo"];typeof v0==="bigint"||e[0](v0);return {"foo":""+v0,}}`,
  )

  t->Assert.deepEqual(
    %raw(`{"foo": 123n}`)->S.parseOrThrow(~to=schema),
    {"foo": "123"},
  )
})

// arrayDecoder analog: refiners on an array schema must observe the
// pre-transform input (for inputRefiner) and the assembled output
// (for outputRefiner). The array element is string -> bigint, so the
// reversed schema decodes bigint -> string; the inputRefiner predicate
// checks bigint and would reject if it ran on the post-decode strings.
test("inputRefiner observes pre-transform input on a reversed transforming array schema", t => {
  let schema =
    S.array(S.string->S.to(S.bigint))
    ->S.refine(
      arr => arr->Js.Array2.every(x => Js.typeof(x) === "bigint"),
      ~error="Array input refine should see bigint elements",
    )
    ->S.reverse

  t->Assert.deepEqual(
    %raw(`[1n, 2n, 3n]`)->S.parseOrThrow(~to=schema),
    ["1", "2", "3"],
  )
})

test("Output refine on an array runs on the assembled output array", t => {
  let schema = S.array(S.int)->S.refine(arr => arr->Js.Array2.length > 0, ~error="Empty array")

  t->Assert.deepEqual(%raw(`[1, 2, 3]`)->S.parseOrThrow(~to=schema), [1, 2, 3])
  t->U.assertThrowsMessage(() => %raw(`[]`)->S.parseOrThrow(~to=schema), `Empty array`)
})

test("Output refine on a tuple runs on the assembled tuple", t => {
  let schema =
    S.tuple(s => (s.item(0, S.string), s.item(1, S.int)))->S.refine(
      ((s, i)) => Js.String2.length(s) === i,
      ~error="String length must match int",
    )

  t->Assert.deepEqual(%raw(`["abc", 3]`)->S.parseOrThrow(~to=schema), ("abc", 3))
  t->U.assertThrowsMessage(
    () => %raw(`["abc", 5]`)->S.parseOrThrow(~to=schema),
    `String length must match int`,
  )
})
