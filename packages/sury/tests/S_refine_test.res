open Vitest

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
    `i=>{typeof i==="object"&&i||e[4](i);let v0=i["foo"],v1=i["bar"];typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);let v2={"foo":v0,"bar":v1,};e[2](v2)||e[3](v2);return v2}`,
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
      `i=>{typeof i==="object"&&i||e[3](i);let v0=i["myField"];if(!(typeof v0==="string"||v0===void 0||v0===null)){e[0](v0)}e[1](v0)||e[2](v0);return v0}`,
    )
    t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{let v0=i["myField"];e[0](v0)||e[1](v0);return i["myField"]}`)

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
      S.schema(s => {"foo": s.matches(S.string)})
      ->S.refine(i => i["foo"] !== "rejectByInput", ~error="Input refine failure")
      ->S.reverse
      ->S.refine(
        i => (i->Obj.magic)["foo"] !== "rejectByOutput",
        ~error="Output refine failure",
      )

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
      {"foo": "ok"}->Obj.magic,
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
    S.schema(s => {"foo": s.matches(S.string->S.to(S.bigint))})
    ->S.refine(
      i => Type.typeof(i["foo"]) === #bigint && i["foo"] >= 0n,
      ~error="Input refine should get a correct input value",
    )
    ->S.reverse

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);e[1](i)||e[3](i);let v0=i["foo"];typeof v0==="bigint"||e[0](v0);return {"foo":""+i["foo"],}}`,
  )

  t->Assert.deepEqual(
    %raw(`{"foo": 123n}`)->S.parseOrThrow(~to=schema),
    {"foo": "123"}->Obj.magic,
  )
  t->U.assertThrowsMessage(
    () => %raw(`{"foo": -1n}`)->S.parseOrThrow(~to=schema),
    `Input refine should get a correct input value`,
  )
})

test("inputRefiner observes pre-transform input on a reversed transforming array schema", t => {
  let schema =
    S.array(S.string->S.to(S.bigint))
    ->S.refine(
      arr =>
        arr->Array.every(x => Type.typeof(x) === #bigint) &&
          arr->Array.every(x => x >= 0n),
      ~error="Array input refine should see bigint elements",
    )
    ->S.reverse

  t->Assert.deepEqual(
    %raw(`[1n, 2n, 3n]`)->S.parseOrThrow(~to=schema),
    ["1", "2", "3"]->Obj.magic,
  )
  t->U.assertThrowsMessage(
    () => %raw(`[-1n]`)->S.parseOrThrow(~to=schema),
    `Array input refine should see bigint elements`,
  )
})

test("Output refine on an array runs on the assembled output array", t => {
  let schema = S.array(S.int)->S.refine(arr => arr->Array.length > 0, ~error="Empty array")

  t->Assert.deepEqual(%raw(`[1, 2, 3]`)->S.parseOrThrow(~to=schema), [1, 2, 3])
  t->U.assertThrowsMessage(() => %raw(`[]`)->S.parseOrThrow(~to=schema), `Empty array`)
})

test("Output refine on a tuple runs on the assembled tuple", t => {
  let schema =
    S.tuple(s => (s.item(0, S.string), s.item(1, S.int)))->S.refine(
      ((s, i)) => String.length(s) === i,
      ~error="String length must match int",
    )

  t->Assert.deepEqual(%raw(`["abc", 3]`)->S.parseOrThrow(~to=schema), ("abc", 3))
  t->U.assertThrowsMessage(
    () => %raw(`["abc", 5]`)->S.parseOrThrow(~to=schema),
    `String length must match int`,
  )
})

// Custom-decoder refiner audit: every advanced decoder must surface
// user S.refine. Each test exercises both the passing and the failing
// branch of a meaningful predicate.

test("Refiner runs on S.dict", t => {
  let schema =
    S.dict(S.int)->S.refine(d => !(d->Dict.keysToArray->Array.includes("fail")), ~error="Dict refine fail")

  t->Assert.deepEqual(%raw(`{"a": 1}`)->S.parseOrThrow(~to=schema), Dict.fromArray([("a", 1)]))
  t->U.assertThrowsMessage(
    () => %raw(`{"fail": 1}`)->S.parseOrThrow(~to=schema),
    `Dict refine fail`,
  )
})

test("Refiner runs on S.json", t => {
  let schema = S.json->S.refine(v => v !== %raw(`null`), ~error="Json refine fail")

  t->Assert.deepEqual(%raw(`{"a": 1}`)->S.parseOrThrow(~to=schema), %raw(`{"a": 1}`))
  t->U.assertThrowsMessage(() => %raw(`null`)->S.parseOrThrow(~to=schema), `Json refine fail`)
})

test("Refiner runs on S.jsonString", t => {
  let schema =
    S.jsonString->S.refine(s => String.length(s) < 10, ~error="JsonString refine fail")

  t->Assert.deepEqual(`1`->S.parseOrThrow(~to=schema), `1`)
  t->U.assertThrowsMessage(
    () => `{"a":1,"b":2}`->S.parseOrThrow(~to=schema),
    `JsonString refine fail`,
  )
})

test("Refiner runs on S.uint8Array", t => {
  let schema =
    S.uint8Array->S.refine(
      arr => (arr->Obj.magic)["byteLength"] < 5,
      ~error="Uint8Array refine fail",
    )

  t->Assert.deepEqual(
    %raw(`new Uint8Array([1, 2, 3])`)->S.parseOrThrow(~to=schema),
    %raw(`new Uint8Array([1, 2, 3])`),
  )
  t->U.assertThrowsMessage(
    () => %raw(`new Uint8Array([1, 2, 3, 4, 5, 6])`)->S.parseOrThrow(~to=schema),
    `Uint8Array refine fail`,
  )
})

test("Refiner runs on S.compactColumns", t => {
  let schema =
    S.compactColumns(S.unknown)
    ->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
          }
        ),
      ),
    )
    ->S.refine(arr => arr->Array.length > 0, ~error="Empty compactColumns")

  t->Assert.deepEqual(
    %raw(`[["a"]]`)->S.parseOrThrow(~to=schema),
    [{"foo": "a"}],
  )
  t->U.assertThrowsMessage(
    () => %raw(`[[]]`)->S.parseOrThrow(~to=schema),
    `Empty compactColumns`,
  )
})

// inputRefiner coverage for the custom decoders. inputRefiner is created
// via S.refine(...)->S.reverse, which swaps refiner ↔ inputRefiner. For
// schemas with nested-item transforms, the predicate must observe the
// pre-transform input type.

test("inputRefiner observes pre-transform input on a reversed transforming dict", t => {
  let schema =
    S.dict(S.string->S.to(S.bigint))
    ->S.refine(
      d =>
        d
        ->Dict.valuesToArray
        ->Array.every(v => Type.typeof(v) === #bigint && v >= 0n),
      ~error="Dict input refine should see bigint values",
    )
    ->S.reverse

  t->Assert.deepEqual(
    %raw(`{"a": 1n, "b": 2n}`)->S.parseOrThrow(~to=schema),
    Dict.fromArray([("a", "1"), ("b", "2")])->Obj.magic,
  )
  t->U.assertThrowsMessage(
    () => %raw(`{"a": -1n}`)->S.parseOrThrow(~to=schema),
    `Dict input refine should see bigint values`,
  )
})

test("inputRefiner observes pre-transform input on a reversed transforming tuple", t => {
  let schema =
    S.tuple(s => (s.item(0, S.string->S.to(S.bigint)), s.item(1, S.int)))
    ->S.refine(
      ((b, i)) => Type.typeof(b) === #bigint && i > 0,
      ~error="Tuple input refine should see bigint at index 0",
    )
    ->S.reverse

  t->Assert.deepEqual(%raw(`[1n, 5]`)->S.parseOrThrow(~to=schema), ("1", 5)->Obj.magic)
  t->U.assertThrowsMessage(
    () => %raw(`[1n, 0]`)->S.parseOrThrow(~to=schema),
    `Tuple input refine should see bigint at index 0`,
  )
})

test("inputRefiner runs on reversed S.json", t => {
  let schema =
    S.json
    ->S.refine(v => v !== %raw(`null`), ~error="Json input refine fail")
    ->S.reverse

  t->Assert.deepEqual(%raw(`{"a": 1}`)->S.parseOrThrow(~to=schema), %raw(`{"a": 1}`))
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Json input refine fail`,
  )
})

test("inputRefiner runs on reversed S.jsonString", t => {
  let schema =
    S.jsonString
    ->S.refine(s => String.length(s) < 10, ~error="JsonString input refine fail")
    ->S.reverse

  t->Assert.deepEqual(`1`->S.parseOrThrow(~to=schema), `1`->Obj.magic)
  t->U.assertThrowsMessage(
    () => `{"a":1,"b":2}`->S.parseOrThrow(~to=schema),
    `JsonString input refine fail`,
  )
})

test("inputRefiner runs on reversed S.uint8Array", t => {
  let schema =
    S.uint8Array
    ->S.refine(arr => (arr->Obj.magic)["byteLength"] < 5, ~error="Uint8Array input refine fail")
    ->S.reverse

  t->Assert.deepEqual(
    %raw(`new Uint8Array([1, 2, 3])`)->S.parseOrThrow(~to=schema),
    %raw(`new Uint8Array([1, 2, 3])`)->Obj.magic,
  )
  t->U.assertThrowsMessage(
    () => %raw(`new Uint8Array([1, 2, 3, 4, 5, 6])`)->S.parseOrThrow(~to=schema),
    `Uint8Array input refine fail`,
  )
})

test("inputRefiner runs on reversed S.compactColumns", t => {
  let schema =
    S.compactColumns(S.unknown)
    ->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
          }
        ),
      ),
    )
    ->S.refine(arr => arr->Array.length > 0, ~error="Empty compactColumns input")
    ->S.reverse

  t->Assert.deepEqual(
    %raw(`[{"foo": "a"}]`)->S.parseOrThrow(~to=schema),
    %raw(`[["a"]]`)->Obj.magic,
  )
  t->U.assertThrowsMessage(
    () => %raw(`[]`)->S.parseOrThrow(~to=schema),
    `Empty compactColumns input`,
  )
})

// shape, recursive, union: gap cases.

test("Refiner runs on S.shape", t => {
  let schema =
    S.string
    ->S.shape(s => Ok(s))
    ->S.refine(
      v =>
        switch v {
        | Ok(s) => String.length(s) < 10
        | _ => false
        },
      ~error="Shape refine fail",
    )

  t->Assert.deepEqual("hello"->S.parseOrThrow(~to=schema), Ok("hello"))
  t->U.assertThrowsMessage(
    () => "hello world"->S.parseOrThrow(~to=schema),
    `Shape refine fail`,
  )
})

test("inputRefiner runs on reversed S.shape", t => {
  let schema =
    S.string
    ->S.shape(s => Ok(s))
    ->S.refine(
      v =>
        switch v {
        | Ok(s) => String.length(s) < 10
        | _ => false
        },
      ~error="Shape input refine fail",
    )
    ->S.reverse

  t->Assert.deepEqual(Ok("hello")->S.parseOrThrow(~to=schema), "hello"->Obj.magic)
  t->U.assertThrowsMessage(
    () => Ok("hello world")->S.parseOrThrow(~to=schema),
    `Shape input refine fail`,
  )
})

test("Refiner runs on S.recursive", t => {
  let schema =
    S.recursive("R", _ => S.string)->S.refine(
      s => String.length(s) < 10,
      ~error="Recursive refine fail",
    )

  t->Assert.deepEqual("hello"->S.parseOrThrow(~to=schema), "hello")
  t->U.assertThrowsMessage(
    () => "hello world"->S.parseOrThrow(~to=schema),
    `Recursive refine fail`,
  )
})

test("inputRefiner runs on reversed S.recursive", t => {
  let schema =
    S.recursive("R", _ => S.string)
    ->S.refine(s => String.length(s) < 10, ~error="Recursive input refine fail")
    ->S.reverse

  t->Assert.deepEqual("hello"->S.parseOrThrow(~to=schema), "hello"->Obj.magic)
  t->U.assertThrowsMessage(
    () => "hello world"->S.parseOrThrow(~to=schema),
    `Recursive input refine fail`,
  )
})

test("Refiner runs on S.union", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.int->S.castToUnknown])->S.refine(
      v => v !== ("fail"->Obj.magic),
      ~error="Union refine fail",
    )

  t->Assert.deepEqual("ok"->Obj.magic->S.parseOrThrow(~to=schema), "ok"->Obj.magic)
  t->U.assertThrowsMessage(
    () => "fail"->Obj.magic->S.parseOrThrow(~to=schema),
    `Union refine fail`,
  )
})

test("inputRefiner runs on reversed S.union", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.int->S.castToUnknown])
    ->S.refine(v => v !== ("fail"->Obj.magic), ~error="Union input refine fail")
    ->S.reverse

  t->Assert.deepEqual("ok"->Obj.magic->S.parseOrThrow(~to=schema), "ok"->Obj.magic)
  t->U.assertThrowsMessage(
    () => "fail"->Obj.magic->S.parseOrThrow(~to=schema),
    `Union input refine fail`,
  )
})
