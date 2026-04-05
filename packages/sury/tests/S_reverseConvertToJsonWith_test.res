open Ava

S.enableJson()

test("Successfully reverse converts jsonable schemas", t => {
  t->Assert.deepEqual(true->S.reverseConvertToJsonOrThrow(S.bool), true->JSON.Encode.bool)
  t->Assert.deepEqual(true->S.reverseConvertToJsonOrThrow(S.literal(true)), true->JSON.Encode.bool)
  t->Assert.deepEqual("abc"->S.reverseConvertToJsonOrThrow(S.string), "abc"->JSON.Encode.string)
  t->Assert.deepEqual(
    "abc"->S.reverseConvertToJsonOrThrow(S.literal("abc")),
    "abc"->JSON.Encode.string,
  )
  t->Assert.deepEqual(123->S.reverseConvertToJsonOrThrow(S.int), 123.->JSON.Encode.float)
  t->Assert.deepEqual(123->S.reverseConvertToJsonOrThrow(S.literal(123)), 123.->JSON.Encode.float)
  t->Assert.deepEqual(123.->S.reverseConvertToJsonOrThrow(S.float), 123.->JSON.Encode.float)
  t->Assert.deepEqual(123.->S.reverseConvertToJsonOrThrow(S.literal(123.)), 123.->JSON.Encode.float)
  t->Assert.deepEqual(
    (true, "foo", 123)->S.reverseConvertToJsonOrThrow(S.literal((true, "foo", 123))),
    JSON.Encode.array([JSON.Encode.bool(true), JSON.Encode.string("foo"), JSON.Encode.float(123.)]),
  )
  t->Assert.deepEqual(
    {"foo": true}->S.reverseConvertToJsonOrThrow(S.literal({"foo": true})),
    JSON.Encode.object(Dict.fromArray([("foo", JSON.Encode.bool(true))])),
  )
  t->Assert.deepEqual(
    {"foo": (true, "foo", 123)}->S.reverseConvertToJsonOrThrow(
      S.literal({"foo": (true, "foo", 123)}),
    ),
    JSON.Encode.object(
      Dict.fromArray([
        (
          "foo",
          JSON.Encode.array([
            JSON.Encode.bool(true),
            JSON.Encode.string("foo"),
            JSON.Encode.float(123.),
          ]),
        ),
      ]),
    ),
  )
  t->Assert.deepEqual(None->S.reverseConvertToJsonOrThrow(S.nullAsOption(S.bool)), JSON.Encode.null)
  t->Assert.deepEqual(
    JSON.Encode.null->S.reverseConvertToJsonOrThrow(S.literal(JSON.Encode.null)),
    JSON.Encode.null,
  )
  t->Assert.deepEqual([]->S.reverseConvertToJsonOrThrow(S.array(S.bool)), JSON.Encode.array([]))
  t->Assert.deepEqual(
    Dict.make()->S.reverseConvertToJsonOrThrow(S.dict(S.bool)),
    JSON.Encode.object(Dict.make()),
  )
  t->Assert.deepEqual(
    true->S.reverseConvertToJsonOrThrow(S.object(s => s.field("foo", S.bool))),
    JSON.Encode.object(Dict.fromArray([("foo", JSON.Encode.bool(true))])),
  )
  t->Assert.deepEqual(
    true->S.reverseConvertToJsonOrThrow(S.tuple1(S.bool)),
    JSON.Encode.array([JSON.Encode.bool(true)]),
  )
  t->Assert.deepEqual(
    "foo"->S.reverseConvertToJsonOrThrow(S.union([S.literal("foo"), S.literal("bar")])),
    JSON.Encode.string("foo"),
  )
})

test("Encodes option schema to JSON", t => {
  let schema = S.option(S.bool)
  t->Assert.deepEqual(None->S.reverseConvertToJsonOrThrow(schema), JSON.Encode.null)
  t->Assert.deepEqual(Some(true)->S.reverseConvertToJsonOrThrow(schema), JSON.Encode.bool(true))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertToJson,
    `i=>{if(i===void 0){i=null}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
})

test("Allows to convert to JSON with option as an object field", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.option(S.bool)),
    }
  )
  t->Assert.deepEqual(
    {"foo": None}->S.reverseConvertToJsonOrThrow(schema),
    %raw(`{}`),
  )
})

test("Allows to convert to JSON with optional S.json as an object field", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.option(S.json)),
    }
  )
  t->Assert.deepEqual(
    {"foo": None}->S.reverseConvertToJsonOrThrow(schema),
    %raw(`{}`),
  )
})

test("Doesn't allow to convert to JSON array with optional items", t => {
  let schema = S.array(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => [None]->S.reverseConvertToJsonOrThrow(schema),
    "Failed at []: Unsupported conversion from boolean | undefined to JSON",
  )
})

test("Doesn't allow to encode tuple with optional item to JSON", t => {
  let schema = S.tuple1(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => None->S.reverseConvertToJsonOrThrow(schema),
    `Unsupported conversion from boolean | undefined to JSON`,
  )
})

test("Allows to convert to JSON with option as dict field", t => {
  let schema = S.dict(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => dict{"foo": None}->S.reverseConvertToJsonOrThrow(schema),
    `Failed at []: Unsupported conversion from boolean | undefined to JSON`,
  )
})

test("Encodes undefined to JSON as null", t => {
  let schema = S.literal()
  t->Assert.deepEqual(()->S.reverseConvertToJsonOrThrow(schema), JSON.Null)
})

test("Fails to encode Function to JSON", t => {
  let fn = () => ()
  let schema = S.literal(fn)
  t->U.assertThrowsMessage(
    () => fn->S.reverseConvertToJsonOrThrow(schema),
    `Unsupported conversion from Function to JSON`,
  )
})

test("Fails to encode Error literal to JSON", t => {
  let error = %raw(`new Error("foo")`)
  let schema = S.literal(error)

  t->U.assertThrowsMessage(
    () => error->S.reverseConvertToJsonOrThrow(schema),
    `Unsupported conversion from [object Error] to JSON`,
  )
  t->Assert.is(error->S.reverseConvertOrThrow(schema), error)
  t->U.assertThrowsMessage(
    () => %raw(`new Error("foo")`)->S.reverseConvertOrThrow(schema),
    `Expected [object Error], received [object Error]`,
  )
})

test("Fails to encode Symbol to JSON", t => {
  let symbol = %raw(`Symbol()`)
  let schema = S.literal(symbol)
  t->U.assertThrowsMessage(
    () => symbol->S.reverseConvertToJsonOrThrow(schema),
    `Unsupported conversion from Symbol() to JSON`,
  )
})

test("Encodes object literal with bigint to JSON", t => {
  let dict = %raw(`{"foo": 123n}`)
  let schema = S.literal(dict)
  t->Assert.deepEqual(
    dict->S.reverseConvertToJsonOrThrow(schema),
    JSON.Object(dict{"foo": JSON.String("123")}),
  )
})

test("Encodes NaN to JSON", t => {
  let schema = S.literal(%raw(`NaN`))
  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertToJsonOrThrow(schema), JSON.Null)
  t->U.assertThrowsMessage(
    () => ()->S.reverseConvertToJsonOrThrow(schema),
    `Expected NaN, received undefined`,
  )
})

test("Fails to encode Never to JSON", t => {
  t->U.assertThrowsMessage(
    () => Obj.magic(123)->S.reverseConvertToJsonOrThrow(S.never),
    `Expected never, received 123`,
  )
})

test("Encodes object with unknown schema to JSON", t => {
  t->Assert.deepEqual(
    Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.object(s => s.field("foo", S.unknown))),
    JSON.Object(dict{"foo": JSON.Boolean(true)}),
  )
  t->U.assertThrowsMessage(
    () => Obj.magic(123n)->S.reverseConvertToJsonOrThrow(S.object(s => s.field("foo", S.unknown))),
    `Expected JSON, received 123n`,
  )
})

test("Encodes tuple with unknown item to JSON", t => {
  t->Assert.deepEqual(
    Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.tuple1(S.unknown)),
    JSON.Array([JSON.Boolean(true)]),
  )
  t->U.assertThrowsMessage(
    () => Obj.magic(123n)->S.reverseConvertToJsonOrThrow(S.tuple1(S.unknown)),
    `Expected JSON, received 123n`,
  )
})

test("Encodes a union to JSON when at least one item is not JSON-able", t => {
  let schema = S.union([S.string, S.unknown->(U.magic: S.t<unknown> => S.t<string>)])

  t->Assert.deepEqual("foo"->S.reverseConvertToJsonOrThrow(schema), JSON.Encode.string("foo"))
  t->Assert.deepEqual(%raw(`true`)->S.reverseConvertToJsonOrThrow(schema), JSON.Encode.bool(true))
  t->U.assertThrowsMessage(
    () => %raw(`123n`)->S.reverseConvertToJsonOrThrow(schema),
    `Expected string | unknown, received 123n
- Expected string, received 123n
- Expected JSON, received 123n`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertToJson,
    `i=>{try{if(typeof i!=="string"){e[0](i)}}catch(e1){try{e[1](i);}catch(e2){e[2](i,e1,e2)}}return i}`,
  )
})

test("Encodes a union of NaN and unknown to JSON", t => {
  let schema = S.union([S.literal(%raw(`NaN`)), S.unknown->(U.magic: S.t<unknown> => S.t<string>)])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertToJson,
    `i=>{try{if(!Number.isNaN(i)){e[0](i)}i=null}catch(e1){try{e[1](i);}catch(e2){e[2](i,e1,e2)}}return i}`,
  )

  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertToJsonOrThrow(schema), JSON.Null)
  t->Assert.deepEqual(
    %raw(`"bar"`)->S.reverseConvertToJsonOrThrow(schema),
    JSON.Encode.string("bar"),
  )
  t->U.assertThrowsMessage(
    () => %raw(`123n`)->S.reverseConvertToJsonOrThrow(schema),
    `Expected NaN | unknown, received 123n
- Expected NaN, received 123n
- Expected JSON, received 123n`,
  )
})

// https://github.com/DZakh/rescript-schema/issues/74
module SerializesDeepRecursive = {
  module Condition = {
    module Connective = {
      type operator = | @as("or") Or | @as("and") And
      type t<'t> = {
        operator: operator,
        conditions: array<'t>,
      }
    }

    module Comparison = {
      module Operator = {
        type t =
          | @as("equal") Equal
          | @as("greater-than") GreaterThan
      }
      type t = {
        operator: Operator.t,
        values: (string, string),
      }
    }

    type rec t =
      | Connective(Connective.t<t>)
      | Comparison(Comparison.t)

    let schema = S.recursive("Condition", innerSchema =>
      S.union([
        S.object(s => {
          s.tag("type", "or")
          Connective({operator: Or, conditions: s.field("value", S.array(innerSchema))})
        }),
        S.object(s => {
          s.tag("type", "and")
          Connective({operator: And, conditions: s.field("value", S.array(innerSchema))})
        }),
        S.object(s => {
          s.tag("type", "equal")
          Comparison({
            operator: Equal,
            values: s.field("value", S.tuple2(S.string, S.string)),
          })
        }),
        S.object(s => {
          s.tag("type", "greater-than")
          Comparison({
            operator: GreaterThan,
            values: s.field("value", S.tuple2(S.string, S.string)),
          })
        }),
      ])
    )
  }

  // This is just a simple wrapper record that causes the error
  type body = {condition: Condition.t}

  let bodySchema = S.schema(s => {
    condition: s.matches(Condition.schema),
  })

  let conditionJSON = %raw(`
{
  "type": "and",
  "value": [
    {
      "type": "equal",
      "value": [
        "account",
        "1234"        
      ]
    },
    {
      "type": "greater-than",
      "value": [
        "cost-center",
        "1000"        
      ]
    }
  ]
}
`)

  let condition = Condition.Connective({
    operator: And,
    conditions: [
      Condition.Comparison({
        operator: Equal,
        values: ("account", "1234"),
      }),
      Condition.Comparison({
        operator: GreaterThan,
        values: ("cost-center", "1000"),
      }),
    ],
  })

  test("Serializes deeply recursive schema", t => {
    t->U.assertCompiledCode(
      ~schema=bodySchema,
      ~op=#ReverseConvert,
      `i=>{let v0;try{v0=e[0](i["condition"]);}catch(v1){v1.path="[\\"condition\\"]"+v1.path;throw v1}return {"condition":v0,}}`,
    )
    // Note: This is incorrect
    t->U.assertCompiledCode(
      ~schema=bodySchema,
      ~op=#ReverseConvertToJson,
      `i=>{let v0;try{v0=e[0](i["condition"]);}catch(v1){v1.path="[\\"condition\\"]"+v1.path;throw v1}try{e[1](v0);}catch(v2){v2.path="[\\"condition\\"]"+v2.path;throw v2}return {"condition":v0,}}`,
    )

    t->Assert.deepEqual(
      {condition: condition}->S.reverseConvertToJsonOrThrow(bodySchema),
      {
        "condition": conditionJSON,
      }->U.magic,
    )
  })
}
