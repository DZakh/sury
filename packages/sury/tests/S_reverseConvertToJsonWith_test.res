open Ava
open RescriptCore

test("Successfully reverse converts jsonable schemas", t => {
  t->Assert.deepEqual(true->S.reverseConvertToJsonOrThrow(S.bool), true->JSON.Encode.bool, ())
  t->Assert.deepEqual(
    true->S.reverseConvertToJsonOrThrow(S.literal(true)),
    true->JSON.Encode.bool,
    (),
  )
  t->Assert.deepEqual("abc"->S.reverseConvertToJsonOrThrow(S.string), "abc"->JSON.Encode.string, ())
  t->Assert.deepEqual(
    "abc"->S.reverseConvertToJsonOrThrow(S.literal("abc")),
    "abc"->JSON.Encode.string,
    (),
  )
  t->Assert.deepEqual(123->S.reverseConvertToJsonOrThrow(S.int), 123.->JSON.Encode.float, ())
  t->Assert.deepEqual(
    123->S.reverseConvertToJsonOrThrow(S.literal(123)),
    123.->JSON.Encode.float,
    (),
  )
  t->Assert.deepEqual(123.->S.reverseConvertToJsonOrThrow(S.float), 123.->JSON.Encode.float, ())
  t->Assert.deepEqual(
    123.->S.reverseConvertToJsonOrThrow(S.literal(123.)),
    123.->JSON.Encode.float,
    (),
  )
  t->Assert.deepEqual(
    (true, "foo", 123)->S.reverseConvertToJsonOrThrow(S.literal((true, "foo", 123))),
    JSON.Encode.array([JSON.Encode.bool(true), JSON.Encode.string("foo"), JSON.Encode.float(123.)]),
    (),
  )
  t->Assert.deepEqual(
    {"foo": true}->S.reverseConvertToJsonOrThrow(S.literal({"foo": true})),
    JSON.Encode.object(Dict.fromArray([("foo", JSON.Encode.bool(true))])),
    (),
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
    (),
  )
  t->Assert.deepEqual(None->S.reverseConvertToJsonOrThrow(S.null(S.bool)), JSON.Encode.null, ())
  t->Assert.deepEqual(
    JSON.Encode.null->S.reverseConvertToJsonOrThrow(S.literal(JSON.Encode.null)),
    JSON.Encode.null,
    (),
  )
  t->Assert.deepEqual([]->S.reverseConvertToJsonOrThrow(S.array(S.bool)), JSON.Encode.array([]), ())
  t->Assert.deepEqual(
    Dict.make()->S.reverseConvertToJsonOrThrow(S.dict(S.bool)),
    JSON.Encode.object(Dict.make()),
    (),
  )
  t->Assert.deepEqual(
    true->S.reverseConvertToJsonOrThrow(S.object(s => s.field("foo", S.bool))),
    JSON.Encode.object(Dict.fromArray([("foo", JSON.Encode.bool(true))])),
    (),
  )
  t->Assert.deepEqual(
    true->S.reverseConvertToJsonOrThrow(S.tuple1(S.bool)),
    JSON.Encode.array([JSON.Encode.bool(true)]),
    (),
  )
  t->Assert.deepEqual(
    "foo"->S.reverseConvertToJsonOrThrow(S.union([S.literal("foo"), S.literal("bar")])),
    JSON.Encode.string("foo"),
    (),
  )
})

test("Fails to reverse convert Option schema", t => {
  let schema = S.option(S.bool)
  t->U.assertThrows(
    () => None->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
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
    %raw(`{"foo":undefined}`),
    ~message="Shouldn't have undefined value here. Needs to be fixed in future versions",
    (),
  )
})

test("Allows to convert to JSON with optional S.json as an object field", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.option(S.json(~validate=false))),
    }
  )
  t->Assert.deepEqual(
    {"foo": None}->S.reverseConvertToJsonOrThrow(schema),
    %raw(`{"foo":undefined}`),
    ~message="Shouldn't have undefined value here. Needs to be fixed in future versions",
    (),
  )
})

test("Doesn't allow to convert to JSON array with optional items", t => {
  let schema = S.array(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => [None]->S.reverseConvertToJsonOrThrow(schema),
    "Failed converting to JSON: (boolean | undefined)[] is not valid JSON",
  )
})

test("Doesn't allow to convert to JSON tuple with optional items", t => {
  let schema = S.tuple1(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => None->S.reverseConvertToJsonOrThrow(schema),
    `Failed converting to JSON at ["0"]: [boolean | undefined] is not valid JSON`,
  )
})

test("Allows to convert to JSON with option as dict field", t => {
  let schema = S.dict(S.option(S.bool))

  t->Assert.deepEqual(
    Dict.fromArray([("foo", None)])->S.reverseConvertToJsonOrThrow(schema),
    %raw(`{foo:undefined}`),
    ~message="Shouldn't have undefined value here. Needs to be fixed in future versions",
    (),
  )
})

test("Fails to reverse convert Undefined literal", t => {
  let schema = S.literal()
  t->U.assertThrows(
    () => ()->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Function literal", t => {
  let fn = () => ()
  let schema = S.literal(fn)
  t->U.assertThrows(
    () => fn->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

// FIXME: S.literal for instances
Failing.test("Fails to reverse convert Object literal", t => {
  let error = %raw(`new Error("foo")`)
  let schema = S.literal(error)
  t->U.assertThrows(
    () => error->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Symbol literal", t => {
  let symbol = %raw(`Symbol()`)
  let schema = S.literal(symbol)
  t->U.assertThrows(
    () => symbol->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert BigInt literal", t => {
  let bigint = %raw(`1234n`)
  let schema = S.literal(bigint)
  t->U.assertThrows(
    () => bigint->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Dict literal with invalid field", t => {
  let dict = %raw(`{"foo": 123n}`)
  let schema = S.literal(dict)
  t->U.assertThrowsMessage(
    () => dict->S.reverseConvertToJsonOrThrow(schema),
    `Failed converting to JSON at ["foo"]: { foo: 123n; } is not valid JSON`,
  )
})

test("Fails to reverse convert NaN literal", t => {
  let schema = S.literal(%raw(`NaN`))
  t->U.assertThrows(
    () => ()->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Unknown schema", t => {
  t->U.assertThrows(
    () => Obj.magic(123)->S.reverseConvertToJsonOrThrow(S.unknown),
    {code: InvalidJsonSchema(S.unknown), operation: ReverseConvertToJson, path: S.Path.empty},
  )
})

test("Fails to reverse convert Never schema", t => {
  t->U.assertThrows(
    () => Obj.magic(123)->S.reverseConvertToJsonOrThrow(S.never),
    {
      code: InvalidType({expected: S.never->S.toUnknown, received: Obj.magic(123)}),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert object with invalid nested schema", t => {
  t->U.assertThrowsMessage(
    () => Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.object(s => s.field("foo", S.unknown))),
    `Failed converting to JSON at ["foo"]: { foo: unknown; } is not valid JSON`,
  )
})

test("Fails to reverse convert tuple with invalid nested schema", t => {
  t->U.assertThrowsMessage(
    () => Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.tuple1(S.unknown)),
    `Failed converting to JSON at ["0"]: [unknown] is not valid JSON`,
  )
})

test("Doesn't serialize union to JSON when at least one item is not JSON-able", t => {
  let schema = S.union([S.string, S.unknown->(U.magic: S.t<unknown> => S.t<string>)])

  t->U.assertThrowsMessage(
    () => "foo"->S.reverseConvertToJsonOrThrow(schema),
    "Failed converting to JSON: string | unknown is not valid JSON",
  )

  // Not related to the test, just check that it doesn't crash while we are at it
  t->Assert.deepEqual("foo"->S.reverseConvertOrThrow(schema), %raw(`"foo"`), ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{if(typeof i!=="string"){e[0](i)}}catch(e0){e[1](i,e0)}return i}`,
  )
})

test("Fails to reverse convert union with invalid json schemas", t => {
  let schema = S.union([S.literal(%raw(`NaN`)), S.unknown->(U.magic: S.t<unknown> => S.t<string>)])

  t->U.assertThrowsMessage(
    () => %raw(`NaN`)->S.reverseConvertToJsonOrThrow(schema),
    "Failed converting to JSON: NaN | unknown is not valid JSON",
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

    let schema = S.recursive(innerSchema =>
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
      `i=>{let v0=i["condition"],v14;let r0=v0=>{if(typeof v0==="object"&&v0){if(v0["TAG"]==="Connective"&&typeof v0["_0"]==="object"&&v0["_0"]&&v0["_0"]["operator"]==="or"){let v1=v0["_0"]["conditions"],v5=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v4;try{v4=r0(v1[v2])}catch(v3){if(v3&&v3.s===s){v3.path="[\\"_0\\"][\\"conditions\\"]"+\'["\'+v2+\'"]\'+v3.path}throw v3}v5[v2]=v4}v0={"type":e[0],"value":v5,}}else if(v0["TAG"]==="Connective"&&typeof v0["_0"]==="object"&&v0["_0"]&&v0["_0"]["operator"]==="and"){let v6=v0["_0"]["conditions"],v10=new Array(v6.length);for(let v7=0;v7<v6.length;++v7){let v9;try{v9=r0(v6[v7])}catch(v8){if(v8&&v8.s===s){v8.path="[\\"_0\\"][\\"conditions\\"]"+\'["\'+v7+\'"]\'+v8.path}throw v8}v10[v7]=v9}v0={"type":e[1],"value":v10,}}else if(v0["TAG"]==="Comparison"&&typeof v0["_0"]==="object"&&v0["_0"]&&v0["_0"]["operator"]==="equal"){let v11=v0["_0"]["values"];v0={"type":e[2],"value":v11,}}else if(v0["TAG"]==="Comparison"&&typeof v0["_0"]==="object"&&v0["_0"]&&v0["_0"]["operator"]==="greater-than"){let v12=v0["_0"]["values"];v0={"type":e[3],"value":v12,}}}return v0};try{v14=r0(v0)}catch(v13){if(v13&&v13.s===s){v13.path="[\\"condition\\"]"+v13.path}throw v13}return {"condition":v14,}}`,
    )

    t->Assert.deepEqual(
      {condition: condition}->S.reverseConvertToJsonOrThrow(bodySchema),
      {
        "condition": conditionJSON,
      }->U.magic,
      (),
    )
  })
}
