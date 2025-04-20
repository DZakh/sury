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
    "Failed converting to JSON: The \'(boolean | undefined)[]\' schema cannot be converted to JSON",
  )
})

test("Doesn't allow to convert to JSON tuple with optional items", t => {
  let schema = S.tuple1(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => None->S.reverseConvertToJsonOrThrow(schema),
    `Failed converting to JSON at ["0"]: The 'boolean | undefined' schema cannot be converted to JSON`,
  )
})

test("Allows to convert to JSON with option as dict field", t => {
  let schema = S.dict(S.option(S.bool))

  t->Assert.deepEqual(
    Js.Dict.fromArray([("foo", None)])->S.reverseConvertToJsonOrThrow(schema),
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
  t->U.assertThrows(
    () => dict->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(S.literal(123n)->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.fromLocation("foo"),
    },
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
    `Failed converting to JSON at ["foo"]: The \'unknown\' schema cannot be converted to JSON`,
  )
})

test("Fails to reverse convert tuple with invalid nested schema", t => {
  t->U.assertThrowsMessage(
    () => Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.tuple1(S.unknown)),
    `Failed converting to JSON at ["0"]: The \'unknown\' schema cannot be converted to JSON`,
  )
})

test("Doesn't serialize union to JSON when at least one item is not JSON-able", t => {
  let schema = S.union([S.string, S.unknown->(U.magic: S.t<unknown> => S.t<string>)])

  t->U.assertThrowsMessage(
    () => "foo"->S.reverseConvertToJsonOrThrow(schema),
    "Failed converting to JSON: The \'string | unknown\' schema cannot be converted to JSON",
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
    "Failed converting to JSON: The \'NaN | unknown\' schema cannot be converted to JSON",
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

  Failing.test("Serializes deeply recursive schema", t => {
    t->U.assertCompiledCode(
      ~schema=bodySchema,
      ~op=#ReverseConvert,
      `i=>{let v0=i["condition"],v22;let r0=v0=>{if(typeof v0==="object"&&v0){if(v0["TAG"]==="Connective"){let v1=v0["TAG"],v2=v0["_0"]["operator"],v3=v0["_0"]["conditions"],v7=new Array(v3.length);if(v1!=="Connective"){e[0](v1)}if(v2!=="or"){e[1](v2)}for(let v4=0;v4<v3.length;++v4){let v6;try{v6=r0(v3[v4])}catch(v5){if(v5&&v5.s===s){v5.path="[\\"_0\\"][\\"conditions\\"]"+\'["\'+v4+\'"]\'+v5.path}throw v5}v7[v4]=v6}v0={"type":e[2],"value":v7,}}else if(v0["TAG"]==="Connective"){let v8=v0["TAG"],v9=v0["_0"]["operator"],v10=v0["_0"]["conditions"],v14=new Array(v10.length);if(v8!=="Connective"){e[3](v8)}if(v9!=="and"){e[4](v9)}for(let v11=0;v11<v10.length;++v11){let v13;try{v13=r0(v10[v11])}catch(v12){if(v12&&v12.s===s){v12.path="[\\"_0\\"][\\"conditions\\"]"+\'["\'+v11+\'"]\'+v12.path}throw v12}v14[v11]=v13}v0={"type":e[5],"value":v14,}}else if(v0["TAG"]==="Comparison"){let v15=v0["TAG"],v16=v0["_0"]["operator"],v17=v0["_0"]["values"];if(v15!=="Comparison"){e[6](v15)}if(v16!=="equal"){e[7](v16)}v0={"type":e[8],"value":v17,}}else if(v0["TAG"]==="Comparison"){let v18=v0["TAG"],v19=v0["_0"]["operator"],v20=v0["_0"]["values"];if(v18!=="Comparison"){e[9](v18)}if(v19!=="greater-than"){e[10](v19)}v0={"type":e[11],"value":v20,}}}return v0};try{v22=r0(v0)}catch(v21){if(v21&&v21.s===s){v21.path="[\\"condition\\"]"+v21.path}throw v21}return {"condition":v22,}}`,
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
