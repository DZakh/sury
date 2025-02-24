open Ava

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
  t->U.assertRaised(
    () => None->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Undefined literal", t => {
  let schema = S.literal()
  t->U.assertRaised(
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
  t->U.assertRaised(
    () => fn->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Object literal", t => {
  let error = %raw(`new Error("foo")`)
  let schema = S.literal(error)
  t->U.assertRaised(
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
  t->U.assertRaised(
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
  t->U.assertRaised(
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
  t->U.assertRaised(
    () => dict->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert NaN literal", t => {
  let schema = S.literal(%raw(`NaN`))
  t->U.assertRaised(
    () => ()->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(schema->S.toUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert Unknown schema", t => {
  t->U.assertRaised(
    () => Obj.magic(123)->S.reverseConvertToJsonOrThrow(S.unknown),
    {code: InvalidJsonSchema(S.unknown), operation: ReverseConvertToJson, path: S.Path.empty},
  )
})

test("Fails to reverse convert Never schema", t => {
  t->U.assertRaised(
    () => Obj.magic(123)->S.reverseConvertToJsonOrThrow(S.never),
    {
      code: InvalidType({expected: S.never->S.toUnknown, received: Obj.magic(123)}),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert object with invalid nested schema", t => {
  t->U.assertRaised(
    () => Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.object(s => s.field("foo", S.unknown))),
    {
      code: InvalidJsonSchema(S.unknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Fails to reverse convert tuple with invalid nested schema", t => {
  t->U.assertRaised(
    () => Obj.magic(true)->S.reverseConvertToJsonOrThrow(S.tuple1(S.unknown)),
    {
      code: InvalidJsonSchema(S.unknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
})

test("Serializes union even one of the items is an invalid JSON schema", t => {
  let schema = S.union([S.string, S.unknown->(U.magic: S.t<unknown> => S.t<string>)])
  t->Assert.deepEqual("foo"->S.reverseConvertToJsonOrThrow(schema), JSON.Encode.string("foo"), ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertToJson,
    `i=>{if(typeof i!=="string"){throw e[0]}return i}`,
  )

  // Not related to the test, just check that it doesn't crash while we are at it
  t->Assert.deepEqual("foo"->S.reverseConvertOrThrow(schema), %raw(`"foo"`), ())
  // TODO: Can be improved to return null
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(typeof i!=="string"){}return i}`)

  let schema = S.union([S.unknown->(U.magic: S.t<unknown> => S.t<string>), S.string])
  t->Assert.deepEqual("foo"->S.reverseConvertToJsonOrThrow(schema), JSON.Encode.string("foo"), ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertToJson,
    `i=>{if(typeof i!=="string"){throw e[0]}return i}`,
  )
})

test("Fails to reverse convert union with invalid json schemas", t => {
  let schema = S.union([S.literal(%raw(`NaN`)), S.unknown->(U.magic: S.t<unknown> => S.t<string>)])
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertToJson,
    `i=>{if(!Number.isNaN(i)){throw e[1]}else{throw e[0]}return i}`,
  )
  t->U.assertRaised(
    () => "foo"->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(S.unknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )
  t->U.assertRaised(
    () => %raw(`NaN`)->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidJsonSchema(S.literal(%raw(`NaN`))),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
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
    t->Assert.deepEqual(
      {condition: condition}->S.reverseConvertToJsonOrThrow(bodySchema),
      {
        "condition": conditionJSON,
      }->U.magic,
      (),
    )
  })
}
