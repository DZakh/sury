open Ava

module Positive = {
  module TestData = {
    type t = {
      discriminantSchema: S.t<unknown>,
      discriminantData: unknown,
      testNamePostfix: string,
    }

    let make = (
      ~discriminantSchema: S.t<'value>,
      ~discriminantData: 'any,
      ~description as maybeDescription=?,
      (),
    ) => {
      discriminantSchema: discriminantSchema->Obj.magic,
      discriminantData: discriminantData->Obj.magic,
      testNamePostfix: switch maybeDescription {
      | Some(description) => ` ${description}`
      | None => ""
      },
    }
  }

  [
    TestData.make(
      ~discriminantSchema=S.literal("asdf"),
      ~discriminantData=%raw(`"asdf"`),
      ~description="String",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal("\"\'\`"),
      ~discriminantData=%raw(`"\"\'\`"`),
      ~description="String which needs to be escaped",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal(123),
      ~discriminantData=%raw("123"),
      ~description="Int",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal(1.3),
      ~discriminantData=%raw("1.3"),
      ~description="Float",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal(true),
      ~discriminantData=%raw("true"),
      ~description="Bool",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal(),
      ~discriminantData=%raw(`undefined`),
      ~description="Unit",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal(Null.null),
      ~discriminantData=%raw(`null`),
      ~description="Null",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal(%raw(`NaN`)),
      ~discriminantData=%raw(`NaN`),
      ~description="NaN",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.literal((false, "bar")),
      ~discriminantData=%raw(`[false, "bar"]`),
      ~description="Tuple",
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.object(s => {
        ignore(s.field("nestedDiscriminant", S.literal("abc")))
        {
          "field": s.field("nestedField", S.literal(false)),
        }
      }),
      ~discriminantData=%raw(`{
        "nestedDiscriminant": "abc",
        "nestedField": false
      }`),
      (),
    ),
    TestData.make(
      ~description="and values needed to be escaped",
      ~discriminantSchema=S.object(s => {
        ignore(s.field("\"\'\`", S.literal("\"\'\`")))
        {
          "field": s.field("nestedField", S.literal(false)),
        }
      }),
      ~discriminantData=%raw(`{
        "\"\'\`": "\"\'\`",
        "nestedField": false
      }`),
      (),
    ),
    TestData.make(
      ~discriminantSchema=S.tuple2(S.literal(false), S.literal("bar")),
      ~discriminantData=%raw(`[false, "bar"]`),
      (),
    ),
  ]->Array.forEach(testData => {
    test(
      `Successfully parses object with discriminant "${testData.discriminantSchema->S.name}"${testData.testNamePostfix}`,
      t => {
        let schema = S.object(
          s => {
            ignore(s.field("discriminant", testData.discriminantSchema))
            {
              "field": s.field("field", S.string),
            }
          },
        )

        t->Assert.deepEqual(
          {
            "discriminant": testData.discriminantData,
            "field": "bar",
          }->S.parseOrThrow(schema),
          {"field": "bar"},
          (),
        )
      },
    )

    test(
      `Successfully serializes object with discriminant "${testData.discriminantSchema->S.name}"${testData.testNamePostfix}`,
      t => {
        let schema = S.object(
          s => {
            ignore(s.field("discriminant", testData.discriminantSchema))
            {
              "field": s.field("field", S.string),
            }
          },
        )

        t->Assert.deepEqual(
          {"field": "bar"}->S.reverseConvertOrThrow(schema),
          {
            "discriminant": testData.discriminantData,
            "field": "bar",
          }->Obj.magic,
          (),
        )
      },
    )
  })
}

module Negative = {
  module TestData = {
    type t = {
      discriminantSchema: S.t<unknown>,
      discriminantData: unknown,
      testNamePostfix: string,
      path: S.Path.t,
    }

    let make = (
      ~discriminantSchema: S.t<'value>,
      ~discriminantData: 'any,
      ~description as maybeDescription=?,
      ~path=S.Path.empty,
    ) => {
      discriminantSchema: discriminantSchema->Obj.magic,
      discriminantData: discriminantData->Obj.magic,
      testNamePostfix: switch maybeDescription {
      | Some(description) => ` ${description}`
      | None => ""
      },
      path,
    }
  }

  [
    TestData.make(~discriminantSchema=S.string, ~discriminantData="foo"),
    TestData.make(~discriminantSchema=S.int, ~discriminantData=123),
    TestData.make(~discriminantSchema=S.float, ~discriminantData=123.),
    TestData.make(~discriminantSchema=S.bool, ~discriminantData=true),
    TestData.make(~discriminantSchema=S.option(S.literal(true)), ~discriminantData=None),
    TestData.make(~discriminantSchema=S.null(S.literal(true)), ~discriminantData=%raw(`null`)),
    TestData.make(~discriminantSchema=S.unknown, ~discriminantData="anything"),
    TestData.make(~discriminantSchema=S.array(S.literal(true)), ~discriminantData=[true, true]),
    TestData.make(
      ~discriminantSchema=S.dict(S.literal(true)),
      ~discriminantData=Dict.fromArray([("foo", true), ("bar", true)]),
    ),
    TestData.make(
      ~discriminantSchema=S.tuple2(S.literal(true), S.bool),
      ~discriminantData=(true, false),
      ~path=S.Path.fromLocation("1"),
    ),
    TestData.make(~discriminantSchema=S.union([S.bool, S.literal(false)]), ~discriminantData=true),
    TestData.make(
      ~discriminantSchema=S.union([S.literal(false), S.bool]),
      ~discriminantData=%raw("false"),
    ),
  ]->Array.forEach(testData => {
    test(
      `Successfully parses object with discriminant that we don't know how to serialize "${testData.discriminantSchema->S.name}"${testData.testNamePostfix}`,
      t => {
        let schema = S.object(
          s => {
            ignore(s.field("discriminant", testData.discriminantSchema))
            {
              "field": s.field("field", S.string),
            }
          },
        )

        t->Assert.deepEqual(
          {
            "discriminant": testData.discriminantData,
            "field": "bar",
          }->S.parseOrThrow(schema),
          {"field": "bar"},
          (),
        )
      },
    )

    test(
      `Fails to serialize object with discriminant that we don't know how to serialize "${testData.discriminantSchema->S.name}"${testData.testNamePostfix}`,
      t => {
        let schema = S.object(
          s => {
            ignore(s.field("discriminant", testData.discriminantSchema))
            {
              "field": s.field("field", S.string),
            }
          },
        )

        t->U.assertRaised(
          () => {"field": "bar"}->S.reverseConvertOrThrow(schema),
          {
            code: InvalidOperation({
              description: `Schema for ["discriminant"]${testData.path->S.Path.toString} isn\'t registered`,
            }),
            operation: ReverseConvert,
            path: S.Path.empty,
          },
        )
      },
    )
  })
}

module NestedNegative = {
  test(
    `Successfully parses object with discriminant object that we don't know how to serialize`,
    t => {
      let schema = S.object(s => {
        ignore(s.field("discriminant", S.object(s => s.field("field", S.bool))))
        {
          "field": s.field("field", S.string),
        }
      })

      t->Assert.deepEqual(
        {
          "discriminant": {"field": true},
          "field": "bar",
        }->S.parseOrThrow(schema),
        {"field": "bar"},
        (),
      )
    },
  )

  test(
    `Fails to serialize object with object discriminant that we don't know how to serialize`,
    t => {
      let schema = S.object(s => {
        ignore(s.field("discriminant", S.object(s => s.field("nestedField", S.bool))))
        {
          "field": s.field("field", S.string),
        }
      })

      t->U.assertRaised(
        () => {"field": "bar"}->S.reverseConvertOrThrow(schema),
        {
          code: InvalidOperation({
            description: `Schema for ["discriminant"] isn\'t registered`,
          }),
          operation: ReverseConvert,
          path: S.Path.empty,
        },
      )
    },
  )
}

test(`Fails to parse object with invalid data passed to discriminant field`, t => {
  let schema = S.object(s => {
    ignore(s.field("discriminant", S.string))
    {
      "field": s.field("field", S.string),
    }
  })

  t->U.assertRaised(
    () =>
      {
        "discriminant": false,
        "field": "bar",
      }->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.toUnknown, received: Obj.magic(false)}),
      operation: Parse,
      path: S.Path.fromArray(["discriminant"]),
    },
  )
})

test(`Parses discriminant fields before registered fields`, t => {
  let schema = S.object(s => {
    ignore(s.field("discriminant", S.string))
    {
      "field": s.field("field", S.string),
    }
  })

  t->U.assertRaised(
    () =>
      {
        "discriminant": false,
        "field": false,
      }->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.toUnknown, received: Obj.magic(false)}),
      operation: Parse,
      path: S.Path.fromArray(["discriminant"]),
    },
  )
})

test(`Fails to serialize object with discriminant "Never"`, t => {
  let schema = S.object(s => {
    ignore(s.field("discriminant", S.never))
    {
      "field": s.field("field", S.string),
    }
  })

  t->U.assertRaised(
    () => {"field": "bar"}->S.reverseConvertOrThrow(schema),
    {
      code: InvalidOperation({
        description: `Schema for ["discriminant"] isn\'t registered`,
      }),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test(`Serializes constant fields before registered non-literal fields`, t => {
  let schema = S.object(s => {
    {
      "literalField": s.field("field", S.literal(true)),
      "constant": true,
    }
  })

  t->U.assertRaised(
    () => {"constant": false, "literalField": false}->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({expected: S.literal(true)->S.toUnknown, received: Obj.magic(false)}),
      operation: ReverseConvert,
      path: S.Path.fromArray(["constant"]),
    },
  )

  let schema = S.object(s => {
    {
      "boolField": s.field("field", S.bool),
      "constant": true,
    }
  })

  t->U.assertRaised(
    () => {"constant": false, "boolField": false}->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({expected: S.literal(true)->S.toUnknown, received: Obj.magic(false)}),
      operation: ReverseConvert,
      path: S.Path.fromArray(["constant"]),
    },
  )
})
