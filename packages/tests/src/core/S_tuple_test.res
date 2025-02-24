open Ava

module Tuple0 = {
  let value = ()
  let any = %raw(`[]`)
  let invalidAny = %raw(`[true]`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.tuple(_ => ())

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: schema->S.toUnknown,
          received: invalidAny,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalidTypeAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.toUnknown, received: invalidTypeAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })
}

test("Fills wholes with S.unit", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->U.assertEqualSchemas(schema->S.toUnknown, S.tuple3(S.string, S.unit, S.int)->S.toUnknown)
})

test("Successfully parses tuple with holes", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->Assert.deepEqual(%raw(`["value",, 123]`)->S.parseOrThrow(schema), ("value", 123), ())
})

test("Fails to parse tuple with holes", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->U.assertRaised(
    () => %raw(`["value", "smth", 123]`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`["value", "smth", 123]`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes tuple with holes", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->Assert.deepEqual(("value", 123)->S.reverseConvertOrThrow(schema), %raw(`["value",, 123]`), ())
})

test("Reverse convert of tuple schema with single item registered multiple times", t => {
  let schema = S.tuple(s => {
    let item = s.item(0, S.string)
    {
      "item1": item,
      "item2": item,
    }
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    // `i=>{let v0=i["item1"];if(v0!==i["item2"]){e[0]()}return [v0,]}`,
    `i=>{return [i["item2"],]}`,
  )

  t->Assert.deepEqual(
    {"item1": "foo", "item2": "foo"}->S.reverseConvertOrThrow(schema),
    %raw(`["foo"]`),
    (),
  )
  // t->U.assertRaised(
  //   () => {"item1": "foo", "item2": "foz"}->S.reverseConvertOrThrow(schema),
  //   {
  //     code: InvalidOperation({
  //       description: `Another source has conflicting data for the field ["0"]`,
  //     }),
  //     operation: ReverseConvert,
  //     path: S.Path.fromArray(["item2"]),
  //   },
  // )
})

test(`Fails to serialize tuple with discriminant "Never"`, t => {
  let schema = S.tuple(s => {
    ignore(s.item(0, S.never))
    s.item(1, S.string)
  })

  t->U.assertRaised(
    () => "bar"->S.reverseConvertOrThrow(schema),
    {
      code: InvalidOperation({
        description: `Schema for ["0"] isn\'t registered`,
      }),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test(`Fails to serialize tuple with discriminant "Never" inside of an object (test path)`, t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(
        S.tuple(
          s => {
            ignore(s.item(0, S.never))
            s.item(1, S.string)
          },
        ),
      ),
    }
  )

  t->U.assertRaised(
    () => {"foo": "bar"}->S.reverseConvertOrThrow(schema),
    {
      code: InvalidOperation({
        description: `Schema for ["0"] isn\'t registered`,
      }),
      operation: ReverseConvert,
      path: S.Path.fromLocation(`foo`),
    },
  )
})

test("Successfully parses tuple transformed to variant", t => {
  let schema = S.tuple(s => #VARIANT(s.item(0, S.bool)))

  t->Assert.deepEqual(%raw(`[true]`)->S.parseOrThrow(schema), #VARIANT(true), ())
})

test("Successfully serializes tuple transformed to variant", t => {
  let schema = S.tuple(s => #VARIANT(s.item(0, S.bool)))

  t->Assert.deepEqual(#VARIANT(true)->S.reverseConvertOrThrow(schema), %raw(`[true]`), ())
})

test("Fails to serialize tuple transformed to variant", t => {
  let schema = S.tuple(s => Ok(s.item(0, S.bool)))

  t->U.assertRaised(
    () => Error("foo")->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({expected: S.literal("Ok")->S.toUnknown, received: %raw(`"Error"`)}),
      operation: ReverseConvert,
      path: S.Path.fromLocation("TAG"),
    },
  )
})

test("Fails to create tuple schema with single item defined multiple times", t => {
  t->Assert.throws(
    () => {
      S.tuple(
        s =>
          {
            "boo": s.item(0, S.string),
            "zoo": s.item(0, S.int),
          },
      )
    },
    ~expectations={
      message: `[rescript-schema] The item ["0"] is defined multiple times`,
    },
    (),
  )
})

test("Tuple schema parsing checks order", t => {
  let schema = S.tuple(s => {
    s.tag(1, "value")
    {
      "key": s.item(0, S.string),
    }
  })

  // Type check should be the first
  t->U.assertRaised(
    () => %raw(`"foo"`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`"foo"`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  // Length check should be the second
  t->U.assertRaised(
    () => %raw(`["value", "value", "value", "value"]`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: schema->S.toUnknown,
        received: %raw(`["value", "value", "value", "value"]`),
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  // Tag check should be the third
  t->U.assertRaised(
    () => %raw(`["value", "wrong"]`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`["value", "wrong"]`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  // Field check should be the last
  t->U.assertRaised(
    () => %raw(`[1, "value"]`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.toUnknown, received: %raw(`1`)}),
      operation: Parse,
      path: S.Path.fromLocation("0"),
    },
  )
  // Parses valid
  t->Assert.deepEqual(
    %raw(`["value", "value"]`)->S.parseOrThrow(schema),
    {
      "key": "value",
    },
    (),
  )
})

test("Works correctly with not-modified object item", t => {
  let schema = S.tuple1(S.object(s => s.field("foo", S.string)))

  t->Assert.deepEqual(%raw(`[{"foo": "bar"}]`)->S.parseOrThrow(schema), "bar", ())
  t->Assert.deepEqual("bar"->S.reverseConvertToJsonOrThrow(schema), %raw(`[{"foo": "bar"}]`), ())
})

module Compiled = {
  test("Compiled parse code snapshot for simple tuple", t => {
    let schema = S.tuple(s => (s.item(0, S.string), s.item(1, S.bool)))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)||i.length!==2){e[2](i)}let v0=i["0"],v1=i["1"];if(typeof v0!=="string"){e[0](v0)}if(typeof v1!=="boolean"){e[1](v1)}return [v0,v1,]}`,
    )
  })

  test("Compiled parse code snapshot for simple tuple with async", t => {
    let schema = S.tuple(s => (
      s.item(0, S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)})),
      s.item(1, S.bool),
    ))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)||i.length!==2){e[2](i)}let v0=i["1"];if(typeof v0!=="boolean"){e[1](v0)}return Promise.all([e[0](i["0"]),]).then(a=>([a[0],v0,]))}`,
    )
  })

  test("Compiled serialize code snapshot for simple tuple", t => {
    let schema = S.tuple(s => (s.item(0, S.string), s.item(1, S.bool)))

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return [i["0"],i["1"],]}`)
  })

  test("Compiled serialize code snapshot for empty tuple", t => {
    let schema = S.tuple(_ => ())

    // TODO: No need to do unit check ?
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(i!==undefined){e[0](i)}return []}`,
    )
  })

  test(
    "Compiled parse code snapshot for simple tuple with transformation, constants and discriminants",
    t => {
      let schema = S.tuple(s => {
        s.tag(0, 0)
        {
          "foo": s.item(1, S.string),
          "bar": s.item(2, S.bool),
          "zoo": 1,
        }
      })

      t->U.assertCompiledCode(
        ~schema,
        ~op=#Parse,
        `i=>{if(!Array.isArray(i)||i.length!==3||i["0"]!==0){e[3](i)}let v0=i["1"],v1=i["2"];if(typeof v0!=="string"){e[0](v0)}if(typeof v1!=="boolean"){e[1](v1)}return {"foo":v0,"bar":v1,"zoo":e[2],}}`,
      )
    },
  )

  test(
    "Compiled serialize code snapshot for simple tuple with transformation, constants and discriminants",
    t => {
      let schema = S.tuple(s => {
        s.tag(0, 0)
        {
          "foo": s.item(1, S.string),
          "bar": s.item(2, S.bool),
          "zoo": 1,
        }
      })

      t->U.assertCompiledCode(
        ~schema,
        ~op=#ReverseConvert,
        `i=>{let v0=i["zoo"];if(v0!==1){e[0](v0)}return [e[1],i["foo"],i["bar"],]}`,
      )
    },
  )
}

test("Works with tuple schema used multiple times as a child schema", t => {
  let appVersionSpecSchema = S.tuple(s =>
    {
      "current": s.item(0, S.string),
      "minimum": s.item(1, S.string),
    }
  )

  let appVersionsSchema = S.object(s =>
    {
      "ios": s.field("ios", appVersionSpecSchema),
      "android": s.field("android", appVersionSpecSchema),
    }
  )

  let rawAppVersions = {
    "ios": ("1.1", "1.0"),
    "android": ("1.2", "1.1"),
  }
  let appVersions = {
    "ios": {"current": "1.1", "minimum": "1.0"},
    "android": {"current": "1.2", "minimum": "1.1"},
  }

  let value = rawAppVersions->S.parseOrThrow(appVersionsSchema)
  t->Assert.deepEqual(value, appVersions, ())

  let data = appVersions->S.reverseConvertToJsonOrThrow(appVersionsSchema)
  t->Assert.deepEqual(data, rawAppVersions->Obj.magic, ())
})

test("Reverse empty tuple schema to literal", t => {
  let schema = S.tuple(_ => ())
  t->U.assertEqualSchemas(schema->S.reverse, S.unit->S.toUnknown)
})

test("Succesfully uses reversed empty tuple schema for parsing back to initial value", t => {
  let schema = S.tuple(_ => ())
  t->U.assertReverseParsesBack(schema, ())
})

test("Reverse tagged tuple to literal without payload", t => {
  let schema = S.tuple(s => {
    s.tag(0, "test")
    #Test
  })
  t->U.assertEqualSchemas(schema->S.reverse, S.literal(#Test)->S.toUnknown)
})

test(
  "Succesfully uses reversed non-payloaded tagged tuple schema for parsing back to initial value",
  t => {
    let schema = S.tuple(s => {
      s.tag(0, "test")
      #Test
    })
    t->U.assertReverseParsesBack(schema, #Test)
  },
)

test("Reverse tagged tuple to primitive schema", t => {
  let schema = S.tuple(s => {
    s.tag(0, "test")
    s.item(1, S.bool)
  })
  t->U.assertEqualSchemas(schema->S.reverse, S.bool->S.toUnknown)
})

test(
  "Succesfully uses reversed tagged tuple schema with item as output for parsing back to initial value",
  t => {
    let schema = S.tuple(s => {
      s.tag(0, "test")
      s.item(1, S.bool)
    })
    t->U.assertReverseParsesBack(schema, true)
  },
)
