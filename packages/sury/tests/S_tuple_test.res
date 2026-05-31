open Vitest

module Tuple0 = {
  let value = ()
  let any = %raw(`[]`)
  let invalidAny = %raw(`[true]`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.tuple(_ => ())

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse extra value in strict mode (default for tuple)", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected [], received [true]`,
    )
  })

  test("Ignores extra items in strip mode", t => {
    let schema = factory()

    t->Assert.deepEqual(invalidAny->S.parseOrThrow(~to=schema->S.strip), ())
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected [], received "Hello world!"`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })
}

test("Fills holes with S.unit", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, ("value", 123))
})

test("Successfully parses tuple with holes", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->Assert.deepEqual(%raw(`["value",, 123]`)->S.parseOrThrow(~to=schema), ("value", 123))
})

test("Fails to parse tuple with holes", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->U.assertThrowsMessage(
    () => %raw(`["value", "smth", 123]`)->S.parseOrThrow(~to=schema),
    `Failed at ["1"]: Expected undefined, received "smth"`,
  )
})

test("Successfully serializes tuple with holes", t => {
  let schema = S.tuple(s => (s.item(0, S.string), s.item(2, S.int)))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return [i["0"],void 0,i["1"],]}`)
  t->Assert.deepEqual(("value", 123)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`["value",, 123]`))
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
    ~op=#Encode,
    // `i=>{let v0=i["item1"];if(v0!==i["item2"]){e[0]()}return [v0,]}`,
    `i=>{return [i["item2"],]}`,
  )

  t->Assert.deepEqual(
    {"item1": "foo", "item2": "foo"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`["foo"]`),
  )
  // t->U.assertThrows(
  //   () => {"item1": "foo", "item2": "foz"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
  //   {
  //     code: InvalidOperation({
  //       description: `Another source has conflicting data for the field ["0"]`,
  //     }),
  //     operation: Encode,
  //     path: S.Path.fromArray(["item2"]),
  //   },
  // )
})

test(`Fails to serialize tuple with discriminant "Never"`, t => {
  let schema = S.tuple(s => {
    ignore(s.item(0, S.never))
    s.item(1, S.string)
  })

  t->U.assertThrowsMessage(
    () => "bar"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Missing input for never at ["0"]`,
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

  t->U.assertThrowsMessage(
    () => {"foo": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Failed at ["foo"]: Missing input for never at ["0"]`,
  )
})

test("Successfully parses tuple transformed to variant", t => {
  let schema = S.tuple(s => #VARIANT(s.item(0, S.bool)))

  t->Assert.deepEqual(%raw(`[true]`)->S.parseOrThrow(~to=schema), #VARIANT(true))
})

test("Successfully serializes tuple transformed to variant", t => {
  let schema = S.tuple(s => #VARIANT(s.item(0, S.bool)))

  t->Assert.deepEqual(#VARIANT(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`[true]`))
})

test("Fails to serialize tuple transformed to variant", t => {
  let schema = S.tuple(s => Ok(s.item(0, S.bool)))

  let invalid = Error("foo")
  t->Assert.deepEqual(
    invalid->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`["foo"]`),
    ~message=`Convert operation doesn't perform exhaustiveness check`,
  )

  t->U.assertThrowsMessage(
    () => Error("foo")->S.parseOrThrow(~to=schema->S.reverse),
    `Failed at ["TAG"]: Expected "Ok", received "Error"`,
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
      message: `[Sury] The item [0] is defined multiple times`,
    },
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
  t->U.assertThrowsMessage(
    () => %raw(`"foo"`)->S.parseOrThrow(~to=schema),
    `Expected [string, "value"], received "foo"`,
  )
  // Length check should be the second
  t->U.assertThrowsMessage(
    () => %raw(`["value"]`)->S.parseOrThrow(~to=schema),
    `Expected [string, "value"], received ["value"]`,
  )
  // Length check should be the second (extra items in strict mode)
  t->U.assertThrowsMessage(
    () => %raw(`["value", "value", "value"]`)->S.parseOrThrow(~to=schema->S.strict),
    `Expected [string, "value"], received ["value", "value", "value"]`,
  )
  // Tag check should be the third
  t->U.assertThrowsMessage(
    () => %raw(`["value", "wrong"]`)->S.parseOrThrow(~to=schema),
    `Failed at ["1"]: Expected "value", received "wrong"`,
  )
  // Field check should be the last
  t->U.assertThrowsMessage(
    () => %raw(`[1, "value"]`)->S.parseOrThrow(~to=schema),
    `Failed at ["0"]: Expected string, received 1`,
  )
  // Parses valid
  t->Assert.deepEqual(
    %raw(`["value", "value"]`)->S.parseOrThrow(~to=schema),
    {
      "key": "value",
    },
  )
})

test("Works correctly with not-modified object item", t => {
  let schema = S.tuple1(S.object(s => s.field("foo", S.string)))

  t->Assert.deepEqual(%raw(`[{"foo": "bar"}]`)->S.parseOrThrow(~to=schema), "bar")
  t->Assert.deepEqual("bar"->S.decodeOrThrow(~from=schema, ~to=S.json), %raw(`[{"foo": "bar"}]`))
})

module Compiled = {
  test("Compiled parse code snapshot for simple tuple", t => {
    let schema = S.tuple(s => (s.item(0, S.string), s.item(1, S.bool)))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{Array.isArray(i)&&i.length===2||e[2](i);let v0=i["0"],v1=i["1"];typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);return [v0,v1,]}`,
    )
  })

  test("Compiled parse code snapshot for simple tuple with async", t => {
    let schema = S.tuple(s => (
      s.item(0, S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)})),
      s.item(1, S.bool),
    ))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{Array.isArray(i)&&i.length===2||e[3](i);let v1=i["1"];let v0;try{v0=e[0](i["0"]).catch(x=>e[1](x))}catch(x){e[1](x)}typeof v1==="boolean"||e[2](v1);return Promise.all([v0,]).then(([v0,])=>{return [v0,v1,]})}`,
    )
  })

  test("Compiled serialize code snapshot for simple tuple", t => {
    let schema = S.tuple(s => (s.item(0, S.string), s.item(1, S.bool)))

    t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return [i["0"],i["1"],]}`)
  })

  test("Compiled serialize code snapshot for empty tuple", t => {
    let schema = S.tuple(_ => ())

    // TODO: No need to do unit check ?
    t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===void 0||e[0](i);return []}`)
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
        `i=>{Array.isArray(i)&&i.length===3||e[3](i);let v0=i["0"],v1=i["1"],v2=i["2"];v0===0||e[0](v0);typeof v1==="string"||e[1](v1);typeof v2==="boolean"||e[2](v2);return {"foo":v1,"bar":v2,"zoo":1,}}`,
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

      t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return [0,i["foo"],i["bar"],]}`)
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

  let value = rawAppVersions->S.parseOrThrow(~to=appVersionsSchema)
  t->Assert.deepEqual(value, appVersions)

  let data = appVersions->S.decodeOrThrow(~from=appVersionsSchema, ~to=S.json)
  t->Assert.deepEqual(data, rawAppVersions->Obj.magic)
})

test("Reverse empty tuple schema to literal", t => {
  let schema = S.tuple(_ => ())
  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, ())
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
  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, #Test)
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
  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, true)
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
