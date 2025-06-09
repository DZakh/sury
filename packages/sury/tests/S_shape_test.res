open Ava

test("Parses with wrapping the value in variant", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), Ok("Hello world!"), ())
})

asyncTest("Parses with wrapping async schema in variant", async t => {
  let schema = S.string->S.transform(_ => {asyncParser: async i => i})->S.shape(s => Ok(s))

  t->Assert.deepEqual(await "Hello world!"->S.parseAsyncOrThrow(schema), Ok("Hello world!"), ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}return Promise.all([e[0](i),]).then(a=>({"TAG":e[1],"_0":a[0],}))}`,
  )
})

test("Fails to parse wrapped schema", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->U.assertThrows(
    () => 123->S.parseOrThrow(schema),
    {
      code: InvalidType({received: 123->Obj.magic, expected: schema->S.castToUnknown}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Serializes with unwrapping the value from variant", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->Assert.deepEqual(
    Ok("Hello world!")->S.reverseConvertOrThrow(schema),
    %raw(`"Hello world!"`),
    (),
  )
})

test("Fails to serialize when can't unwrap the value from variant", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->Assert.deepEqual(
    Error("Hello world!")->S.reverseConvertOrThrow(schema),
    %raw(`"Hello world!"`),
    ~message=`Convert operation doesn't perform exhaustiveness check`,
    (),
  )

  t->U.assertThrowsMessage(
    () => Error("Hello world!")->S.parseOrThrow(schema->S.reverse),
    `Failed parsing: Expected { TAG: "Ok"; _0: string; }, received { TAG: "Error"; _0: "Hello world!"; }`,
  )
})

test("Successfully parses when the value is not used as the variant payload", t => {
  let schema = S.string->S.shape(_ => #foo)

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), #foo, ())
})

test("Fails to serialize when the value is not used as the variant payload", t => {
  let schema = S.string->S.shape(_ => #foo)

  t->U.assertThrows(
    () => #foo->S.reverseConvertOrThrow(schema),
    {
      code: InvalidOperation({
        description: `Schema isn\'t registered`,
      }),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test(
  "Successfully serializes when the value is not used as the variant payload for literal schemas",
  t => {
    let schema = S.literal((true, 12))->S.shape(_ => #foo)

    t->Assert.deepEqual(#foo->S.reverseConvertOrThrow(schema), %raw(`[true, 12]`), ())
  },
)

test("Successfully parses when tuple is destructured", t => {
  let schema = S.literal((true, 12))->S.shape(((_, twelve)) => twelve)

  t->Assert.deepEqual(%raw(`[true, 12]`)->S.parseOrThrow(schema), %raw(`12`), ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)||i.length!==2||i["0"]!==true||i["1"]!==12){e[0](i)}return i["1"]}`,
  )
})

test(
  "Successfully parses when S.schema object is destructured - it doesn't create an object",
  t => {
    let schema = S.schema(s =>
      {
        "foo": s.matches(S.string),
      }
    )->S.shape(obj => obj["foo"])

    t->Assert.deepEqual(
      {
        "foo": "bar",
      }->S.parseOrThrow(schema),
      %raw(`"bar"`),
      (),
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"];if(typeof v0!=="string"){e[1](v0)}return v0}`,
    )
  },
)

test(
  "Successfully parses when nested S.schema object is destructured - it doesn't create an object",
  t => {
    let schema = S.schema(s =>
      {
        "foo": {
          "bar": s.matches(S.string),
        },
      }
    )->S.shape(obj => obj["foo"]["bar"])

    t->Assert.deepEqual(
      {
        "foo": {"bar": "jazz"},
      }->S.parseOrThrow(schema),
      %raw(`"jazz"`),
      (),
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||typeof i["foo"]!=="object"||!i["foo"]){e[0](i)}let v0=i["foo"],v1=v0["bar"];if(typeof v1!=="string"){e[1](v1)}return v1}`,
    )
  },
)

test(
  "Successfully parses when transformed object schema is destructured - it does create an object and extracts a field from it afterwards",
  t => {
    t->Assert.throws(
      () => {
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
            },
        )
        ->S.transform(
          _ => {
            parser: obj =>
              {
                "faz": obj["foo"],
              },
          },
        )
        ->S.shape(obj => obj["faz"])
      },
      ~expectations={
        message: `[Sury] Cannot read property "faz" of unknown`,
      },
      ~message=`Case without S.to before S.shape`,
      (),
    )

    let schema =
      S.schema(s =>
        {
          "foo": s.matches(S.string),
        }
      )
      ->S.transform(_ => {
        parser: obj =>
          {
            "faz": obj["foo"],
          },
      })
      ->S.to(
        S.schema(s =>
          {
            "faz": s.matches(S.string),
          }
        ),
      )
      ->S.shape(obj => obj["faz"])

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"];if(typeof v0!=="string"){e[1](v0)}let v1=e[2]({"foo":v0,});if(typeof v1!=="object"||!v1){e[3](v1)}let v2=v1["faz"];if(typeof v2!=="string"){e[4](v2)}return v2}`,
    )
    t->Assert.deepEqual(
      {
        "foo": "bar",
      }->S.parseOrThrow(schema),
      %raw(`"bar"`),
      (),
    )
  },
)

test("Reverse convert of tagged tuple with destructured literal", t => {
  let schema = S.tuple2(S.literal(true), S.literal(12))->S.shape(((_, twelve)) => twelve)

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.literal(12)->S.shape(i1 => (true, i1))->S.castToUnknown,
  )

  t->Assert.deepEqual(12->S.reverseConvertOrThrow(schema), %raw(`[true, 12]`), ())

  let code = `i=>{if(i!==12){e[0](i)}return [true,i,]}`
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, code)
  t->U.assertCompiledCode(~schema, ~op=#ReverseParse, code)
})

test("Reverse convert of tagged tuple with destructured bool", t => {
  let schema =
    S.tuple3(S.literal(true), S.literal("foo"), S.bool)->S.shape(((_, literal, item)) => (
      item,
      literal,
    ))

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.tuple2(S.bool, S.literal("foo"))
    ->S.shape(((item, literal)) => (true, literal, item))
    ->S.castToUnknown,
  )

  t->Assert.deepEqual(
    (false, "foo")->S.reverseConvertOrThrow(schema),
    %raw(`[true, "foo",false]`),
    (),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["1"];if(v0!=="foo"){e[0](v0)}return [true,v0,i["0"],]}`,
  )
  // FIXME: Prevent duplicated check for literal
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(!Array.isArray(i)||i.length!==2||i["1"]!=="foo"){e[0](i)}let v0=i["1"],v1=i["0"];if(v0!=="foo"){e[1](v0)}if(typeof v1!=="boolean"){e[2](v1)}return [true,v0,v1,]}`,
  )
})

test("Successfully parses when value registered multiple times", t => {
  let schema = S.string->S.shape(s => #Foo(s, s))

  t->Assert.deepEqual(%raw(`"abc"`)->S.parseOrThrow(schema), #Foo("abc", "abc"), ())
})

test("Reverse convert with value registered multiple times", t => {
  let schema = S.string->S.shape(s => #Foo(s, s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    // `i=>{let v0=i["NAME"],v1=i["VAL"]["0"];if(v0!=="Foo"){e[0](v0)}if(v1!==i["VAL"]["1"]){e[1]()}return v1}`,
    `i=>{return i["VAL"]["1"]}`,
  )

  t->Assert.deepEqual(#Foo("abc", "abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`), ())
  // t->U.assertThrows(
  //   () => #Foo("abc", "abcd")->S.reverseConvertOrThrow(schema),
  //   {
  //     code: InvalidOperation({
  //       description: `Another source has conflicting data`,
  //     }),
  //     operation: ReverseConvert,
  //     path: S.Path.fromArray(["VAL", "1"]),
  //   },
  // )
})

test("Can destructure object value passed to S.shape", t => {
  let schema =
    S.object(s => (s.field("foo", S.string), s.field("bar", S.string)))->S.shape(((foo, bar)) =>
      {"foo": foo, "bar": bar}
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"],v1=i["bar"];if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="string"){e[2](v1)}return {"foo":v0,"bar":v1,}}`,
  )
  // FIXME: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=[i["foo"],i["bar"],];return {"foo":v0["0"],"bar":v0["1"],}}`,
  )
})

test("Compiled code snapshot of variant applied to object", t => {
  let schema = S.object(s => s.field("foo", S.string))->S.shape(s => Ok(s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"];if(typeof v0!=="string"){e[1](v0)}return {"TAG":e[2],"_0":v0,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":i["_0"],}}`)

  let schema = S.object(s => s.field("foo", S.string->S.to(S.bool)))->S.shape(s => Ok(s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"];if(typeof v0!=="string"){e[1](v0)}let v1;(v1=v0==="true")||v0==="false"||e[2](v0);return {"TAG":e[3],"_0":v1,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":""+i["_0"],}}`)
})

test("Compiled parse code snapshot", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}return {"TAG":e[1],"_0":i,}}`,
  )
})

test("Compiled parse code snapshot without transform", t => {
  let schema = S.string->S.shape(s => s)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(typeof i!=="string"){e[0](i)}return i}`)
})

test("Compiled serialize code snapshot", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return i["_0"]}`)
})

test("Compiled serialize code snapshot without transform", t => {
  let schema = S.string->S.shape(s => s)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test(
  "Compiled serialize code snapshot when the value is not used as the variant payload for literal schemas",
  t => {
    let schema = S.literal((true, 12))->S.shape(_ => #foo)

    t->Assert.deepEqual(#foo->S.reverseConvertOrThrow(schema), %raw(`[true,12]`), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(i!=="foo"){e[0](i)}return [true,12,]}`,
    )
  },
)

test("Works with variant schema used multiple times as a child schema", t => {
  let appVersionSpecSchema = S.string->S.shape(current => {"current": current, "minimum": "1.0"})

  let appVersionsSchema = S.object(s =>
    {
      "ios": s.field("ios", appVersionSpecSchema),
      "android": s.field("android", appVersionSpecSchema),
    }
  )

  t->U.assertCompiledCode(
    ~schema=appVersionsSchema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["ios"],v1=i["android"];if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="string"){e[3](v1)}return {"ios":{"current":v0,"minimum":e[2],},"android":{"current":v1,"minimum":e[4],},}}`,
  )
  t->U.assertCompiledCode(
    ~schema=appVersionsSchema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["ios"];let v1=i["android"];return {"ios":v0["current"],"android":v1["current"],}}`,
  )

  let rawAppVersions = {
    "ios": "1.1",
    "android": "1.2",
  }
  let appVersions = {
    "ios": {"current": "1.1", "minimum": "1.0"},
    "android": {"current": "1.2", "minimum": "1.0"},
  }

  t->Assert.deepEqual(rawAppVersions->S.parseOrThrow(appVersionsSchema), appVersions, ())

  t->Assert.deepEqual(
    appVersions->S.reverseConvertToJsonOrThrow(appVersionsSchema),
    rawAppVersions->Obj.magic,
    (),
  )
})

test("Reverse variant schema to literal", t => {
  let schema = S.literal("foo")->S.shape(_ => ())
  t->U.assertEqualSchemas(schema->S.reverse, S.unit->S.to(S.literal("foo"))->S.castToUnknown)
})

test("Succesfully uses reversed variant schema to literal for parsing back to initial value", t => {
  let schema = S.literal("foo")->S.shape(_ => ())
  t->U.assertReverseParsesBack(schema, ())
})

test("Reverse variant schema to self", t => {
  let schema = S.bool->S.shape(v => v)
  t->Assert.not(schema->S.reverse, schema->S.castToUnknown, ())
  t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
})

test("Succesfully uses reversed variant schema to self for parsing back to initial value", t => {
  let schema = S.bool->S.shape(v => v)
  t->U.assertReverseParsesBack(schema, true)
})

test("Reverse convert tuple turned to Ok", t => {
  let schema = S.tuple2(S.string, S.bool)->S.shape(t => Ok(t))

  t->Assert.deepEqual(Ok(("foo", true))->S.reverseConvertOrThrow(schema), %raw(`["foo", true]`), ())
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{let v0=i["_0"];return v0}`)
})

test("Reverse with output of nested object/tuple schema", t => {
  let schema = S.bool->S.shape(v => {
    {
      "nested": {
        "field": (v, true),
      },
    }
  })
  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.schema(s => {
      {
        "nested": {
          "field": (s.matches(S.bool), true),
        },
      }
    })
    ->S.shape(v => {
      let (b, _) = v["nested"]["field"]
      b
    })
    ->S.castToUnknown,
  )
})

test(
  "Succesfully parses reversed schema with output of nested object/tuple and parses it back to initial value",
  t => {
    let schema = S.bool->S.shape(v => {
      {
        "nested": {
          "field": (v, true),
        },
      }
    })
    t->U.assertReverseParsesBack(schema, {"nested": {"field": (true, true)}})
  },
)
