open Ava

S.enableJson()

test("Parses with wrapping the value in variant", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), Ok("Hello world!"))
})

asyncTest("Parses with wrapping async schema in variant", async t => {
  let schema = S.string->S.transform(_ => {asyncParser: async i => i})->S.shape(s => Ok(s))

  t->Assert.deepEqual(await "Hello world!"->S.parseAsyncOrThrow(schema), Ok("Hello world!"))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}return v0.then(v0=>{return {"TAG":"Ok","_0":v0,}})}`,
  )
})

test("Fails to parse wrapped schema", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(schema), `Expected string, received 123`)
})

test("Serializes with unwrapping the value from variant", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->Assert.deepEqual(Ok("Hello world!")->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`))
})

test("Fails to serialize when can't unwrap the value from variant", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->Assert.deepEqual(
    Error("Hello world!")->S.reverseConvertOrThrow(schema),
    %raw(`"Hello world!"`),
    ~message=`Convert operation doesn't perform exhaustiveness check`,
  )

  t->U.assertThrowsMessage(
    () => Error("Hello world!")->S.parseOrThrow(schema->S.reverse),
    `Failed at ["TAG"]: Expected "Ok", received "Error"`,
  )
})

test("Successfully parses when the value is not used as the variant payload", t => {
  let schema = S.string->S.shape(_ => #foo)

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), #foo)
})

test("Fails to serialize when the value is not used as the variant payload", t => {
  let schema = S.string->S.shape(_ => #foo)

  t->U.assertThrowsMessage(() => #foo->S.reverseConvertOrThrow(schema), `Missing input for string`)
})

test(
  "Successfully serializes when the value is not used as the variant payload for literal schemas",
  t => {
    let schema = S.literal((true, 12))->S.shape(_ => #foo)

    t->Assert.deepEqual(#foo->S.reverseConvertOrThrow(schema), %raw(`[true, 12]`))
  },
)

test("Successfully parses when tuple is destructured", t => {
  let schema = S.literal((true, 12))->S.shape(((_, twelve)) => twelve)

  t->Assert.deepEqual(%raw(`[true, 12]`)->S.parseOrThrow(schema), %raw(`12`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{Array.isArray(i)&&i.length===2||e[2](i);let v0=i["0"],v1=i["1"];v0===true||e[0](v0);v1===12||e[1](v1);return v1}`,
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
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[1](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);return v0}`,
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
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[2](i);let v0=i["foo"];typeof v0==="object"&&v0||e[1](v0);let v1=v0["bar"];typeof v1==="string"||e[0](v1);return v1}`,
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
      `i=>{typeof i==="object"&&i||e[5](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);let v1;try{v1=e[1]({"foo":v0,})}catch(x){e[2](x)}typeof v1==="object"&&v1||e[4](v1);let v2=v1["faz"];typeof v2==="string"||e[3](v2);return v2}`,
    )
    t->Assert.deepEqual(
      {
        "foo": "bar",
      }->S.parseOrThrow(schema),
      %raw(`"bar"`),
    )
  },
)

test("Reverse convert of tagged tuple with destructured literal", t => {
  let schema = S.tuple2(S.literal(true), S.literal(12))->S.shape(((_, twelve)) => twelve)

  t->Assert.deepEqual(12->S.reverseConvertOrThrow(schema), %raw(`[true, 12]`))

  let code = `i=>{i===12||e[0](i);return [true,i,]}`
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, code)
  t->U.assertCompiledCode(~schema, ~op=#ReverseParse, code)
})

test("Reverse convert of tagged tuple with destructured bool", t => {
  let schema =
    S.tuple3(S.literal(true), S.literal("foo"), S.bool)->S.shape(((_, literal, item)) => (
      item,
      literal,
    ))

  t->Assert.deepEqual((false, "foo")->S.reverseConvertOrThrow(schema), %raw(`[true, "foo",false]`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return [true,"foo",i["0"],]}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{Array.isArray(i)&&i.length===2||e[2](i);let v0=i["0"],v1=i["1"];typeof v0==="boolean"||e[0](v0);v1==="foo"||e[1](v1);return [true,v1,v0,]}`,
  )
})

test("Successfully parses when value registered multiple times", t => {
  let schema = S.string->S.shape(s => #Foo(s, s))

  t->Assert.deepEqual(%raw(`"abc"`)->S.parseOrThrow(schema), #Foo("abc", "abc"))
})

test("Reverse convert with value registered multiple times", t => {
  let schema = S.string->S.shape(s => #Foo(s, s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    // `i=>{let v0=i["NAME"],v1=i["VAL"]["0"];if(v0!=="Foo"){e[0](v0)}if(v1!==i["VAL"]["1"]){e[1]()}return v1}`,
    `i=>{let v0=i["VAL"];return v0["1"]}`,
  )

  t->Assert.deepEqual(#Foo("abc", "abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`))
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
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["foo"],v1=i["bar"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"foo":v0,"bar":v1,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"foo":i["foo"],"bar":i["bar"],}}`,
  )
})

test("Compiled code snapshot of variant applied to object", t => {
  let schema = S.object(s => s.field("foo", S.string))->S.shape(s => Ok(s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[1](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);return {"TAG":"Ok","_0":v0,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":i["_0"],}}`)

  let schema = S.object(s => s.field("foo", S.string->S.to(S.bool)))->S.shape(s => Ok(s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v1=i["foo"];typeof v1==="string"||e[1](v1);let v0;(v0=v1==="true")||v1==="false"||e[0](v1);return {"TAG":"Ok","_0":v0,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":""+i["_0"],}}`)
})

test("Compiled parse code snapshot", t => {
  let schema = S.string->S.shape(s => Ok(s))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[0](i);return {"TAG":"Ok","_0":i,}}`,
  )
})

test("Compiled parse code snapshot without transform", t => {
  let schema = S.string->S.shape(s => s)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{typeof i==="string"||e[0](i);return i}`)
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

    t->Assert.deepEqual(#foo->S.reverseConvertOrThrow(schema), %raw(`[true,12]`))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{i==="foo"||e[0](i);return [true,12,]}`,
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
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["ios"],v1=i["android"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"ios":{"current":i["ios"],"minimum":"1.0",},"android":{"current":i["android"],"minimum":"1.0",},}}`,
  )
  t->U.assertCompiledCode(
    ~schema=appVersionsSchema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["ios"],v1=i["android"];return {"ios":v0["current"],"android":v1["current"],}}`,
  )

  let rawAppVersions = {
    "ios": "1.1",
    "android": "1.2",
  }
  let appVersions = {
    "ios": {"current": "1.1", "minimum": "1.0"},
    "android": {"current": "1.2", "minimum": "1.0"},
  }

  t->Assert.deepEqual(rawAppVersions->S.parseOrThrow(appVersionsSchema), appVersions)

  t->Assert.deepEqual(
    appVersions->S.reverseConvertToJsonOrThrow(appVersionsSchema),
    rawAppVersions->Obj.magic,
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
  t->Assert.not(schema->S.reverse, schema->S.castToUnknown)
  t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
})

test("Succesfully uses reversed variant schema to self for parsing back to initial value", t => {
  let schema = S.bool->S.shape(v => v)
  t->U.assertReverseParsesBack(schema, true)
})

test("Reverse convert tuple turned to Ok", t => {
  let schema = S.tuple2(S.string, S.bool)->S.shape(t => Ok(t))

  t->Assert.deepEqual(Ok(("foo", true))->S.reverseConvertOrThrow(schema), %raw(`["foo", true]`))
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{let v0=i["_0"];return v0}`)
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

test("S.json shaped to literal should keep validation", t => {
  let schema = S.json->S.shape(_ => "foo")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{e[0](i);return "foo"}
JSON: i=>{if(Array.isArray(i)){for(let v0=0;v0<i.length;++v0){try{e[0]["unknown->JSON--0"](i[v0]);}catch(v1){v1.path='["'+v0+'"]'+v1.path;throw v1}}}else if(typeof i==="object"&&i&&!Array.isArray(i)){for(let v2 in i){try{e[1]["unknown->JSON--0"](i[v2]);}catch(v3){v3.path='["'+v2+'"]'+v3.path;throw v3}}}else if(!(typeof i==="string"||typeof i==="boolean"||typeof i==="number"&&!Number.isNaN(i)||i===null)){e[2](i)}return i}`,
  )

  t->Assert.deepEqual("foo"->S.parseOrThrow(schema), "foo")
  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    "Expected JSON, received undefined",
  )
  t->Assert.deepEqual("bar"->S.parseOrThrow(schema), "foo")
})
