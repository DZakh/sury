open Ava

let preprocessNumberToString = S.preprocess(_, _ => {
  parser: unknown => {
    if unknown->typeof === #number {
      unknown->Obj.magic->Float.toString
    } else {
      unknown->Obj.magic
    }
  },
  serializer: unknown => {
    if unknown->typeof === #string {
      let string: string = unknown->Obj.magic
      switch string->Float.fromString {
      | Some(float) => float->Obj.magic
      | None => string
      }
    } else {
      unknown->Obj.magic
    }
  },
})

test("Successfully parses", t => {
  let schema = S.string->preprocessNumberToString

  t->Assert.deepEqual(123->S.parseOrThrow(schema), "123", ())
  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), "Hello world!", ())
})

test("Fails to parse when user raises error in parser", t => {
  let schema = S.string->S.preprocess(s => {parser: _ => s.fail("User error")})

  t->U.assertRaised(
    () => "Hello world!"->S.parseOrThrow(schema),
    {code: OperationFailed("User error"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully serializes", t => {
  let schema = S.string->preprocessNumberToString

  t->Assert.deepEqual("Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`), ())
  t->Assert.deepEqual("123"->S.reverseConvertOrThrow(schema), %raw(`123`), ())
})

test("Fails to serialize when user raises error in serializer", t => {
  let schema = S.string->S.preprocess(s => {serializer: _ => s.fail("User error")})

  t->U.assertRaised(
    () => "Hello world!"->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("User error"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Preprocess operations applyed in the right order when parsing", t => {
  let schema =
    S.int
    ->S.preprocess(s => {parser: _ => s.fail("First preprocess")})
    ->S.preprocess(s => {parser: _ => s.fail("Second preprocess")})

  t->U.assertRaised(
    () => 123->S.parseOrThrow(schema),
    {code: OperationFailed("Second preprocess"), operation: Parse, path: S.Path.empty},
  )
})

test("Preprocess operations applyed in the right order when serializing", t => {
  let schema =
    S.int
    ->S.preprocess(s => {serializer: _ => s.fail("First preprocess")})
    ->S.preprocess(s => {serializer: _ => s.fail("Second preprocess")})

  t->U.assertRaised(
    () => 123->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("First preprocess"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse async using parseOrThrow", t => {
  let schema = S.string->S.preprocess(_ => {asyncParser: value => Promise.resolve(value)})

  t->U.assertRaised(
    () => %raw(`"Hello world!"`)->S.parseOrThrow(schema),
    {code: UnexpectedAsync, operation: Parse, path: S.Path.empty},
  )
})

asyncTest("Successfully parses async using parseAsyncOrThrow", async t => {
  let schema = S.string->S.preprocess(_ => {asyncParser: value => Promise.resolve(value)})

  t->Assert.deepEqual(await %raw(`"Hello world!"`)->S.parseAsyncOrThrow(schema), "Hello world!", ())
})

asyncTest("Fails to parse async with user error", t => {
  let schema = S.string->S.preprocess(s => {asyncParser: _ => s.fail("User error")})

  t->U.assertRaisedAsync(
    () => %raw(`"Hello world!"`)->S.parseAsyncOrThrow(schema),
    {code: OperationFailed("User error"), operation: ParseAsync, path: S.Path.empty},
  )
})

test("Successfully parses with empty preprocess", t => {
  let schema = S.string->S.preprocess(_ => {})

  t->Assert.deepEqual(%raw(`"Hello world!"`)->S.parseOrThrow(schema), "Hello world!", ())
})

test("Successfully serializes with empty preprocess", t => {
  let schema = S.string->S.preprocess(_ => {})

  t->Assert.deepEqual("Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`), ())
})

asyncTest("Can apply other actions after async preprocess", async t => {
  let schema =
    S.string
    ->S.preprocess(_ => {asyncParser: value => Promise.resolve(value)})
    ->S.trim
    ->S.preprocess(_ => {asyncParser: value => Promise.resolve(value)})

  // TODO: Can improve builder to use string schema and trim without .then in between
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{return e[0](i).then(v0=>{return e[1](v0).then(v1=>{if(typeof v1!=="string"){e[2](v1)}return v1}).then(e[3])})}`,
  )

  t->Assert.deepEqual(
    await %raw(`"    Hello world!"`)->S.parseAsyncOrThrow(schema),
    "Hello world!",
    (),
  )
})

test("Applies preproces parser for union schemas separately", t => {
  let prepareEnvSchema = S.preprocess(_, s => {
    switch s.schema->S.classify {
    | Literal(Boolean(_))
    | Bool => {
        parser: unknown => {
          switch unknown->Obj.magic {
          | "true"
          | "t"
          | "1" => true
          | "false"
          | "f"
          | "0" => false
          | _ => unknown->Obj.magic
          }->Obj.magic
        },
      }
    | Int
    | Float
    | Literal(Number(_)) => {
        parser: unknown => {
          if unknown->typeof === #string {
            %raw(`+unknown`)
          } else {
            unknown
          }
        },
      }
    | _ => {}
    }
  })

  let schema =
    S.union([
      S.bool->S.shape(bool => #Bool(bool)),
      S.int->S.shape(int => #Int(int)),
    ])->prepareEnvSchema

  t->Assert.deepEqual("f"->S.parseOrThrow(schema), #Bool(false), ())
  t->Assert.deepEqual("1"->S.parseOrThrow(schema), #Bool(true), ())
  t->Assert.deepEqual("2"->S.parseOrThrow(schema), #Int(2), ())
})

test("Applies preproces serializer for union schemas separately", t => {
  let schema = S.union([
    S.bool->S.shape(bool => #Bool(bool)),
    S.int->S.shape(int => #Int(int)),
  ])->S.preprocess(s => {
    switch s.schema->S.classify {
    | Bool => {
        serializer: unknown => {
          if unknown->Obj.magic === true {
            "1"->Obj.magic
          } else if unknown->Obj.magic === false {
            "0"->Obj.magic
          } else {
            unknown->Obj.magic
          }
        },
      }
    | Int => {
        serializer: unknown => {
          if unknown->typeof === #number {
            unknown->Obj.magic->Int.toString->Obj.magic
          } else {
            unknown->Obj.magic
          }
        },
      }
    | _ => {}
    }
  })

  t->Assert.deepEqual(#Bool(false)->S.reverseConvertOrThrow(schema), %raw(`"0"`), ())
  t->Assert.deepEqual(#Bool(true)->S.reverseConvertOrThrow(schema), %raw(`"1"`), ())
  t->Assert.deepEqual(#Int(2)->S.reverseConvertOrThrow(schema), %raw(`"2"`), ())
})

test("Doesn't fail to parse with preprocess when parser isn't provided", t => {
  let schema = S.string->S.preprocess(_ => {serializer: value => value})

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), "Hello world!", ())
})

test("Doesn't fail to serialize with preprocess when serializer isn't provided", t => {
  let schema = S.string->S.preprocess(_ => {parser: value => value})

  t->Assert.deepEqual("Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`), ())
})

test("Compiled parse code snapshot", t => {
  let schema = S.int->S.preprocess(_ => {
    parser: _ => 1->Int.toFloat,
    serializer: _ => 1.->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{let v0=e[0](i);if(typeof v0!=="number"||v0>2147483647||v0<-2147483648||v0%1!==0){e[1](v0)}return v0}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema = S.int->S.preprocess(_ => {
    asyncParser: _ => 1->Int.toFloat->Promise.resolve,
    serializer: _ => 1.->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{return e[0](i).then(v0=>{if(typeof v0!=="number"||v0>2147483647||v0<-2147483648||v0%1!==0){e[1](v0)}return v0})}`,
  )
})

test("Compiled async parse code snapshot for object", t => {
  let schema = S.object(s =>
    {
      "foo": s.field("foo", S.string),
    }
  )->S.preprocess(_ => {
    asyncParser: _ => 1->Int.toFloat->Promise.resolve,
    serializer: _ => 1.->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{return e[0](i).then(v0=>{if(typeof v0!=="object"||!v0){e[1](v0)}let v1=v0["foo"];if(typeof v1!=="string"){e[2](v1)}return {"foo":v1,}})}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.int->S.preprocess(_ => {
    parser: _ => 1->Int.toFloat,
    serializer: _ => 1.->Int.fromFloat,
  })

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return e[0](i)}`)
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.preprocess(_ => {
    parser: _ => 1->Int.toFloat,
    serializer: _ => 1.->Int.fromFloat,
  })
  t->Assert.truthy(
    switch schema->S.classify {
    | Int => true
    | _ => false
    },
    (),
  )
  t->Assert.truthy(
    switch schema->S.reverse->S.classify {
    | Int => true
    | _ => false
    },
    (),
  )
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.preprocess(_ => {
    parser: _ => 1->Int.toFloat,
    serializer: _ => 1.->Int.fromFloat,
  })
  t->U.assertReverseParsesBack(schema, 1)
})
