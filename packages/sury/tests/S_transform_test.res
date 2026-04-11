open Ava

S.enableJson()

test("Parses unknown primitive with transformation to the same type", t => {
  let schema = S.string->S.transform(_ => {parser: value => value->String.trim})

  t->Assert.deepEqual("  Hello world!"->S.parseOrThrow(schema), "Hello world!")
})

test("Parses unknown primitive with transformation to another type", t => {
  let schema = S.int->S.transform(_ => {parser: value => value->Int.toFloat})

  t->Assert.deepEqual(123->S.parseOrThrow(schema), 123.)
})

asyncTest(
  "Asynchronously parses unknown primitive with transformation to another type",
  async t => {
    let schema = S.int->S.transform(_ => {
      asyncParser: value => Promise.resolve()->Promise.thenResolve(() => value->Int.toFloat),
    })

    t->Assert.deepEqual(await 123->S.parseAsyncOrThrow(schema), 123.)
  },
)

test("Fails to parse primitive with transform when parser isn't provided", t => {
  let schema = S.string->S.transform(_ => {serializer: value => value})

  t->U.assertThrowsMessage(
    () => "Hello world!"->S.parseOrThrow(schema),
    `The S.transform parser is missing`,
  )
})

test("Fails to parse when user throws error in a Transformed Primitive parser", t => {
  let schema = S.string->S.transform(s => {parser: _ => s.fail("User error")})

  t->U.assertThrowsMessage(() => "Hello world!"->S.parseOrThrow(schema), `User error`)
})

test("Uses the path from S.Error.throw called in the transform parser", t => {
  let schema = S.array(
    S.string->S.transform(_ => {
      parser: _ =>
        U.throwError(
          S.Error.make(
            Custom({
              reason: "User error",
              path: S.Path.fromArray(["a", "b"]),
            }),
          ),
        ),
    }),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    `Failed at ["0"]["a"]["b"]: User error`,
  )
})

test("Uses the path from S.Error.throw called in the transform serializer", t => {
  let schema = S.array(
    S.string->S.transform(_ => {
      serializer: _ =>
        U.throwError(
          S.Error.make(
            Custom({
              reason: "User error",
              path: S.Path.fromArray(["a", "b"]),
            }),
          ),
        ),
    }),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.reverseConvertToJsonOrThrow(schema),
    `Failed at ["0"]["a"]["b"]: User error`,
  )
})

test("All errors thrown in operation context are caught and wrapped in SuryError", t => {
  let jsError = JsError.make("Application crashed")
  let schema = S.array(S.string->S.transform(_ => {parser: _ => JsError.throw(jsError)}))

  t->U.assertThrowsMessage(
    () => {["Hello world!"]->S.parseOrThrow(schema)},
    `Failed at ["0"]: Application crashed`,
  )
  switch ["Hello world!"]->S.parseOrThrow(schema) {
  | _ => t->Assert.fail("Didn't throw")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidConversion({cause}) => t->Assert.is(cause->Obj.magic, jsError)
    | _ => t->Assert.fail("Thrown another exception")
    }
  }
})

test("Operation context catches ReScript exceptions as they are", t => {
  let schema = S.array(S.string->S.transform(_ => {parser: _ => U.throwTestException()}))

  t->U.assertThrowsMessage(
    () => {["Hello world!"]->S.parseOrThrow(schema)},
    `Failed at ["0"]: { RE_EXN_ID: "U.Test"; Error: [object Error]; }`,
  )
})

test("Transform definition passes through non rescript-schema errors", t => {
  let schema = S.array(S.string->S.transform(_ => JsError.throwWithMessage("Application crashed")))

  t->Assert.throws(
    () => {["Hello world!"]->S.parseOrThrow(schema)},
    ~expectations={
      message: "Application crashed",
    },
  )
})

test("Rescript exceptions caught in transform", t => {
  let schema = S.array(S.string->S.transform(_ => U.throwTestException()))
  t->U.assertThrowsTestException(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    ~message="When exn thrown outside of the operation context, it's not wrapped in SuryError",
  )

  let schema = S.array(S.string->S.transform(_ => {parser: _ => U.throwTestException()}))
  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    `Failed at ["0"]: { RE_EXN_ID: "U.Test"; Error: [object Error]; }`,
  )
})

test("Successfully serializes primitive with transformation to the same type", t => {
  let schema = S.string->S.transform(_ => {serializer: value => value->String.trim})

  t->Assert.deepEqual("  Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`))
})

test("Successfully serializes primitive with transformation to another type", t => {
  let schema = S.float->S.transform(_ => {serializer: value => value->Int.toFloat})

  t->Assert.deepEqual(123->S.reverseConvertOrThrow(schema), %raw(`123`))
})

test("Transformed Primitive serializing fails when serializer isn't provided", t => {
  let schema = S.string->S.transform(_ => {parser: value => value})

  t->U.assertThrowsMessage(
    () => "Hello world!"->S.reverseConvertOrThrow(schema),
    `The S.transform serializer is missing`,
  )
})

test("Fails to serialize when user throws error in a Transformed Primitive serializer", t => {
  let schema = S.string->S.transform(s => {serializer: _ => s.fail("User error")})

  t->U.assertThrowsMessage(() => "Hello world!"->S.reverseConvertOrThrow(schema), `User error`)
})

test("Transform operations applyed in the right order when parsing", t => {
  let schema =
    S.int
    ->S.transform(s => {parser: _ => s.fail("First transform")})
    ->S.transform(s => {parser: _ => s.fail("Second transform")})

  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(schema), `First transform`)
})

test("Transform operations applyed in the right order when serializing", t => {
  let schema =
    S.int
    ->S.transform(s => {serializer: _ => s.fail("First transform")})
    ->S.transform(s => {serializer: _ => s.fail("Second transform")})

  t->U.assertThrowsMessage(() => 123->S.reverseConvertOrThrow(schema), `Second transform`)
})

test(
  "Successfully parses a Transformed Primitive and serializes it back to the initial state",
  t => {
    let any = %raw(`123`)

    let schema = S.int->S.transform(_ => {
      parser: int => int->Int.toFloat,
      serializer: value => value->Int.fromFloat,
    })

    t->Assert.deepEqual(any->S.parseOrThrow(schema)->S.reverseConvertOrThrow(schema), any)
  },
)

test("Fails to parse schema with transform having both parser and asyncParser", t => {
  let schema = S.string->S.transform(_ => {parser: _ => (), asyncParser: _ => Promise.resolve()})

  t->U.assertThrowsMessage(
    () => "foo"->S.parseOrThrow(schema),
    `The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
  )
})

test("Fails to parse async using parseOrThrow", t => {
  let schema = S.string->S.transform(_ => {asyncParser: value => Promise.resolve(value)})

  t->U.assertThrowsMessage(
    () => %raw(`"Hello world!"`)->S.parseOrThrow(schema),
    `Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead`,
  )
})

test("Successfully parses with empty transform", t => {
  let schema = S.string->S.transform(_ => {})

  t->Assert.deepEqual(%raw(`"Hello world!"`)->S.parseOrThrow(schema), "Hello world!")
})

test("Successfully serializes with empty transform", t => {
  let schema = S.string->S.transform(_ => {})

  t->Assert.deepEqual("Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`))
})

asyncTest("Successfully parses async using parseAsyncOrThrow", t => {
  let schema = S.string->S.transform(_ => {asyncParser: value => Promise.resolve(value)})

  %raw(`"Hello world!"`)
  ->S.parseAsyncOrThrow(schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, "Hello world!")
  })
})

asyncTest("Fails to parse async with user error", t => {
  let schema = S.string->S.transform(s => {asyncParser: _ => s.fail("User error")})

  t->U.asyncAssertThrowsMessage(
    () => %raw(`"Hello world!"`)->S.parseAsyncOrThrow(schema),
    `User error`,
  )
})

asyncTest("Can apply other actions after async transform", t => {
  let schema =
    S.string
    ->S.transform(_ => {asyncParser: value => Promise.resolve(value)})
    ->S.trim
    ->S.transform(_ => {asyncParser: value => Promise.resolve(value)})

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{typeof i==="string"||e[6](i);let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}return v0.then(v0=>{let v1;try{v1=e[2](v0)}catch(x){e[3](x)}let v2;try{v2=e[4](v1).catch(x=>e[5](x))}catch(x){e[5](x)}return v2})}`,
  )

  %raw(`"    Hello world!"`)
  ->S.parseAsyncOrThrow(schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, "Hello world!")
  })
})

test("Compiled parse code snapshot", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="number"&&i<=2147483647&&i>=-2147483648&&i%1===0||e[2](i);let v0;try{v0=e[0](i)}catch(x){e[1](x)}return v0}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema = S.int->S.transform(_ => {
    asyncParser: int => int->Int.toFloat->Promise.resolve,
    serializer: value => value->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{typeof i==="number"&&i<=2147483647&&i>=-2147483648&&i%1===0||e[2](i);let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}return v0}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="number"&&v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[2](v0);return v0}`,
  )
})

test("Compiled serialize code snapshot with two transforms", t => {
  let schema =
    S.string
    ->S.transform(_ => {
      parser: string => string->Int.fromString->Option.getOrThrow,
      serializer: int => int->Int.toString,
    })
    ->S.to(
      S.int->S.transform(_ => {
        parser: int => int->Int.toFloat,
        serializer: float => float->Float.toInt,
      }),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="number"&&v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[5](v0);let v1;try{v1=e[2](v0)}catch(x){e[3](x)}typeof v1==="string"||e[4](v1);return v1}`,
  )
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })
  t->U.assertEqualSchemas(schema->S.reverse, S.unknown->S.to(S.int)->S.castToUnknown)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })
  t->U.assertReverseParsesBack(schema, 12.)
})
