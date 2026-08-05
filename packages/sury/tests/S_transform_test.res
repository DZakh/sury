open Vitest

test("Parses unknown primitive with transformation to the same type", t => {
  let schema = S.string->S.transform(() => {parser: value => value->String.trim})

  t->Assert.deepEqual("  Hello world!"->S.parseOrThrow(~to=schema), "Hello world!")
})

test("Parses unknown primitive with transformation to another type", t => {
  let schema = S.int->S.transform(() => {parser: value => value->Int.toFloat})

  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), 123.)
})

asyncTest(
  "Asynchronously parses unknown primitive with transformation to another type",
  async t => {
    let schema = S.int->S.transform(() => {
      asyncParser: value => Promise.resolve()->Promise.thenResolve(() => value->Int.toFloat),
    })

    t->Assert.deepEqual(await 123->S.parseAsyncOrThrow(~to=schema), 123.)
  },
)

test("Fails to parse primitive with transform when parser isn't provided", t => {
  let schema = S.string->S.transform(() => {serializer: value => value})

  t->U.assertThrowsMessage(
    () => "Hello world!"->S.parseOrThrow(~to=schema),
    `The S.transform parser is missing`,
  )
})

test("Fails to parse when user throws error in a Transformed Primitive parser", t => {
  let schema = S.string->S.transform(() => {parser: _ => U.fail("User error")})

  t->U.assertThrowsMessage(() => "Hello world!"->S.parseOrThrow(~to=schema), `User error`)
})

test("Uses the path from S.Error.throw called in the transform parser", t => {
  let schema = S.array(
    S.string->S.transform(() => {
      parser: _ =>
        U.throwError(
          S.Error.make(
            InvalidInput({
              reason: "User error",
              path: S.Path.fromArray(["a", "b"]),
              expected: S.unknown,
              received: S.unknown,
            }),
          ),
        ),
    }),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at [0].a.b: User error`,
  )
})

test("Uses the path from S.Error.throw called in the transform serializer", t => {
  let schema = S.array(
    S.string->S.transform(() => {
      serializer: _ =>
        U.throwError(
          S.Error.make(
            InvalidInput({
              reason: "User error",
              path: S.Path.fromArray(["a", "b"]),
              expected: S.unknown,
              received: S.unknown,
            }),
          ),
        ),
    }),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.decodeOrThrow(~from=schema, ~to=S.json),
    `Failed at [0].a.b: User error`,
  )
})

test("All errors thrown in operation context are caught and wrapped in SuryError", t => {
  let jsError = JsError.make("Application crashed")
  let schema = S.array(S.string->S.transform(() => {parser: _ => JsError.throw(jsError)}))

  t->U.assertThrowsMessage(
    () => {["Hello world!"]->S.parseOrThrow(~to=schema)},
    `Failed at [0]: Application crashed`,
  )
  switch ["Hello world!"]->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Didn't throw")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidConversion({cause}) => t->Assert.is(cause->Obj.magic, jsError)
    | _ => t->Assert.fail("Thrown another exception")
    }
  }
})

test("Operation context catches ReScript exceptions as they are", t => {
  let schema = S.array(S.string->S.transform(() => {parser: _ => U.throwTestException()}))

  t->U.assertThrowsMessage(
    () => {["Hello world!"]->S.parseOrThrow(~to=schema)},
    `Failed at [0]: { RE_EXN_ID: "U.Test"; Error: Error; }`,
  )
})

test("Transform definition passes through non rescript-schema errors", t => {
  let schema = S.array(S.string->S.transform(() => JsError.throwWithMessage("Application crashed")))

  t->Assert.throws(
    () => {["Hello world!"]->S.parseOrThrow(~to=schema)},
    ~expectations={
      message: "Application crashed",
    },
  )
})

test("Rescript exceptions caught in transform", t => {
  let schema = S.array(S.string->S.transform(() => U.throwTestException()))
  t->U.assertThrowsTestException(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    ~message="When exn thrown outside of the operation context, it's not wrapped in SuryError",
  )

  let schema = S.array(S.string->S.transform(() => {parser: _ => U.throwTestException()}))
  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at [0]: { RE_EXN_ID: "U.Test"; Error: Error; }`,
  )
})

test("Successfully serializes primitive with transformation to the same type", t => {
  let schema = S.string->S.transform(() => {serializer: value => value->String.trim})

  t->Assert.deepEqual("  Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"Hello world!"`))
})

test("Successfully serializes primitive with transformation to another type", t => {
  let schema = S.float->S.transform(() => {serializer: value => value->Int.toFloat})

  t->Assert.deepEqual(123->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`123`))
})

test("Transformed Primitive serializing fails when serializer isn't provided", t => {
  let schema = S.string->S.transform(() => {parser: value => value})

  t->U.assertThrowsMessage(
    () => "Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `The S.transform serializer is missing`,
  )
})

test("Fails to serialize when user throws error in a Transformed Primitive serializer", t => {
  let schema = S.string->S.transform(() => {serializer: _ => U.fail("User error")})

  t->U.assertThrowsMessage(() => "Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `User error`)
})

test("Transform operations applyed in the right order when parsing", t => {
  let schema =
    S.int
    ->S.transform(() => {parser: _ => U.fail("First transform")})
    ->S.transform(() => {parser: _ => U.fail("Second transform")})

  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `First transform`)
})

test("Transform operations applyed in the right order when serializing", t => {
  let schema =
    S.int
    ->S.transform(() => {serializer: _ => U.fail("First transform")})
    ->S.transform(() => {serializer: _ => U.fail("Second transform")})

  t->U.assertThrowsMessage(() => 123->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Second transform`)
})

test(
  "Successfully parses a Transformed Primitive and serializes it back to the initial state",
  t => {
    let any = %raw(`123`)

    let schema = S.int->S.transform(() => {
      parser: int => int->Int.toFloat,
      serializer: value => value->Int.fromFloat,
    })

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema)->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  },
)

test("Fails to parse schema with transform having both parser and asyncParser", t => {
  let schema = S.string->S.transform(() => {parser: _ => (), asyncParser: _ => Promise.resolve()})

  t->U.assertThrowsMessage(
    () => "foo"->S.parseOrThrow(~to=schema),
    `The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
  )
})

test("Fails to parse async using parseOrThrow", t => {
  let schema = S.string->S.transform(() => {asyncParser: value => Promise.resolve(value)})

  t->U.assertThrowsMessage(
    () => %raw(`"Hello world!"`)->S.parseOrThrow(~to=schema),
    `Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead`,
  )
})

test("Successfully parses with empty transform", t => {
  let schema = S.string->S.transform(() => {})

  t->Assert.deepEqual(%raw(`"Hello world!"`)->S.parseOrThrow(~to=schema), "Hello world!")
})

test("Successfully serializes with empty transform", t => {
  let schema = S.string->S.transform(() => {})

  t->Assert.deepEqual("Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"Hello world!"`))
})

asyncTest("Successfully parses async using parseAsyncOrThrow", t => {
  let schema = S.string->S.transform(() => {asyncParser: value => Promise.resolve(value)})

  %raw(`"Hello world!"`)
  ->S.parseAsyncOrThrow(~to=schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, "Hello world!")
  })
})

asyncTest("Fails to parse async with user error", t => {
  let schema = S.string->S.transform(() => {asyncParser: _ => U.fail("User error")})

  t->U.asyncAssertThrowsMessage(
    () => %raw(`"Hello world!"`)->S.parseAsyncOrThrow(~to=schema),
    `User error`,
  )
})

asyncTest("Can apply other actions after async transform", t => {
  let schema =
    S.string
    ->S.transform(() => {asyncParser: value => Promise.resolve(value)})
    ->S.trim
    ->S.transform(() => {asyncParser: value => Promise.resolve(value)})

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{typeof i==="string"||e[6](i);let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}return v0.then(v0=>{let v1;try{v1=e[2](v0)}catch(x){e[3](x)}let v2;try{v2=e[4](v1).catch(x=>e[5](x))}catch(x){e[5](x)}return v2})}`,
  )

  %raw(`"    Hello world!"`)
  ->S.parseAsyncOrThrow(~to=schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, "Hello world!")
  })
})

test("Compiled parse code snapshot", t => {
  let schema = S.int->S.transform(() => {
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
  let schema = S.int->S.transform(() => {
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
  let schema = S.int->S.transform(() => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="number"&&v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[2](v0);return v0}`,
  )
})

test("Compiled serialize code snapshot with two transforms", t => {
  let schema =
    S.string
    ->S.transform(() => {
      parser: string => string->Int.fromString->Option.getOrThrow,
      serializer: int => int->Int.toString,
    })
    ->S.to(
      S.int->S.transform(() => {
        parser: int => int->Int.toFloat,
        serializer: float => float->Float.toInt,
      }),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="number"&&v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[5](v0);let v1;try{v1=e[2](v0)}catch(x){e[3](x)}typeof v1==="string"||e[4](v1);return v1}`,
  )
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.transform(() => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })
  t->U.assertEqualSchemas(schema->S.reverse, S.unknown->S.to(S.int)->S.castToUnknown)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.transform(() => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })
  t->U.assertReverseParsesBack(schema, 12.)
})
