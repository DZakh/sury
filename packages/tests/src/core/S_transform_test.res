open Ava

test("Parses unknown primitive with transformation to the same type", t => {
  let schema = S.string->S.transform(_ => {parser: value => value->String.trim})

  t->Assert.deepEqual("  Hello world!"->S.parseOrThrow(schema), "Hello world!", ())
})

test("Parses unknown primitive with transformation to another type", t => {
  let schema = S.int->S.transform(_ => {parser: value => value->Int.toFloat})

  t->Assert.deepEqual(123->S.parseOrThrow(schema), 123., ())
})

asyncTest(
  "Asynchronously parses unknown primitive with transformation to another type",
  async t => {
    let schema = S.int->S.transform(_ => {
      asyncParser: value => Promise.resolve()->Promise.thenResolve(() => value->Int.toFloat),
    })

    t->Assert.deepEqual(await 123->S.parseAsyncOrThrow(schema), 123., ())
  },
)

test("Fails to parse primitive with transform when parser isn't provided", t => {
  let schema = S.string->S.transform(_ => {serializer: value => value})

  t->U.assertRaised(
    () => "Hello world!"->S.parseOrThrow(schema),
    {
      code: InvalidOperation({description: "The S.transform parser is missing"}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse when user raises error in a Transformed Primitive parser", t => {
  let schema = S.string->S.transform(s => {parser: _ => s.fail("User error")})

  t->U.assertRaised(
    () => "Hello world!"->S.parseOrThrow(schema),
    {code: OperationFailed("User error"), operation: Parse, path: S.Path.empty},
  )
})

test("Uses the path from S.Error.raise called in the transform parser", t => {
  let schema = S.array(
    S.string->S.transform(_ => {
      parser: _ =>
        S.Error.raise(
          U.error({
            code: OperationFailed("User error"),
            operation: Parse,
            path: S.Path.fromArray(["a", "b"]),
          }),
        ),
    }),
  )

  t->U.assertRaised(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: Parse,
      path: S.Path.fromArray(["0", "a", "b"]),
    },
  )
})

test("Uses the path from S.Error.raise called in the transform serializer", t => {
  let schema = S.array(
    S.string->S.transform(_ => {
      serializer: _ =>
        S.Error.raise(
          U.error({
            code: OperationFailed("User error"),
            operation: ReverseConvert,
            path: S.Path.fromArray(["a", "b"]),
          }),
        ),
    }),
  )

  t->U.assertRaised(
    () => ["Hello world!"]->S.reverseConvertToJsonOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: ReverseConvert,
      path: S.Path.fromArray(["0", "a", "b"]),
    },
  )
})

test("Transform parser passes through non rescript-schema errors", t => {
  let schema = S.array(
    S.string->S.transform(_ => {parser: _ => Exn.raiseError("Application crashed")}),
  )

  t->Assert.throws(
    () => {["Hello world!"]->S.parseOrThrow(schema)},
    ~expectations={
      message: "Application crashed",
    },
    (),
  )
})

test("Transform parser passes through other rescript exceptions", t => {
  let schema = S.array(S.string->S.transform(_ => {parser: _ => U.raiseTestException()}))

  t->U.assertThrowsTestException(() => {["Hello world!"]->S.parseOrThrow(schema)}, ())
})

test("Transform definition passes through non rescript-schema errors", t => {
  let schema = S.array(S.string->S.transform(_ => Exn.raiseError("Application crashed")))

  t->Assert.throws(
    () => {["Hello world!"]->S.parseOrThrow(schema)},
    ~expectations={
      message: "Application crashed",
    },
    (),
  )
})

test("Transform definition passes through other rescript exceptions", t => {
  let schema = S.array(S.string->S.transform(_ => U.raiseTestException()))

  t->U.assertThrowsTestException(() => {["Hello world!"]->S.parseOrThrow(schema)}, ())
})

test("Successfully serializes primitive with transformation to the same type", t => {
  let schema = S.string->S.transform(_ => {serializer: value => value->String.trim})

  t->Assert.deepEqual("  Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`), ())
})

test("Successfully serializes primitive with transformation to another type", t => {
  let schema = S.float->S.transform(_ => {serializer: value => value->Int.toFloat})

  t->Assert.deepEqual(123->S.reverseConvertOrThrow(schema), %raw(`123`), ())
})

test("Transformed Primitive serializing fails when serializer isn't provided", t => {
  let schema = S.string->S.transform(_ => {parser: value => value})

  t->U.assertRaised(
    () => "Hello world!"->S.reverseConvertOrThrow(schema),
    {
      code: InvalidOperation({description: "The S.transform serializer is missing"}),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Fails to serialize when user raises error in a Transformed Primitive serializer", t => {
  let schema = S.string->S.transform(s => {serializer: _ => s.fail("User error")})

  t->U.assertRaised(
    () => "Hello world!"->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("User error"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Transform operations applyed in the right order when parsing", t => {
  let schema =
    S.int
    ->S.transform(s => {parser: _ => s.fail("First transform")})
    ->S.transform(s => {parser: _ => s.fail("Second transform")})

  t->U.assertRaised(
    () => 123->S.parseOrThrow(schema),
    {code: OperationFailed("First transform"), operation: Parse, path: S.Path.empty},
  )
})

test("Transform operations applyed in the right order when serializing", t => {
  let schema =
    S.int
    ->S.transform(s => {serializer: _ => s.fail("First transform")})
    ->S.transform(s => {serializer: _ => s.fail("Second transform")})

  t->U.assertRaised(
    () => 123->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Second transform"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test(
  "Successfully parses a Transformed Primitive and serializes it back to the initial state",
  t => {
    let any = %raw(`123`)

    let schema = S.int->S.transform(_ => {
      parser: int => int->Int.toFloat,
      serializer: value => value->Int.fromFloat,
    })

    t->Assert.deepEqual(any->S.parseOrThrow(schema)->S.reverseConvertOrThrow(schema), any, ())
  },
)

test("Fails to parse schema with transform having both parser and asyncParser", t => {
  let schema = S.string->S.transform(_ => {parser: _ => (), asyncParser: _ => Promise.resolve()})

  t->U.assertRaised(
    () => "foo"->S.parseOrThrow(schema),
    {
      code: InvalidOperation({
        description: "The S.transform doesn\'t allow parser and asyncParser at the same time. Remove parser in favor of asyncParser",
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse async using parseOrThrow", t => {
  let schema = S.string->S.transform(_ => {asyncParser: value => Promise.resolve(value)})

  t->U.assertRaised(
    () => %raw(`"Hello world!"`)->S.parseOrThrow(schema),
    {code: UnexpectedAsync, operation: Parse, path: S.Path.empty},
  )
})

test("Successfully parses with empty transform", t => {
  let schema = S.string->S.transform(_ => {})

  t->Assert.deepEqual(%raw(`"Hello world!"`)->S.parseOrThrow(schema), "Hello world!", ())
})

test("Successfully serializes with empty transform", t => {
  let schema = S.string->S.transform(_ => {})

  t->Assert.deepEqual("Hello world!"->S.reverseConvertOrThrow(schema), %raw(`"Hello world!"`), ())
})

asyncTest("Successfully parses async using parseAsyncOrThrow", t => {
  let schema = S.string->S.transform(_ => {asyncParser: value => Promise.resolve(value)})

  %raw(`"Hello world!"`)
  ->S.parseAsyncOrThrow(schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, "Hello world!", ())
  })
})

asyncTest("Fails to parse async with user error", t => {
  let schema = S.string->S.transform(s => {asyncParser: _ => s.fail("User error")})

  t->U.assertRaisedAsync(
    () => %raw(`"Hello world!"`)->S.parseAsyncOrThrow(schema),
    {code: OperationFailed("User error"), operation: ParseAsync, path: S.Path.empty},
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
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}return e[0](i).then(e[1]).then(e[2])}`,
  )

  %raw(`"    Hello world!"`)
  ->S.parseAsyncOrThrow(schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, "Hello world!", ())
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
    `i=>{if(typeof i!=="number"||i>2147483647||i<-2147483648||i%1!==0){e[1](i)}return e[0](i)}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema = S.int->S.transform(_ => {
    asyncParser: int => int->Int.toFloat->Promise.resolve,
    serializer: value => value->Int.fromFloat,
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="number"||i>2147483647||i<-2147483648||i%1!==0){e[1](i)}return e[0](i)}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return e[0](i)}`)
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })
  t->U.assertEqualSchemas(schema->S.reverse, S.unknown->S.toUnknown)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.transform(_ => {
    parser: int => int->Int.toFloat,
    serializer: value => value->Int.fromFloat,
  })
  t->U.assertReverseParsesBack(schema, 12.)
})
