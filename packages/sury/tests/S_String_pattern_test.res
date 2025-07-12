open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), "123")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}if(!e[1].test(i)){e[2]()}return i}`,
  )
})

test("Successfully parses valid data with global flag", t => {
  let schema = S.string->S.pattern(%re(`/[0-9]/g`))

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), "123")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}e[1].lastIndex=0;if(!e[2].test(i)){e[3]()}return i}`,
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->U.assertThrows(
    () => "abc"->S.parseOrThrow(schema),
    {code: OperationFailed("Invalid"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual("123"->S.reverseConvertOrThrow(schema), %raw(`"123"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->U.assertThrows(
    () => "abc"->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Invalid"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.pattern(~message="Custom", /[0-9]/)

  t->U.assertThrows(
    () => "abc"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Pattern({re: /[0-9]/}), message: "Invalid"}],
  )
})

test("Returns multiple refinement", t => {
  let schema1 = S.string
  let schema2 = schema1->S.pattern(~message="Should have digit", /[0-9]+/)
  let schema3 = schema2->S.pattern(~message="Should have text", /\w+/)

  t->Assert.deepEqual(schema1->S.String.refinements, [])
  t->Assert.deepEqual(
    schema2->S.String.refinements,
    [{kind: Pattern({re: /[0-9]+/}), message: "Should have digit"}],
  )
  t->Assert.deepEqual(
    schema3->S.String.refinements,
    [
      {kind: Pattern({re: /[0-9]+/}), message: "Should have digit"},
      {kind: Pattern({re: /\w+/}), message: "Should have text"},
    ],
  )
})
