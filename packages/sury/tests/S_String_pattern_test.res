open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), "123")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);e[0].test(i)||e[2](i);return i}`,
  )
})

test("Successfully parses valid data with global flag", t => {
  let schema = S.string->S.pattern(/[0-9]/g)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), "123")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);(e[0].lastIndex=0,e[0].test(i))||e[2](i);return i}`,
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Invalid pattern`)
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual("123"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->U.assertThrowsMessage(() => "abc"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Invalid pattern`)
})

test("Returns custom error message", t => {
  let schema = S.string->S.pattern(~message="Custom", /[0-9]/)

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Pattern({re: /[0-9]/}), message: "Invalid pattern"}],
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
