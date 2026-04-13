open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), "123")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);e[0].test(i)||e[1](i);return i}`,
  )
})

test("Successfully parses valid data with global flag", t => {
  let schema = S.string->S.pattern(/[0-9]/g)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), "123")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);(e[0].lastIndex=0,e[0].test(i))||e[1](i);return i}`,
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

test("Reflects pattern on schema", t => {
  let schema = S.string->S.pattern(/[0-9]/)

  switch schema {
  | String({pattern}) => t->Assert.deepEqual(pattern, /[0-9]/)
  | _ => t->Assert.fail("Expected String with pattern")
  }
})

test("Reflects errorMessages on schema", t => {
  let schema = S.string->S.pattern(~message="Custom", /[0-9]/)

  switch schema {
  | String({errorMessages}) =>
    t->Assert.deepEqual(errorMessages, dict{"pattern": "Custom"})
  | _ => t->Assert.fail("Expected String")
  }
})

test("Chaining patterns overwrites pattern but keeps last", t => {
  let schema = S.string->S.pattern(~message="Should have digit", /[0-9]+/)->S.pattern(~message="Should have text", /\w+/)

  switch schema {
  | String({pattern, errorMessages}) => {
      t->Assert.deepEqual(pattern, /\w+/)
      t->Assert.deepEqual(
        errorMessages,
        dict{"pattern": "Should have text"},
      )
    }
  | _ => t->Assert.fail("Expected String with pattern")
  }
})
