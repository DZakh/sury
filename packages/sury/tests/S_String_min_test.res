open Vitest

test("Successfully parses valid data", t => {
  let schema = S.string->S.min(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(~to=schema), "1")
  t->Assert.deepEqual("1234"->S.parseOrThrow(~to=schema), "1234")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.min(1)

  t->U.assertThrowsMessage(
    () => ""->S.parseOrThrow(~to=schema),
    `String must be 1 or more characters long`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.min(1)

  t->Assert.deepEqual("1"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1"`))
  t->Assert.deepEqual("1234"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1234"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.min(1)

  t->U.assertThrowsMessage(
    () => ""->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `String must be 1 or more characters long`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.min(~message="Custom", 1)

  t->U.assertThrowsMessage(() => ""->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.min(1)

  switch schema {
  | String({minLength, errorMessage}) => {
      t->Assert.deepEqual(minLength, 1)
      t->Assert.deepEqual(
        errorMessage,
        {minLength: "String must be 1 or more characters long"},
      )
    }
  | _ => t->Assert.fail("Expected String schema with minLength")
  }
})

test("Chaining refinements does not mutate the original schema", t => {
  let schema1 = S.string->S.min(1)
  let schema2 = schema1->S.max(10)

  switch schema1 {
  | String({minLength, ?maxLength, errorMessage}) => {
      t->Assert.deepEqual(minLength, 1)
      t->Assert.deepEqual(maxLength, None)
      t->Assert.deepEqual(
        errorMessage,
        {minLength: "String must be 1 or more characters long"},
      )
    }
  | _ => t->Assert.fail("Expected String schema with minLength only")
  }
  switch schema2 {
  | String({minLength, maxLength, errorMessage}) => {
      t->Assert.deepEqual(minLength, 1)
      t->Assert.deepEqual(maxLength, 10)
      t->Assert.deepEqual(
        errorMessage,
        {
          minLength: "String must be 1 or more characters long",
          maxLength: "String must be 10 or fewer characters long",
        },
      )
    }
  | _ => t->Assert.fail("Expected String schema with minLength and maxLength")
  }
})
