open Vitest

test("Successfully parses valid data", t => {
  let schema = S.string->S.minLength(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(~to=schema), "1")
  t->Assert.deepEqual("1234"->S.parseOrThrow(~to=schema), "1234")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.minLength(1)

  t->U.assertThrowsMessage(
    () => ""->S.parseOrThrow(~to=schema),
    `Expected string.length >= 1, received ""`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.minLength(1)

  t->Assert.deepEqual("1"->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`"1"`))
  t->Assert.deepEqual("1234"->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`"1234"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.minLength(1)

  t->U.assertThrowsMessage(
    () => ""->S.convertOrThrow(~from=schema, ~to=S.unknown),
    `Expected string.length >= 1, received ""`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.minLength(~message="Custom", 1)

  t->U.assertThrowsMessage(() => ""->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.minLength(1)

  switch schema {
  | String({minLength, ?errorMessage}) => {
      t->Assert.deepEqual(minLength, 1)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected String schema with minLength")
  }
})

test("Chaining refinements does not mutate the original schema", t => {
  let schema1 = S.string->S.minLength(1)
  let schema2 = schema1->S.maxLength(10)

  switch schema1 {
  | String({minLength, ?maxLength, ?errorMessage}) => {
      t->Assert.deepEqual(minLength, 1)
      t->Assert.deepEqual(maxLength, None)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected String schema with minLength only")
  }
  switch schema2 {
  | String({minLength, maxLength, ?errorMessage}) => {
      t->Assert.deepEqual(minLength, 1)
      t->Assert.deepEqual(maxLength, 10)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected String schema with minLength and maxLength")
  }
})
