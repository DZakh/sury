open Vitest

test("Successfully parses valid data", t => {
  let schema = S.string->S.length(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(~to=schema), "1")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.length(1)

  t->U.assertThrowsMessage(
    () => ""->S.parseOrThrow(~to=schema),
    `Expected string.length == 1, received ""`,
  )
  t->U.assertThrowsMessage(
    () => "1234"->S.parseOrThrow(~to=schema),
    `Expected string.length == 1, received "1234"`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.length(1)

  t->Assert.deepEqual("1"->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`"1"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.length(1)

  t->U.assertThrowsMessage(
    () => ""->S.convertOrThrow(~from=schema, ~to=S.unknown),
    `Expected string.length == 1, received ""`,
  )
  t->U.assertThrowsMessage(
    () => "1234"->S.convertOrThrow(~from=schema, ~to=S.unknown),
    `Expected string.length == 1, received "1234"`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.length(~message="Custom", 12)

  t->U.assertThrowsMessage(() => "123"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.length(4)

  switch schema {
  | String({minLength, maxLength, ?errorMessage}) => {
      t->Assert.deepEqual(minLength, 4)
      t->Assert.deepEqual(maxLength, 4)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected String schema with minLength and maxLength")
  }
})
