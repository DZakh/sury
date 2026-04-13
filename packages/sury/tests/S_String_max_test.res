open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.max(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(~to=schema), "1")
  t->Assert.deepEqual(""->S.parseOrThrow(~to=schema), "")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.max(1)

  t->U.assertThrowsMessage(
    () => "1234"->S.parseOrThrow(~to=schema),
    `String must be 1 or fewer characters long`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.max(1)

  t->Assert.deepEqual("1"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1"`))
  t->Assert.deepEqual(""->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`""`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.max(1)

  t->U.assertThrowsMessage(
    () => "1234"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `String must be 1 or fewer characters long`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.max(~message="Custom", 1)

  t->U.assertThrowsMessage(() => "1234"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.max(1)

  switch schema {
  | String({maxLength, errorMessage}) => {
      t->Assert.deepEqual(maxLength, 1)
      t->Assert.deepEqual(
        errorMessage,
        { maxLength:: "String must be 1 or fewer characters long"},
      )
    }
  | _ => t->Assert.fail("Expected String schema with maxLength")
  }
})
