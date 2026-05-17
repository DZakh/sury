open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.length(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(~to=schema), "1")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.length(1)

  t->U.assertThrowsMessage(
    () => ""->S.parseOrThrow(~to=schema),
    `String must be exactly 1 characters long`,
  )
  t->U.assertThrowsMessage(
    () => "1234"->S.parseOrThrow(~to=schema),
    `String must be exactly 1 characters long`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.length(1)

  t->Assert.deepEqual("1"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.length(1)

  t->U.assertThrowsMessage(
    () => ""->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `String must be exactly 1 characters long`,
  )
  t->U.assertThrowsMessage(
    () => "1234"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `String must be exactly 1 characters long`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.length(~message="Custom", 12)

  t->U.assertThrowsMessage(() => "123"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.length(4)

  switch schema {
  | String({minLength, maxLength, errorMessage}) => {
      t->Assert.deepEqual(minLength, 4)
      t->Assert.deepEqual(maxLength, 4)
      t->Assert.deepEqual(
        errorMessage,
        {
          minLength: "String must be exactly 4 characters long",
          maxLength: "String must be exactly 4 characters long",
        },
      )
    }
  | _ => t->Assert.fail("Expected String schema with minLength and maxLength")
  }
})
