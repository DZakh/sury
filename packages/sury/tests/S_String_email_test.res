open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual("dzakh.dev@gmail.com"->S.parseOrThrow(~to=schema), "dzakh.dev@gmail.com")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.email

  t->U.assertThrowsMessage(() => "dzakh.dev"->S.parseOrThrow(~to=schema), `Invalid email address`)
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual(
    "dzakh.dev@gmail.com"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"dzakh.dev@gmail.com"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.email

  t->U.assertThrowsMessage(
    () => "dzakh.dev"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Invalid email address`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.email(~message="Custom")

  t->U.assertThrowsMessage(() => "dzakh.dev"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Email, message: "Invalid email address"}],
  )
})
