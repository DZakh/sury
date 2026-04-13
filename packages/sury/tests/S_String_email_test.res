open Ava

S.enableEmail()

test("Successfully parses valid data", t => {
  let schema = S.email

  t->Assert.deepEqual("dzakh.dev@gmail.com"->S.parseOrThrow(~to=schema), "dzakh.dev@gmail.com")
})

test("Fails to parse invalid data", t => {
  let schema = S.email

  t->U.assertThrowsMessage(() => "dzakh.dev"->S.parseOrThrow(~to=schema), `Invalid email address`)
})

test("Successfully serializes valid value", t => {
  let schema = S.email

  t->Assert.deepEqual(
    "dzakh.dev@gmail.com"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"dzakh.dev@gmail.com"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.email

  t->U.assertThrowsMessage(
    () => "dzakh.dev"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Invalid email address`,
  )
})

test("Reflects format on schema", t => {
  let schema = S.email

  t->Assert.deepEqual((schema->S.untag).format, Some(Email))
  switch schema {
  | String({format}) => t->Assert.deepEqual(format, Email)
  | _ => t->Assert.fail("Expected String with format Email")
  }
})

test("Custom error message via S.meta", t => {
  let schema = S.email->S.meta({errorMessage: dict{"format": "Custom"}})

  t->U.assertThrowsMessage(() => "dzakh.dev"->S.parseOrThrow(~to=schema), `Custom`)
  // Original singleton is not mutated
  t->U.assertThrowsMessage(() => "dzakh.dev"->S.parseOrThrow(~to=S.email), `Invalid email address`)
})
