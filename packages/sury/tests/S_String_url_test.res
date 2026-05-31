open Vitest

test("Successfully parses valid data", t => {
  let schema = S.url

  t->Assert.deepEqual("http://dzakh.dev"->S.parseOrThrow(~to=schema), "http://dzakh.dev")
})

test("Fails to parse invalid data", t => {
  let schema = S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.parseOrThrow(~to=schema), `Expected url, received "cifjhdsfhsd"`)
})

test("Successfully serializes valid value", t => {
  let schema = S.url

  t->Assert.deepEqual(
    "http://dzakh.dev"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"http://dzakh.dev"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Expected url, received "cifjhdsfhsd"`)
})

test("Custom error message via S.meta", t => {
  let schema = S.url->S.meta({errorMessage: {format: "Custom"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Reflects format on schema", t => {
  let schema = S.url

  t->Assert.deepEqual((schema->S.untag).format, Some(Url))
  switch schema {
  | String({format}) => t->Assert.deepEqual(format, Url)
  | _ => t->Assert.fail("Expected String with format Url")
  }
})
