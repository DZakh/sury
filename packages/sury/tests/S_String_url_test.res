open Ava

S.enableUrl()

test("Successfully parses valid data", t => {
  let schema = S.url

  t->Assert.deepEqual("http://dzakh.dev"->S.parseOrThrow(~to=schema), "http://dzakh.dev")
})

test("Fails to parse invalid data", t => {
  let schema = S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.parseOrThrow(~to=schema), `Invalid url`)
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

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Invalid url`)
})

test("Returns custom error message", t => {
  // Custom message is no longer supported for standalone schemas
  // URL validation always uses the default message
  let schema = S.url

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Invalid url`)
})

test("Reflects format on schema", t => {
  let schema = S.url

  t->Assert.deepEqual((schema->S.untag).format, Some(Url))
  switch schema {
  | String({format}) => t->Assert.deepEqual(format, Url)
  | _ => t->Assert.fail("Expected String with format Url")
  }
})
