open Vitest

@get external href: 'a => string = "href"

// `S.uri` is the string form — RFC 3986 syntax, carrying `format: "uri"`.

test("Successfully parses valid data", t => {
  let schema = S.uri

  t->Assert.deepEqual("http://dzakh.dev"->S.parseOrThrow(~to=schema), "http://dzakh.dev")
})

test("Fails to parse invalid data", t => {
  let schema = S.uri

  t->U.assertThrowsMessage(
    () => "cifjhdsfhsd"->S.parseOrThrow(~to=schema),
    `Expected uri, received "cifjhdsfhsd"`,
  )
})

test("Rejects what new URL accepts but RFC 3986 does not", t => {
  let schema = S.uri

  t->U.assertThrowsMessage(
    () => "https://example.org/foo bar.txt"->S.parseOrThrow(~to=schema),
    `Expected uri, received "https://example.org/foo bar.txt"`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.uri

  t->Assert.deepEqual(
    "http://dzakh.dev"->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"http://dzakh.dev"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.uri

  t->U.assertThrowsMessage(
    () => "cifjhdsfhsd"->S.convertOrThrow(~from=schema, ~to=S.unknown),
    `Expected uri, received "cifjhdsfhsd"`,
  )
})

test("Custom error message via S.meta", t => {
  let schema = S.uri->S.meta({errorMessage: {format: "Custom"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Reflects format on schema", t => {
  let schema = S.uri

  t->Assert.deepEqual((schema->S.untag).format, Some(Uri))
  switch schema {
  | String({format}) => t->Assert.deepEqual(format, Uri)
  | _ => t->Assert.fail("Expected String with format Uri")
  }
})

// `S.url` is an instance of the JS `URL` class, the way `S.date` is an
// instance of `Date`.

test("Is an instance schema", t => {
  t->Assert.deepEqual((S.url->S.untag).tag, Instance)
})

test("Successfully parses a URL instance", t => {
  let value = %raw(`new URL("http://dzakh.dev/")`)

  t->Assert.deepEqual(value->S.parseOrThrow(~to=S.url), value)
})

test("Fails to parse a string directly", t => {
  t->U.assertThrowsMessage(
    () => "http://dzakh.dev/"->S.parseOrThrow(~to=S.url),
    `Expected URL, received "http://dzakh.dev/"`,
  )
})

test("Parses a string into a URL through S.to", t => {
  let schema = S.string->S.to(S.url)

  t->Assert.deepEqual("http://dzakh.dev/"->S.parseOrThrow(~to=schema)->href, "http://dzakh.dev/")
})

test("Fails to parse a malformed string through S.to", t => {
  let schema = S.string->S.to(S.url)

  t->U.assertThrowsMessage(
    () => "cifjhdsfhsd"->S.parseOrThrow(~to=schema),
    `Expected URL, received "cifjhdsfhsd"`,
  )
})

test("Encodes a URL back to a uri string", t => {
  let value = %raw(`new URL("http://dzakh.dev/")`)

  t->Assert.deepEqual(
    value->S.convertOrThrow(~from=S.url, ~to=S.string),
    %raw(`"http://dzakh.dev/"`),
  )
})
