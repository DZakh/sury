open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual("http://dzakh.dev"->S.parseOrThrow(~to=schema), "http://dzakh.dev")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.parseOrThrow(~to=schema), `Invalid url`)
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual(
    "http://dzakh.dev"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"http://dzakh.dev"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.url

  t->U.assertThrowsMessage(() => "cifjhdsfhsd"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Invalid url`)
})

test("Returns custom error message", t => {
  let schema = S.string->S.url(~message="Custom")

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Custom`)
})
