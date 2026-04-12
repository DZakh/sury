open Ava

S.enableIsoDateTime()

test("Successfully parses valid data", t => {
  let schema = S.isoDateTime

  t->Assert.deepEqual("2020-01-01T00:00:00Z"->S.parseOrThrow(~to=schema), "2020-01-01T00:00:00Z")
  t->Assert.deepEqual(
    "2020-01-01T00:00:00.123Z"->S.parseOrThrow(~to=schema),
    "2020-01-01T00:00:00.123Z",
  )
  t->Assert.deepEqual(
    "2020-01-01T00:00:00.123456Z"->S.parseOrThrow(~to=schema),
    "2020-01-01T00:00:00.123456Z",
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!e[0].test(i)){e[1]()}typeof i==="string"||e[2](i);return i}`,
  )
})

test("Fails to parse non UTC date string", t => {
  let schema = S.isoDateTime

  t->U.assertThrowsMessage(
    () => "Thu Apr 20 2023 10:45:48 GMT+0400"->S.parseOrThrow(~to=schema),
    `Invalid datetime string! Expected UTC`,
  )
})

test("Fails to parse UTC date with timezone offset", t => {
  let schema = S.isoDateTime

  t->U.assertThrowsMessage(
    () => "2020-01-01T00:00:00+02:00"->S.parseOrThrow(~to=schema),
    `Invalid datetime string! Expected UTC`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.isoDateTime

  t->Assert.deepEqual(
    "2020-01-01T00:00:00.123Z"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"2020-01-01T00:00:00.123Z"`),
  )
})

test("Can be combined with S.to(S.date) for string-to-Date decoding", t => {
  let schema = S.isoDateTime->S.to(S.date)

  t->Assert.deepEqual(
    "2020-01-01T00:00:00.123Z"->S.parseOrThrow(~to=schema),
    Date.fromString("2020-01-01T00:00:00.123Z"),
  )
  t->U.assertThrowsMessage(
    () => "not-a-date"->S.parseOrThrow(~to=schema),
    `Invalid datetime string! Expected UTC`,
  )
})
