open Vitest

test("Successfully parses valid data", t => {
  let schema = S.isoDateTime

  t->Assert.deepEqual(
    "2020-01-01T00:00:00Z"->S.parseOrThrow(~to=schema),
    S.IsoDateTime("2020-01-01T00:00:00Z"),
  )
  t->Assert.deepEqual(
    "2020-01-01T00:00:00.123Z"->S.parseOrThrow(~to=schema),
    S.IsoDateTime("2020-01-01T00:00:00.123Z"),
  )
  t->Assert.deepEqual(
    "2020-01-01T00:00:00.123456Z"->S.parseOrThrow(~to=schema),
    S.IsoDateTime("2020-01-01T00:00:00.123456Z"),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);e[0](i)||e[1](i);return i}`,
  )
})

test("Fails to parse a non ISO date string", t => {
  let schema = S.isoDateTime

  t->U.assertThrowsMessage(
    () => "Thu Apr 20 2023 10:45:48 GMT+0400"->S.parseOrThrow(~to=schema),
    `Expected date-time, received "Thu Apr 20 2023 10:45:48 GMT+0400"`,
  )
})

test("Accepts a timezone offset", t => {
  let schema = S.isoDateTime

  t->Assert.deepEqual(
    "2020-01-01T00:00:00+02:00"->S.parseOrThrow(~to=schema),
    S.IsoDateTime("2020-01-01T00:00:00+02:00"),
  )
})

test("utcDateTime rejects a timezone offset", t => {
  let schema = S.utcDateTime

  t->Assert.deepEqual(
    "2020-01-01T00:00:00Z"->S.parseOrThrow(~to=schema),
    S.UtcDateTime("2020-01-01T00:00:00Z"),
  )
  t->U.assertThrowsMessage(
    () => "2020-01-01T00:00:00+02:00"->S.parseOrThrow(~to=schema),
    `Expected UTC date-time, received "2020-01-01T00:00:00+02:00"`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.isoDateTime

  t->Assert.deepEqual(
    S.IsoDateTime("2020-01-01T00:00:00.123Z")->S.convertOrThrow(~from=schema, ~to=S.unknown),
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
    `Expected date-time, received "not-a-date"`,
  )
})
