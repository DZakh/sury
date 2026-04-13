open Ava

test("Successfully parses valid Date", t => {
  let date = Date.fromString("2024-01-01T00:00:00Z")
  t->Assert.deepEqual(date->S.parseOrThrow(~to=S.date), date)
})

test("Successfully parses Date with time", t => {
  let date = Date.fromString("2024-06-15T12:30:45.123Z")
  t->Assert.deepEqual(date->S.parseOrThrow(~to=S.date), date)
})

test("Fails to parse string", t => {
  t->U.assertThrowsMessage(
    () => %raw(`"2024-01-01"`)->S.parseOrThrow(~to=S.date),
    `Expected Date, received "2024-01-01"`,
  )
})

test("Fails to parse number", t => {
  t->U.assertThrowsMessage(
    () => %raw(`123`)->S.parseOrThrow(~to=S.date),
    `Expected Date, received 123`,
  )
})

test("Fails to parse Invalid Date", t => {
  t->U.assertThrowsMessage(
    () => %raw(`new Date("invalid")`)->S.parseOrThrow(~to=S.date),
    `Expected Date, received [object Date]`,
  )
})

test("Successfully reverse converts", t => {
  let date = Date.fromString("2024-01-01T00:00:00Z")
  t->Assert.deepEqual(date->S.decodeOrThrow(~from=S.date, ~to=S.unknown), date->Obj.magic)
})

test("Schema has instance tag and Date class", t => {
  switch S.date->Obj.magic {
  | S.Instance({class: c}) => t->Assert.is(c, %raw(`Date`))
  | _ => t->Assert.fail("Expected instance schema")
  }
})

// S.string->S.to(S.date) conversion tests

test("Successfully parses string to Date with S.to", t => {
  let schema = S.string->S.to(S.date)
  t->Assert.deepEqual(
    "2024-01-01T00:00:00.000Z"->S.parseOrThrow(~to=schema),
    Date.fromString("2024-01-01T00:00:00.000Z"),
  )
})

test("Successfully parses ISO string with fractional seconds to Date with S.to", t => {
  let schema = S.string->S.to(S.date)
  t->Assert.deepEqual(
    "2024-06-15T12:30:45.123Z"->S.parseOrThrow(~to=schema),
    Date.fromString("2024-06-15T12:30:45.123Z"),
  )
})

test("Fails to parse invalid string to Date with S.to", t => {
  let schema = S.string->S.to(S.date)
  t->U.assertThrowsMessage(
    () => "invalid"->S.parseOrThrow(~to=schema),
    `Expected Date, received [object Date]`,
  )
})

test("Successfully reverse converts string-to-date schema", t => {
  let schema = S.string->S.to(S.date)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")
  t->Assert.deepEqual(
    date->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    "2024-01-01T00:00:00.000Z"->Obj.magic,
  )
})

// S.date->S.to(S.string) conversion tests

test("Successfully converts Date to string with S.to", t => {
  let schema = S.date->S.to(S.string)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")
  t->Assert.deepEqual(
    S.decoder1(schema)(date->Obj.magic),
    "2024-01-01T00:00:00.000Z"->Obj.magic,
  )
})

test("Successfully reverse converts date-to-string schema", t => {
  let schema = S.date->S.to(S.string)
  t->Assert.deepEqual(
    "2024-01-01T00:00:00.000Z"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    Date.fromString("2024-01-01T00:00:00.000Z")->Obj.magic,
  )
})

// JSON → Date conversion tests

S.enableJson()
S.enableJsonString()

test("Successfully decodes JSON string to Date", t => {
  let date = Date.fromString("2024-01-01T00:00:00.000Z")
  let decoder = S.decoder(~from=S.json, ~to=S.date)
  t->Assert.deepEqual(
    decoder(JSON.Encode.string("2024-01-01T00:00:00.000Z")),
    date,
  )
})

test("Successfully decodes JSON object with date field", t => {
  let dateSchema = S.schema(s =>
    {
      "field": s.matches(S.date),
    }
  )
  let date = Date.fromString("2024-01-01T00:00:00.000Z")
  let decoder = S.decoder(~from=S.json, ~to=dateSchema)
  t->Assert.deepEqual(
    decoder(%raw(`{"field":"2024-01-01T00:00:00.000Z"}`)),
    {"field": date},
  )
})

test("Fails to decode non-string JSON value to Date", t => {
  let decoder = S.decoder(~from=S.json, ~to=S.date)
  t->U.assertThrowsMessage(
    () => decoder(%raw(`123`)),
    `Expected string, received 123`,
  )
})

test("Fails to decode invalid date string from JSON", t => {
  let decoder = S.decoder(~from=S.json, ~to=S.date)
  t->U.assertThrowsMessage(
    () => decoder(JSON.Encode.string("invalid")),
    `Expected Date, received [object Date]`,
  )
})

test("Successfully decodes JSON string to Date via jsonString", t => {
  let dateSchema = S.schema(s =>
    {
      "field": s.matches(S.date),
    }
  )
  let date = Date.fromString("2024-01-01T00:00:00.000Z")
  t->Assert.deepEqual(
    `{"field":"2024-01-01T00:00:00.000Z"}`->S.decodeOrThrow(~from=S.jsonString, ~to=dateSchema),
    {"field": date},
  )
})

test("Successfully round-trips date through JSON", t => {
  let dateSchema = S.schema(s =>
    {
      "field": s.matches(S.date),
    }
  )
  let date = Date.fromString("2024-06-15T12:30:45.123Z")
  let toJson = S.decoder(~from=dateSchema, ~to=S.json)
  let fromJson = S.decoder(~from=S.json, ~to=dateSchema)
  t->Assert.deepEqual(
    fromJson(toJson({"field": date})),
    {"field": date},
  )
})
