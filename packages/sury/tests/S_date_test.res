open Vitest

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

test("Successfully decodes JSON array of dates", t => {
  let schema = S.array(S.date)
  let decoder = S.decoder(~from=S.json, ~to=schema)
  let d1 = Date.fromString("2024-01-01T00:00:00.000Z")
  let d2 = Date.fromString("2024-06-15T12:30:45.123Z")
  t->Assert.deepEqual(
    decoder(%raw(`["2024-01-01T00:00:00.000Z", "2024-06-15T12:30:45.123Z"]`)),
    [d1, d2],
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

// Regression guard: encoding a `@s.nullable option<Timestamp.t>` field (ppx-expanded
// to `S.nullableAsOption(Timestamp.schema)`) used to throw `received [object Date]`
// instead of serializing the Date back to a string. Parsing was never affected — only
// the reverse, where the union variant's type-check narrow dropped the member's encoder.

module Timestamp = {
  type t = Date.t

  let schema: S.t<t> = S.string->S.to(S.date)
}

test("Reverse converts nullableAsOption string-to-date schema", t => {
  let schema = S.nullableAsOption(Timestamp.schema)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")

  t->Assert.deepEqual("2024-01-01T00:00:00.000Z"->S.parseOrThrow(~to=schema), Some(date))
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0=new Date(i);!Number.isNaN(v0.getTime())||e[0](v0);i=v0}else if(i===null){i=void 0}else if(!(i===void 0)){e[1](i)}return i}`,
  )

  t->Assert.deepEqual(
    Some(date)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    "2024-01-01T00:00:00.000Z"->Obj.magic,
  )
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i instanceof e[0]){i=i.toISOString()}else if(!(i===void 0)){e[1](i)}return i}`,
  )
})

test("Reverse converts nullable string-to-date schema", t => {
  let schema = S.nullable(Timestamp.schema)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")

  t->Assert.deepEqual(Nullable.Null->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(
    Nullable.Value(date)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    "2024-01-01T00:00:00.000Z"->Obj.magic,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i instanceof e[0]){i=i.toISOString()}else if(!(i===void 0||i===null)){e[1](i)}return i}`,
  )
})

test("Reverse converts deeply nested records/array sharing a nullable Timestamp field", t => {
  // `@s.nullable option<Timestamp.t>` field, present both on a nested record
  // (inside an array) and on the parent record with the same name and type.
  let field = () => S.nullableAsOption(Timestamp.schema)

  let itemSchema = S.schema(s =>
    {
      "createdAt": s.matches(field()),
    }
  )
  let parentSchema = S.schema(s =>
    {
      "createdAt": s.matches(field()),
      "items": s.matches(S.array(itemSchema)),
    }
  )

  let date = Date.fromString("2024-01-01T00:00:00.000Z")

  t->Assert.deepEqual(
    %raw(`{createdAt: "2024-01-01T00:00:00.000Z", items: [{createdAt: "2024-01-01T00:00:00.000Z"}]}`)
    ->S.parseOrThrow(~to=parentSchema),
    {
      "createdAt": Some(date),
      "items": [{"createdAt": Some(date)}],
    },
  )

  let value = {
    "createdAt": Some(date),
    "items": [{"createdAt": Some(date)}],
  }
  t->Assert.deepEqual(
    value->S.decodeOrThrow(~from=parentSchema, ~to=S.unknown),
    %raw(`{createdAt: "2024-01-01T00:00:00.000Z", items: [{createdAt: "2024-01-01T00:00:00.000Z"}]}`),
  )
})

test("Encodes a nullable optional Timestamp whose input is string | number (issue repro)", t => {
  // The reported Timestamp accepts a string or a numeric timestamp:
  //   S.union([S.string, S.float])->S.to(S.date)
  // so `@s.nullable option<Timestamp.t>` reverses to a Date variant whose `.to`
  // target is `string | number`. Encoding used to throw exactly
  // `Expected string | number, received [object Date]`.
  let timestamp = S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(S.date)
  let schema = S.nullableAsOption(timestamp)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")

  t->Assert.deepEqual("2024-01-01T00:00:00.000Z"->S.parseOrThrow(~to=schema), Some(date))
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)

  t->Assert.deepEqual(
    Some(date)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    "2024-01-01T00:00:00.000Z"->Obj.magic,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i instanceof e[2]){try{i=i.toISOString()}catch(e0){e[1](i,e0,e[0])}}else if(!(i===void 0)){e[3](i)}return i}`,
  )
})
