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

// Reverse (encode) of a nullable/optional `string->S.to(S.date)` field.
//
// Reproduces a user report: given a `@s.nullable option<Timestamp.t>` field
// (which the ppx expands to `S.nullableAsOption(Timestamp.schema)`), encoding
// the value back throws `received [object Date]` instead of serializing the Date
// to its ISO string. Parsing works — only the reverse direction is broken, and
// only because the transformed value is an `S.date` (an Instance schema) wrapped
// in the union: reverse swaps each variant's parser/serializer but the `.to`
// conversion to the Date is driven by the date's encoder, which isn't re-applied
// per variant, so the reversed variant is left as a bare `Date` with no way back
// to a string.
//
// The bare `S.string->S.to(S.date)` reverses correctly (see "Successfully
// reverse converts string-to-date schema" above), and the same union wrapper
// reverses correctly for primitive transforms such as `S.bool->S.to(S.string)`
// (see S_nullable_test.res), so the defect is specific to a transformed Instance
// schema inside a nullable/optional union.
//
// The asserts below pin the current (buggy) output; the `FIXME`s describe what
// they should become once the reverse is fixed.

module Timestamp = {
  type t = Date.t

  let schema: S.t<t> = S.string->S.to(S.date)
}

test("Reverse converts nullableAsOption string-to-date schema", t => {
  let schema = S.nullableAsOption(Timestamp.schema)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")

  // Parsing the string into an optional Date works as expected.
  t->Assert.deepEqual("2024-01-01T00:00:00.000Z"->S.parseOrThrow(~to=schema), Some(date))
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0=new Date(i);!Number.isNaN(v0.getTime())||e[0](v0);i=v0}else if(i===null){i=void 0}else if(!(i===void 0)){e[1](i)}return i}`,
  )

  // Encoding `None` happens to round-trip, since that variant needs no transform.
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  // FIXME: Encoding the Date should serialize it back to its ISO string
  // ("2024-01-01T00:00:00.000Z"), like the bare `S.string->S.to(S.date)` does.
  // Instead the reversed schema dispatches on `instanceof Date` and then has no
  // conversion to apply, so it throws.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i instanceof e[2]){e[1](i,e[0])}else if(!(i===void 0)){e[3](i)}return i}`,
  )
  t->U.assertThrowsMessage(
    () => Some(date)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected Date | undefined | undefined, received [object Date]
- Can't decode Date to string. Use S.to to define a custom decoder`,
  )
})

test("Reverse converts nullable string-to-date schema", t => {
  let schema = S.nullable(Timestamp.schema)
  let date = Date.fromString("2024-01-01T00:00:00.000Z")

  t->Assert.deepEqual(Nullable.Null->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))

  // FIXME: Should serialize the Date back to "2024-01-01T00:00:00.000Z".
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i instanceof e[2]){e[1](i,e[0])}else if(!(i===void 0||i===null)){e[3](i)}return i}`,
  )
  t->U.assertThrowsMessage(
    () => Nullable.Value(date)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected Date | undefined | null, received [object Date]
- Can't decode Date to string. Use S.to to define a custom decoder`,
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

  // Parsing the nested structure works.
  t->Assert.deepEqual(
    %raw(`{createdAt: "2024-01-01T00:00:00.000Z", items: [{createdAt: "2024-01-01T00:00:00.000Z"}]}`)
    ->S.parseOrThrow(~to=parentSchema),
    {
      "createdAt": Some(date),
      "items": [{"createdAt": Some(date)}],
    },
  )

  // FIXME: Encoding back should produce the original JSON with ISO strings, e.g.
  // `{createdAt: "2024-01-01T00:00:00.000Z", items: [{createdAt: "...Z"}]}`.
  let value = {
    "createdAt": Some(date),
    "items": [{"createdAt": Some(date)}],
  }
  t->U.assertThrowsMessage(
    () => value->S.decodeOrThrow(~from=parentSchema, ~to=S.unknown),
    `Failed at ["createdAt"]: Expected Date | undefined | undefined, received [object Date]
- At ["createdAt"]: Can't decode Date to string. Use S.to to define a custom decoder`,
  )
})
