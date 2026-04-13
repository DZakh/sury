open Ava
open JSONSchema

S.enableJson()

// Helper for round-trip: S -> toJSONSchema -> fromJSONSchema -> S
let roundTrip = schema => schema->S.toJSONSchema->S.fromJSONSchema

// Helper for round-trip: JSONSchema -> fromJSONSchema -> toJSONSchema
let jsonRoundTrip = js => js->S.fromJSONSchema->S.toJSONSchema

// Helper for parsing
let parse = (schema, value) => value->S.parseOrThrow(~to=schema)->Obj.magic

// Helper for deepEqual
let eq = (a, b) => JSON.stringify(a) == JSON.stringify(b)

// 1. Primitive types

test("fromJSONSchema: string", t => {
  let js = {type_: Arrayable.single(#string)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "foo"), "foo")
  t->Assert.throws(() => parse(schema, 123))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: number", t => {
  let js = {type_: Arrayable.single(#number)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, 1.5), 1.5)
  t->Assert.throws(() => parse(schema, "foo"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: integer", t => {
  let js = {type_: Arrayable.single(#integer)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, 42), 42)
  t->Assert.throws(() => parse(schema, 1.5))
  t->Assert.deepEqual(
    jsonRoundTrip(js),
    {type_: Arrayable.single(#integer), minimum: -2147483648., maximum: 2147483647.},
  )
})

test("fromJSONSchema: boolean", t => {
  let js = {type_: Arrayable.single(#boolean)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, true), true)
  t->Assert.throws(() => parse(schema, 0))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: null", t => {
  let js = {type_: Arrayable.single(#null)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, %raw("null")), %raw("null"))
  t->Assert.throws(() => parse(schema, 0))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

// 2. Literals: const, enum

test("fromJSONSchema: const", t => {
  let js = {const: %raw(`"foo"`)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "foo"), "foo")
  t->Assert.throws(() => parse(schema, "bar"))
  // toJSONSchema adds type for literal schemas
  t->Assert.deepEqual(jsonRoundTrip(js), {...js, type_: Arrayable.single(#string)})
})

test("fromJSONSchema: enum", t => {
  let js = {enum: [%raw(`"a"`), %raw(`"b"`), %raw(`"c"`)]}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "a"), "a")
  t->Assert.throws(() => parse(schema, "z"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

// 3. Arrays

test("fromJSONSchema: array of string", t => {
  let js = {
    type_: Arrayable.single(#array),
    items: Arrayable.single(Schema({type_: Arrayable.single(#string)})),
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, ["a", "b"]), ["a", "b"])
  t->Assert.throws(() => parse(schema, [1, 2]))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: array with minItems/maxItems", t => {
  let js = {
    type_: Arrayable.single(#array),
    items: Arrayable.single(Schema({type_: Arrayable.single(#number)})),
    minItems: 2,
    maxItems: 3,
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, [1, 2]), [1, 2])
  t->Assert.throws(() => parse(schema, [1]))
  t->Assert.throws(() => parse(schema, [1, 2, 3, 4]))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: tuple", t => {
  let js = {
    type_: Arrayable.single(#array),
    items: Arrayable.array([
      Schema({type_: Arrayable.single(#string)}),
      Schema({type_: Arrayable.single(#number)}),
    ]),
    minItems: 2,
    maxItems: 2,
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, ("a", 1)), ("a", 1))
  t->Assert.throws(() => parse(schema, (1, "a")))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

// 4. Objects

test("fromJSONSchema: object with properties", t => {
  let js = {
    type_: Arrayable.single(#object),
    properties: Dict.fromArray([
      ("foo", Schema({type_: Arrayable.single(#string)})),
      ("bar", Schema({type_: Arrayable.single(#number)})),
    ]),
    required: ["foo"],
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"foo": "hi", "bar": 1}), {"foo": "hi", "bar": 1})
  t->Assert.throws(() => parse(schema, {"bar": 1}))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: object with additionalProperties false", t => {
  let js = {
    type_: Arrayable.single(#object),
    properties: Dict.fromArray([("foo", Schema({type_: Arrayable.single(#string)}))]),
    additionalProperties: Never,
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"foo": "hi"}), {"foo": "hi"})
  t->Assert.throws(() => parse(schema, {"foo": "hi", "bar": 1}))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: object with additionalProperties true", t => {
  let js = {
    type_: Arrayable.single(#object),
    additionalProperties: Any,
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"foo": 1, "bar": 2}), {"foo": 1, "bar": 2})
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

// 5. Combinators

test("fromJSONSchema: anyOf", t => {
  let js = {
    anyOf: [Schema({type_: Arrayable.single(#string)}), Schema({type_: Arrayable.single(#number)})],
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "hi"), "hi")
  t->Assert.deepEqual(parse(schema, 1), 1)
  t->Assert.throws(() => parse(schema, true))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: oneOf", t => {
  let js = {
    oneOf: [Schema({type_: Arrayable.single(#string)}), Schema({type_: Arrayable.single(#number)})],
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "hi"), "hi")
  t->Assert.deepEqual(parse(schema, 1), 1)
  t->Assert.throws(() => parse(schema, true))
  // refine-based oneOf can't round-trip the structural info
  t->Assert.deepEqual(jsonRoundTrip(js), {})
})

test("fromJSONSchema: allOf", t => {
  let js = {
    allOf: [
      Schema({type_: Arrayable.single(#number), minimum: 0.}),
      Schema({type_: Arrayable.single(#number), maximum: 10.}),
    ],
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, 5), 5)
  t->Assert.throws(() => parse(schema, 20))
  // refine-based allOf can't round-trip the structural info
  t->Assert.deepEqual(jsonRoundTrip(js), {})
})

test("fromJSONSchema: not", t => {
  let js = {not: Schema({type_: Arrayable.single(#string)})}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, 1), 1)
  t->Assert.throws(() => parse(schema, "hi"))
  // refine-based not can't round-trip the structural info
  t->Assert.deepEqual(jsonRoundTrip(js), {})
})

// 6. Nullable

test("fromJSONSchema: nullable true", t => {
  let js = {type_: Arrayable.single(#string), nullable: true}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "hi"), "hi")
  t->Assert.deepEqual(parse(schema, %raw("null")), %raw("null"))
  // toJSONSchema uses anyOf style for nullable
  t->Assert.deepEqual(
    jsonRoundTrip(js),
    {anyOf: [Schema({type_: Arrayable.single(#string)}), Schema({type_: Arrayable.single(#null)})]},
  )
})

test("fromJSONSchema: nullable false", t => {
  let js = {type_: Arrayable.single(#string), nullable: false}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "hi"), "hi")
  t->Assert.throws(() => parse(schema, %raw("null")))
  // nullable: false is the default, so toJSONSchema omits it
  t->Assert.deepEqual(jsonRoundTrip(js), {type_: Arrayable.single(#string)})
})

// 7. Format

test("fromJSONSchema: string format email", t => {
  let js = {type_: Arrayable.single(#string), format: "email"}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "foo@bar.com"), "foo@bar.com")
  t->Assert.throws(() => parse(schema, "not-an-email"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: string format uuid", t => {
  let js = {type_: Arrayable.single(#string), format: "uuid"}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(
    parse(schema, "123e4567-e89b-12d3-a456-426614174000"),
    "123e4567-e89b-12d3-a456-426614174000",
  )
  t->Assert.throws(() => parse(schema, "not-a-uuid"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: string format date-time", t => {
  let js = {type_: Arrayable.single(#string), format: "date-time"}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "2020-01-01T00:00:00Z"), "2020-01-01T00:00:00Z")
  t->Assert.throws(() => parse(schema, "not-a-date"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("Round-trip S.string->S.to(S.date) through toJSONSchema/fromJSONSchema", t => {
  let schema = S.string->S.to(S.date)
  let js = schema->S.toJSONSchema
  t->Assert.deepEqual(js, %raw(`{"type": "string", "format": "date-time"}`))
  // fromJSONSchema then toJSONSchema should preserve the format
  t->Assert.deepEqual(js->S.fromJSONSchema->S.toJSONSchema, js)
})

// Unlike email/uri/uuid formats, which compose with minLength/maxLength/pattern
// (via schema->email, etc.), `format: "date-time"` resolves to the standalone
// S.isoDateTime singleton. Any sibling pattern/min/max refinements on the
// incoming JSON schema are intentionally discarded for this case — ISO 8601
// datetime strings have a fully anchored shape so length/pattern constraints
// are redundant with the regex validator inside enableIsoDateTime. This test
// pins that behavior so it can't silently regress.
test("fromJSONSchema: format date-time ignores sibling minLength/maxLength/pattern", t => {
  let js = {
    type_: Arrayable.single(#string),
    format: "date-time",
    minLength: 10000,
    maxLength: 1,
    pattern: "^nope$",
  }
  let schema = S.fromJSONSchema(js)
  // A valid ISO datetime still parses even though minLength=10000 would reject it.
  t->Assert.deepEqual(parse(schema, "2020-01-01T00:00:00Z"), "2020-01-01T00:00:00Z"->Obj.magic)
  // A non-ISO string still fails — the datetime validator runs.
  t->Assert.throws(() => parse(schema, "not-a-date"))
})

test("fromJSONSchema: string pattern", t => {
  let js = {type_: Arrayable.single(#string), pattern: "^foo$"}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "foo"), "foo")
  t->Assert.throws(() => parse(schema, "bar"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

// 8. Meta

test("fromJSONSchema: title, description, deprecated, examples", t => {
  let js = {
    type_: Arrayable.single(#string),
    title: "title",
    description: "desc",
    deprecated: true,
    examples: [%raw(`"a"`), %raw(`"b"`)],
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual((schema->S.untag).title, Some("title"))
  t->Assert.deepEqual((schema->S.untag).description, Some("desc"))
  t->Assert.deepEqual((schema->S.untag).deprecated, Some(true))
  t->Assert.deepEqual((schema->S.untag).examples, Some([%raw(`"a"`), %raw(`"b"`)]))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

// 9. Edge cases

test("fromJSONSchema: empty schema is any", t => {
  let js: JSONSchema.t = {}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "foo"), "foo")
  t->Assert.deepEqual(parse(schema, 1), 1)
  t->Assert.deepEqual(parse(schema, true), true)
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: unknown type throws", t => {
  let js = {type_: Arrayable.single((Obj.magic("unknownType"): typeName))}
  t->Assert.throws(() => S.fromJSONSchema(js), ~expectations={message: "[Sury] Unknown JSON Schema type: unknownType"})
})

// 10. Round-trip S -> toJSONSchema -> fromJSONSchema -> S

test("fromJSONSchema: round-trip for string schema", t => {
  let orig = S.string
  let round = roundTrip(orig)
  t->Assert.deepEqual(parse(round, "foo"), "foo")
  t->Assert.throws(() => parse(round, 1))
  t->Assert.deepEqual(round->S.toJSONSchema, orig->S.toJSONSchema)
})

test("fromJSONSchema: round-trip for object schema", t => {
  let orig = S.object(s => s.field("foo", S.string))
  let round = roundTrip(orig)
  t->Assert.deepEqual(parse(round, {"foo": "bar"}), {"foo": "bar"})
  t->Assert.throws(() => parse(round, {"foo": 1}))
  t->Assert.deepEqual(round->S.toJSONSchema, orig->S.toJSONSchema)
})
