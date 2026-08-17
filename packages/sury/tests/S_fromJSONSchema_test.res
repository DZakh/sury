open Vitest
open JSONSchema

// Helper for round-trip: S -> toJSONSchema -> fromJSONSchema -> S
let roundTrip = schema => schema->S.toJSONSchema->S.fromJSONSchema

// Helper for round-trip: JSONSchema -> fromJSONSchema -> toJSONSchema
let jsonRoundTrip = js => js->S.fromJSONSchema->S.toJSONSchema

// Helper for parsing
let parse = (schema, value) => value->S.parseOrThrow(~to=schema)->Obj.magic

// Helper for deepEqual
let eq = (a, b) => JSON.stringify(a) == JSON.stringify(b)

// 1. Primitive types

test("fromJSONSchema: boolean definitions", t => {
  t->Assert.deepEqual(parse(S.fromJSONSchemaDefinition(Any), {"ok": true}), {"ok": true})
  t->Assert.throws(() => parse(S.fromJSONSchemaDefinition(Never), %raw("null")))
})

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
  t->Assert.deepEqual(jsonRoundTrip(js), js)
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
  t->Assert.deepEqual(jsonRoundTrip(js), %raw(`{"type": "string", "const": "foo"}`))
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

test("fromJSONSchema: tuple via draft-2020-12 prefixItems", t => {
  let js = {
    type_: Arrayable.single(#array),
    prefixItems: [
      Schema({type_: Arrayable.single(#string)}),
      Schema({type_: Arrayable.single(#number)}),
    ],
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, ("a", 1)), ("a", 1))
  t->Assert.throws(() => parse(schema, (1, "a")))
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
  t->Assert.deepEqual(jsonRoundTrip(js), {type_: Arrayable.single(#object)})
})

test("fromJSONSchema: bare object with no properties or additionalProperties", t => {
  let js = {type_: Arrayable.single(#object)}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"foo": 1}), {"foo": 1})
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
  t->Assert.deepEqual(jsonRoundTrip(js), js)
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
  t->Assert.deepEqual(jsonRoundTrip(js), js)
})

test("fromJSONSchema: not", t => {
  let js = {not: Schema({type_: Arrayable.single(#string)})}
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, 1), 1)
  t->Assert.throws(() => parse(schema, "hi"))
  t->Assert.deepEqual(jsonRoundTrip(js), js)
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

// All format schemas (including date-time) compose with sibling constraints.
test("fromJSONSchema: format date-time composes with sibling minLength/maxLength", t => {
  let js = {
    type_: Arrayable.single(#string),
    format: "date-time",
    minLength: 10,
    maxLength: 30,
  }
  let schema = S.fromJSONSchema(js)
  // A valid ISO datetime within length bounds parses.
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
  t->Assert.throws(
    () => S.fromJSONSchema(js),
    ~expectations={message: "Unsupported JSON Schema type: unknownType"},
  )
})

// 10. $ref

test("fromJSONSchema: a finite $ref inlines, and round-trips as what it inlined", t => {
  let js = {
    ref: "#/$defs/Name",
    defs: Dict.fromArray([("Name", Schema({type_: Arrayable.single(#string)}))]),
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, "foo"), "foo")
  t->Assert.throws(() => parse(schema, 1))
  // No cycle came back to the pointer, so it left no `$defs` entry to point at
  // — and the same document comes back out whether or not options are passed.
  t->Assert.deepEqual(jsonRoundTrip(js), {type_: Arrayable.single(#string)})
})

test("fromJSONSchema: draft-07 spells the same defs `definitions`", t => {
  let js = {
    type_: Arrayable.single(#array),
    items: Arrayable.single(Schema({ref: "#/definitions/Item"})),
    definitions: Dict.fromArray([("Item", Schema({type_: Arrayable.single(#boolean)}))]),
  }
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, [true, false]), [true, false])
  t->Assert.throws(() => parse(schema, [1]))
})

test("fromJSONSchema: a pointer resolves through any path, not just a defs dict", t => {
  // OpenAPI keeps its definitions under `components/schemas`, which is a plain
  // JSON Pointer like any other.
  let js: JSONSchema.t = %raw(`{
    "$ref": "#/components/schemas/Pet",
    "components": {"schemas": {"Pet": {"type": "string"}}}
  }`)
  t->Assert.deepEqual(parse(S.fromJSONSchema(js), "cat"), "cat")
})

test("fromJSONSchema: pointer segments are unescaped per RFC 6901", t => {
  let js: JSONSchema.t = %raw(`{
    "$defs": {"a/b": {"type": "string"}, "c~d": {"type": "boolean"}},
    "type": "object",
    "properties": {"slash": {"$ref": "#/$defs/a~1b"}, "tilde": {"$ref": "#/$defs/c~0d"}},
    "required": ["slash", "tilde"]
  }`)
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"slash": "s", "tilde": true}), {"slash": "s", "tilde": true})
  t->Assert.throws(() => parse(schema, {"slash": true, "tilde": true}))
})

test("fromJSONSchema: a recursive $ref round-trips as a $ref plus its $defs", t => {
  let js: JSONSchema.t = %raw(`{
    "$ref": "#/$defs/Node",
    "$defs": {
      "Node": {
        "type": "object",
        "properties": {"next": {"$ref": "#/$defs/Node"}}
      }
    }
  }`)
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(
    parse(schema, {"next": {"next": Dict.make()}}),
    {"next": {"next": Dict.make()}},
  )
  let out = jsonRoundTrip(js)
  t->Assert.deepEqual((out->Obj.magic)["$ref"], %raw(`"#/$defs/Node"`))
  t->Assert.deepEqual((out->Obj.magic)["$defs"]["Node"]["additionalProperties"], %raw(`undefined`))
  t->Assert.deepEqual(parse(S.fromJSONSchema(out), {"next": Dict.make()}), {"next": Dict.make()})
})

test("fromJSONSchema: `#` points at the document itself", t => {
  let js: JSONSchema.t = %raw(`{
    "type": "object",
    "properties": {"self": {"$ref": "#"}}
  }`)
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"self": Dict.make()}), {"self": Dict.make()})
})

test("fromJSONSchema: a $ref resolves inside allOf, which compiles on its own", t => {
  let js: JSONSchema.t = %raw(`{
    "$defs": {"Node": {"type": "object", "properties": {"next": {"$ref": "#/$defs/Node"}}}},
    "allOf": [{"$ref": "#/$defs/Node"}]
  }`)
  let schema = S.fromJSONSchema(js)
  t->Assert.deepEqual(parse(schema, {"next": Dict.make()}), {"next": Dict.make()})
  t->Assert.throws(() => parse(schema, {"next": 1}))
})

test("fromJSONSchema: an unresolvable $ref throws", t => {
  t->Assert.throws(
    () => S.fromJSONSchema({ref: "#/$defs/Missing"}),
    ~expectations={message: "Failed to resolve JSON Schema $ref: #/$defs/Missing"},
  )
})

test("fromJSONSchema: a $ref out of the document throws", t => {
  t->Assert.throws(
    () => S.fromJSONSchema({ref: "https://example.com/Pet.json"}),
    ~expectations={
      message: "Unsupported JSON Schema $ref: https://example.com/Pet.json. Only JSON Pointers into the same document (#/…) resolve — $id, $anchor and remote refs don't",
    },
  )
})

test("fromJSONSchema: a $ref to a non-schema value throws instead of widening to any", t => {
  // Resolvable pointers into a string, null and an array — all valid JSON, none a schema.
  t->Assert.throws(
    () => S.fromJSONSchema(%raw(`{"$ref": "#/$defs/A/type", "$defs": {"A": {"type": "string"}}}`)),
    ~expectations={message: "Failed to resolve JSON Schema $ref: #/$defs/A/type"},
  )
  t->Assert.throws(
    () => S.fromJSONSchema(%raw(`{"$ref": "#/$defs/N", "$defs": {"N": null}}`)),
    ~expectations={message: "Failed to resolve JSON Schema $ref: #/$defs/N"},
  )
  t->Assert.throws(
    () => S.fromJSONSchema(%raw(`{"$ref": "#/$defs/A/enum", "$defs": {"A": {"enum": [1]}}}`)),
    ~expectations={message: "Failed to resolve JSON Schema $ref: #/$defs/A/enum"},
  )
})

test("fromJSONSchema: a content-free $ref cycle throws instead of overflowing the stack", t => {
  t->Assert.throws(
    () => S.fromJSONSchema(%raw(`{"$ref": "#"}`)),
    ~expectations={message: "Infinite JSON Schema $ref loop: #"},
  )
  t->Assert.throws(
    () =>
      S.fromJSONSchema(
        %raw(`{"$ref": "#/$defs/A", "$defs": {"A": {"$ref": "#/$defs/B"}, "B": {"$ref": "#/$defs/A"}}}`),
      ),
    ~expectations={message: "Infinite JSON Schema $ref loop: #/$defs/A"},
  )
})

test("fromJSONSchema: a % that isn't percent-encoding resolves literally", t => {
  let js: JSONSchema.t = %raw(`{
    "$ref": "#/$defs/50%",
    "$defs": {"50%": {"type": "string"}}
  }`)
  t->Assert.deepEqual(parse(S.fromJSONSchema(js), "half"), "half")
})

test("fromJSONSchema: a recursive def whose name needs escaping round-trips resolvable", t => {
  let js: JSONSchema.t = %raw(`{
    "$ref": "#/$defs/a~1b",
    "$defs": {
      "a/b": {"type": "object", "properties": {"next": {"$ref": "#/$defs/a~1b"}}}
    }
  }`)
  let out = jsonRoundTrip(js)
  // The emitted name is sanitized so the emitted pointer parses back to the
  // same key; the document must feed fromJSONSchema again unchanged.
  let schema = S.fromJSONSchema(out)
  t->Assert.deepEqual(parse(schema, {"next": Dict.make()}), {"next": Dict.make()})
})

test("fromJSONSchema: an inlined ref releases its def name for a later cycle", t => {
  let js: JSONSchema.t = %raw(`{
    "type": "object",
    "properties": {
      "finite": {"$ref": "#/components/schemas/Node"},
      "rec": {"$ref": "#/$defs/Node"}
    },
    "components": {"schemas": {"Node": {"type": "string"}}},
    "$defs": {"Node": {"type": "object", "properties": {"next": {"$ref": "#/$defs/Node"}}}}
  }`)
  let out = jsonRoundTrip(js)
  t->Assert.deepEqual((out->Obj.magic)["$defs"]["Node"]["type"], %raw(`"object"`))
})

// 11. Round-trip S -> toJSONSchema -> fromJSONSchema -> S

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
