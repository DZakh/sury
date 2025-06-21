open Ava

test("JSONSchema of bool schema", t => {
  t->Assert.deepEqual(S.bool->S.toJSONSchema, %raw(`{"type": "boolean"}`))
})

test("JSONSchema of string schema", t => {
  t->Assert.deepEqual(S.string->S.toJSONSchema, %raw(`{"type": "string"}`))
})

test("JSONSchema of int schema", t => {
  t->Assert.deepEqual(S.int->S.toJSONSchema, %raw(`{"type": "integer"}`))
})

test("JSONSchema of float schema", t => {
  t->Assert.deepEqual(S.float->S.toJSONSchema, %raw(`{"type": "number"}`))
})

test("JSONSchema of email schema", t => {
  t->Assert.deepEqual(
    S.string->S.email->S.toJSONSchema,
    %raw(`{"type": "string", "format": "email"}`),
  )
})

test("JSONSchema of url schema", t => {
  t->Assert.deepEqual(
    S.string->S.url->S.toJSONSchema,
    %raw(`{"type": "string", "format": "uri"}`),
    ~message="The format should be uri for url schema",
  )
})

test("JSONSchema of datetime schema", t => {
  t->Assert.deepEqual(
    S.string->S.datetime->S.toJSONSchema,
    %raw(`{"type": "string", "format": "date-time"}`),
  )
})

test("JSONSchema of cuid schema", t => {
  t->Assert.deepEqual(S.string->S.cuid->S.toJSONSchema, %raw(`{"type": "string"}`))
})

test("JSONSchema of uuid schema", t => {
  t->Assert.deepEqual(
    S.string->S.uuid->S.toJSONSchema,
    %raw(`{"type": "string", "format": "uuid"}`),
  )
})

test("JSONSchema of pattern schema", t => {
  t->Assert.deepEqual(
    S.string->S.pattern(%re("/abc/g"))->S.toJSONSchema,
    %raw(`{"type": "string","pattern": "/abc/g"}`),
  )
})

test("JSONSchema of string schema uses the last refinement for format", t => {
  t->Assert.deepEqual(
    S.string->S.email->S.datetime->S.toJSONSchema,
    %raw(`{"type": "string", "format": "date-time"}`),
  )
})

test("JSONSchema of string with min", t => {
  t->Assert.deepEqual(
    S.string->S.min(1)->S.toJSONSchema,
    %raw(`{"type": "string", "minLength": 1}`),
  )
})

test("JSONSchema of string with max", t => {
  t->Assert.deepEqual(
    S.string->S.max(1)->S.toJSONSchema,
    %raw(`{"type": "string", "maxLength": 1}`),
  )
})

test("JSONSchema of string with length", t => {
  t->Assert.deepEqual(
    S.string->S.length(1)->S.toJSONSchema,
    %raw(`{"type": "string", "minLength": 1, "maxLength": 1}`),
  )
})

test("JSONSchema of string with both min and max", t => {
  t->Assert.deepEqual(
    S.string->S.min(1)->S.max(4)->S.toJSONSchema,
    %raw(`{"type": "string", "minLength": 1, "maxLength": 4}`),
  )
})

test("JSONSchema of int with min", t => {
  t->Assert.deepEqual(S.int->S.min(1)->S.toJSONSchema, %raw(`{"type": "integer", "minimum": 1}`))
})

test("JSONSchema of int with max", t => {
  t->Assert.deepEqual(S.int->S.max(1)->S.toJSONSchema, %raw(`{"type": "integer", "maximum": 1}`))
})

test("JSONSchema of port", t => {
  t->Assert.deepEqual(
    S.int->S.port->S.toJSONSchema,
    %raw(`{
      "type": "integer",
      "minimum": 0,
      "maximum": 65535,
    }`),
  )
})

test("JSONSchema of float with min", t => {
  t->Assert.deepEqual(
    S.float->S.floatMin(1.)->S.toJSONSchema,
    %raw(`{"type": "number", "minimum": 1}`),
  )
})

test("JSONSchema of float with max", t => {
  t->Assert.deepEqual(
    S.float->S.floatMax(1.)->S.toJSONSchema,
    %raw(`{"type": "number", "maximum": 1}`),
  )
})

test("JSONSchema of nullable float", t => {
  t->Assert.deepEqual(
    S.null(S.float)->S.toJSONSchema,
    %raw(`{"anyOf": [{"type": "number"}, {"type": "null"}]}`),
  )
})

test("JSONSchema of never", t => {
  t->Assert.deepEqual(S.never->S.toJSONSchema, %raw(`{"not": {}}`))
})

test("JSONSchema of true", t => {
  t->Assert.deepEqual(S.literal(true)->S.toJSONSchema, %raw(`{"type": "boolean", "const": true}`))
})

test("JSONSchema of false", t => {
  t->Assert.deepEqual(S.literal(false)->S.toJSONSchema, %raw(`{"type": "boolean", "const": false}`))
})

test("JSONSchema of string literal", t => {
  t->Assert.deepEqual(
    S.literal("Hello World!")->S.toJSONSchema,
    %raw(`{"type": "string", "const": "Hello World!"}`),
  )
})

test("JSONSchema of object literal", t => {
  t->Assert.deepEqual(
    S.literal({"received": true})->S.toJSONSchema,
    %raw(`{
        "type": "object",
        "additionalProperties": true,
        "properties": {
          "received": {
            "type": "boolean",
            "const": true
          }
        },
        "required": ["received"]
      }`),
  )
})

test("JSONSchema of number literal", t => {
  t->Assert.deepEqual(S.literal(123)->S.toJSONSchema, %raw(`{"type": "number", "const": 123}`))
})

test("JSONSchema of null", t => {
  t->Assert.deepEqual(S.literal(%raw(`null`))->S.toJSONSchema, %raw(`{"type": "null"}`))
})

test("JSONSchema of undefined", t => {
  t->U.assertThrowsMessage(
    () => S.literal(%raw(`undefined`))->S.toJSONSchema,
    `Failed converting to JSON: undefined is not valid JSON`,
  )
})

test("JSONSchema of NaN", t => {
  t->U.assertThrowsMessage(
    () => S.literal(%raw(`NaN`))->S.toJSONSchema,
    `Failed converting to JSON: NaN is not valid JSON`,
  )
})

test("JSONSchema of tuple", t => {
  t->Assert.deepEqual(
    S.tuple2(S.string, S.bool)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "minItems": 2,
      "maxItems": 2,
      "items": [{"type": "string"}, {"type": "boolean"}],
  }`),
  )
})

test("JSONSchema of object of literals schema", t => {
  t->Assert.deepEqual(
    S.schema(_ =>
      {
        "foo": "bar",
        "zoo": 123,
      }
    )->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "additionalProperties": true,
      "properties": {
        "foo": {
          "type": "string",
          "const": "bar"
        },
        "zoo": {
          "type": "number",
          "const": 123
        }
      },
      "required": ["foo", "zoo"]
  }`),
  )
})

test("JSONSchema of enum", t => {
  t->Assert.deepEqual(
    S.enum(["Yes", "No"])->S.toJSONSchema,
    %raw(`{
      "enum": ["Yes", "No"],
    }`),
  )
})

test("JSONSchema of union", t => {
  t->Assert.deepEqual(
    S.union([S.literal("Yes"), S.string])->S.toJSONSchema,
    %raw(`{
      "anyOf": [
        {
          const: 'Yes',
          type: 'string'
        },
        {
          type: 'string'
        }
      ]
    }`),
  )
})

test("JSONSchema of string array", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
    }`),
  )
})

test("JSONSchema of array with min length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.min(1)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "minItems": 1
    }`),
  )
})

test("JSONSchema of array with max length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.max(1)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "maxItems": 1
    }`),
  )
})

test("JSONSchema of array with fixed length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.length(1)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "minItems": 1,
      "maxItems": 1
    }`),
  )
})

test("JSONSchema of string dict", t => {
  t->Assert.deepEqual(
    S.dict(S.string)->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "additionalProperties": {"type": "string"},
    }`),
  )
})

test("JSONSchema of dict with optional fields", t => {
  t->Assert.deepEqual(
    S.dict(S.option(S.string))->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "additionalProperties": {"type": "string"},
    }`),
  )
})

test("JSONSchema of dict with optional invalid field", t => {
  t->U.assertThrowsMessage(
    () => S.dict(S.option(S.bigint))->S.toJSONSchema,
    `Failed converting to JSON: { [key: string]: bigint | undefined; } is not valid JSON`,
  )
})

test("JSONSchema of object with single string field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.string))->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"type": "string"}},
      "required": ["field"],
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema of object with strict mode", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.string))->S.strict->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"type": "string"}},
      "required": ["field"],
      "additionalProperties": false,
    }`),
  )
})

test("JSONSchema of object with optional field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.option(S.string)))->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"type": "string"}},
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema of object with deprecated field", t => {
  t->Assert.deepEqual(
    S.object(s =>
      s.field("field", S.string->S.meta({description: "Use another field", deprecated: true}))
    )->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {
        "type": "string",
        "deprecated": true,
        "description": "Use another field"
      }},
      "required": ["field"],
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema with title", t => {
  t->Assert.deepEqual(
    S.string->S.meta({title: "My field"})->S.toJSONSchema,
    %raw(`{"title": "My field", "type": "string"}`),
  )
})

test("Deprecated message overrides existing description", t => {
  t->Assert.deepEqual(
    S.string
    ->S.meta({description: "Previous description"})
    ->S.meta({description: "Use another field", deprecated: true})
    ->S.toJSONSchema,
    %raw(`{
      "type": "string",
      "deprecated": true,
      "description": "Use another field"
    }`),
  )
})

test("JSONSchema of nested object", t => {
  t->Assert.deepEqual(
    S.object(s =>
      s.field("objectWithOneStringField", S.object(s => s.field("Field", S.string)))
    )->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "objectWithOneStringField": {
          "type": "object",
          "properties": {"Field": {"type": "string"}},
          "required": ["Field"],
          "additionalProperties": true,
        },
      },
      "required": ["objectWithOneStringField"],
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema of object with one optional and one normal field", t => {
  t->Assert.deepEqual(
    S.object(s => (
      s.field("field", S.string),
      s.field("optionalField", S.option(S.string)),
    ))->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "field": {
          "type": "string",
        },
        "optionalField": {"type": "string"},
      },
      "required": ["field"],
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema of optional root schema", t => {
  t->U.assertThrowsMessage(
    () => S.option(S.string)->S.toJSONSchema,
    "Failed converting to JSON: string | undefined is not valid JSON",
  )
})

test("JSONSchema of object with S.option(S.option(_)) field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.option(S.option(S.string))))->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "field": {
          "type": "string",
        },
      },
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema of reversed object with S.option(S.option(_)) field", t => {
  t->U.assertThrowsMessage(
    () => S.object(s => s.field("field", S.option(S.option(S.string))))->S.reverse->S.toJSONSchema,
    // FIXME: Should work. Investigate why this test fails
    // %raw(`{
    //   "type": "object",
    //   "properties": {
    //     "field": {
    //       "type": "string",
    //     },
    //   },
    //   "additionalProperties": true,
    // }`),
    // (),
    `Failed converting to JSON: string | undefined | { BS_PRIVATE_NESTED_SOME_NONE: 0; } is not valid JSON`,
  )
})

test("Transformed schema schema with default fails when destruction failed", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.option(
        S.bool->S.transform(
          _ => {
            parser: bool => {
              switch bool {
              | true => "true"
              | false => ""
              }
            },
          },
        ),
      )->S.Option.getOr("true"),
    )
  )

  t->U.assertThrowsMessage(
    () => schema->S.toJSONSchema,
    `Failed converting: The S.transform serializer is missing`,
  )
})

test("Transformed schema schema uses default with correct type", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.option(
        S.bool->S.transform(
          _ => {
            parser: bool => {
              switch bool {
              | true => "true"
              | false => ""
              }
            },
            serializer: string => {
              switch string {
              | "true" => true
              | _ => false
              }
            },
          },
        ),
      )->S.Option.getOrWith(() => "true"),
    )
  )

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"default": true, "type": "boolean"}},
      "additionalProperties": true,
    }`),
  )
})

test("Primitive schema schema with additional raw schema", t => {
  let schema = S.bool->S.meta({description: "foo"})

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "type": "boolean",
      "description": "foo",
    }`),
  )
})

test("Primitive schema with an example", t => {
  let schema = S.bool->S.meta({examples: [true]})

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "type": "boolean",
      "examples": [true],
    }`),
  )
})

test("Transformed schema with an example", t => {
  let schema = S.null(S.bool)->S.meta({examples: [%raw(`null`)]})

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "anyOf": [{"type": "boolean"}, {"type": "null"}],
      "examples": [null],
    }`),
  )
})

test("Multiple examples", t => {
  let schema = S.string->S.meta({examples: ["Hi", "It's me"]})

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "type": "string",
      "examples": ["Hi", "It's me"],
    }`),
  )
})

test("Multiple additional raw schemas are merged together", t => {
  let schema =
    S.bool
    ->S.extendJSONSchema({nullable: true})
    ->S.extendJSONSchema({deprecated: true})

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "type": "boolean",
      "deprecated": true,
      "nullable": true,
    }`),
  )
})

test("Additional raw schema works with optional fields", t => {
  let schema = S.object(s =>
    s.field("optionalField", S.option(S.string)->S.extendJSONSchema({nullable: true}))
  )

  t->Assert.deepEqual(
    schema->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "optionalField": {"nullable": true, "type": "string"},
      },
      "additionalProperties": true,
    }`),
  )
})

test("JSONSchema of unknown schema", t => {
  t->U.assertThrowsMessage(
    () => S.unknown->S.toJSONSchema,
    `Failed converting to JSON: unknown is not valid JSON`,
  )
})

test("JSON schema doesn't affect final schema", t => {
  let schema = S.json(~validate=false)

  t->Assert.deepEqual(schema->S.toJSONSchema, %raw(`{}`))
})

test("Fails to create schema for schemas with optional items", t => {
  t->U.assertThrowsMessage(
    () => S.array(S.option(S.string))->S.toJSONSchema,
    "Failed converting to JSON: (string | undefined)[] is not valid JSON",
  )
  t->U.assertThrowsMessage(
    () => S.union([S.option(S.string), S.null(S.string)])->S.toJSONSchema,
    "Failed converting to JSON: string | undefined | null is not valid JSON",
  )
  t->U.assertThrowsMessage(
    () => S.tuple1(S.option(S.string))->S.toJSONSchema,
    `Failed converting to JSON at ["0"]: [string | undefined] is not valid JSON`,
  )
  t->U.assertThrowsMessage(
    () => S.tuple1(S.array(S.option(S.string)))->S.toJSONSchema,
    `Failed converting to JSON at ["0"]: [(string | undefined)[]] is not valid JSON`,
  )
})

test("JSONSchema error of nested object has path", t => {
  t->U.assertThrowsMessage(
    () => S.object(s => s.nested("nested").field("field", S.bigint))->S.toJSONSchema,
    `Failed converting to JSON at ["nested"]["field"]: { field: bigint; } is not valid JSON`,
  )
})

module Example = {
  type rating =
    | @as("G") GeneralAudiences
    | @as("PG") ParentalGuidanceSuggested
    | @as("PG13") ParentalStronglyCautioned
    | @as("R") Restricted
  type film = {
    id: float,
    title: string,
    tags: array<string>,
    rating: rating,
    deprecatedAgeRestriction: option<int>,
  }

  test("Example", t => {
    let filmSchema = S.object(s => {
      id: s.field("Id", S.float),
      title: s.field("Title", S.string),
      tags: s.fieldOr("Tags", S.array(S.string), []),
      rating: s.field(
        "Rating",
        S.union([
          S.literal(GeneralAudiences),
          S.literal(ParentalGuidanceSuggested),
          S.literal(ParentalStronglyCautioned),
          S.literal(Restricted),
        ]),
      ),
      deprecatedAgeRestriction: s.field(
        "Age",
        S.option(S.int)->S.meta({description: "Use rating instead", deprecated: true}),
      ),
    })

    t->Assert.deepEqual(
      filmSchema->S.toJSONSchema,
      %raw(`{
        type: "object",
        properties: {
          Id: { type: "number" },
          Title: { type: "string" },
          Tags: { items: { type: "string" }, type: "array", default: [] },
          Rating: {
            enum: ["G", "PG", "PG13", "R"],
          },
          Age: {
            type: "integer",
            deprecated: true,
            description: "Use rating instead",
          },
        },
        additionalProperties: true,
        required: ["Id", "Title", "Rating"],
      }`),
    )
  })
}
