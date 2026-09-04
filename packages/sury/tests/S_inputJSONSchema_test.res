open Vitest

test("JSONSchema of bool schema", t => {
  t->Assert.deepEqual(S.bool->S.inputJSONSchema, %raw(`{"type": "boolean"}`))
})

test("JSONSchema of string schema", t => {
  t->Assert.deepEqual(S.string->S.inputJSONSchema, %raw(`{"type": "string"}`))
})

test("JSONSchema of int schema", t => {
  t->Assert.deepEqual(
    S.int->S.inputJSONSchema,
    %raw(`{"type": "integer", "minimum": -2147483648, "maximum": 2147483647}`),
  )
})

test("JSONSchema of float schema", t => {
  t->Assert.deepEqual(S.float->S.inputJSONSchema, %raw(`{"type": "number"}`))
})

test("JSONSchema of S.json transformed to object with bigint and array of optional items", t => {
  let nonJsonableSchema = S.schema(s =>
    {
      "id": s.matches(S.bigint),
      "data": s.matches(S.unknown),
      "items": s.matches(S.array(S.option(S.float->S.lte(1.)))),
    }
  )
  // Was `{}` while an array of optional items had no JSON form at all. Now that
  // None decodes to null the whole shape is describable: bigint by its string
  // form, unknown by the empty schema, the items as number-or-null.
  // FIXME: the item's `maximum: 1` is dropped — a variant converted through
  // `.to(json)` reports the target's type without the source's refinements.
  // See specs/codec-json-array-optional-bounded.yaml.
  t->Assert.deepEqual(
    S.json->S.to(nonJsonableSchema)->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "id": {"type": "string"},
        "data": {},
        "items": {"type": "array", "items": {"anyOf": [{"type": "number"}, {"type": "null"}]}}
      },
      "additionalProperties": false,
      "required": ["id", "data", "items"]
    }`),
  )
})

test("JSONSchema of email schema", t => {
  t->Assert.deepEqual(S.email->S.inputJSONSchema, %raw(`{"type": "string", "format": "email"}`))
})

test("JSONSchema of uri schema", t => {
  t->Assert.deepEqual(
    S.uri->S.inputJSONSchema,
    %raw(`{"type": "string", "format": "uri"}`),
    ~message="The format should be uri for uri schema",
  )
})

test("JSONSchema of S.string->S.to(S.url)", t => {
  t->Assert.deepEqual(
    S.string->S.to(S.url)->S.inputJSONSchema,
    %raw(`{"type": "string", "format": "uri"}`),
    ~message="A URL instance describes itself as a uri string",
  )
})

test("JSONSchema of S.string->S.to(S.date)", t => {
  t->Assert.deepEqual(
    S.string->S.to(S.date)->S.inputJSONSchema,
    %raw(`{"type": "string", "format": "date-time"}`),
  )
})

test("JSONSchema of S.string->S.to(S.date) with description", t => {
  t->Assert.deepEqual(
    S.string->S.to(S.date)->S.meta({description: "A date"})->S.inputJSONSchema,
    %raw(`{"type": "string", "format": "date-time", "description": "A date"}`),
  )
})

test("JSONSchema of S.string with description converted to S.date", t => {
  t->Assert.deepEqual(
    S.string->S.meta({description: "A date"})->S.to(S.date)->S.inputJSONSchema,
    %raw(`{"type": "string", "format": "date-time", "description": "A date"}`),
  )
})

test("JSONSchema of S.isoDateTime", t => {
  t->Assert.deepEqual(
    S.isoDateTime->S.inputJSONSchema,
    %raw(`{"type": "string", "format": "date-time"}`),
  )
})

test("JSONSchema of object with transformed field preserves field metadata", t => {
  t->Assert.deepEqual(
    S.object(s =>
      s.field("birthDate", S.string->S.meta({description: "Birth date"})->S.to(S.date))
    )->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "birthDate": {"type": "string", "format": "date-time", "description": "Birth date"}
      },
      "required": ["birthDate"]
    }`),
  )
})

// A format outside the JSON Schema vocabulary publishes its own regex as
// `pattern`, so what round-trips is the behavior rather than the name.
test("JSONSchema of cuid schema", t => {
  t->Assert.deepEqual(
    S.cuid->S.inputJSONSchema,
    %raw(`{"type": "string", "pattern": "^[cC][0-9a-z]{6,}$"}`),
  )
})

test("JSONSchema of uuid schema", t => {
  t->Assert.deepEqual(S.uuid->S.inputJSONSchema, %raw(`{"type": "string", "format": "uuid"}`))
})

// A version-pinned UUID narrows a format that does exist, so it keeps the name
// and lets the pattern carry the version.
test("JSONSchema of uuidv7 schema", t => {
  t->Assert.deepEqual(
    S.uuidv7->S.inputJSONSchema,
    %raw(`{
      "type": "string",
      "format": "uuid",
      "pattern": "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-7[0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$"
    }`),
  )
})

// `cidrv6` reuses the case-insensitive `ipv6` grammar, and a JSON Schema
// pattern carries no flags — so it is the one format with neither spelling.
test("JSONSchema of cidrv6 schema", t => {
  t->Assert.deepEqual(S.cidrv6->S.inputJSONSchema, %raw(`{"type": "string"}`))
})

test("JSONSchema of pattern schema", t => {
  t->Assert.deepEqual(
    S.string->S.pattern(/abc/g)->S.inputJSONSchema,
    %raw(`{"type": "string","pattern": "abc"}`),
  )
})

test("JSONSchema of string with min", t => {
  t->Assert.deepEqual(
    S.string->S.minLength(1)->S.inputJSONSchema,
    %raw(`{"type": "string", "minLength": 1}`),
  )
})

test("JSONSchema of string with max", t => {
  t->Assert.deepEqual(
    S.string->S.maxLength(1)->S.inputJSONSchema,
    %raw(`{"type": "string", "maxLength": 1}`),
  )
})

test("JSONSchema of string with length", t => {
  t->Assert.deepEqual(
    S.string->S.length(1)->S.inputJSONSchema,
    %raw(`{"type": "string", "minLength": 1, "maxLength": 1}`),
  )
})

test("JSONSchema of string with both min and max", t => {
  t->Assert.deepEqual(
    S.string->S.minLength(1)->S.maxLength(4)->S.inputJSONSchema,
    %raw(`{"type": "string", "minLength": 1, "maxLength": 4}`),
  )
})

test("JSONSchema of int with min", t => {
  t->Assert.deepEqual(
    S.int->S.gte(1)->S.inputJSONSchema,
    %raw(`{"type": "integer", "minimum": 1, "maximum": 2147483647}`),
  )
})

test("JSONSchema of int with max", t => {
  t->Assert.deepEqual(
    S.int->S.lte(1)->S.inputJSONSchema,
    %raw(`{"type": "integer", "minimum": -2147483648, "maximum": 1}`),
  )
})

test("JSONSchema of port", t => {
  t->Assert.deepEqual(
    S.port->S.inputJSONSchema,
    %raw(`{
      "type": "integer",
      "minimum": 0,
      "maximum": 65535,
    }`),
  )
})

test("JSONSchema of float with min", t => {
  t->Assert.deepEqual(S.float->S.gte(1.)->S.inputJSONSchema, %raw(`{"type": "number", "minimum": 1}`))
})

test("JSONSchema of float with max", t => {
  t->Assert.deepEqual(S.float->S.lte(1.)->S.inputJSONSchema, %raw(`{"type": "number", "maximum": 1}`))
})

test("JSONSchema of nullable float", t => {
  t->Assert.deepEqual(
    S.nullAsOption(S.float)->S.inputJSONSchema,
    %raw(`{"anyOf": [{"type": "number"}, {"type": "null"}]}`),
  )
})

test("JSONSchema of never", t => {
  t->Assert.deepEqual(S.never->S.inputJSONSchema, %raw(`{"not": {}}`))
})

test("JSONSchema of true", t => {
  t->Assert.deepEqual(S.literal(true)->S.inputJSONSchema, %raw(`{"type": "boolean", "const": true}`))
})

test("JSONSchema of false", t => {
  t->Assert.deepEqual(S.literal(false)->S.inputJSONSchema, %raw(`{"type": "boolean", "const": false}`))
})

test("JSONSchema of string literal", t => {
  t->Assert.deepEqual(
    S.literal("Hello World!")->S.inputJSONSchema,
    %raw(`{"type": "string", "const": "Hello World!"}`),
  )
})

test("JSONSchema of object literal", t => {
  t->Assert.deepEqual(
    S.literal({"received": true})->S.inputJSONSchema,
    %raw(`{
        "type": "object",
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
  t->Assert.deepEqual(S.literal(123)->S.inputJSONSchema, %raw(`{"type": "number", "const": 123}`))
})

test("JSONSchema of null", t => {
  t->Assert.deepEqual(S.literal(%raw(`null`))->S.inputJSONSchema, %raw(`{"type": "null"}`))
})

test("JSONSchema of undefined", t => {
  t->U.assertThrowsMessage(
    () => S.literal(%raw(`undefined`))->S.inputJSONSchema,
    `Expected JSON, received undefined`,
  )
})

test("JSONSchema of NaN", t => {
  t->U.assertThrowsMessage(
    () => S.literal(%raw(`NaN`))->S.inputJSONSchema,
    `Expected JSON, received NaN`,
  )
})

// A schema with no JSON Schema equivalent fails the conversion itself — nothing
// was parsed, so there is no input to report and no schema a value failed
// against. That is `InvalidOperation`, where the same message from `S.json`
// rejecting a *value* stays `InvalidInput`.
test("JSONSchema of a non-JSON schema is an InvalidOperation, not an InvalidInput", t => {
  t->Assert.deepEqual(
    switch S.object(s => s.field("a", S.bigint))->S.inputJSONSchema {
    | _ => None
    | exception S.Exn(error) =>
      switch error->S.Error.classify {
      | InvalidOperation({path, reason}) => Some((path, reason))
      | _ => None
      }
    },
    Some((S.Path.fromArray(["a"]), `Expected JSON, received bigint`)),
  )

  // The same sentence from `S.json` rejecting a value keeps `InvalidInput` —
  // there a value really did fail a schema.
  t->Assert.deepEqual(
    switch %raw(`1n`)->S.parseOrThrow(~to=S.json) {
    | _ => false
    | exception S.Exn(error) =>
      switch error->S.Error.classify {
      | InvalidInput(_) => true
      | _ => false
      }
    },
    true,
  )
})

test("JSONSchema of tuple", t => {
  t->Assert.deepEqual(
    S.tuple2(S.string, S.bool)->S.inputJSONSchema,
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
    )->S.inputJSONSchema,
    %raw(`{
      "type": "object",
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
    S.enum(["Yes", "No"])->S.inputJSONSchema,
    %raw(`{
      "enum": ["Yes", "No"],
    }`),
  )
})

test("JSONSchema of union", t => {
  t->Assert.deepEqual(
    S.union([S.literal("Yes"), S.string])->S.inputJSONSchema,
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

test("JSONSchema of union narrowed by .to: union([string, bigint])->to(string)", t => {
  // string matches the target and bigint doesn't, so the conversion itself is
  // rejected — S.inputJSONSchema falls back to describing the union's own input.
  let schema = S.union([S.string->S.castToUnknown, S.bigint->S.castToUnknown])->S.to(S.string)
  t->U.assertThrowsMessage(
    () => schema->S.inputJSONSchema->ignore,
    `Expected JSON, received string | bigint`,
  )

  // Spelled out per member, the bigint arm converts and the JSON Schema narrows.
  let explicit =
    S.union([S.string->S.castToUnknown, S.bigint->S.to(S.string)->S.castToUnknown])->S.to(S.string)
  t->Assert.deepEqual(explicit->S.inputJSONSchema, %raw(`{"type": "string"}`))
})

test("JSONSchema of string array", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.inputJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
    }`),
  )
})

test("JSONSchema of array with min length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.minLength(1)->S.inputJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "minItems": 1
    }`),
  )
})

test("JSONSchema of array with max length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.maxLength(1)->S.inputJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "maxItems": 1
    }`),
  )
})

test("JSONSchema of array with fixed length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.length(1)->S.inputJSONSchema,
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
    S.dict(S.string)->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "additionalProperties": {"type": "string"},
    }`),
  )
})

test("JSONSchema of dict with optional fields", t => {
  t->Assert.deepEqual(
    S.dict(S.option(S.string))->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "additionalProperties": {"type": "string"},
    }`),
  )
})

test("JSONSchema of dict with optional invalid field", t => {
  t->U.assertThrowsMessage(
    () => S.dict(S.option(S.bigint))->S.inputJSONSchema,
    `Failed at []: Expected JSON, received bigint | undefined`,
  )
})

test("JSONSchema of object with single string field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.string))->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"type": "string"}},
      "required": ["field"],
    }`),
  )
})

test("JSONSchema of object with strict mode", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.string))->S.strict->S.inputJSONSchema,
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
    S.object(s => s.field("field", S.option(S.string)))->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"type": "string"}},
    }`),
  )
})

test("JSONSchema of object with deprecated field", t => {
  t->Assert.deepEqual(
    S.object(s =>
      s.field("field", S.string->S.meta({description: "Use another field", deprecated: true}))
    )->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {
        "type": "string",
        "deprecated": true,
        "description": "Use another field"
      }},
      "required": ["field"],
    }`),
  )
})

test("JSONSchema with title", t => {
  t->Assert.deepEqual(
    S.string->S.meta({title: "My field"})->S.inputJSONSchema,
    %raw(`{"title": "My field", "type": "string"}`),
  )
})

test("Deprecated message overrides existing description", t => {
  t->Assert.deepEqual(
    S.string
    ->S.meta({description: "Previous description"})
    ->S.meta({description: "Use another field", deprecated: true})
    ->S.inputJSONSchema,
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
    )->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "objectWithOneStringField": {
          "type": "object",
          "properties": {"Field": {"type": "string"}},
          "required": ["Field"],
        },
      },
      "required": ["objectWithOneStringField"],
    }`),
  )
})

test("JSONSchema of object with one optional and one normal field", t => {
  t->Assert.deepEqual(
    S.object(s => (
      s.field("field", S.string),
      s.field("optionalField", S.option(S.string)),
    ))->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "field": {
          "type": "string",
        },
        "optionalField": {"type": "string"},
      },
      "required": ["field"],
    }`),
  )
})

test("JSONSchema of optional root schema", t => {
  t->U.assertThrowsMessage(
    () => S.option(S.string)->S.inputJSONSchema,
    "Expected JSON, received string | undefined",
  )
})

test("JSONSchema of object with S.option(S.option(_)) field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.option(S.option(S.string))))->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "field": {
          "type": "string",
        },
      },
    }`),
  )
})

test("JSONSchema of reversed object with S.option(S.option(_)) field", t => {
  t->U.assertThrowsMessage(
    () => S.object(s => s.field("field", S.option(S.option(S.string))))->S.reverse->S.inputJSONSchema,
    `Expected JSON, received string | undefined | { BS_PRIVATE_NESTED_SOME_NONE: 0; }`,
  )
})

test(
  "Successfully creates JSON schema for default field which we can't serialize. Just omit it from JSON Schema",
  t => {
    let schema = S.object(s =>
      s.field(
        "field",
        S.option(
          S.bool->S.to(
            S.any,
            ~custom={
              decode: Sync(
                bool => {
                  switch bool {
                  | true => "true"
                  | false => ""
                  }
                },
              ),
              encode: Never,
            },
          ),
        )->S.Option.getOr("true"),
      )
    )

    t->Assert.deepEqual(
      schema->S.inputJSONSchema,
      %raw(`{
        "type": "object",
        "properties": {"field": {"type": "boolean"}}, // No 'default: true' here, but that's fine
      }`),
    )
  },
)

test("Transformed schema schema uses default with correct type", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.option(
        S.bool->S.to(
          S.any,
          ~custom={
            decode: Sync(
              bool => {
                switch bool {
                | true => "true"
                | false => ""
                }
              },
            ),
            encode: Sync(
              string => {
                switch string {
                | "true" => true
                | _ => false
                }
              },
            ),
          },
        ),
      )->S.Option.getOr("true"),
    )
  )

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {"field": {"default": true, "type": "boolean"}},
    }`),
  )
})

test("Currently Option.getOrWith is not reflected on JSON schema", t => {
  let schema = S.nullAsOption(S.bool)->S.Option.getOrWith(() => true)

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      "anyOf": [
        {"type": "boolean"},
        {"type": "null"}
      ],
    }`),
  )
})

test("Primitive schema schema with additional raw schema", t => {
  let schema = S.bool->S.meta({description: "foo"})

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      "type": "boolean",
      "description": "foo",
    }`),
  )
})

test("Primitive schema with an example", t => {
  let schema = S.bool->S.meta({examples: [true]})

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      "type": "boolean",
      "examples": [true],
    }`),
  )
})

test("Transformed schema with an example", t => {
  let schema = S.nullAsOption(S.bool)->S.meta({examples: [None]})

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      "anyOf": [{"type": "boolean"}, {"type": "null"}],
      "examples": [null],
    }`),
  )
})

test("Multiple examples", t => {
  let schema = S.string->S.meta({examples: ["Hi", "It's me"]})

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
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
    schema->S.inputJSONSchema,
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
    schema->S.inputJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "optionalField": {"nullable": true, "type": "string"},
      },
    }`),
  )
})

test("JSONSchema of unknown schema", t => {
  t->U.assertThrowsMessage(() => S.unknown->S.inputJSONSchema, `Expected JSON, received unknown`)
})

test("JSON schema doesn't affect final schema", t => {
  let schema = S.json
  t->Assert.deepEqual(schema->S.inputJSONSchema, %raw(`{}`))
})

test("JSONSchema of recursive schema", t => {
  let schema = S.recursive("Node", nodeSchema => {
    S.object(
      s =>
        {
          "id": s.field("Id", S.string),
          "children": s.field("Children", S.array(nodeSchema)),
        },
    )
  })

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      $defs: {
        Node: {
          properties: {
            Children: { items: { $ref: "#/$defs/Node" }, type: "array" },
            Id: { type: "string" },
          },
          required: ["Id", "Children"],
          type: "object",
        },
      },
      $ref: "#/$defs/Node",
    }`),
  )
})

test("JSONSchema of nested recursive schema", t => {
  let schema = S.schema(s =>
    {
      "node": s.matches(
        S.recursive(
          "Node",
          nodeSchema => {
            S.object(
              s =>
                {
                  "id": s.field("Id", S.string),
                  "children": s.field("Children", S.array(nodeSchema)),
                },
            )
          },
        ),
      ),
    }
  )

  t->Assert.deepEqual(
    schema->S.inputJSONSchema,
    %raw(`{
      type: 'object',
      properties: { node: { '$ref': '#/$defs/Node' } },
      required: [ 'node' ],
      '$defs': {
        Node: {
          type: 'object',
          properties: {
            Children: { items: { $ref: "#/$defs/Node" }, type: "array" },
            Id: { type: "string" },
          },
          required: [ 'Id', 'Children' ]
        }
      }
    }`),
  )
})

test("JSONSchema of recursive schema with non-jsonable field", t => {
  t->U.assertThrowsMessage(() => {
    let schema = S.recursive(
      "Node",
      nodeSchema => {
        S.object(
          s =>
            {
              "id": s.field("Id", S.bigint),
              "children": s.field("Children", S.array(nodeSchema)),
            },
        )
      },
    )
    schema->S.inputJSONSchema
  }, `Failed at Id: Expected JSON, received bigint`)
})

test("Fails to create schema for schemas with optional items", t => {
  t->U.assertThrowsMessage(
    () => S.array(S.option(S.string))->S.inputJSONSchema,
    "Failed at []: Expected JSON, received string | undefined",
  )
  t->U.assertThrowsMessage(
    () => S.union([S.option(S.string), S.nullAsOption(S.string)])->S.inputJSONSchema,
    "Expected JSON, received string | undefined | null",
  )
  t->U.assertThrowsMessage(
    () => S.tuple1(S.option(S.string))->S.inputJSONSchema,
    `Failed at [0]: Expected JSON, received string | undefined`,
  )
  t->U.assertThrowsMessage(
    () => S.tuple1(S.array(S.option(S.string)))->S.inputJSONSchema,
    `Failed at [0][]: Expected JSON, received string | undefined`,
  )
})

test("JSONSchema error of nested object has path", t => {
  t->U.assertThrowsMessage(
    () => S.object(s => s.nested("nested").field("field", S.bigint))->S.inputJSONSchema,
    `Failed at nested.field: Expected JSON, received bigint`,
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
      filmSchema->S.inputJSONSchema,
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
            minimum: -2147483648,
            maximum: 2147483647,
            deprecated: true,
            description: "Use rating instead",
          },
        },
        required: ["Id", "Title", "Rating"],
      }`),
    )
  })
}
