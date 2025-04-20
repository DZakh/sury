open Ava

test("JSONSchema of bool schema", t => {
  t->Assert.deepEqual(S.bool->S.toJSONSchema, %raw(`{"type": "boolean"}`), ())
})

test("JSONSchema of string schema", t => {
  t->Assert.deepEqual(S.string->S.toJSONSchema, %raw(`{"type": "string"}`), ())
})

test("JSONSchema of int schema", t => {
  t->Assert.deepEqual(S.int->S.toJSONSchema, %raw(`{"type": "integer"}`), ())
})

test("JSONSchema of float schema", t => {
  t->Assert.deepEqual(S.float->S.toJSONSchema, %raw(`{"type": "number"}`), ())
})

test("JSONSchema of email schema", t => {
  t->Assert.deepEqual(
    S.string->S.email->S.toJSONSchema,
    %raw(`{"type": "string", "format": "email"}`),
    (),
  )
})

test("JSONSchema of url schema", t => {
  t->Assert.deepEqual(
    S.string->S.url->S.toJSONSchema,
    %raw(`{"type": "string", "format": "uri"}`),
    ~message="The format should be uri for url schema",
    (),
  )
})

test("JSONSchema of datetime schema", t => {
  t->Assert.deepEqual(
    S.string->S.datetime->S.toJSONSchema,
    %raw(`{"type": "string", "format": "date-time"}`),
    (),
  )
})

test("JSONSchema of cuid schema", t => {
  t->Assert.deepEqual(S.string->S.cuid->S.toJSONSchema, %raw(`{"type": "string"}`), ())
})

test("JSONSchema of uuid schema", t => {
  t->Assert.deepEqual(
    S.string->S.uuid->S.toJSONSchema,
    %raw(`{"type": "string", "format": "uuid"}`),
    (),
  )
})

test("JSONSchema of pattern schema", t => {
  t->Assert.deepEqual(
    S.string->S.pattern(%re("/abc/g"))->S.toJSONSchema,
    %raw(`{"type": "string","pattern": "/abc/g"}`),
    (),
  )
})

test("JSONSchema of string schema uses the last refinement for format", t => {
  t->Assert.deepEqual(
    S.string->S.email->S.datetime->S.toJSONSchema,
    %raw(`{"type": "string", "format": "date-time"}`),
    (),
  )
})

test("JSONSchema of string with min", t => {
  t->Assert.deepEqual(
    S.string->S.stringMinLength(1)->S.toJSONSchema,
    %raw(`{"type": "string", "minLength": 1}`),
    (),
  )
})

test("JSONSchema of string with max", t => {
  t->Assert.deepEqual(
    S.string->S.stringMaxLength(1)->S.toJSONSchema,
    %raw(`{"type": "string", "maxLength": 1}`),
    (),
  )
})

test("JSONSchema of string with length", t => {
  t->Assert.deepEqual(
    S.string->S.stringLength(1)->S.toJSONSchema,
    %raw(`{"type": "string", "minLength": 1, "maxLength": 1}`),
    (),
  )
})

test("JSONSchema of string with both min and max", t => {
  t->Assert.deepEqual(
    S.string->S.stringMinLength(1)->S.stringMaxLength(4)->S.toJSONSchema,
    %raw(`{"type": "string", "minLength": 1, "maxLength": 4}`),
    (),
  )
})

test("JSONSchema of int with min", t => {
  t->Assert.deepEqual(
    S.int->S.intMin(1)->S.toJSONSchema,
    %raw(`{"type": "integer", "minimum": 1}`),
    (),
  )
})

test("JSONSchema of int with max", t => {
  t->Assert.deepEqual(
    S.int->S.intMax(1)->S.toJSONSchema,
    %raw(`{"type": "integer", "maximum": 1}`),
    (),
  )
})

test("JSONSchema of port", t => {
  // FIXME: Use minimum and maximum
  t->Assert.deepEqual(S.int->S.port->S.toJSONSchema, %raw(`{"type": "integer"}`), ())
})

test("JSONSchema of float with min", t => {
  t->Assert.deepEqual(
    S.float->S.floatMin(1.)->S.toJSONSchema,
    %raw(`{"type": "number", "minimum": 1}`),
    (),
  )
})

test("JSONSchema of float with max", t => {
  t->Assert.deepEqual(
    S.float->S.floatMax(1.)->S.toJSONSchema,
    %raw(`{"type": "number", "maximum": 1}`),
    (),
  )
})

test("JSONSchema of nullable float", t => {
  t->Assert.deepEqual(
    S.null(S.float)->S.toJSONSchema,
    %raw(`{"anyOf": [{"type": "number"}, {"type": "null"}]}`),
    (),
  )
})

test("JSONSchema of never", t => {
  t->Assert.deepEqual(S.never->S.toJSONSchema, %raw(`{"not": {}}`), ())
})

test("JSONSchema of true", t => {
  t->Assert.deepEqual(
    S.literal(true)->S.toJSONSchema,
    %raw(`{"type": "boolean", "const": true}`),
    (),
  )
})

test("JSONSchema of false", t => {
  t->Assert.deepEqual(
    S.literal(false)->S.toJSONSchema,
    %raw(`{"type": "boolean", "const": false}`),
    (),
  )
})

test("JSONSchema of string literal", t => {
  t->Assert.deepEqual(
    S.literal("Hello World!")->S.toJSONSchema,
    %raw(`{"type": "string", "const": "Hello World!"}`),
    (),
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
    (),
  )
})

test("JSONSchema of number literal", t => {
  t->Assert.deepEqual(S.literal(123)->S.toJSONSchema, %raw(`{"type": "number", "const": 123}`), ())
})

test("JSONSchema of null", t => {
  t->Assert.deepEqual(S.literal(%raw(`null`))->S.toJSONSchema, %raw(`{"type": "null"}`), ())
})

test("JSONSchema of undefined", t => {
  t->U.assertThrowsMessage(
    () => S.literal(%raw(`undefined`))->S.toJSONSchema,
    `Failed converting to JSON: The 'undefined' schema cannot be converted to JSON`,
  )
})

test("JSONSchema of NaN", t => {
  t->U.assertThrowsMessage(
    () => S.literal(%raw(`NaN`))->S.toJSONSchema,
    `Failed converting to JSON: The 'NaN' schema cannot be converted to JSON`,
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
    (),
  )
})

test("JSONSchema of enum", t => {
  t->Assert.deepEqual(
    S.enum(["Yes", "No"])->S.toJSONSchema,
    %raw(`{
      "enum": ["Yes", "No"],
    }`),
    (),
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
    (),
  )
})

test("JSONSchema of string array", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
    }`),
    (),
  )
})

test("JSONSchema of array with min length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.arrayMinLength(1)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "minItems": 1
    }`),
    (),
  )
})

test("JSONSchema of array with max length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.arrayMaxLength(1)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "maxItems": 1
    }`),
    (),
  )
})

test("JSONSchema of array with fixed length", t => {
  t->Assert.deepEqual(
    S.array(S.string)->S.arrayLength(1)->S.toJSONSchema,
    %raw(`{
      "type": "array",
      "items": {"type": "string"},
      "minItems": 1,
      "maxItems": 1
    }`),
    (),
  )
})

test("JSONSchema of string dict", t => {
  t->Assert.deepEqual(
    S.dict(S.string)->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "additionalProperties": {"type": "string"},
    }`),
    (),
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
    (),
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
    (),
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
    (),
  )
})

test("JSONSchema of object with deprecated field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.string->S.deprecated("Use another field")))->S.toJSONSchema,
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
    (),
  )
})

test("Deprecated message overrides existing description", t => {
  t->Assert.deepEqual(
    S.string
    ->S.description("Previous description")
    ->S.deprecated("Use another field")
    ->S.toJSONSchema,
    %raw(`{
      "type": "string",
      "deprecated": true,
      "description": "Use another field"
    }`),
    (),
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
    (),
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
    (),
  )
})

test("JSONSchema of optional root schema", t => {
  t->U.assertThrowsMessage(
    () => S.option(S.string)->S.toJSONSchema,
    "Failed converting to JSON: The \'string | undefined\' schema cannot be converted to JSON",
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
    (),
  )
})

// FIXME: Investigate why this test fails
Failing.test("JSONSchema of reversed object with S.option(S.option(_)) field", t => {
  t->Assert.deepEqual(
    S.object(s => s.field("field", S.option(S.option(S.string))))->S.reverse->S.toJSONSchema,
    %raw(`{
      "type": "object",
      "properties": {
        "field": {
          "type": "string",
        },
      },
      "additionalProperties": true,
    }`),
    (),
  )
})

// test("Transformed schema schema with default fails when destruction failed", t => {
//   let schema = S.object(s =>
//     s.field(
//       "field",
//       S.option(
//         S.bool->S.transform(
//           _ => {
//             parser: bool => {
//               switch bool {
//               | true => "true"
//               | false => ""
//               }
//             },
//           },
//         ),
//       )->S.Option.getOr("true"),
//     )
//   )

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Error(`[ReScript JSON Schema] Failed converting at ["field"]. Reason: Couldn't destruct default value. Error: Failed reverse converting to JSON at root. Reason: The S.transform serializer is missing`),
//   )
// })

// test("Transformed schema schema uses default with correct type", t => {
//   let schema = S.object(s =>
//     s.field(
//       "field",
//       S.option(
//         S.bool->S.transform(
//           _ => {
//             parser: bool => {
//               switch bool {
//               | true => "true"
//               | false => ""
//               }
//             },
//             serializer: string => {
//               switch string {
//               | "true" => true
//               | _ => false
//               }
//             },
//           },
//         ),
//       )->S.Option.getOrWith(() => "true"),
//     )
//   )

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "additionalProperties": true,
//         "properties": {"field": {"default": true, "type": "boolean"}},
//         "type": "object",
//       }`),
//     ),
//   )
// })

// test("Primitive schema schema with additional raw schema", t => {
//   let schema = S.bool->JSONSchema.extend({description: "foo"})

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "type": "boolean",
//         "description": "foo",
//       }`),
//     ),
//   )
// })

// test("Primitive schema with an example", t => {
//   let schema = S.bool->JSONSchema.example(true)

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "type": "boolean",
//         "examples": [true],
//       }`),
//     ),
//   )
// })

// test("Transformed schema with an example", t => {
//   let schema = S.null(S.bool)->JSONSchema.example(None)

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "anyOf": [{"type": "boolean"}, {"type": "null"}],
//         "examples": [null],
//       }`),
//     ),
//   )
// })

// test("Multiple examples", t => {
//   let schema = S.string->JSONSchema.example("Hi")->JSONSchema.example("It's me")

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "type": "string",
//         "examples": ["Hi", "It's me"],
//       }`),
//     ),
//   )
// })

// test("Multiple additional raw schemas are merged together", t => {
//   let schema =
//     S.bool
//     ->JSONSchema.extend({"nullable": true}->Obj.magic)
//     ->JSONSchema.extend({"deprecated": true}->Obj.magic)

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "type": "boolean",
//         "deprecated": true,
//         "nullable": true,
//       }`),
//     ),
//   )
// })

// test("Additional raw schema works with optional fields", t => {
//   let schema = S.object(s =>
//     s.field("optionalField", S.option(S.string)->JSONSchema.extend({"nullable": true}->Obj.magic))
//   )

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//         "type": "object",
//         "properties": {
//           "optionalField": {"nullable": true, "type": "string"},
//         },
//         "additionalProperties": true,
//       }`),
//     ),
//   )
// })

// test("Unknown schema doesn't affect final schema", t => {
//   let schema = S.unknown

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//       }`),
//     ),
//   )
// })

// test("JSON schema doesn't affect final schema", t => {
//   let schema = S.json(~validate=false)

//   t->Assert.deepEqual(
//     JSONSchema.make(schema),
//     Ok(
//       %raw(`{
//         "$schema": "http://json-schema.org/draft-07/schema#",
//       }`),
//     ),
//   )
// })

// test("Fails to create schema for schemas with optional items", t => {
//   t->Assert.deepEqual(
//     JSONSchema.make(S.dict(S.option(S.string))),
//     Error(
//       "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as dict<string | undefined> item",
//     ),
//   )
//   t->Assert.deepEqual(
//     JSONSchema.make(S.array(S.option(S.string))),
//     Error(
//       "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as array<string | undefined> item",
//     ),
//   )
//   t->Assert.deepEqual(
//     JSONSchema.make(S.union([S.option(S.string), S.null(S.string)])),
//     Error(
//       "[ReScript JSON Schema] Failed converting at root. Reason: Optional schema is not supported as string | undefined | string | null item",
//     ),
//   )
//   t->Assert.deepEqual(
//     JSONSchema.make(S.tuple1(S.option(S.string))),
//     Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional schema is not supported as [string | undefined] item`),
//   )
//   t->Assert.deepEqual(
//     JSONSchema.make(S.tuple1(S.array(S.option(S.string)))),
//     Error(`[ReScript JSON Schema] Failed converting at ["0"]. Reason: Optional schema is not supported as array<string | undefined> item`),
//   )
// })

// module Example = {
//   type rating =
//     | @as("G") GeneralAudiences
//     | @as("PG") ParentalGuidanceSuggested
//     | @as("PG13") ParentalStronglyCautioned
//     | @as("R") Restricted
//   type film = {
//     id: float,
//     title: string,
//     tags: array<string>,
//     rating: rating,
//     deprecatedAgeRestriction: option<int>,
//   }

//   test("Example", t => {
//     let filmSchema = S.object(s => {
//       id: s.field("Id", S.float),
//       title: s.field("Title", S.string),
//       tags: s.fieldOr("Tags", S.array(S.string), []),
//       rating: s.field(
//         "Rating",
//         S.union([
//           S.literal(GeneralAudiences),
//           S.literal(ParentalGuidanceSuggested),
//           S.literal(ParentalStronglyCautioned),
//           S.literal(Restricted),
//         ]),
//       ),
//       deprecatedAgeRestriction: s.field("Age", S.option(S.int)->S.deprecate("Use rating instead")),
//     })

//     t->Assert.deepEqual(
//       JSONSchema.make(filmSchema),
//       Ok(
//         %raw(`{
//           $schema: "http://json-schema.org/draft-07/schema#",
//           type: "object",
//           properties: {
//             Id: { type: "number" },
//             Title: { type: "string" },
//             Tags: { items: { type: "string" }, type: "array", default: [] },
//             Rating: {
//               enum: ["G", "PG", "PG13", "R"],
//             },
//             Age: {
//               type: "integer",
//               deprecated: true,
//               description: "Use rating instead",
//             },
//           },
//           additionalProperties: true,
//           required: ["Id", "Title", "Rating"],
//         }`),
//       ),
//     )
//   })
// }
