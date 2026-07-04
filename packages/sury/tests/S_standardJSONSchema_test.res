open Vitest

// Bindings to the Standard JSON Schema interface exposed on `~standard`.
// See https://standardschema.dev/json-schema
// These tests mirror @valibot/to-json-schema's `toStandardJsonSchema` test
// suite to ensure Sury behaves the same way (shape, target validation and
// `$schema` stamping for both `input` and `output`).
@get_index
external standardOf: (S.t<'a>, string) => StandardSchema.props<unknown, unknown> = ""
let standardOf = schema => schema->standardOf("~standard")
let jsonSchemaConverter = schema => (schema->standardOf).jsonSchema

test("Standard ~standard exposes version, vendor, validate and a jsonSchema converter", t => {
  let standard = S.string->standardOf
  t->Assert.deepEqual(standard.version, 1)
  t->Assert.deepEqual(standard.vendor, "sury")
  t->Assert.deepEqual(standard.validate->Type.typeof, #function)
  t->Assert.deepEqual(standard.jsonSchema.input->Type.typeof, #function)
  t->Assert.deepEqual(standard.jsonSchema.output->Type.typeof, #function)
})

test("Standard ~standard.jsonSchema throws for an unsupported target", t => {
  let converter = S.string->jsonSchemaConverter
  t->Assert.throws(
    () => converter.input({target: "unsupported-target"}),
    ~expectations={message: "Unsupported JSON Schema target: unsupported-target"},
  )
  t->Assert.throws(
    () => converter.output({target: "unsupported-target"}),
    ~expectations={message: "Unsupported JSON Schema target: unsupported-target"},
  )
})

test("Standard ~standard.jsonSchema.input returns the input-type JSON Schema with $schema", t => {
  let schema = S.string->S.to(S.int)
  let converter = schema->jsonSchemaConverter
  t->Assert.deepEqual(
    converter.input({target: "draft-07"}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`),
  )
})

test("Standard ~standard.jsonSchema.output returns the output-type JSON Schema with $schema", t => {
  let schema = S.string->S.to(S.int)
  let converter = schema->jsonSchemaConverter
  // The output-type schema is the JSON Schema of the reversed (output) type.
  t->Assert.deepEqual(
    converter.output({target: "draft-07"}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "integer",
      "minimum": -2147483648,
      "maximum": 2147483647
    }`),
  )
})

test("Standard ~standard.jsonSchema stamps the draft-2020-12 $schema URI", t => {
  let converter = S.string->jsonSchemaConverter
  t->Assert.deepEqual(
    converter.input({target: "draft-2020-12"}),
    %raw(`{"$schema": "https://json-schema.org/draft/2020-12/schema", "type": "string"}`),
  )
})

test("Standard ~standard.jsonSchema omits $schema for the openapi-3.0 target", t => {
  let converter = S.string->jsonSchemaConverter
  t->Assert.deepEqual(converter.input({target: "openapi-3.0"}), %raw(`{"type": "string"}`))
})

test("Standard ~standard.jsonSchema input and output differ for a transforming schema", t => {
  let schema = S.string->S.to(S.int)
  let converter = schema->jsonSchemaConverter
  t->Assert.notDeepEqual(
    converter.input({target: "draft-07"}),
    converter.output({target: "draft-07"}),
  )
})
