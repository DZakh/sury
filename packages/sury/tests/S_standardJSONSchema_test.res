open Ava

// Minimal bindings to the Standard JSON Schema converter exposed on `~standard`.
// See https://standardschema.dev/json-schema
type standardJSONSchemaOptions = {target: string}
type standardJSONSchemaConverter = {
  input: standardJSONSchemaOptions => JSONSchema.t,
  output: standardJSONSchemaOptions => JSONSchema.t,
}
@get_index
external standardOf: (S.t<'a>, string) => {"jsonSchema": standardJSONSchemaConverter} = ""
let jsonSchemaConverter = schema => (schema->standardOf("~standard"))["jsonSchema"]

test("Standard ~standard.jsonSchema.input equals S.toJSONSchema for a plain schema", t => {
  let schema = S.string
  let converter = schema->jsonSchemaConverter
  t->Assert.deepEqual(converter.input({target: "draft-07"}), S.toJSONSchema(schema))
})

test("Standard ~standard.jsonSchema.input returns the input-type JSON Schema", t => {
  let schema = S.string->S.to(S.int)
  let converter = schema->jsonSchemaConverter
  t->Assert.deepEqual(converter.input({target: "draft-07"}), %raw(`{"type": "string"}`))
})

test("Standard ~standard.jsonSchema.output returns the output-type JSON Schema", t => {
  let schema = S.string->S.to(S.int)
  let converter = schema->jsonSchemaConverter
  // The output-type schema is the JSON Schema of the reversed (output) type
  // and equals S.toJSONSchema(S.reverse(schema)).
  t->Assert.deepEqual(
    converter.output({target: "draft-07"}),
    %raw(`{"type": "integer", "minimum": -2147483648, "maximum": 2147483647}`),
  )
  t->Assert.deepEqual(converter.output({target: "draft-07"}), S.toJSONSchema(schema->S.reverse))
})

test("Standard ~standard.jsonSchema input and output differ for a transforming schema", t => {
  let schema = S.string->S.to(S.int)
  let converter = schema->jsonSchemaConverter
  t->Assert.notDeepEqual(
    converter.input({target: "draft-07"}),
    converter.output({target: "draft-07"}),
  )
})
