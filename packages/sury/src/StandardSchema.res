// Types for the Standard Schema spec (https://standardschema.dev) and its
// Standard JSON Schema extension (https://standardschema.dev/json-schema).

// The target JSON Schema dialect. Compiles to the spec target strings
// ("draft-07", "draft-2020-12", "openapi-3.0"), so it interops directly with the
// raw `target` string coming from the Standard JSON Schema `Options`.
type target = [#"draft-07" | #"draft-2020-12" | #"openapi-3.0"]

// Options passed to the `jsonSchema` converter's `input`/`output` methods.
type jsonSchemaOptions = {
  target: string,
  libraryOptions?: dict<unknown>,
}

// The `jsonSchema` converter exposed on the `~standard` value.
type jsonSchemaConverter = {
  input: jsonSchemaOptions => JSONSchema.t,
  output: jsonSchemaOptions => JSONSchema.t,
}

// The schema's inferred `input` and `output` types (the spec's `types` field).
type types<'input, 'output> = {
  input: 'input,
  output: 'output,
}

// The `~standard` value carried by every schema. Parametrized by the schema's
// inferred `input` and `output` types.
type t<'input, 'output> = {
  version: int,
  vendor: string,
  validate: 'any. 'any => {"value": 'output},
  jsonSchema: jsonSchemaConverter,
  types?: types<'input, 'output>,
}
