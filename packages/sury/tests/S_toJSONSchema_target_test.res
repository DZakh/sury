open Vitest

// Per-target JSON Schema output, mirroring @valibot/to-json-schema's `target`
// option. The default (no options) path is covered by S_toJSONSchema_test.res
// and must stay byte-identical; these tests pin the explicit-target behavior.

test("toJSONSchema with target draft-07 stamps the draft-07 $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={target: #"draft-07"}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`),
  )
})

test("toJSONSchema with target draft-2020-12 stamps the draft-2020-12 $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={target: #"draft-2020-12"}),
    %raw(`{"$schema": "https://json-schema.org/draft/2020-12/schema", "type": "string"}`),
  )
})

test("toJSONSchema with target openapi-3.0 omits $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{"type": "string"}`),
  )
})

test("toJSONSchema with empty options defaults to draft-07 and stamps $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`),
  )
})

test("toJSONSchema without options stays unchanged (no $schema)", t => {
  t->Assert.deepEqual(S.string->S.toJSONSchema, %raw(`{"type": "string"}`))
})

test("toJSONSchema with an unsupported target throws", t => {
  // ReScript callers are constrained to `StandardSchema.JsonSchema.target` by the type
  // system, but an untyped JS caller (e.g. via `~standard`) can pass an arbitrary
  // string; `toJSONSchema` validates it at runtime. Simulate that with a cast.
  t->Assert.throws(
    () =>
      S.string->S.toJSONSchema(
        ~options={target: "unsupported-target"->(U.magic: string => StandardSchema.JsonSchema.target)},
      ),
    ~expectations={message: "Unsupported JSON Schema target: unsupported-target"},
  )
})

// --- Tuples ---

test("toJSONSchema tuple draft-07 uses an items array", t => {
  t->Assert.deepEqual(
    S.tuple2(S.string, S.bool)->S.toJSONSchema(~options={target: #"draft-07"}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "array",
      "minItems": 2,
      "maxItems": 2,
      "items": [{"type": "string"}, {"type": "boolean"}]
    }`),
  )
})

test("toJSONSchema tuple draft-2020-12 uses prefixItems", t => {
  t->Assert.deepEqual(
    S.tuple2(S.string, S.bool)->S.toJSONSchema(~options={target: #"draft-2020-12"}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "minItems": 2,
      "maxItems": 2,
      "prefixItems": [{"type": "string"}, {"type": "boolean"}]
    }`),
  )
})

test("toJSONSchema tuple openapi-3.0 uses items anyOf", t => {
  t->Assert.deepEqual(
    S.tuple2(S.string, S.bool)->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{
      "type": "array",
      "minItems": 2,
      "maxItems": 2,
      "items": {"anyOf": [{"type": "string"}, {"type": "boolean"}]}
    }`),
  )
})

// --- Null literal ---

test("toJSONSchema null draft-07 uses type null", t => {
  t->Assert.deepEqual(
    S.literal(%raw(`null`))->S.toJSONSchema(~options={target: #"draft-07"}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "null"}`),
  )
})

test("toJSONSchema null openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal(%raw(`null`))->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{"enum": [null]}`),
  )
})

// --- Const literals ---

test("toJSONSchema string literal draft-07 uses const", t => {
  t->Assert.deepEqual(
    S.literal("Hello")->S.toJSONSchema(~options={target: #"draft-07"}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string", "const": "Hello"}`),
  )
})

test("toJSONSchema string literal openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal("Hello")->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{"type": "string", "enum": ["Hello"]}`),
  )
})

test("toJSONSchema number literal openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal(123)->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{"type": "number", "enum": [123]}`),
  )
})

test("toJSONSchema boolean literal openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal(true)->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{"type": "boolean", "enum": [true]}`),
  )
})

// --- Nullable union collapse (openapi-3.0) ---

test("toJSONSchema nullable float draft-07 keeps anyOf with type null", t => {
  t->Assert.deepEqual(
    S.nullAsOption(S.float)->S.toJSONSchema(~options={target: #"draft-07"}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "anyOf": [{"type": "number"}, {"type": "null"}]
    }`),
  )
})

test("toJSONSchema nullable float openapi-3.0 collapses to nullable", t => {
  t->Assert.deepEqual(
    S.nullAsOption(S.float)->S.toJSONSchema(~options={target: #"openapi-3.0"}),
    %raw(`{"type": "number", "nullable": true}`),
  )
})
