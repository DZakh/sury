open Vitest

// Per-target JSON Schema output, mirroring @valibot/to-json-schema's `target`
// option. The default (no options) path is covered by S_toJSONSchema_test.res
// and must stay byte-identical; these tests pin the explicit-target behavior.

test("toJSONSchema with target draft-07 stamps the draft-07 $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string"}`),
  )
})

test("toJSONSchema with target draft-2020-12 stamps the draft-2020-12 $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{"$schema": "https://json-schema.org/draft/2020-12/schema", "type": "string"}`),
  )
})

test("toJSONSchema with target openapi-3.0 omits $schema", t => {
  t->Assert.deepEqual(
    S.string->S.toJSONSchema(~options={target: OpenApi30}),
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
  // `Unknown` carries any target that isn't a known dialect - a ReScript
  // caller can construct it directly, no cast needed - and `toJSONSchema`
  // validates it at runtime.
  t->Assert.throws(
    () => S.string->S.toJSONSchema(~options={target: Unknown("unsupported-target")}),
    ~expectations={message: "Unsupported JSON Schema target: unsupported-target"},
  )
})

// --- Tuples ---

test("toJSONSchema tuple draft-07 uses an items array", t => {
  t->Assert.deepEqual(
    S.tuple2(S.string, S.bool)->S.toJSONSchema(~options={target: Draft07}),
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
    S.tuple2(S.string, S.bool)->S.toJSONSchema(~options={target: Draft202012}),
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
    S.tuple2(S.string, S.bool)->S.toJSONSchema(~options={target: OpenApi30}),
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
    S.literal(%raw(`null`))->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "null"}`),
  )
})

test("toJSONSchema null openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal(%raw(`null`))->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"enum": [null]}`),
  )
})

// --- Const literals ---

test("toJSONSchema string literal draft-07 uses const", t => {
  t->Assert.deepEqual(
    S.literal("Hello")->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{"$schema": "http://json-schema.org/draft-07/schema#", "type": "string", "const": "Hello"}`),
  )
})

test("toJSONSchema string literal openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal("Hello")->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "string", "enum": ["Hello"]}`),
  )
})

test("toJSONSchema number literal openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal(123)->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "number", "enum": [123]}`),
  )
})

test("toJSONSchema boolean literal openapi-3.0 uses enum", t => {
  t->Assert.deepEqual(
    S.literal(true)->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "boolean", "enum": [true]}`),
  )
})

// --- Nullable union collapse (openapi-3.0) ---

test("toJSONSchema nullable float draft-07 keeps anyOf with type null", t => {
  t->Assert.deepEqual(
    S.nullAsOption(S.float)->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "anyOf": [{"type": "number"}, {"type": "null"}]
    }`),
  )
})

test("toJSONSchema nullable float openapi-3.0 collapses to nullable", t => {
  t->Assert.deepEqual(
    S.nullAsOption(S.float)->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "number", "nullable": true}`),
  )
})

// --- Exclusive bounds ---

test("toJSONSchema exclusive bound draft-07 uses the numeric keyword", t => {
  t->Assert.deepEqual(
    S.float->S.gt(5.)->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "number",
      "exclusiveMinimum": 5
    }`),
  )
})

// OpenAPI 3.0 follows draft-04, where exclusivity is a boolean modifying
// `minimum` rather than a bound of its own.
test("toJSONSchema exclusive bound openapi-3.0 uses the draft-04 boolean form", t => {
  t->Assert.deepEqual(
    S.float->S.gt(5.)->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "number", "minimum": 5, "exclusiveMinimum": true}`),
  )
})

test("toJSONSchema exclusive upper bound openapi-3.0 uses the draft-04 boolean form", t => {
  t->Assert.deepEqual(
    S.float->S.lt(5.)->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "number", "maximum": 5, "exclusiveMaximum": true}`),
  )
})

test("toJSONSchema keeps both bounds when only one is exclusive", t => {
  t->Assert.deepEqual(
    S.float->S.gte(0.)->S.lt(5.)->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "number",
      "minimum": 0,
      "exclusiveMaximum": 5
    }`),
  )
})

// `contentSchema` is 2019-09 and later, and OpenAPI 3.0 predates the whole
// content family — so the same schema says three different amounts about the
// document it carries. Not a spec: the format snapshots one target (the
// default), so the dialect gating is only observable here.
test("toJSONSchema of a JSON string describes the document it carries", t => {
  let schema = S.jsonString->S.to(S.object(s => s.field("port", S.int)))

  t->Assert.deepEqual(
    schema->S.toJSONSchema(~options={target: Draft07}),
    %raw(`{
      "$schema": "http://json-schema.org/draft-07/schema#",
      "type": "string",
      "contentMediaType": "application/json"
    }`),
  )

  t->Assert.deepEqual(
    schema->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/json",
      "contentSchema": {
        "type": "object",
        "properties": {
          "port": {"type": "integer", "minimum": -2147483648, "maximum": 2147483647}
        },
        "required": ["port"]
      }
    }`),
  )

  t->Assert.deepEqual(
    schema->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "string"}`),
  )
})

// A blob is octets, which no JSON type describes — so each dialect describes
// the carrier instead, and they disagree on how. `S.extendJSONSchema` holds one
// document for every target and could not express this; the schema converting
// itself is what can.
test("toJSONSchema of a binary instance follows the target's spelling", t => {
  t->Assert.deepEqual(
    S.file->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "string", "format": "binary"}`),
  )

  t->Assert.deepEqual(
    S.blob->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/octet-stream"
    }`),
  )

  // The override still lands on top of what the schema produced for itself.
  t->Assert.deepEqual(
    S.blob
    ->S.extendJSONSchema({contentMediaType: "image/png"})
    ->S.toJSONSchema(~options={target: OpenApi30}),
    %raw(`{"type": "string", "format": "binary", "contentMediaType": "image/png"}`),
  )
})

test("toJSONSchema of a JSON string keeps converting when the document has no JSON Schema", t => {
  // `contentSchema` is an annotation, so a `to` with no JSON Schema form takes
  // it off rather than failing the conversion of a schema that is otherwise
  // perfectly describable.
  t->Assert.deepEqual(
    (S.jsonString->S.to(S.bigint))->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/json"
    }`),
  )

  t->Assert.deepEqual(
    (S.jsonString->S.to(S.uint8Array))->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/json"
    }`),
  )
})

test("toJSONSchema of a JSON string omits contentSchema when the document is any JSON", t => {
  t->Assert.deepEqual(
    (S.jsonString->S.to(S.json))->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/json"
    }`),
  )
})

test("toJSONSchema lets extendJSONSchema override both content keywords", t => {
  let schema =
    S.jsonString
    ->S.to(S.object(s => s.field("port", S.int)))
    ->S.extendJSONSchema({
      contentMediaType: "application/geo+json",
      contentSchema: JSONSchema.Schema({type_: JSONSchema.Arrayable.single(#object)}),
    })

  t->Assert.deepEqual(
    schema->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/geo+json",
      "contentSchema": {"type": "object"}
    }`),
  )
})

test("toJSONSchema publishes the $defs a contentSchema reaches", t => {
  let node = S.recursive("Node", node =>
    S.object(s =>
      {
        "name": s.field("name", S.string),
        "child": s.field("child", S.option(node)),
      }
    )
  )

  // A recursive document converts to a `$ref`, so `contentSchema` is only
  // resolvable if the definition it names is published beside it.
  t->Assert.deepEqual(
    (S.jsonString->S.to(node))->S.toJSONSchema(~options={target: Draft202012}),
    %raw(`{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "contentMediaType": "application/json",
      "contentSchema": {"$ref": "#/$defs/Node"},
      "$defs": {
        "Node": {
          "type": "object",
          "properties": {"name": {"type": "string"}, "child": {"$ref": "#/$defs/Node"}},
          "required": ["name"]
        }
      }
    }`),
  )
})
