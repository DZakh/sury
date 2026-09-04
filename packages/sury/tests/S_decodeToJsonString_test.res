open Vitest

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual(true->S.convertOrThrow(~from=schema, ~to=S.jsonString), S.JsonString("true"))
})

test("Successfully parses object", t => {
  let schema = S.object(s =>
    {
      "id": s.field("id", S.string),
      "isDeleted": s.field("isDeleted", S.bool),
    }
  )

  t->Assert.deepEqual(
    {
      "id": "0",
      "isDeleted": true,
    }->S.convertOrThrow(~from=schema, ~to=S.jsonString),
    S.JsonString(`{"id":"0","isDeleted":true}`),
  )
})

test("Successfully parses object with space", t => {
  let schema = S.object(s =>
    {
      "id": s.field("id", S.string),
      "isDeleted": s.field("isDeleted", S.bool),
    }
  )

  t->Assert.deepEqual(
    {
      "id": "0",
      "isDeleted": true,
    }->S.convertOrThrow(~from=schema, ~to=S.jsonStringWithSpace(2)),
    S.JsonString(`{
  "id": "0",
  "isDeleted": true
}`),
  )
})

test("unknown <-> json string expects unknown to be a json string", t => {
  let schema = S.unknown

  t->U.assertThrowsMessage(
    () => Obj.magic(123)->S.convertOrThrow(~from=S.unknown, ~to=S.jsonString),
    "Expected JSON string, received 123",
  )
  t->Assert.deepEqual(Obj.magic("123")->S.convertOrThrow(~from=S.unknown, ~to=S.jsonString), S.JsonString("123"))
  t->U.assertCompiledCode(~schema, ~op=#EncodeToJson, `i=>{e[0](i);return i}`)
})

// https://github.com/DZakh/sury/issues/252
test("Encodes object with a union of objects field to JSON string", t => {
  let aSchema = S.schema(s =>
    {
      "type": s.matches(S.literal("a")),
      "s": s.matches(S.nullable(S.string)),
    }
  )
  let bSchema = S.schema(s =>
    {
      "type": s.matches(S.literal("b")),
      "v": s.matches(S.int),
    }
  )
  let xSchema = S.union([aSchema->Obj.magic, bSchema->Obj.magic])
  let testSchema = S.schema(s => {"x": s.matches(xSchema)})

  // The union at the top level worked before the fix
  t->Assert.deepEqual(
    %raw(`{type: "a", s: undefined}`)->S.convertOrThrow(~from=xSchema, ~to=S.jsonString),
    S.JsonString(`{"type":"a"}`),
  )
  // While nested in an object it used to fail with:
  // Can't decode { s: string | undefined; type: "a"; } | { v: int32; type: "b"; } to JSON
  t->Assert.deepEqual(
    %raw(`{x: {type: "a", s: undefined}}`)->S.convertOrThrow(~from=testSchema, ~to=S.jsonString),
    S.JsonString(`{"x":{"type":"a"}}`),
  )
  t->Assert.deepEqual(
    %raw(`{x: {type: "a", s: "hi"}}`)->S.convertOrThrow(~from=testSchema, ~to=S.jsonString),
    S.JsonString(`{"x":{"type":"a","s":"hi"}}`),
  )
  t->Assert.deepEqual(
    %raw(`{x: {type: "b", v: 1}}`)->S.convertOrThrow(~from=testSchema, ~to=S.jsonString),
    S.JsonString(`{"x":{"type":"b","v":1}}`),
  )
})

// https://github.com/DZakh/sury/issues/252#issuecomment-4867670534
// The test above covers a union built from plain object schemas, but the
// original report builds each variant with `s.tag` + `s.flatten` (the
// pattern sury-ppx generates for `A(s.flatten(aSchema))`). That construction
// used to fail to encode to JSON once nested inside another object with:
// `Failed at x.s: Expected JSON, received undefined`.
//
// Root cause: nested, the field converts in two steps — a JSON-unaware plain
// encode of the union (which keeps the undefined "s" key), then a per-variant
// `.to(json)` re-dispatch. Inside that re-dispatch, objectDecoder's
// no-transform pass-through kept the union dispatch narrow
// ({properties:{}, additionalItems: unknown}) as the case output's schema
// instead of the validated variant schema, so jsonDecoderFn misrouted the
// conversion into the dict path — which rejects undefined values instead of
// omitting optional fields the way the fixed-properties path does.
type flattenedA = {s: option<string>}
type flattenedB = {v: int}
type flattenedX = FlattenedA(flattenedA) | FlattenedB(flattenedB)
type flattenedContainer = {x: flattenedX}

// aSchema/bSchema/testSchema are `@schema`-derived in the original report.
// sury-ppx compiles plain records via `S.schema` + `s.matches` (see
// generateRecordSchema in packages/sury-ppx/src/ppx/Structure.ml), not
// `S.object` + `s.field` — only the hand-written union below uses `S.object`.
let flattenedASchema: S.t<flattenedA> = S.schema(s => {
  s: s.matches(S.nullableAsOption(S.string)),
})
let flattenedBSchema: S.t<flattenedB> = S.schema(s => {
  v: s.matches(S.int),
})
let flattenedXSchema: S.t<flattenedX> = S.union([
  S.object(s => {
    s.tag("type", "a")
    FlattenedA(s.flatten(flattenedASchema))
  }),
  S.object(s => {
    s.tag("type", "b")
    FlattenedB(s.flatten(flattenedBSchema))
  }),
])
let flattenedContainerSchema: S.t<flattenedContainer> = S.schema(s => {
  x: s.matches(flattenedXSchema),
})

test("Encodes object with a union of flattened tagged objects field to JSON string", t => {
  // Works at the top level
  t->Assert.deepEqual(
    FlattenedA({s: None})->S.convertOrThrow(~from=flattenedXSchema, ~to=S.jsonString),
    S.JsonString(`{"type":"a"}`),
  )

  // Regression: used to fail once nested inside another object
  t->Assert.deepEqual(
    {x: FlattenedA({s: None})}->S.convertOrThrow(~from=flattenedContainerSchema, ~to=S.jsonString),
    S.JsonString(`{"x":{"type":"a"}}`),
  )
})

// https://github.com/DZakh/sury/pull/297#discussion_r3565781924
// arrayDecoder has the same no-transform pass-through as objectDecoder (fixed
// in the same commit), so cover the array/tuple side of the class too.
test("Encodes an array of flattened tagged union values to JSON string", t => {
  let arraySchema = S.array(flattenedXSchema)

  t->Assert.deepEqual(
    [FlattenedA({s: None}), FlattenedB({v: 1})]->S.convertOrThrow(
      ~from=arraySchema,
      ~to=S.jsonString,
    ),
    S.JsonString(`[{"type":"a"},{"v":1,"type":"b"}]`),
  )

  // Nested inside an object, matching the object regression above
  let containerSchema = S.schema(s => {"items": s.matches(arraySchema)})
  t->Assert.deepEqual(
    {"items": [FlattenedA({s: None})]}->S.convertOrThrow(~from=containerSchema, ~to=S.jsonString),
    S.JsonString(`{"items":[{"type":"a"}]}`),
  )
})

test("Encodes a tuple of a flattened tagged union value to JSON string", t => {
  let tupleSchema = S.tuple1(flattenedXSchema)

  t->Assert.deepEqual(
    FlattenedA({s: None})->S.convertOrThrow(~from=tupleSchema, ~to=S.jsonString),
    S.JsonString(`[{"type":"a"}]`),
  )
})
