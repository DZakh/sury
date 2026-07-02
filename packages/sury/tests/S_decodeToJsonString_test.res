open Vitest

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual(true->S.decodeOrThrow(~from=schema, ~to=S.jsonString), "true")
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
    }->S.decodeOrThrow(~from=schema, ~to=S.jsonString),
    `{"id":"0","isDeleted":true}`,
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
    }->S.decodeOrThrow(~from=schema, ~to=S.jsonStringWithSpace(2)),
    `{
  "id": "0",
  "isDeleted": true
}`,
  )
})

test("unknown <-> json string expects unknown to be a json string", t => {
  let schema = S.unknown

  t->U.assertThrowsMessage(
    () => Obj.magic(123)->S.decodeOrThrow(~from=S.unknown, ~to=S.jsonString),
    "Expected JSON string, received 123",
  )
  t->Assert.deepEqual(Obj.magic("123")->S.decodeOrThrow(~from=S.unknown, ~to=S.jsonString), "123")
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
    %raw(`{type: "a", s: undefined}`)->S.decodeOrThrow(~from=xSchema, ~to=S.jsonString),
    `{"type":"a"}`,
  )
  // While nested in an object it used to fail with:
  // Can't decode { s: string | undefined; type: "a"; } | { v: int32; type: "b"; } to JSON
  t->Assert.deepEqual(
    %raw(`{x: {type: "a", s: undefined}}`)->S.decodeOrThrow(~from=testSchema, ~to=S.jsonString),
    `{"x":{"type":"a"}}`,
  )
  t->Assert.deepEqual(
    %raw(`{x: {type: "a", s: "hi"}}`)->S.decodeOrThrow(~from=testSchema, ~to=S.jsonString),
    `{"x":{"type":"a","s":"hi"}}`,
  )
  t->Assert.deepEqual(
    %raw(`{x: {type: "b", v: 1}}`)->S.decodeOrThrow(~from=testSchema, ~to=S.jsonString),
    `{"x":{"type":"b","v":1}}`,
  )
})

// https://github.com/DZakh/sury/issues/252#issuecomment-4867670534
// The fix above covers a union built from plain object schemas, but the
// original report builds each variant with `s.tag` + `s.flatten` (the
// pattern sury-ppx generates for `A(s.flatten(aSchema))`). That construction
// still fails to encode to JSON once nested inside another object, now with
// a different error: `Failed at ["x"]["s"]: Expected JSON, received undefined`.
//
// Root cause: the compiled encoder runs the union-to-JSON conversion twice.
// The 1st pass turns the {TAG,_0} variant into a plain dict
// ({"type":"a","s":undefined}) without omitting the undefined "s" key. The
// 2nd pass re-dispatches on that dict's "type" field and, for the matched
// case, generically iterates every own key of the dict asserting each value
// is JSON-encodable — "s" is still present (with value undefined) since pass
// 1 didn't omit it, so that generic check fails on it.
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
    FlattenedA({s: None})->S.decodeOrThrow(~from=flattenedXSchema, ~to=S.jsonString),
    `{"type":"a"}`,
  )

  // Still fails once nested inside another object
  t->Assert.deepEqual(
    {x: FlattenedA({s: None})}->S.decodeOrThrow(~from=flattenedContainerSchema, ~to=S.jsonString),
    `{"x":{"type":"a"}}`,
  )
})
