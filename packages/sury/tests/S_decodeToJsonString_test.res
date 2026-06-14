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
