open Ava

S.enableJsonString()

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual(true->S.reverseConvertToJsonStringOrThrow(schema), "true")
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
    }->S.reverseConvertToJsonStringOrThrow(schema),
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
    }->S.reverseConvertToJsonStringOrThrow(~space=2, schema),
    `{
  "id": "0",
  "isDeleted": true
}`,
  )
})

test("unknown <-> json string expects unknown to be a json string", t => {
  let schema = S.unknown

  t->U.assertThrowsMessage(
    () => Obj.magic(123)->S.reverseConvertToJsonStringOrThrow(S.unknown),
    "Expected JSON string, received 123",
  )
  t->Assert.deepEqual(Obj.magic("123")->S.reverseConvertToJsonStringOrThrow(S.unknown), "123")
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvertToJson, `i=>{e[0](i);return i}`)
})
