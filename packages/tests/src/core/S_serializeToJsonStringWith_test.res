open Ava

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual(true->S.reverseConvertToJsonStringOrThrow(schema), "true", ())
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
    (),
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
    (),
  )
})

test("Fails to serialize Unknown schema", t => {
  let schema = S.unknown

  t->Assert.throws(
    () => {
      Obj.magic(123)->S.reverseConvertToJsonStringOrThrow(schema)
    },
    ~expectations={
      message: "Failed converting to JSON: The 'unknown' schema cannot be converted to JSON",
    },
    (),
  )
})
