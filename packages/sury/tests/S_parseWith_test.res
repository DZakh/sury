open Ava
open RescriptCore

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(schema), true)
})

test("Successfully parses unknown", t => {
  let schema = S.unknown

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(schema), true->Obj.magic)
})

test("Fails to parse", t => {
  let schema = S.bool

  t->U.assertThrows(
    () => %raw("123")->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw("123")}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse with unwraped result", t => {
  let schema = S.bool

  t->Assert.throws(() => {
    %raw("123")->S.parseOrThrow(schema)
  }, ~expectations={message: "Failed parsing: Expected boolean, received 123"})
})
