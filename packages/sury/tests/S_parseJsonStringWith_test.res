open Ava

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual("true"->S.decodeOrThrow(~from=S.jsonString, ~to=schema), true)
})

test("Successfully parses unknown", t => {
  let schema = S.unknown

  t->Assert.deepEqual(
    "true"->S.decodeOrThrow(~from=S.jsonString, ~to=schema),
    "true"->Obj.magic,
    ~message="S.unknown should keep json schema as a value",
  )

  t->Assert.deepEqual(
    "tru"->S.decodeOrThrow(~from=S.jsonString, ~to=schema),
    "tru"->Obj.magic,
    ~message="It also doesn't validate the value being a json string, because it expects input to already be a valid json string",
  )
})

test("Fails to parse JSON", t => {
  let schema = S.bool

  U.assertThrowsMessage(
    t,
    () => "123,"->S.decodeOrThrow(~from=S.jsonString, ~to=schema),
    `Expected JSON string, received "123,"`,
  )
})

test("Fails to parse", t => {
  let schema = S.bool

  t->U.assertThrowsMessage(
    () => "123"->S.decodeOrThrow(~from=S.jsonString, ~to=schema),
    `Expected boolean, received 123`,
  )
})
