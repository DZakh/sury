open Vitest

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(~to=schema), true)
})

test("Successfully parses unknown", t => {
  let schema = S.unknown

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(~to=schema), true->Obj.magic)
})

test("Fails to parse", t => {
  let schema = S.bool

  t->U.assertThrowsMessage(
    () => %raw("123")->S.parseOrThrow(~to=schema),
    `Expected boolean, received 123`,
  )
})

test("Fails to parse with unwraped result", t => {
  let schema = S.bool

  t->Assert.throws(() => {
    %raw("123")->S.parseOrThrow(~to=schema)
  }, ~expectations={message: "Expected boolean, received 123"})
})
