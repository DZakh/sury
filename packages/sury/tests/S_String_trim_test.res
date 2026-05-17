open Ava

test("Successfully parses", t => {
  let schema = S.string->S.trim

  t->Assert.deepEqual("   Hello world!"->S.parseOrThrow(~to=schema), "Hello world!")
})

test("Successfully serializes", t => {
  let schema = S.string->S.trim

  t->Assert.deepEqual("   Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"Hello world!"`))
})
