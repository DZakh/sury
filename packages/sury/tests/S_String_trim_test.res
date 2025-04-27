open Ava

test("Successfully parses", t => {
  let schema = S.string->S.trim

  t->Assert.deepEqual("   Hello world!"->S.parseOrThrow(schema), "Hello world!", ())
})

test("Successfully serializes", t => {
  let schema = S.string->S.trim

  t->Assert.deepEqual(
    "   Hello world!"->S.reverseConvertOrThrow(schema),
    %raw(`"Hello world!"`),
    (),
  )
})
