open Ava

test("Works", t => {
  t->Assert.deepEqual(S.Path.fromLocation("123")->S.Path.toString, `["123"]`, ())
})

test("Escapes quotes", t => {
  t->Assert.deepEqual(S.Path.fromLocation(`"123"`)->S.Path.toString, `["\\"123\\""]`, ())
})
