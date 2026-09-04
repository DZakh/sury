open Vitest

test("Constructor validates and hands back the value it was given", t => {
  let schema = S.object(s => {"id": s.field("id", S.string), "email": s.field("email", S.email)})
  let make = S.compileMakeOrThrow(~schema=schema)
  let user = {"id": "1", "email": "a@b.com"}

  t->Assert.is(make(user), user)
  t->U.assertThrowsMessage(
    () => make({"id": "1", "email": "nope"}),
    `Failed at email: Expected email, received "nope"`,
  )
})

test("Constructor checks the output side of a codec", t => {
  let schema = S.string->S.to(S.float)
  let make = S.compileMakeOrThrow(~schema=schema)

  t->Assert.deepEqual(make(1.), 1.)
  t->U.assertThrowsMessage(() => make(%raw(`"1"`)), `Expected number, received "1"`)
})

asyncTest("AsyncConstructor awaits the conversion before handing the value back", async t => {
  let schema =
    S.string->S.to(S.float, ~custom={decode: Async(s => Promise.resolve(Float.parseFloat(s))), encode: Auto})
  let make = S.compileMakeAsyncOrThrow(~schema=schema)

  t->Assert.deepEqual(await make(5.), 5.)
})

test("makeOrThrow is the one-shot flavor of compileMakeOrThrow", t => {
  let schema = S.string->S.to(S.float)

  t->Assert.deepEqual(1.->S.makeOrThrow(~schema), 1.)
  t->U.assertThrowsMessage(() => %raw(`"1"`)->S.makeOrThrow(~schema), `Expected number, received "1"`)
})

asyncTest("makeAsyncOrThrow awaits the conversion", async t => {
  let schema =
    S.string->S.to(S.float, ~custom={decode: Async(s => Promise.resolve(Float.parseFloat(s))), encode: Auto})

  t->Assert.deepEqual(await 5.->S.makeAsyncOrThrow(~schema), 5.)
})
