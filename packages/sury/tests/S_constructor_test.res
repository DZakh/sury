open Vitest

test("Constructor validates and hands back the value it was given", t => {
  let schema = S.object(s => {"id": s.field("id", S.string), "email": s.field("email", S.email)})
  let make = S.constructor(schema)
  let user = {"id": "1", "email": "a@b.com"}

  t->Assert.is(make(user), user)
  t->U.assertThrowsMessage(
    () => make({"id": "1", "email": "nope"}),
    `Failed at ["email"]: Expected email, received "nope"`,
  )
})

test("Constructor checks the output side of a codec", t => {
  let schema = S.string->S.to(S.float)
  let make = S.constructor(schema)

  t->Assert.deepEqual(make(1.), 1.)
  t->U.assertThrowsMessage(() => make(%raw(`"1"`)), `Expected number, received "1"`)
})

asyncTest("AsyncConstructor awaits the conversion before handing the value back", async t => {
  let schema =
    S.string->S.to(S.float, ~custom={decode: Async(s => Promise.resolve(Float.parseFloat(s))), encode: Auto})
  let make = S.asyncConstructor(schema)

  t->Assert.deepEqual(await make(5.), 5.)
})

test("Validators answer for the side they are named for", t => {
  let schema = S.string->S.to(S.float)
  let isInput = S.inputValidator(schema)
  let isOutput = S.outputValidator(schema)

  t->Assert.is(isInput(%raw(`"1"`)), true)
  t->Assert.is(isInput(%raw(`1`)), false)
  t->Assert.is(isOutput(%raw(`1`)), true)
  t->Assert.is(isOutput(%raw(`"1"`)), false)
})
