open Ava

asyncTest("Resets S.float cache after disableNanNumberValidation=true removed", async t => {
  let nan = %raw(`NaN`)

  S.global({
    disableNanNumberValidation: true,
  })
  t->Assert.deepEqual(nan->S.parseOrThrow(~to=S.float), nan)
  t->Assert.deepEqual(await nan->S.parseAsyncOrThrow(~to=S.float), nan)

  S.global({})
  t->U.assertThrowsMessage(() => nan->S.parseOrThrow(~to=S.float), `Expected number, received NaN`)
  await t->U.asyncAssertThrowsMessage(
    () => nan->S.parseAsyncOrThrow(~to=S.float),
    `Expected number, received NaN`,
  )
  t->Assert.throws(
    () => {
      nan->S.assertOrThrow(~to=S.float)
    },
    ~expectations={
      message: "Expected number, received NaN",
    },
  )
})
