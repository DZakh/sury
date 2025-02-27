open Ava

asyncTest("Resets S.float cache after disableNanNumberValidation=true removed", async t => {
  let nan = %raw(`NaN`)

  S.setGlobalConfig({
    disableNanNumberValidation: true,
  })
  t->Assert.deepEqual(nan->S.parseOrThrow(S.float), nan, ())
  t->Assert.deepEqual(await nan->S.parseAsyncOrThrow(S.float), nan, ())
  t->Assert.deepEqual(nan->S.assertOrThrow(S.float), (), ())

  S.setGlobalConfig({})
  t->U.assertRaised(
    () => nan->S.parseOrThrow(S.float),
    {
      code: S.InvalidType({
        expected: S.float->S.toUnknown,
        received: nan,
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  await t->U.assertRaisedAsync(
    () => nan->S.parseAsyncOrThrow(S.float),
    {
      code: S.InvalidType({
        expected: S.float->S.toUnknown,
        received: nan,
      }),
      operation: ParseAsync,
      path: S.Path.empty,
    },
  )
  t->Assert.throws(
    () => {
      nan->S.assertOrThrow(S.float)
    },
    ~expectations={
      message: "Failed asserting at root. Reason: Must be number (was NaN)",
    },
    (),
  )
})
