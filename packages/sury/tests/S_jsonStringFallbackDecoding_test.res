open Vitest

test(
  "String fallback in JSON encoder propagates errors from downstream conversions",
  t => {
    // Without JSON wrapper: clear compile-time error about the actual problem (bigint -> Date)
    t->U.assertThrowsMessage(
      () => {
        let _ = S.decoder1(S.bigint->S.to(S.date))
      },
      `Can't decode bigint to Date. Use S.to to define a custom decoder`,
    )

    // With JSON wrapper: the downstream error (bigint -> Date) should propagate,
    // not be swallowed by the string fallback's try-catch.
    t->U.assertThrowsMessage(
      () => {
        let _ = S.decoder1(S.json->S.to(S.bigint)->S.to(S.date))
      },
      `Can't decode bigint to Date. Use S.to to define a custom decoder`,
    )
  },
)
