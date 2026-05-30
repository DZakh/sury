open Vitest

test(
  "Raised error is instance of S.Error and displayed with a nice error message when not caught",
  t => {
    t->Assert.throws(
      () => {
        S.Error.make(
          InvalidInput({
            reason: "Should be positive",
            path: S.Path.empty,
            expected: S.unknown,
            received: S.unknown,
          }),
        )->U.throwError
      },
      ~expectations={
        message: "Should be positive",
        instanceOf: S.Error.class->(U.magic: S.Error.class => 'instanceOf),
      },
    )
  },
)

test("Raised error is also the S.Error exeption and can be caught with catch", t => {
  let error = S.Error.make(
    InvalidInput({
      reason: "Should be positive",
      path: S.Path.empty,
      expected: S.unknown,
      received: S.unknown,
    }),
  )
  t->ExecutionContext.plan(1)
  try {
    let _ = U.throwError(error)
    t->Assert.fail("Should throw before the line")
  } catch {
  | S.Exn(throwdError) => t->Assert.is(error, throwdError)
  }
})
