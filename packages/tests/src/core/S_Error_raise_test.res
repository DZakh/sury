open Ava

test(
  "Raised error is instance of S.Error and displayed with a nice error message when not caught",
  t => {
    t->Assert.throws(
      () => {
        S.ErrorClass.constructor(
          ~code=OperationFailed("Should be positive"),
          ~flag=S.Flag.typeValidation,
          ~path=S.Path.empty,
        )->U.raiseError
      },
      ~expectations={
        message: "Failed parsing: Should be positive",
        instanceOf: S.ErrorClass.value->(U.magic: S.ErrorClass.t => ThrowsExpectation.instanceOf),
      },
      (),
    )
  },
)

test("Raised error is also the S.Error exeption and can be caught with catch", t => {
  let error = S.ErrorClass.constructor(
    ~code=OperationFailed("Should be positive"),
    ~flag=S.Flag.typeValidation,
    ~path=S.Path.empty,
  )
  t->ExecutionContext.plan(1)
  try {
    let _ = U.raiseError(error)
    t->Assert.fail("Should raise before the line")
  } catch {
  | S.Error(raisedError) => t->Assert.is(error, raisedError, ())
  }
})
