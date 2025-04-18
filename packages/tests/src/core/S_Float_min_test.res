open Ava

test("Successfully parses valid data", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(1.->S.parseOrThrow(schema), 1., ())
  t->Assert.deepEqual(1234.->S.parseOrThrow(schema), 1234., ())
})

test("Fails to parse invalid data", t => {
  let schema = S.float->S.floatMin(1.)

  t->U.assertThrows(
    () => 0->S.parseOrThrow(schema),
    {
      code: OperationFailed("Number must be greater than or equal to 1"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(1.->S.reverseConvertOrThrow(schema), %raw(`1`), ())
  t->Assert.deepEqual(1234.->S.reverseConvertOrThrow(schema), %raw(`1234`), ())
})

test("Fails to serialize invalid value", t => {
  let schema = S.float->S.floatMin(1.)

  t->U.assertThrows(
    () => 0.->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Number must be greater than or equal to 1"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.float->S.floatMin(~message="Custom", 1.)

  t->U.assertThrows(
    () => 0.->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(
    schema->S.Float.refinements,
    [{kind: Min({value: 1.}), message: "Number must be greater than or equal to 1"}],
    (),
  )
})
