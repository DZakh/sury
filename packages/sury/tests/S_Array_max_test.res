open Ava

test("Successfully parses valid data", t => {
  let schema = S.array(S.int)->S.max(1)

  t->Assert.deepEqual([1]->S.parseOrThrow(schema), [1], ())
  t->Assert.deepEqual([]->S.parseOrThrow(schema), [], ())
})

test("Fails to parse invalid data", t => {
  let schema = S.array(S.int)->S.max(1)

  t->U.assertThrows(
    () => [1, 2, 3, 4]->S.parseOrThrow(schema),
    {
      code: OperationFailed("Array must be 1 or fewer items long"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.array(S.int)->S.max(1)

  t->Assert.deepEqual([1]->S.reverseConvertOrThrow(schema), %raw(`[1]`), ())
  t->Assert.deepEqual([]->S.reverseConvertOrThrow(schema), %raw(`[]`), ())
})

test("Fails to serialize invalid value", t => {
  let schema = S.array(S.int)->S.max(1)

  t->U.assertThrows(
    () => [1, 2, 3, 4]->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Array must be 1 or fewer items long"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.array(S.int)->S.max(~message="Custom", 1)

  t->U.assertThrows(
    () => [1, 2]->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.array(S.int)->S.max(1)

  t->Assert.deepEqual(
    schema->S.Array.refinements,
    [{kind: Max({length: 1}), message: "Array must be 1 or fewer items long"}],
    (),
  )
})
