open Ava

test("Successfully parses valid data", t => {
  let schema = S.array(S.int)->S.min(1)

  t->Assert.deepEqual([1]->S.parseOrThrow(schema), [1])
  t->Assert.deepEqual([1, 2, 3, 4]->S.parseOrThrow(schema), [1, 2, 3, 4])
})

test("Fails to parse invalid data", t => {
  let schema = S.array(S.int)->S.min(1)

  t->U.assertThrows(
    () => []->S.parseOrThrow(schema),
    {
      code: OperationFailed("Array must be 1 or more items long"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.array(S.int)->S.min(1)

  t->Assert.deepEqual([1]->S.reverseConvertOrThrow(schema), %raw(`[1]`))
  t->Assert.deepEqual([1, 2, 3, 4]->S.reverseConvertOrThrow(schema), %raw(`[1,2,3,4]`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.array(S.int)->S.min(1)

  t->U.assertThrows(
    () => []->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Array must be 1 or more items long"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.array(S.int)->S.min(~message="Custom", 1)

  t->U.assertThrows(
    () => []->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.array(S.int)->S.min(1)

  t->Assert.deepEqual(
    schema->S.Array.refinements,
    [{kind: Min({length: 1}), message: "Array must be 1 or more items long"}],
  )
})
