open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.length(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(schema), "1")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.length(1)

  t->U.assertThrows(
    () => ""->S.parseOrThrow(schema),
    {
      code: OperationFailed("String must be exactly 1 characters long"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  t->U.assertThrows(
    () => "1234"->S.parseOrThrow(schema),
    {
      code: OperationFailed("String must be exactly 1 characters long"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.length(1)

  t->Assert.deepEqual("1"->S.reverseConvertOrThrow(schema), %raw(`"1"`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.length(1)

  t->U.assertThrows(
    () => ""->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("String must be exactly 1 characters long"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
  t->U.assertThrows(
    () => "1234"->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("String must be exactly 1 characters long"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.length(~message="Custom", 12)

  t->U.assertThrows(
    () => "123"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.string->S.length(4)

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Length({length: 4}), message: "String must be exactly 4 characters long"}],
  )
})
