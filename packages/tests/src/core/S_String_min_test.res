open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.min(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(schema), "1", ())
  t->Assert.deepEqual("1234"->S.parseOrThrow(schema), "1234", ())
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.min(1)

  t->U.assertThrows(
    () => ""->S.parseOrThrow(schema),
    {
      code: OperationFailed("String must be 1 or more characters long"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.min(1)

  t->Assert.deepEqual("1"->S.reverseConvertOrThrow(schema), %raw(`"1"`), ())
  t->Assert.deepEqual("1234"->S.reverseConvertOrThrow(schema), %raw(`"1234"`), ())
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.min(1)

  t->U.assertThrows(
    () => ""->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("String must be 1 or more characters long"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.min(~message="Custom", 1)

  t->U.assertThrows(
    () => ""->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.string->S.min(1)

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Min({length: 1}), message: "String must be 1 or more characters long"}],
    (),
  )
})
