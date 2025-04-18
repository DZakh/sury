open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.stringMaxLength(1)

  t->Assert.deepEqual("1"->S.parseOrThrow(schema), "1", ())
  t->Assert.deepEqual(""->S.parseOrThrow(schema), "", ())
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.stringMaxLength(1)

  t->U.assertThrows(
    () => "1234"->S.parseOrThrow(schema),
    {
      code: OperationFailed("String must be 1 or fewer characters long"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.stringMaxLength(1)

  t->Assert.deepEqual("1"->S.reverseConvertOrThrow(schema), %raw(`"1"`), ())
  t->Assert.deepEqual(""->S.reverseConvertOrThrow(schema), %raw(`""`), ())
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.stringMaxLength(1)

  t->U.assertThrows(
    () => "1234"->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("String must be 1 or fewer characters long"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.stringMaxLength(~message="Custom", 1)

  t->U.assertThrows(
    () => "1234"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.string->S.stringMaxLength(1)

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Max({length: 1}), message: "String must be 1 or fewer characters long"}],
    (),
  )
})
