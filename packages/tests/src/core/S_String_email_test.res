open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual("dzakh.dev@gmail.com"->S.parseOrThrow(schema), "dzakh.dev@gmail.com", ())
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.email

  t->U.assertThrows(
    () => "dzakh.dev"->S.parseOrThrow(schema),
    {
      code: OperationFailed("Invalid email address"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual(
    "dzakh.dev@gmail.com"->S.reverseConvertOrThrow(schema),
    %raw(`"dzakh.dev@gmail.com"`),
    (),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.email

  t->U.assertThrows(
    () => "dzakh.dev"->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Invalid email address"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.email(~message="Custom")

  t->U.assertThrows(
    () => "dzakh.dev"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.string->S.email

  t->Assert.deepEqual(
    schema->S.String.refinements,
    [{kind: Email, message: "Invalid email address"}],
    (),
  )
})
