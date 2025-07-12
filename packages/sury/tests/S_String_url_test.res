open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual("http://dzakh.dev"->S.parseOrThrow(schema), "http://dzakh.dev")
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.url

  t->U.assertThrows(
    () => "cifjhdsfhsd"->S.parseOrThrow(schema),
    {code: OperationFailed("Invalid url"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.url

  t->Assert.deepEqual(
    "http://dzakh.dev"->S.reverseConvertOrThrow(schema),
    %raw(`"http://dzakh.dev"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.url

  t->U.assertThrows(
    () => "cifjhdsfhsd"->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("Invalid url"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.url(~message="Custom")

  t->U.assertThrows(
    () => "abc"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})
