open Ava

test("Successfully parses valid data", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual(8080->S.parseOrThrow(schema), 8080, ())
})

test("Fails to parse invalid data", t => {
  let schema = S.int->S.port

  t->U.assertThrows(
    () => 65536->S.parseOrThrow(schema),
    {code: OperationFailed("Invalid port"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual(8080->S.reverseConvertOrThrow(schema), %raw(`8080`), ())
})

test("Fails to serialize invalid value", t => {
  let schema = S.int->S.port

  t->U.assertThrows(
    () => -80->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("Invalid port"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Returns custom error message", t => {
  let schema = S.int->S.port(~message="Custom")

  t->U.assertThrows(
    () => 400000->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.int->S.port

  t->Assert.deepEqual(schema->S.Int.refinements, [{kind: Port, message: "Invalid port"}], ())
})
