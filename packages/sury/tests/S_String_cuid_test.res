open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(
    "ckopqwooh000001la8mbi2im9"->S.parseOrThrow(schema),
    "ckopqwooh000001la8mbi2im9",
    (),
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.cuid

  t->U.assertThrows(
    () => "cifjhdsfhsd-invalid-cuid"->S.parseOrThrow(schema),
    {code: OperationFailed("Invalid CUID"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(
    "ckopqwooh000001la8mbi2im9"->S.reverseConvertOrThrow(schema),
    %raw(`"ckopqwooh000001la8mbi2im9"`),
    (),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.cuid

  t->U.assertThrows(
    () => "cifjhdsfhsd-invalid-cuid"->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("Invalid CUID"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.cuid(~message="Custom")

  t->U.assertThrows(
    () => "cifjhdsfhsd-invalid-cuid"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(schema->S.String.refinements, [{kind: Cuid, message: "Invalid CUID"}], ())
})
