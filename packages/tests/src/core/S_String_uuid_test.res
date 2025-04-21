open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    "123e4567-e89b-12d3-a456-426614174000"->S.parseOrThrow(schema),
    "123e4567-e89b-12d3-a456-426614174000",
    (),
  )
})

test("Successfully parses uuid V7", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    "019122ba-bb79-75ef-9a97-190f1effbb54"->S.parseOrThrow(schema),
    "019122ba-bb79-75ef-9a97-190f1effbb54",
    (),
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.uuid

  t->U.assertThrows(
    () => "123e4567"->S.parseOrThrow(schema),
    {code: OperationFailed("Invalid UUID"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    "123e4567-e89b-12d3-a456-426614174000"->S.reverseConvertOrThrow(schema),
    %raw(`"123e4567-e89b-12d3-a456-426614174000"`),
    (),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.uuid

  t->U.assertThrows(
    () => "123e4567"->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("Invalid UUID"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.uuid(~message="Custom")

  t->U.assertThrows(
    () => "abc"->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})
