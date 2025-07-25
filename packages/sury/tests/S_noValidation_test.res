open Ava

test("Successfully parses", t => {
  let schema = S.string
  let schemaWithoutTypeValidation = schema->S.noValidation(true)

  t->U.assertThrows(
    () => 1->S.parseOrThrow(schema),
    {
      code: S.InvalidType({
        expected: schema->S.castToUnknown,
        received: %raw(`1`),
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  t->Assert.deepEqual(1->S.parseOrThrow(schemaWithoutTypeValidation), %raw(`1`))
})

test("Works for literals", t => {
  let schema = S.literal("foo")
  let schemaWithoutTypeValidation = schema->S.noValidation(true)

  t->U.assertThrows(
    () => 1->S.parseOrThrow(schema),
    {
      code: S.InvalidType({
        expected: schema->S.castToUnknown,
        received: %raw(`1`),
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  t->Assert.deepEqual(1->S.parseOrThrow(schemaWithoutTypeValidation), %raw(`1`))
  t->U.assertCompiledCodeIsNoop(~schema=schemaWithoutTypeValidation, ~op=#Parse)
})
