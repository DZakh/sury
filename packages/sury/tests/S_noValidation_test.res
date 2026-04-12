open Ava

test("Successfully parses", t => {
  let schema = S.string
  let schemaWithoutTypeValidation = schema->S.noValidation(true)

  t->U.assertThrowsMessage(() => 1->S.parseOrThrow(~to=schema), `Expected string, received 1`)
  t->Assert.deepEqual(1->S.parseOrThrow(~to=schemaWithoutTypeValidation), %raw(`1`))
})

test("Works for literals", t => {
  let schema = S.literal("foo")
  let schemaWithoutTypeValidation = schema->S.noValidation(true)

  t->U.assertThrowsMessage(() => 1->S.parseOrThrow(~to=schema), `Expected "foo", received 1`)
  t->Assert.deepEqual(1->S.parseOrThrow(~to=schemaWithoutTypeValidation), "foo")
  t->U.assertCompiledCode(~schema=schemaWithoutTypeValidation, ~op=#Parse, `i=>{return "foo"}`)
})
