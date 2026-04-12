open Ava

test("Successfully parses", t => {
  let schema = S.string
  let schemaWithoutTypeValidation = schema->S.noValidation(true)

  t->U.assertThrowsMessage(() => 1->S.parseOrThrow(schema), `Expected string, received 1`)
  t->Assert.deepEqual(1->S.parseOrThrow(schemaWithoutTypeValidation), %raw(`1`))
})

test("Works for literals", t => {
  let schema = S.literal("foo")
  let schemaWithoutTypeValidation = schema->S.noValidation(true)

  t->U.assertThrowsMessage(() => 1->S.parseOrThrow(schema), `Expected "foo", received 1`)
  t->Assert.deepEqual(1->S.parseOrThrow(schemaWithoutTypeValidation), "foo")
  t->U.assertCompiledCode(~schema=schemaWithoutTypeValidation, ~op=#Parse, `i=>{return "foo"}`)
})

test("Union dispatch still works when a case has noValidation", t => {
  let schema = S.union([
    S.literal("a")->S.noValidation(true),
    S.literal("b"),
  ])

  t->Assert.deepEqual("a"->S.parseOrThrow(schema), "a")
  t->Assert.deepEqual("b"->S.parseOrThrow(schema), "b")
  t->U.assertThrowsMessage(
    () => "c"->S.parseOrThrow(schema),
    `Expected "a" | "b", received "c"`,
  )
})
