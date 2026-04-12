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

// KNOWN BUG: `noValidation` on a union case silently breaks dispatch.
//
// `literalDecoder` short-circuits when `expectedSchema.noValidation` is set
// (Sury.res:2001) and emits no check at all — so there's nothing for the
// union discriminant hoister to lift, and that case becomes a catch-all
// that swallows every input ahead of subsequent cases.
//
// The `noValidation`-bypass comment on `emitValidation` is at the wrong
// layer to fix this (by the time union codegen runs, the literal's val has
// no checks to preserve). Proper fix would be for `literalDecoder` to emit
// its equality check regardless of `noValidation` when the val ends up in
// a union, or for `S.noValidation` on a literal inside a union to be
// rejected at schema construction time.
test("Union dispatch still works when a case has noValidation", t => {
  let schema = S.union([
    S.literal("a")->S.noValidation(true),
    S.literal("b"),
  ])

  t->Assert.deepEqual("a"->S.parseOrThrow(schema), "a")
  // BUG: returns "a" instead of "b" — first case becomes catch-all.
  t->Assert.deepEqual("b"->S.parseOrThrow(schema), "b")
  t->U.assertThrowsMessage(
    () => "c"->S.parseOrThrow(schema),
    `Expected "a" | "b", received "c"`,
  )
})
