open Vitest

test("Successfully parses valid data", t => {
  let schema = S.float->S.multipleOf(2.)

  t->Assert.deepEqual(4.->S.parseOrThrow(~to=schema), 4.)
  t->Assert.deepEqual(0.->S.parseOrThrow(~to=schema), 0.)
  t->Assert.deepEqual(-4.->S.parseOrThrow(~to=schema), -4.)
})

test("Fails to parse invalid data", t => {
  let schema = S.float->S.multipleOf(2.)

  t->U.assertThrowsMessage(() => 3->S.parseOrThrow(~to=schema), `Expected multiple of 2`)
})

test("Works on bigint", t => {
  let schema = S.bigint->S.multipleOf(2n)

  t->Assert.deepEqual(4n->S.parseOrThrow(~to=schema), 4n)
  t->U.assertThrowsMessage(() => 3n->S.parseOrThrow(~to=schema), `Expected multiple of 2n`)
})

test("Works on integer, which accepts values beyond int32", t => {
  let schema = S.integer->S.multipleOf(2.)

  t->Assert.deepEqual(3000000000.->S.parseOrThrow(~to=schema), 3000000000.)
  t->U.assertThrowsMessage(
    () => 2.5->S.parseOrThrow(~to=schema),
    `Expected integer, received 2.5`,
  )
})

test("Returns custom error message", t => {
  let schema = S.float->S.multipleOf(~message="Custom", 2.)

  t->U.assertThrowsMessage(() => 3.->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.float->S.multipleOf(2.)

  switch schema {
  | Number({multipleOf, ?errorMessage}) => {
      t->Assert.deepEqual(multipleOf, 2.)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected Number schema with multipleOf")
  }
})
