open Vitest

test("Successfully parses valid data", t => {
  let schema = S.float->S.gte(1.)

  t->Assert.deepEqual(1.->S.parseOrThrow(~to=schema), 1.)
  t->Assert.deepEqual(1234.->S.parseOrThrow(~to=schema), 1234.)
})

test("Fails to parse invalid data", t => {
  let schema = S.float->S.gte(1.)

  t->U.assertThrowsMessage(() => 0->S.parseOrThrow(~to=schema), `Expected number >= 1, received 0`)
})

test("Successfully serializes valid value", t => {
  let schema = S.float->S.gte(1.)

  t->Assert.deepEqual(1.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1`))
  t->Assert.deepEqual(1234.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1234`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.float->S.gte(1.)

  t->U.assertThrowsMessage(
    () => 0.->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected number >= 1, received 0`,
  )
})

test("Returns custom error message", t => {
  let schema = S.float->S.gte(~message="Custom", 1.)

  t->U.assertThrowsMessage(() => 0.->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.float->S.gte(1.)

  switch schema {
  | Number({minimum, ?errorMessage}) => {
      t->Assert.deepEqual(minimum, 1.)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected Number schema with minimum")
  }
})
