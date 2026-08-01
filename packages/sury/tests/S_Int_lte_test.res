open Vitest

test("Successfully parses valid data", t => {
  let schema = S.int->S.lte(1)

  t->Assert.deepEqual(1->S.parseOrThrow(~to=schema), 1)
  t->Assert.deepEqual(-1->S.parseOrThrow(~to=schema), -1)
})

test("Fails to parse invalid data", t => {
  let schema = S.int->S.lte(1)

  t->U.assertThrowsMessage(
    () => 1234->S.parseOrThrow(~to=schema),
    `Expected int32 <= 1, received 1234`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.int->S.lte(1)

  t->Assert.deepEqual(1->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1`))
  t->Assert.deepEqual(-1->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`-1`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.int->S.lte(1)

  t->U.assertThrowsMessage(
    () => 1234->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected int32 <= 1, received 1234`,
  )
})

test("Returns custom error message", t => {
  let schema = S.int->S.lte(~message="Custom", 1)

  t->U.assertThrowsMessage(() => 12->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.int->S.lte(1)

  switch schema {
  | Number({maximum, ?errorMessage}) => {
      t->Assert.deepEqual(maximum, 1.)
      t->Assert.deepEqual(errorMessage, None)
    }
  | _ => t->Assert.fail("Expected Number schema with maximum")
  }
})
