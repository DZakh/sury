open Ava

test("Successfully parses valid data", t => {
  let schema = S.int->S.max(1)

  t->Assert.deepEqual(1->S.parseOrThrow(~to=schema), 1)
  t->Assert.deepEqual(-1->S.parseOrThrow(~to=schema), -1)
})

test("Fails to parse invalid data", t => {
  let schema = S.int->S.max(1)

  t->U.assertThrowsMessage(
    () => 1234->S.parseOrThrow(~to=schema),
    `Number must be lower than or equal to 1`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.int->S.max(1)

  t->Assert.deepEqual(1->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1`))
  t->Assert.deepEqual(-1->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`-1`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.int->S.max(1)

  t->U.assertThrowsMessage(
    () => 1234->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Number must be lower than or equal to 1`,
  )
})

test("Returns custom error message", t => {
  let schema = S.int->S.max(~message="Custom", 1)

  t->U.assertThrowsMessage(() => 12->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.int->S.max(1)

  switch schema {
  | Number({maximum, errorMessages}) => {
      t->Assert.deepEqual(maximum, 1.)
      t->Assert.deepEqual(
        errorMessages,
        dict{"maximum": "Number must be lower than or equal to 1"},
      )
    }
  | _ => t->Assert.fail("Expected Number schema with maximum")
  }
})
