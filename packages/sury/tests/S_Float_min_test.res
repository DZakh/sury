open Ava

test("Successfully parses valid data", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(1.->S.parseOrThrow(~to=schema), 1.)
  t->Assert.deepEqual(1234.->S.parseOrThrow(~to=schema), 1234.)
})

test("Fails to parse invalid data", t => {
  let schema = S.float->S.floatMin(1.)

  t->U.assertThrowsMessage(
    () => 0->S.parseOrThrow(~to=schema),
    `Number must be greater than or equal to 1`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(1.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1`))
  t->Assert.deepEqual(1234.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1234`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.float->S.floatMin(1.)

  t->U.assertThrowsMessage(
    () => 0.->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Number must be greater than or equal to 1`,
  )
})

test("Returns custom error message", t => {
  let schema = S.float->S.floatMin(~message="Custom", 1.)

  t->U.assertThrowsMessage(() => 0.->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.float->S.floatMin(1.)

  t->Assert.deepEqual(
    schema->S.Float.refinements,
    [{kind: Min({value: 1.}), message: "Number must be greater than or equal to 1"}],
  )
})
