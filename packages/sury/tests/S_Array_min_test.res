open Ava

test("Successfully parses valid data", t => {
  let schema = S.array(S.int)->S.min(1)

  t->Assert.deepEqual([1]->S.parseOrThrow(~to=schema), [1])
  t->Assert.deepEqual([1, 2, 3, 4]->S.parseOrThrow(~to=schema), [1, 2, 3, 4])
})

test("Fails to parse invalid data", t => {
  let schema = S.array(S.int)->S.min(1)

  t->U.assertThrowsMessage(() => []->S.parseOrThrow(~to=schema), `Array must be 1 or more items long`)
})

test("Successfully serializes valid value", t => {
  let schema = S.array(S.int)->S.min(1)

  t->Assert.deepEqual([1]->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`[1]`))
  t->Assert.deepEqual([1, 2, 3, 4]->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`[1,2,3,4]`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.array(S.int)->S.min(1)

  t->U.assertThrowsMessage(
    () => []->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Array must be 1 or more items long`,
  )
})

test("Returns custom error message", t => {
  let schema = S.array(S.int)->S.min(~message="Custom", 1)

  t->U.assertThrowsMessage(() => []->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.array(S.int)->S.min(1)

  switch schema {
  | Array({minItems, errorMessage}) => {
      t->Assert.deepEqual(minItems, 1)
      t->Assert.deepEqual(
        errorMessage,
        {minItems: "Array must be 1 or more items long"},
      )
    }
  | _ => t->Assert.fail("Expected Array schema with minItems")
  }
})
