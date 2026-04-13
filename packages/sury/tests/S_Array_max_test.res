open Ava

test("Successfully parses valid data", t => {
  let schema = S.array(S.int)->S.max(1)

  t->Assert.deepEqual([1]->S.parseOrThrow(~to=schema), [1])
  t->Assert.deepEqual([]->S.parseOrThrow(~to=schema), [])
})

test("Fails to parse invalid data", t => {
  let schema = S.array(S.int)->S.max(1)

  t->U.assertThrowsMessage(
    () => [1, 2, 3, 4]->S.parseOrThrow(~to=schema),
    `Array must be 1 or fewer items long`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.array(S.int)->S.max(1)

  t->Assert.deepEqual([1]->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`[1]`))
  t->Assert.deepEqual([]->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`[]`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.array(S.int)->S.max(1)

  t->U.assertThrowsMessage(
    () => [1, 2, 3, 4]->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Array must be 1 or fewer items long`,
  )
})

test("Returns custom error message", t => {
  let schema = S.array(S.int)->S.max(~message="Custom", 1)

  t->U.assertThrowsMessage(() => [1, 2]->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.array(S.int)->S.max(1)

  switch schema {
  | Array({maxItems, errorMessage}) => {
      t->Assert.deepEqual(maxItems, 1)
      t->Assert.deepEqual(
        errorMessage,
        { maxItems:: "Array must be 1 or fewer items long"},
      )
    }
  | _ => t->Assert.fail("Expected Array schema with maxItems")
  }
})
