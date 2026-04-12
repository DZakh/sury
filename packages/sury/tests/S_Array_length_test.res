open Ava

test("Successfully parses valid data", t => {
  let schema = S.array(S.int)->S.length(1)

  t->Assert.deepEqual([1]->S.parseOrThrow(~to=schema), [1])
})

test("Fails to parse invalid data", t => {
  let schema = S.array(S.int)->S.length(1)

  t->U.assertThrowsMessage(() => []->S.parseOrThrow(~to=schema), `Array must be exactly 1 items long`)
  t->U.assertThrowsMessage(
    () => [1, 2, 3, 4]->S.parseOrThrow(~to=schema),
    `Array must be exactly 1 items long`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.array(S.int)->S.length(1)

  t->Assert.deepEqual([1]->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`[1]`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.array(S.int)->S.length(1)

  t->U.assertThrowsMessage(
    () => []->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Array must be exactly 1 items long`,
  )
  t->U.assertThrowsMessage(
    () => [1, 2, 3, 4]->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Array must be exactly 1 items long`,
  )
})

test("Returns custom error message", t => {
  let schema = S.array(S.int)->S.length(~message="Custom", 1)

  t->U.assertThrowsMessage(() => []->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.array(S.int)->S.length(1)

  switch schema {
  | Array({minItems: ?Some(minItems), maxItems: ?Some(maxItems), errorMessages: ?Some(errorMessages)}) => {
      t->Assert.deepEqual(minItems, 1)
      t->Assert.deepEqual(maxItems, 1)
      t->Assert.deepEqual(
        errorMessages->Js.Dict.get("minItems"),
        Some("Array must be exactly 1 items long"),
      )
    }
  | _ => t->Assert.fail("Expected Array schema with minItems and maxItems")
  }
})
