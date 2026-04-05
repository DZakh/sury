open Ava

test("Successfully parses valid Date", t => {
  let date = Date.fromString("2024-01-01T00:00:00Z")
  t->Assert.deepEqual(date->S.parseOrThrow(S.date), date)
})

test("Successfully parses Date with time", t => {
  let date = Date.fromString("2024-06-15T12:30:45.123Z")
  t->Assert.deepEqual(date->S.parseOrThrow(S.date), date)
})

test("Fails to parse string", t => {
  t->U.assertThrowsMessage(
    () => %raw(`"2024-01-01"`)->S.parseOrThrow(S.date),
    `Expected Date, received "2024-01-01"`,
  )
})

test("Fails to parse number", t => {
  t->U.assertThrowsMessage(
    () => %raw(`123`)->S.parseOrThrow(S.date),
    `Expected Date, received 123`,
  )
})

test("Fails to parse Invalid Date", t => {
  t->U.assertThrowsMessage(
    () => %raw(`new Date("invalid")`)->S.parseOrThrow(S.date),
    `Expected Date, received [object Date]`,
  )
})

test("Successfully reverse converts", t => {
  let date = Date.fromString("2024-01-01T00:00:00Z")
  t->Assert.deepEqual(date->S.reverseConvertOrThrow(S.date), date->Obj.magic)
})

test("Schema has instance tag and Date class", t => {
  switch S.date->Obj.magic {
  | S.Instance({class: c}) => t->Assert.is(c, %raw(`Date`))
  | _ => t->Assert.fail("Expected instance schema")
  }
})
