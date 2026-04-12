open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    "123e4567-e89b-12d3-a456-426614174000"->S.parseOrThrow(~to=schema),
    "123e4567-e89b-12d3-a456-426614174000",
  )
})

test("Successfully parses uuid V7", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    "019122ba-bb79-75ef-9a97-190f1effbb54"->S.parseOrThrow(~to=schema),
    "019122ba-bb79-75ef-9a97-190f1effbb54",
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.uuid

  t->U.assertThrowsMessage(() => "123e4567"->S.parseOrThrow(~to=schema), `Invalid UUID`)
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.uuid

  t->Assert.deepEqual(
    "123e4567-e89b-12d3-a456-426614174000"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"123e4567-e89b-12d3-a456-426614174000"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.uuid

  t->U.assertThrowsMessage(() => "123e4567"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Invalid UUID`)
})

test("Returns custom error message", t => {
  let schema = S.string->S.uuid(~message="Custom")

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Custom`)
})
