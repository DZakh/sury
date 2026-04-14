open Ava

S.enableUuid()

test("Successfully parses valid data", t => {
  let schema = S.uuid

  t->Assert.deepEqual(
    "123e4567-e89b-12d3-a456-426614174000"->S.parseOrThrow(~to=schema),
    "123e4567-e89b-12d3-a456-426614174000",
  )
})

test("Successfully parses uuid V7", t => {
  let schema = S.uuid

  t->Assert.deepEqual(
    "019122ba-bb79-75ef-9a97-190f1effbb54"->S.parseOrThrow(~to=schema),
    "019122ba-bb79-75ef-9a97-190f1effbb54",
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.uuid

  t->U.assertThrowsMessage(() => "123e4567"->S.parseOrThrow(~to=schema), `Expected uuid, received "123e4567"`)
})

test("Successfully serializes valid value", t => {
  let schema = S.uuid

  t->Assert.deepEqual(
    "123e4567-e89b-12d3-a456-426614174000"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"123e4567-e89b-12d3-a456-426614174000"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.uuid

  t->U.assertThrowsMessage(() => "123e4567"->S.decodeOrThrow(~from=schema, ~to=S.unknown), `Expected uuid, received "123e4567"`)
})

test("Custom error message via S.meta", t => {
  let schema = S.uuid->S.meta({errorMessage: {format: "Custom"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Reflects format on schema", t => {
  let schema = S.uuid

  t->Assert.deepEqual((schema->S.untag).format, Some(Uuid))
  switch schema {
  | String({format}) => t->Assert.deepEqual(format, Uuid)
  | _ => t->Assert.fail("Expected String with format Uuid")
  }
})
