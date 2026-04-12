open Ava

S.enableCuid()

test("Successfully parses valid data", t => {
  let schema = S.cuid

  t->Assert.deepEqual(
    "ckopqwooh000001la8mbi2im9"->S.parseOrThrow(~to=schema),
    "ckopqwooh000001la8mbi2im9",
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.cuid

  t->U.assertThrowsMessage(() => "cifjhdsfhsd-invalid-cuid"->S.parseOrThrow(~to=schema), `Invalid CUID`)
})

test("Successfully serializes valid value", t => {
  let schema = S.cuid

  t->Assert.deepEqual(
    "ckopqwooh000001la8mbi2im9"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"ckopqwooh000001la8mbi2im9"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.cuid

  t->U.assertThrowsMessage(
    () => "cifjhdsfhsd-invalid-cuid"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Invalid CUID`,
  )
})

test("Returns custom error message", t => {
  // Custom message is no longer supported for standalone schemas
  // CUID validation always uses the default message
  let schema = S.cuid

  t->U.assertThrowsMessage(() => "cifjhdsfhsd-invalid-cuid"->S.parseOrThrow(~to=schema), `Invalid CUID`)
})

test("Reflects format on schema", t => {
  let schema = S.cuid

  t->Assert.deepEqual((schema->S.untag).format, Some(Cuid))
  switch schema {
  | String({format}) => t->Assert.deepEqual(format, Cuid)
  | _ => t->Assert.fail("Expected String with format Cuid")
  }
})
