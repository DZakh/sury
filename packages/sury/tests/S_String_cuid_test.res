open Ava

test("Successfully parses valid data", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(
    "ckopqwooh000001la8mbi2im9"->S.parseOrThrow(~to=schema),
    "ckopqwooh000001la8mbi2im9",
  )
})

test("Fails to parse invalid data", t => {
  let schema = S.string->S.cuid

  t->U.assertThrowsMessage(() => "cifjhdsfhsd-invalid-cuid"->S.parseOrThrow(~to=schema), `Invalid CUID`)
})

test("Successfully serializes valid value", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(
    "ckopqwooh000001la8mbi2im9"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"ckopqwooh000001la8mbi2im9"`),
  )
})

test("Fails to serialize invalid value", t => {
  let schema = S.string->S.cuid

  t->U.assertThrowsMessage(
    () => "cifjhdsfhsd-invalid-cuid"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Invalid CUID`,
  )
})

test("Returns custom error message", t => {
  let schema = S.string->S.cuid(~message="Custom")

  t->U.assertThrowsMessage(() => "cifjhdsfhsd-invalid-cuid"->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.string->S.cuid

  t->Assert.deepEqual(schema->S.String.refinements, [{kind: Cuid, message: "Invalid CUID"}])
})
