open Ava

S.enablePort()

test("Successfully parses valid data", t => {
  let schema = S.port

  t->Assert.deepEqual(8080->S.parseOrThrow(~to=schema), 8080)
})

test("Fails to parse invalid data", t => {
  let schema = S.port

  t->U.assertThrowsMessage(() => 65536->S.parseOrThrow(~to=schema), `Expected port, received 65536`)
})

test("Successfully serializes valid value", t => {
  let schema = S.port

  t->Assert.deepEqual(8080->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`8080`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.port

  t->U.assertThrowsMessage(
    () => -80->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected port, received -80`,
  )
})

test("Reflects format on schema", t => {
  let schema = S.port

  t->Assert.deepEqual((schema->S.untag).format, Some(Port))
  switch schema {
  | Number({format}) => t->Assert.deepEqual(format, Port)
  | _ => t->Assert.fail("Expected Number with format Port")
  }
})
