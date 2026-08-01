open Vitest

module Common = {
  let value = 123
  let any = %raw(`[123]`)
  let invalidAny = %raw(`[123, true]`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.tuple1(S.int)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse extra item in strict mode", t => {
    let schema = factory()->S.strict

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected [int32], received [123, true]`,
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected [int32], received "Hello world!"`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })
}
