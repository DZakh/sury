open Ava

module Common = {
  let value = (123, true)
  let any = %raw(`[123, true]`)
  let invalidAny = %raw(`[123]`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.tuple2(S.int, S.bool)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected [int32, boolean], received [123]`,
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidTypeAny->S.parseOrThrow(~to=schema),
      `Expected [int32, boolean], received "Hello world!"`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })
}
