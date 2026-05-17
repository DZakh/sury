open Ava

module CommonWithNested = {
  let value = list{"Hello world!", ""}
  let any = %raw(`["Hello world!", ""]`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`["Hello world!", 1]`)
  let factory = () => S.list(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected string[], received true`,
    )
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => nestedInvalidAny->S.parseOrThrow(~to=schema),
      `Failed at ["1"]: Expected string, received 1`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })
}

test("Successfully parses list of optional items", t => {
  let schema = S.list(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`["a", undefined, undefined, "b"]`)->S.parseOrThrow(~to=schema),
    list{Some("a"), None, None, Some("b")},
  )
})
