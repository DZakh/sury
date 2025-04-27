open Ava

module Common = {
  let value = (123, true)
  let any = %raw(`[123, true]`)
  let invalidAny = %raw(`[123]`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.tuple2(S.int, S.bool)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse invalid value", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: schema->S.toUnknown,
          received: invalidAny,
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse invalid type", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidTypeAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.toUnknown, received: invalidTypeAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })
}
