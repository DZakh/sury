open Ava

module Common = {
  let value = 123
  let any = %raw(`[123]`)
  let invalidAny = %raw(`[123, true]`)
  let invalidTypeAny = %raw(`"Hello world!"`)
  let factory = () => S.tuple1(S.int)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
  })

  test("Fails to parse extra item in strict mode", t => {
    let schema = factory()->S.strict

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: schema->S.castToUnknown,
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
        code: InvalidType({expected: schema->S.castToUnknown, received: invalidTypeAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any)
  })
}
