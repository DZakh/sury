open Ava

module Common = {
  let value = None
  let any = %raw(`undefined`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.string->S.option->S.deprecate("Some warning")

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.toUnknown, received: invalidAny}),
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

test("Successfully parses primitive", t => {
  let schema = S.bool->S.option->S.deprecate("Deprecated")

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(schema), Some(true), ())
})

test("Successfully parses undefined", t => {
  let schema = S.bool->S.option->S.deprecate("Deprecated")

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
})

test("Fails to parse null", t => {
  let schema = S.bool->S.option->S.deprecate("Deprecated")

  t->U.assertRaised(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`null`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully parses null for deprecated nullable schema", t => {
  let schema = S.null(S.bool)->S.option->S.deprecate("Deprecated")

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None, ())
})
