open Ava

let nullableSchema = innerSchema => {
  S.custom("Nullable", _ => {
    parser: unknown => {
      if unknown === %raw(`undefined`) || unknown === %raw(`null`) {
        None
      } else {
        Some(unknown->S.parseOrThrow(innerSchema))
      }
    },
    serializer: value => {
      switch value {
      | Some(innerValue) => innerValue->S.reverseConvertOrThrow(innerSchema)
      | None => %raw(`null`)
      }
    },
  })
}

test("Correctly parses custom schema", t => {
  let schema = nullableSchema(S.string)

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(schema), Some("Hello world!"), ())
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None, ())
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
  t->U.assertRaised(
    () => 123->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.toUnknown, received: %raw(`123`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Correctly serializes custom schema", t => {
  let schema = nullableSchema(S.string)

  t->Assert.deepEqual(
    Some("Hello world!")->S.reverseConvertOrThrow(schema),
    %raw(`"Hello world!"`),
    (),
  )
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`), ())
})

test("Reverses custom schema to unknown", t => {
  let schema = nullableSchema(S.string)

  t->U.assertEqualSchemas(schema->S.reverse, S.unknown)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = nullableSchema(S.string)
  t->U.assertReverseParsesBack(schema, Some("abc"))
})

test("Fails to serialize with user error", t => {
  let schema = S.custom("Test", s => {
    serializer: _ => s.fail("User error"),
  })

  t->U.assertRaised(
    () => None->S.reverseConvertOrThrow(schema),
    {code: OperationFailed("User error"), operation: ReverseConvert, path: S.Path.empty},
  )
})

test("Fails to serialize with serializer is missing", t => {
  let schema = S.custom("Test", _ => {
    parser: _ => (),
  })

  t->U.assertRaised(
    () => ()->S.reverseConvertOrThrow(schema),
    {
      code: InvalidOperation({description: "The S.custom serializer is missing"}),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

asyncTest("Parses with asyncParser", async t => {
  let schema = S.custom("Test", _ => {
    asyncParser: _ => Promise.resolve(),
  })

  t->Assert.deepEqual(await %raw(`undefined`)->S.parseAsyncOrThrow(schema), (), ())
})
