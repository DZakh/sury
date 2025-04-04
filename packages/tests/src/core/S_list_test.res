open Ava

module CommonWithNested = {
  let value = list{"Hello world!", ""}
  let any = %raw(`["Hello world!", ""]`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`["Hello world!", 1]`)
  let factory = () => S.list(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse", t => {
    let schema = factory()

    switch invalidAny->S.parseOrThrow(schema) {
    | _ => t->Assert.fail("Unexpected result.")
    | exception S.SchemaError(e) => {
        t->Assert.deepEqual(e.flag, S.Flag.typeValidation, ())
        t->Assert.deepEqual(e.path, S.Path.empty, ())
        switch e.code {
        | InvalidType({expected, received}) => {
            t->Assert.deepEqual(received, invalidAny, ())
            t->U.unsafeAssertEqualSchemas(expected, schema)
          }
        | _ => t->Assert.fail("Unexpected code.")
        }
      }
    }
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertRaised(
      () => nestedInvalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.string->S.toUnknown, received: 1->Obj.magic}),
        operation: Parse,
        path: S.Path.fromArray(["1"]),
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })
}

test("Successfully parses list of optional items", t => {
  let schema = S.list(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`["a", undefined, undefined, "b"]`)->S.parseOrThrow(schema),
    list{Some("a"), None, None, Some("b")},
    (),
  )
})
