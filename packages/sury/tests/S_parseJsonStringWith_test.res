open Ava

test("Successfully parses", t => {
  let schema = S.bool

  t->Assert.deepEqual("true"->S.parseJsonStringOrThrow(schema), true)
})

test("Successfully parses unknown", t => {
  let schema = S.unknown

  t->Assert.deepEqual("true"->S.parseJsonStringOrThrow(schema), true->Obj.magic)
})

test("Fails to parse JSON", t => {
  let schema = S.bool

  switch "123,"->S.parseJsonStringOrThrow(schema) {
  | _ => t->Assert.fail("Must return Error")
  | exception S.Error({code, flag, path}) => {
      t->Assert.deepEqual(flag, S.Flag.typeValidation)
      t->Assert.deepEqual(path, S.Path.empty)
      switch code {
      // Different errors for different Node.js versions
      | OperationFailed("Unexpected token , in JSON at position 3")
      | OperationFailed("Unexpected non-whitespace character after JSON at position 3")
      | OperationFailed(
        "Unexpected non-whitespace character after JSON at position 3 (line 1 column 4)",
      ) => ()
      | _ => t->Assert.fail("Code must be OperationFailed")
      }
    }
  }
})

test("Fails to parse", t => {
  let schema = S.bool

  t->U.assertThrows(
    () => "123"->S.parseJsonStringOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: Obj.magic(123)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})
