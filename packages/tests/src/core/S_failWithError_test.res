open Ava

test("Keeps operation of the error passed to S.Error.raise", t => {
  let schema = S.array(
    S.string->S.transform(_ => {
      parser: _ =>
        S.Error.raise(
          U.error({
            code: OperationFailed("User error"),
            operation: ReverseConvert,
            path: S.Path.fromArray(["a", "b"]),
          }),
        ),
    }),
  )

  t->U.assertThrows(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: ReverseConvert,
      path: S.Path.fromArray(["0", "a", "b"]),
    },
  )
})

test("Works with failing outside of the parser", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.string->S.transform(s => s.fail("User error", ~path=S.Path.fromArray(["a", "b"]))),
    )
  )

  t->U.assertThrows(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: Parse,
      path: S.Path.fromLocation("field")->S.Path.concat(S.Path.fromArray(["a", "b"])),
    },
  )
})

test("Works with failing outside of the parser inside of array", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.array(S.string->S.transform(s => s.fail("User error", ~path=S.Path.fromArray(["a", "b"])))),
    )
  )

  t->U.assertThrows(
    () => ["Hello world!"]->S.parseOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: Parse,
      path: S.Path.fromLocation("field")
      ->S.Path.concat(S.Path.dynamic)
      ->S.Path.concat(S.Path.fromArray(["a", "b"])),
    },
  )
})
