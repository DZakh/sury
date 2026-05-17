open Ava

test("Keeps operation of the error passed to S.Error.throw", t => {
  let schema = S.array(
    S.string->S.transform(_ => {
      parser: _ =>
        U.throwError(
          S.Error.make(
            InvalidInput({
              reason: "User error",
              path: S.Path.fromArray(["a", "b"]),
              expected: S.unknown,
              received: S.unknown,
            }),
          ),
        ),
    }),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at ["0"]["a"]["b"]: User error`,
  )
})

test("Works with failing outside of the parser", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.string->S.transform(
        s => {
          s.fail("User error", ~path=S.Path.fromArray(["a", "b"]))
        },
      ),
    )
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at ["field"]["a"]["b"]: User error`,
  )
})

test("Works with failing outside of the parser inside of array", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.array(
        S.string->S.transform(
          s => {
            s.fail("User error", ~path=S.Path.fromArray(["a", "b"]))
          },
        ),
      ),
    )
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at ["field"][]["a"]["b"]: User error`,
  )
})
