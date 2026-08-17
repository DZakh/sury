open Vitest

test("Keeps operation of the error passed to S.Error.throw", t => {
  let schema = S.array(
    S.string->S.to(
      S.any,
      ~custom={
        decode: Sync(
          _ =>
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
        ),
        encode: Never,
      },
    ),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at ["0"]["a"]["b"]: User error`,
  )
})

// These two used to fail from the transformer body — outside any parser —
// through the effect ctx's `fail`, which baked the whole path in at build time.
// That ctx is gone: a transform fails by throwing from the parser it runs in,
// and the path it was reached through is prepended to the one the error names.
test("Prepends the field path to a thrown error", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.string->S.to(
        S.any,
        ~custom={
          decode: Sync(_ => U.fail("User error", ~path=S.Path.fromArray(["a", "b"]))),
          encode: Never,
        },
      ),
    )
  )

  t->U.assertThrowsMessage(
    () => {"field": "Hello world!"}->S.parseOrThrow(~to=schema),
    `Failed at ["field"]["a"]["b"]: User error`,
  )
})

test("Prepends the field and item path to a thrown error inside an array", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.array(
        S.string->S.to(
          S.any,
          ~custom={
            decode: Sync(_ => U.fail("User error", ~path=S.Path.fromArray(["a", "b"]))),
            encode: Never,
          },
        ),
      ),
    )
  )

  t->U.assertThrowsMessage(
    () => {"field": ["Hello world!"]}->S.parseOrThrow(~to=schema),
    `Failed at ["field"]["0"]["a"]["b"]: User error`,
  )
})
