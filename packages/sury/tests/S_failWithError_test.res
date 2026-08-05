open Vitest

test("Keeps operation of the error passed to S.Error.throw", t => {
  let schema = S.array(
    S.string->S.transform(() => {
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
    `Failed at [0].a.b: User error`,
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
      S.string->S.transform(
        () => {
          parser: _ => U.fail("User error", ~path=S.Path.fromArray(["a", "b"])),
        },
      ),
    )
  )

  t->U.assertThrowsMessage(
    () => {"field": "Hello world!"}->S.parseOrThrow(~to=schema),
    `Failed at field.a.b: User error`,
  )
})

test("Prepends the field and item path to a thrown error inside an array", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.array(
        S.string->S.transform(
          () => {
            parser: _ => U.fail("User error", ~path=S.Path.fromArray(["a", "b"])),
          },
        ),
      ),
    )
  )

  t->U.assertThrowsMessage(
    () => {"field": ["Hello world!"]}->S.parseOrThrow(~to=schema),
    `Failed at field[0].a.b: User error`,
  )
})

// Throwing from the transformer body itself escapes schema compilation rather
// than becoming a parse failure — nothing is being parsed yet, so there is no
// operation context to attribute it to and no path to reach it through. Pinned
// because it is the case the removed effect ctx used to paper over.
test("A throw from the transformer body escapes compilation", t => {
  let schema = S.object(s => s.field("field", S.string->S.transform(() => U.fail("Built wrong"))))

  t->U.assertThrowsMessage(() => S.parser(~to=schema)->ignore, `Built wrong`)
})
