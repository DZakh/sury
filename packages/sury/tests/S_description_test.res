open Ava

test("Initially there's no description", t => {
  let schema = S.string

  t->Assert.deepEqual((schema->S.untag).description, None)
})

test("Set description", t => {
  let original = S.string
  let withDescription =
    original->S.meta({description: "A useful bit of text, if you know what to do with it."})

  t->Assert.deepEqual(
    (withDescription->S.untag).description,
    Some("A useful bit of text, if you know what to do with it."),
  )
  t->Assert.deepEqual(
    (original->S.untag).description,
    None,
    ~message="Original schema is not mutated",
  )
})

test("Transforms don't remove description", t => {
  let schema =
    S.string->S.meta({description: "A useful bit of text, if you know what to do with it."})->S.trim

  t->Assert.deepEqual(
    (schema->S.untag).description,
    Some("A useful bit of text, if you know what to do with it."),
  )
})
