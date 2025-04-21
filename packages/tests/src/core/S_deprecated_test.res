open Ava

test("Initially there's no deprecation", t => {
  let schema = S.string

  t->Assert.deepEqual((schema->S.untag).deprecated, None, ())
})

test("Set deprecation", t => {
  let original = S.string
  let withDeprecation = original->S.meta({deprecated: true, description: "Use number instead"})

  t->Assert.deepEqual((withDeprecation->S.untag).deprecated, Some(true), ())
  t->Assert.deepEqual((withDeprecation->S.untag).description, Some("Use number instead"), ())
  t->Assert.deepEqual(
    (original->S.untag).deprecated,
    None,
    (),
    ~message="Original schema is not mutated",
  )
})

test("Transforms don't remove deprecation", t => {
  let schema =
    S.string->S.meta({deprecated: true, description: "Original schema is not mutated."})->S.trim

  t->Assert.deepEqual((schema->S.untag).deprecated, Some(true), ())
  t->Assert.deepEqual((schema->S.untag).description, Some("Original schema is not mutated."), ())
})

test("Deprecated is a metadata only and doesn't make the field optional", t => {
  let schema = S.string->S.meta({deprecated: true, description: "Use number instead."})

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(typeof i!=="string"){e[0](i)}return i}`)
})
