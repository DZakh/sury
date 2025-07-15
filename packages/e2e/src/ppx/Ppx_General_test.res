open Ava
open U

@schema
type t = string
test("Creates schema with the name schema from t type", t => {
  t->assertEqualSchemas(schema, S.string)
})

@schema
type foo = int
test("Creates schema with the type name and schema at the for non t types", t => {
  t->assertEqualSchemas(fooSchema, S.int)
})

type bar = bool

@schema
type reusedTypes = (t, foo, @s.matches(S.bool) bar, float)
test("Can reuse schemas from other types", t => {
  t->assertEqualSchemas(
    reusedTypesSchema,
    S.schema(s => (s.matches(schema), s.matches(fooSchema), s.matches(S.bool), s.matches(S.float))),
  )
})

// TODO: Support recursive schemas

@schema
type stringWithDefault = @s.default("Foo") string
test("Creates schema with default", t => {
  t->assertEqualSchemas(stringWithDefaultSchema, S.option(S.string)->S.Option.getOr("Foo"))
})

@schema
type stringWithDefaultAndMatches = @s.default("Foo") @s.matches(S.string->S.url) string
test("Creates schema with default using @s.matches", t => {
  t->assertEqualSchemas(
    stringWithDefaultAndMatchesSchema,
    S.option(S.string->S.url)->S.Option.getOr("Foo"),
  )
})

@schema
type stringWithDefaultNullAndMatches = @s.default("Foo") @s.null @s.matches(S.string->S.url) string
test("Creates schema with default null using @s.matches", t => {
  t->assertEqualSchemas(
    stringWithDefaultNullAndMatchesSchema,
    S.null(S.string->S.url)->S.Option.getOr("Foo"),
  )
})

@schema
type ignoredNullWithMatches = @s.null @s.matches(S.option(S.string)) option<string>
test("@s.null doesn't override @s.matches(S.option(_))", t => {
  t->assertEqualSchemas(ignoredNullWithMatchesSchema, S.option(S.string))
})
