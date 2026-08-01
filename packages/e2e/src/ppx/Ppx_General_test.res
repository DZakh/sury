open Vitest
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
type stringWithDefaultAndMatches = @s.default("https://example.com") @s.matches(S.url) string
test("Creates schema with default using @s.matches", t => {
  t->assertEqualSchemas(
    stringWithDefaultAndMatchesSchema,
    S.option(S.url)->S.Option.getOr("https://example.com"),
  )
})

@schema
type stringWithDefaultNullAndMatches = @s.default("https://example.com") @s.null @s.matches(S.url) string
test("Creates schema with default null using @s.matches", t => {
  t->assertEqualSchemas(
    stringWithDefaultNullAndMatchesSchema,
    S.nullAsOption(S.url)->S.Option.getOr("https://example.com"),
  )
})

@schema
type ignoredNullWithMatches = @s.null @s.matches(S.option(S.string)) option<string>
test("@s.null doesn't override @s.matches(S.option(_))", t => {
  t->assertEqualSchemas(ignoredNullWithMatchesSchema, S.option(S.string))
})

@schema
type stringWithWith = @s.with(S.trim) string
test("Creates schema with @s.with transform", t => {
  t->assertEqualSchemas(stringWithWithSchema, S.string->S.trim)
})

@schema
type stringWithMultipleWith = @s.with(S.trim) @s.with(s => s->S.min(1)) @s.with(s => s->S.max(5)) string
test("Applies multiple @s.with transforms in order of appearance", t => {
  t->assertEqualSchemas(stringWithMultipleWithSchema, S.string->S.trim->S.min(1)->S.max(5))
})

@schema @s.with(s => s->S.meta({description: "A user"}))
type userWithWith = {
  name: @s.with(s => s->S.length(2)) string,
  age: @s.with(s => s->S.min(18)) int,
}
test("Applies @s.with on type declaration and on fields of different types", t => {
  t->assertEqualSchemas(
    userWithWithSchema,
    S.schema(s => {
      name: s.matches(S.string->S.length(2)),
      age: s.matches(S.int->S.min(18)),
    })->S.meta({description: "A user"}),
  )
})

@schema
type stringWithDefaultAndWith = @s.default("Foo") @s.with(S.trim) string
test("Combines @s.with with @s.default", t => {
  t->assertEqualSchemas(
    stringWithDefaultAndWithSchema,
    S.option(S.string)->S.Option.getOr("Foo")->S.trim,
  )
})

@schema
type intWithWithPlaceholder = @s.with(S.min(_, 1)) @s.with(S.max(_, 5)) int
test("Applies @s.with with partial application placeholder", t => {
  t->assertEqualSchemas(intWithWithPlaceholderSchema, S.int->S.min(1)->S.max(5))
})

@schema
type recordWithOptionalWithField = {maybe?: @s.with(S.trim) string}
test("Applies @s.with on an optional field", t => {
  t->assertEqualSchemas(
    recordWithOptionalWithFieldSchema,
    S.schema(s => {
      maybe: ?s.matches(S.option(S.string->S.trim)),
    }),
  )
})

// Regression: the pin must not capture user type variables of any name
@schema
type paramWithWith<'sWith1> = @s.with(s => s->S.meta({description: "wrapped"})) array<'sWith1>
test("Applies @s.with on a parametrized type", t => {
  t->assertEqualSchemas(
    paramWithWithSchema(S.string),
    S.array(S.string)->S.meta({description: "wrapped"}),
  )
})
