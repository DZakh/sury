open Ava
open U

@schema
type rating =
  | @as("G") GeneralAudiences
  | @as("PG") ParentalGuidanceSuggested
  | @as("PG13") ParentalStronglyCautioned
  | @as("R") Restricted
@schema
type film = {
  @as("Id")
  id: float,
  @as("Title")
  title: string,
  @as("Tags")
  tags: @s.default([]) array<string>,
  @as("Rating")
  rating: rating,
  @as("Age")
  deprecatedAgeRestriction: @s.meta({description: "Use rating instead", deprecated: true})
  option<int>,
}

test("Main example", t => {
  t->assertEqualSchemas(
    filmSchema,
    S.schema(s => {
      id: s.matches(S.float),
      title: s.matches(S.string),
      tags: s.matches(S.option(S.array(S.string))->S.Option.getOr([])),
      rating: s.matches(
        S.union([
          S.literal(GeneralAudiences),
          S.literal(ParentalGuidanceSuggested),
          S.literal(ParentalStronglyCautioned),
          S.literal(Restricted),
        ]),
      ),
      deprecatedAgeRestriction: s.matches(
        S.option(S.int)->S.meta({description: "Use rating instead", deprecated: true}),
      ),
    }),
  )
})

@schema
type matches = @s.matches(S.string->S.url) string
test("@s.matches", t => {
  t->assertEqualSchemas(matchesSchema, S.string->S.url)
})

@schema
type default = @s.default("Unknown") string
test("@s.default", t => {
  t->assertEqualSchemas(defaultSchema, S.option(S.string)->S.Option.getOr("Unknown"))
})

@schema
type defaultWith = @s.defaultWith(() => []) array<string>
test("@s.defaultWith", t => {
  t->assertEqualSchemas(
    defaultWithSchema,
    S.option(S.array(S.string))->S.Option.getOrWith(() => []),
  )
})

@schema
type null = @s.null option<string>
test("@s.null", t => {
  t->assertEqualSchemas(nullSchema, S.null(S.string))
})

@schema
type nullWithDefault = @s.null @s.default("Unknown") string
test("@s.null with @s.default", t => {
  t->assertEqualSchemas(nullWithDefaultSchema, S.null(S.string)->S.Option.getOr("Unknown"))
})

@schema
type nullable = @s.nullable option<string>
test("@s.nullable", t => {
  t->assertEqualSchemas(nullableSchema, S.nullableAsOption(S.string))
})

@schema
type nullableWithDefault = @s.nullable @s.default("Unknown") string
test("@s.nullable with @s.default", t => {
  t->assertEqualSchemas(
    nullableWithDefaultSchema,
    S.nullableAsOption(S.string)->S.Option.getOr("Unknown"),
  )
})

@schema
type deprecated = @s.meta({description: "Will be removed in APIv2", deprecated: true}) string
test("@s.deprecated", t => {
  t->assertEqualSchemas(
    deprecatedSchema,
    S.string->S.meta({description: "Will be removed in APIv2", deprecated: true}),
  )
})

@schema
type describe = @s.meta({description: "A useful bit of text, if you know what to do with it."})
string
test("@s.description", t => {
  t->assertEqualSchemas(
    describeSchema,
    S.string->S.meta({description: "A useful bit of text, if you know what to do with it."}),
  )
})
