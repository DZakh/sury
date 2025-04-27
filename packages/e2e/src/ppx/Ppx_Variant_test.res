open Ava
open U

@schema
type variant = One | Two
test("Variant", t => {
  t->assertEqualSchemas(variantSchema, S.union([S.literal(One), S.literal(Two)]))
})

@schema
type variantWithSingleItem = Single
test("Variant with single item becomes a literal schema of the item", t => {
  t->assertEqualSchemas(variantWithSingleItemSchema, S.literal(Single))
})

@schema
type variantWithAlias = | @as(`하나`) One | Two
test("Variant with partial @as usage", t => {
  t->assertEqualSchemas(variantWithAliasSchema, S.union([S.literal(One), S.literal(Two)]))
})

@schema
type variantWithPayloads =
  Constant | SinglePayload(int) | TuplePayload(int, string) | RecordPayload({foo: float})
test("Variant with payloads", t => {
  t->assertEqualSchemas(
    variantWithPayloadsSchema,
    S.union([
      S.literal(Constant),
      S.schema(s => SinglePayload(s.matches(S.int))),
      S.schema(s => TuplePayload(s.matches(S.int), s.matches(S.string))),
      S.schema(s => RecordPayload({foo: s.matches(S.float)})),
    ]),
  )
})

@unboxed @schema
type unboxedVariant = Constant | Int(int) | String(string)
test("Unboxed variant", t => {
  t->assertEqualSchemas(
    unboxedVariantSchema,
    S.union([
      S.literal(Constant),
      S.schema(s => Int(s.matches(S.int))),
      S.schema(s => String(s.matches(S.string))),
    ]),
  )
})

@tag("kind") @schema
type taggedVariant =
  | @as("circle") Circle({radius: float})
  | @as("square") Square({x: float})
  | @as("triangle") Triangle({x: float, y: float})
test("Tagged variant", t => {
  t->assertEqualSchemas(
    taggedVariantSchema,
    S.union([
      S.schema(s => Circle({radius: s.matches(S.float)})),
      S.schema(s => Square({x: s.matches(S.float)})),
      S.schema(s => Triangle({x: s.matches(S.float), y: s.matches(S.float)})),
    ]),
  )
})

@schema @tag("type")
type taggedInlinedAlias = Foo({@as("Foo") foo: string}) | Bar({@as("Bar") bar: string})

test("Tagged variant with inlined alias", t => {
  t->assertEqualSchemas(
    taggedInlinedAliasSchema,
    S.union([
      S.schema(s => Foo({foo: s.matches(S.string)})),
      S.schema(s => Bar({bar: s.matches(S.string)})),
    ]),
  )
})
