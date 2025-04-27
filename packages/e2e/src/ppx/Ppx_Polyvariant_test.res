open Ava
open U
open RescriptCore

@schema
type poly = [#one | #two]
test("Polymorphic variant", t => {
  t->assertEqualSchemas(polySchema, S.union([S.literal(#one), S.literal(#two)]))
})

@schema
type polyWithSingleItem = [#single]
test("Polymorphic variant with single item becomes a literal schema of the item", t => {
  t->assertEqualSchemas(polyWithSingleItemSchema, S.literal(#single))
})

@schema
type polyEmbeded = @s.matches(S.string->S.shape(_ => #one)) [#one]
test("Embed custom schema for polymorphic variants", t => {
  t->assertEqualSchemas(polyEmbededSchema, S.string->S.shape(_ => #one))
})

@schema
type dictField = Dict.t<[#one]>
test("Supported as a dict field", t => {
  t->assertEqualSchemas(dictFieldSchema, S.dict(S.literal(#one)))
})

@schema
type recordField = {poly: [#one]}
test("Supported as a record field", t => {
  t->assertEqualSchemas(
    recordFieldSchema,
    S.object(s => {
      poly: s.field("poly", S.literal(#one)),
    }),
  )
})

@schema
type objectField = {"poly": [#one]}
test("Supported as a object field", t => {
  t->assertEqualSchemas(
    objectFieldSchema,
    S.object(s =>
      {
        "poly": s.field("poly", S.literal(#one)),
      }
    ),
  )
})

// TODO: Support
// type polyWithPayloads = [#one | #two(int) | #three({"foo": string})]
// TODO: Support
// type basicBlueTone<'a> = [> #Blue | #DeepBlue | #LightBlue] as 'a
// TODO: Support
// type polyWithInheritance = [poly | #three]
