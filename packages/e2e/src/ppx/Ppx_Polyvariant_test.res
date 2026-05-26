open Ava
open U

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
    S.schema(s => {
      poly: s.matches(S.literal(#one)),
    }),
  )
})

@schema
type objectField = {"poly": [#one]}
test("Supported as an object field", t => {
  t->assertEqualSchemas(
    objectFieldSchema,
    S.schema(s =>
      {
        "poly": s.matches(S.literal(#one)),
      }
    ),
  )
})

// Issue #160: Polymorphic variants with payloads don't compile with @schema.
// The PPX drops payload types and emits S.literal instead of S.schema.
// Uncomment @schema lines once the PPX is fixed:

// @schema
// type polyWithPayloads = [#one | #two(int) | #three(string, float)]
type polyWithPayloads = [#one | #two(int) | #three(string, float)]
let polyWithPayloadsSchema: S.t<polyWithPayloads> = S.union([
  S.literal(#one),
  S.schema(s => #two(s.matches(S.int))),
  S.schema(s => #three(s.matches(S.string), s.matches(S.float))),
])
test("Polymorphic variant with payloads (issue #160)", t => {
  t->Assert.notThrows(() => {
    polyWithPayloadsSchema->S.parseOrThrow(%raw(`{"TAG":"two","_0":123}`))
  })
  t->Assert.deepEqual(
    polyWithPayloadsSchema->S.parseOrThrow(%raw(`{"TAG":"two","_0":123}`)),
    #two(123),
  )
})

// @schema
// type polyWithSinglePayload = [#value(string)]
type polyWithSinglePayload = [#value(string)]
let polyWithSinglePayloadSchema: S.t<polyWithSinglePayload> = S.schema(
  s => #value(s.matches(S.string)),
)
test("Polymorphic variant with single payload (issue #160)", t => {
  t->Assert.notThrows(() => {
    polyWithSinglePayloadSchema->S.parseOrThrow(%raw(`{"TAG":"value","_0":"hello"}`))
  })
  t->Assert.deepEqual(
    polyWithSinglePayloadSchema->S.parseOrThrow(%raw(`{"TAG":"value","_0":"hello"}`)),
    #value("hello"),
  )
})

// TODO: Support
// type basicBlueTone<'a> = [> #Blue | #DeepBlue | #LightBlue] as 'a
// TODO: Support
// type polyWithInheritance = [poly | #three]
