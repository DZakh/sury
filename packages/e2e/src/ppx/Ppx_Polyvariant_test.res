open Vitest
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

@schema
type polyWithPayloads = [#one | #two(int) | #three(string, float) | #four({"foo": string})]
test("Polymorphic variant with payloads (issue #160)", t => {
  t->assertEqualSchemas(
    polyWithPayloadsSchema,
    S.union([
      S.literal(#one),
      S.schema(s => #two(s.matches(S.int))),
      S.schema(s => #three(s.matches(S.string), s.matches(S.float))),
      S.schema(s => #four(s.matches(S.schema(s => {"foo": s.matches(S.string)})))),
    ]),
  )
  t->Assert.deepEqual(%raw(`"one"`)->S.parseOrThrow(~to=polyWithPayloadsSchema), #one)
  t->Assert.deepEqual(
    %raw(`{NAME:"two",VAL:123}`)->S.parseOrThrow(~to=polyWithPayloadsSchema),
    #two(123),
  )
  t->Assert.deepEqual(
    %raw(`{NAME:"three",VAL:["hello",1.5]}`)->S.parseOrThrow(~to=polyWithPayloadsSchema),
    #three("hello", 1.5),
  )
  t->Assert.deepEqual(
    %raw(`{NAME:"four",VAL:{foo:"bar"}}`)->S.parseOrThrow(~to=polyWithPayloadsSchema),
    #four({"foo": "bar"}),
  )
})

@schema
type polyWithSinglePayload = [#value(string)]
test("Polymorphic variant with single payload (issue #160)", t => {
  t->assertEqualSchemas(
    polyWithSinglePayloadSchema,
    S.schema(s => #value(s.matches(S.string))),
  )
  t->Assert.deepEqual(
    %raw(`{NAME:"value",VAL:"hello"}`)->S.parseOrThrow(~to=polyWithSinglePayloadSchema),
    #value("hello"),
  )
})

@schema
type polyWithInheritance = [poly | #three]
test("Polymorphic variant with inheritance/spread of a named type", t => {
  t->assertEqualSchemas(
    polyWithInheritanceSchema,
    // The inherited `polySchema` is narrower than the enclosing variant and
    // `S.t` is invariant, so it is cast to unify within the union, mirroring
    // what the ppx generates (a typed `%identity`, runtime no-op).
    S.union([polySchema->S.castToAny, S.literal(#three)]),
  )
  t->Assert.deepEqual(%raw(`"one"`)->S.parseOrThrow(~to=polyWithInheritanceSchema), #one)
  t->Assert.deepEqual(%raw(`"three"`)->S.parseOrThrow(~to=polyWithInheritanceSchema), #three)
  // Reverse: an inherited tag must serialize back through the nested schema.
  t->assertReverseParsesBack(polyWithInheritanceSchema, #one)
  t->assertReverseParsesBack(polyWithInheritanceSchema, #three)
})

@schema
type polyWithPayloadInheritance = [polyWithPayloads | #five(bool)]
test("Polymorphic variant inheriting a type that has payloads", t => {
  t->assertEqualSchemas(
    polyWithPayloadInheritanceSchema,
    S.union([polyWithPayloadsSchema->S.castToAny, S.schema(s => #five(s.matches(S.bool)))]),
  )
  t->Assert.deepEqual(%raw(`"one"`)->S.parseOrThrow(~to=polyWithPayloadInheritanceSchema), #one)
  t->Assert.deepEqual(
    %raw(`{NAME:"two",VAL:123}`)->S.parseOrThrow(~to=polyWithPayloadInheritanceSchema),
    #two(123),
  )
  t->Assert.deepEqual(
    %raw(`{NAME:"five",VAL:true}`)->S.parseOrThrow(~to=polyWithPayloadInheritanceSchema),
    #five(true),
  )
  // Reverse: both an inherited payload tag and the own tag must round-trip.
  t->assertReverseParsesBack(polyWithPayloadInheritanceSchema, #two(123))
  t->assertReverseParsesBack(polyWithPayloadInheritanceSchema, #five(true))
})

@schema
type polyInheritanceField = {variants: [poly | #three]}
test("Polymorphic variant inheritance nested as a record field", t => {
  t->assertEqualSchemas(
    polyInheritanceFieldSchema,
    S.schema(s => {
      variants: s.matches(S.union([polySchema->S.castToAny, S.literal(#three)])),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{variants: "one"}`)->S.parseOrThrow(~to=polyInheritanceFieldSchema),
    {variants: #one},
  )
  // Reverse: the nested union inside a record field must round-trip too.
  t->assertReverseParsesBack(polyInheritanceFieldSchema, {variants: #one})
  t->assertReverseParsesBack(polyInheritanceFieldSchema, {variants: #three})
})

@schema
type polyInlineInheritance = [[#a | #b(int)] | #c]
test("Polymorphic variant with an inline spread is flattened into one union", t => {
  // An inline spread expands its rows directly into the parent union, so no
  // nested union (and no cast) is generated.
  t->assertEqualSchemas(
    polyInlineInheritanceSchema,
    S.union([S.literal(#a), S.schema(s => #b(s.matches(S.int))), S.literal(#c)]),
  )
  t->Assert.deepEqual(%raw(`"a"`)->S.parseOrThrow(~to=polyInlineInheritanceSchema), #a)
  t->Assert.deepEqual(
    %raw(`{NAME:"b",VAL:7}`)->S.parseOrThrow(~to=polyInlineInheritanceSchema),
    #b(7),
  )
  t->Assert.deepEqual(%raw(`"c"`)->S.parseOrThrow(~to=polyInlineInheritanceSchema), #c)
  // Reverse: an inline-spliced tag (incl. a payload one) must round-trip.
  t->assertReverseParsesBack(polyInlineInheritanceSchema, #a)
  t->assertReverseParsesBack(polyInlineInheritanceSchema, #b(7))
  t->assertReverseParsesBack(polyInlineInheritanceSchema, #c)
})

// TODO: Support
// type basicBlueTone<'a> = [> #Blue | #DeepBlue | #LightBlue] as 'a
