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
test("Polymorphic variant with payloads is externally tagged like a Serde enum", t => {
  t->assertEqualSchemas(
    polyWithPayloadsSchema,
    S.union([
      S.literal(#one),
      S.object(s => #two(s.field("two", S.int)))->S.strict,
      S.object(s =>
        #three(s.field("three", S.schema(s => (s.matches(S.string), s.matches(S.float)))))
      )->S.strict,
      S.object(s => #four(s.field("four", S.schema(s => {"foo": s.matches(S.string)}))))->S.strict,
    ]),
  )
  t->Assert.deepEqual(%raw(`"one"`)->S.parseOrThrow(~to=polyWithPayloadsSchema), #one)
  t->Assert.deepEqual(%raw(`{"two": 123}`)->S.parseOrThrow(~to=polyWithPayloadsSchema), #two(123))
  t->Assert.deepEqual(
    %raw(`{"three": ["hello", 1.5]}`)->S.parseOrThrow(~to=polyWithPayloadsSchema),
    #three("hello", 1.5),
  )
  t->Assert.deepEqual(
    %raw(`{"four": {"foo": "bar"}}`)->S.parseOrThrow(~to=polyWithPayloadsSchema),
    #four({"foo": "bar"}),
  )
  t->Assert.deepEqual(
    #three("hello", 1.5)->S.convertOrThrow(~from=polyWithPayloadsSchema, ~to=S.unknown),
    %raw(`{"three": ["hello", 1.5]}`),
  )
  t->assertThrowsMessage(
    () => %raw(`{"two": 1, "one": null}`)->S.parseOrThrow(~to=polyWithPayloadsSchema),
    `Expected "one" | { two: int32; } | { three: [string, number]; } | { four: { foo: string; }; }, received { two: 1; one: null; }
- Unrecognized key "one"
- At three: Expected [string, number], received undefined
- At four: Expected { foo: string; }, received undefined`,
  )
})

@schema
type polyWithSinglePayload = [#value(string)]
test("Polymorphic variant with single payload", t => {
  t->assertEqualSchemas(
    polyWithSinglePayloadSchema,
    S.object(s => #value(s.field("value", S.string)))->S.strict,
  )
  t->Assert.deepEqual(
    %raw(`{"value": "hello"}`)->S.parseOrThrow(~to=polyWithSinglePayloadSchema),
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
    S.union([
      polyWithPayloadsSchema->S.castToAny,
      S.object(s => #five(s.field("five", S.bool)))->S.strict,
    ]),
  )
  t->Assert.deepEqual(%raw(`"one"`)->S.parseOrThrow(~to=polyWithPayloadInheritanceSchema), #one)
  t->Assert.deepEqual(
    %raw(`{"two": 123}`)->S.parseOrThrow(~to=polyWithPayloadInheritanceSchema),
    #two(123),
  )
  t->Assert.deepEqual(
    %raw(`{"five": true}`)->S.parseOrThrow(~to=polyWithPayloadInheritanceSchema),
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
    S.union([S.literal(#a), S.object(s => #b(s.field("b", S.int)))->S.strict, S.literal(#c)]),
  )
  t->Assert.deepEqual(%raw(`"a"`)->S.parseOrThrow(~to=polyInlineInheritanceSchema), #a)
  t->Assert.deepEqual(%raw(`{"b": 7}`)->S.parseOrThrow(~to=polyInlineInheritanceSchema), #b(7))
  t->Assert.deepEqual(%raw(`"c"`)->S.parseOrThrow(~to=polyInlineInheritanceSchema), #c)
  // Reverse: an inline-spliced tag (incl. a payload one) must round-trip.
  t->assertReverseParsesBack(polyInlineInheritanceSchema, #a)
  t->assertReverseParsesBack(polyInlineInheritanceSchema, #b(7))
  t->assertReverseParsesBack(polyInlineInheritanceSchema, #c)
})

// TODO: Support
// type basicBlueTone<'a> = [> #Blue | #DeepBlue | #LightBlue] as 'a

assertSerdeCompatible(
  "Polyvariant",
  [
    ("poly", polySchema->S.castToUnknown),
    ("polyWithSingleItem", polyWithSingleItemSchema->S.castToUnknown),
    ("dictField", dictFieldSchema->S.castToUnknown),
    ("recordField", recordFieldSchema->S.castToUnknown),
    ("objectField", objectFieldSchema->S.castToUnknown),
    ("polyWithPayloads", polyWithPayloadsSchema->S.castToUnknown),
    ("polyWithSinglePayload", polyWithSinglePayloadSchema->S.castToUnknown),
    ("polyWithInheritance", polyWithInheritanceSchema->S.castToUnknown),
    ("polyWithPayloadInheritance", polyWithPayloadInheritanceSchema->S.castToUnknown),
    ("polyInheritanceField", polyInheritanceFieldSchema->S.castToUnknown),
    ("polyInlineInheritance", polyInlineInheritanceSchema->S.castToUnknown),
  ],
)
