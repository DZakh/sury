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

// TODO: Support
// type basicBlueTone<'a> = [> #Blue | #DeepBlue | #LightBlue] as 'a
// TODO: Support
// type polyWithInheritance = [poly | #three]
