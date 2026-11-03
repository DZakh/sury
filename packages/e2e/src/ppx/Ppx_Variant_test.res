open Vitest
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
test("Variant with payloads is externally tagged like a Serde enum", t => {
  t->assertEqualSchemas(
    variantWithPayloadsSchema,
    S.union([
      S.literal(Constant),
      S.object(s => SinglePayload(s.field("SinglePayload", S.int)))->S.strict,
      S.object(s =>
        s.field("TuplePayload", S.tuple(s => TuplePayload(s.item(0, S.int), s.item(1, S.string))))
      )->S.strict,
      S.object(s =>
        s.field("RecordPayload", S.object(s => RecordPayload({foo: s.field("foo", S.float)})))
      )->S.strict,
    ]),
  )
  t->Assert.deepEqual(%raw(`"Constant"`)->S.parseOrThrow(~to=variantWithPayloadsSchema), Constant)
  t->Assert.deepEqual(
    %raw(`{"SinglePayload": 1}`)->S.parseOrThrow(~to=variantWithPayloadsSchema),
    SinglePayload(1),
  )
  t->Assert.deepEqual(
    %raw(`{"TuplePayload": [1, "a"]}`)->S.parseOrThrow(~to=variantWithPayloadsSchema),
    TuplePayload(1, "a"),
  )
  t->Assert.deepEqual(
    %raw(`{"RecordPayload": {"foo": 1.5}}`)->S.parseOrThrow(~to=variantWithPayloadsSchema),
    RecordPayload({foo: 1.5}),
  )
  t->Assert.deepEqual(
    TuplePayload(1, "a")->S.convertOrThrow(~from=variantWithPayloadsSchema, ~to=S.unknown),
    %raw(`{"TuplePayload": [1, "a"]}`),
  )
  t->assertThrowsMessage(
    () => %raw(`{"TAG": "SinglePayload", "_0": 1}`)->S.parseOrThrow(~to=variantWithPayloadsSchema),
    `Expected "Constant" | { SinglePayload: int32; } | { TuplePayload: [int32, string]; } | { RecordPayload: { foo: number; }; }, received { TAG: "SinglePayload"; _0: 1; }
- At SinglePayload: Expected int32, received undefined
- At TuplePayload: Expected [int32, string], received undefined
- At RecordPayload: Expected { foo: number; }, received undefined`,
  )
})

@schema
type renamedPayloads =
  | @as("one") One(int)
  | @as("two") Two({value: string})
  | @as("maybe") Maybe(option<int>)
  | Empty(unit)
test("A renamed constructor names the key its payload sits under", t => {
  t->Assert.deepEqual(%raw(`{"one": 1}`)->S.parseOrThrow(~to=renamedPayloadsSchema), One(1))
  t->Assert.deepEqual(
    Two({value: "a"})->S.convertOrThrow(~from=renamedPayloadsSchema, ~to=S.unknown),
    %raw(`{"two": {"value": "a"}}`),
  )
  t->Assert.deepEqual(
    %raw(`{"maybe": null}`)->S.parseOrThrow(~to=renamedPayloadsSchema),
    Maybe(None),
  )
  t->Assert.deepEqual(
    Empty()->S.convertOrThrow(~from=renamedPayloadsSchema, ~to=S.unknown),
    %raw(`{"Empty": null}`),
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

@schema @s.strict
type strictVariant = StrictA | StrictB(int)

@schema @tag("type")
type taggedInlinedAlias = Foo({@as("Foo") foo: string}) | Bar({@as("Bar") bar: string})

test("@s.strict on root variant type", t => {
  t->assertEqualSchemas(
    strictVariantSchema,
    S.strict(
      S.union([S.literal(StrictA), S.object(s => StrictB(s.field("StrictB", S.int)))->S.strict]),
    ),
  )
})

test("Tagged variant with inlined alias", t => {
  t->assertEqualSchemas(
    taggedInlinedAliasSchema,
    S.union([
      S.schema(s => Foo({foo: s.matches(S.string)})),
      S.schema(s => Bar({bar: s.matches(S.string)})),
    ]),
  )
})

@schema
type baseColors = Red | Blue | Green

@schema
type extendedColors = | ...baseColors | Yellow
test("Variant with type spread", t => {
  t->Assert.deepEqual(%raw(`"Red"`)->S.parseOrThrow(~to=extendedColorsSchema), Red)
  t->Assert.deepEqual(%raw(`"Yellow"`)->S.parseOrThrow(~to=extendedColorsSchema), Yellow)
  t->assertReverseReversesBack(extendedColorsSchema)
})

@schema
type singleCase = Only

@schema
type extendedFromSingleCase = | ...singleCase | Another
test("Variant spread of a single-case (literal) schema", t => {
  t->Assert.deepEqual(%raw(`"Only"`)->S.parseOrThrow(~to=extendedFromSingleCaseSchema), Only)
  t->Assert.deepEqual(%raw(`"Another"`)->S.parseOrThrow(~to=extendedFromSingleCaseSchema), Another)
  t->assertReverseReversesBack(extendedFromSingleCaseSchema)
})

@tag("kind") @schema
type taggedWithUnit = Idle | @as("busy") Busy({since: float})
test("Tagged variant with a constructor without payload", t => {
  t->assertEqualSchemas(
    taggedWithUnitSchema,
    S.union([
      S.object(s => {
        s.tag("kind", Idle)
        Idle
      }),
      S.schema(s => Busy({since: s.matches(S.float)})),
    ]),
  )
  t->Assert.deepEqual(%raw(`{"kind": "Idle"}`)->S.parseOrThrow(~to=taggedWithUnitSchema), Idle)
  t->Assert.deepEqual(
    Idle->S.convertOrThrow(~from=taggedWithUnitSchema, ~to=S.unknown),
    %raw(`{"kind": "Idle"}`),
  )
})

assertSerdeCompatible(
  "Variant",
  [
    ("variant", variantSchema->S.castToUnknown),
    ("variantWithSingleItem", variantWithSingleItemSchema->S.castToUnknown),
    ("variantWithAlias", variantWithAliasSchema->S.castToUnknown),
    ("variantWithPayloads", variantWithPayloadsSchema->S.castToUnknown),
    ("renamedPayloads", renamedPayloadsSchema->S.castToUnknown),
    ("unboxedVariant", unboxedVariantSchema->S.castToUnknown),
    ("taggedVariant", taggedVariantSchema->S.castToUnknown),
    ("taggedWithUnit", taggedWithUnitSchema->S.castToUnknown),
    ("strictVariant", strictVariantSchema->S.castToUnknown),
    ("taggedInlinedAlias", taggedInlinedAliasSchema->S.castToUnknown),
    ("baseColors", baseColorsSchema->S.castToUnknown),
    ("extendedColors", extendedColorsSchema->S.castToUnknown),
    ("singleCase", singleCaseSchema->S.castToUnknown),
    ("extendedFromSingleCase", extendedFromSingleCaseSchema->S.castToUnknown),
  ],
)
