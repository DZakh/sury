open Ava
open U

// ============================================================================
// Issue #162: PPX support for parametrized types
// https://github.com/DZakh/sury/issues/162
//
// The @schema PPX should generate a function for parametrized types:
//
//   @schema
//   type thing<'a> = { a: 'a }
//
// Expected generated code:
//   let thingSchema = aSchema => S.schema(s => { a: s.matches(aSchema) })
//
// Currently, the PPX:
//   1. Does NOT wrap the binding in a function taking schema params
//   2. Does NOT pass type args as schema args at call sites
//   3. The type constraint omits type parameters
//
// The lines below are commented out because they fail to compile.
// Uncomment them when the feature is implemented.
// ============================================================================

// --- These should work once the feature is implemented ---
//
// @schema
// type wrapper<'a> = {value: 'a}
//
// @schema
// type pair<'a, 'b> = {first: 'a, second: 'b}
//
// @schema
// type parent = {wrapped: wrapper<int>, paired: pair<string, bool>}

// --- Manual equivalents proving the desired API works ---

type wrapper<'a> = {value: 'a}
let wrapperSchema = (aSchema: S.t<'a>): S.t<wrapper<'a>> =>
  S.schema(s => {value: s.matches(aSchema)})

test("Parametrized record with single type param (manual)", t => {
  let schema = wrapperSchema(S.int)
  t->Assert.deepEqual(
    %raw(`{"value": 42}`)->S.parseOrThrow(~to=schema),
    {value: 42},
  )
  t->Assert.deepEqual(
    {value: 42}->S.decodeOrThrow(~from=schema->S.reverse, ~to=S.unknown),
    %raw(`{"value": 42}`),
  )
})

test("Parametrized record with string type param (manual)", t => {
  let schema = wrapperSchema(S.string)
  t->Assert.deepEqual(
    %raw(`{"value": "hello"}`)->S.parseOrThrow(~to=schema),
    {value: "hello"},
  )
})

type pair<'a, 'b> = {first: 'a, second: 'b}
let pairSchema = (aSchema: S.t<'a>, bSchema: S.t<'b>): S.t<pair<'a, 'b>> =>
  S.schema(s => {first: s.matches(aSchema), second: s.matches(bSchema)})

test("Parametrized record with multiple type params (manual)", t => {
  let schema = pairSchema(S.string, S.int)
  t->Assert.deepEqual(
    %raw(`{"first": "hello", "second": 42}`)->S.parseOrThrow(~to=schema),
    {first: "hello", second: 42},
  )
})

type parent = {wrapped: wrapper<int>, paired: pair<string, bool>}
let parentSchema: S.t<parent> = S.schema(s => {
  wrapped: s.matches(wrapperSchema(S.int)),
  paired: s.matches(pairSchema(S.string, S.bool)),
})

test("Record using parametrized types as fields (manual)", t => {
  t->Assert.deepEqual(
    %raw(`{"wrapped": {"value": 42}, "paired": {"first": "hello", "second": true}}`)
    ->S.parseOrThrow(~to=parentSchema),
    {wrapped: {value: 42}, paired: {first: "hello", second: true}},
  )
})

type nested<'a> = {inner: wrapper<'a>}
let nestedSchema = (aSchema: S.t<'a>): S.t<nested<'a>> =>
  S.schema(s => {inner: s.matches(wrapperSchema(aSchema))})

test("Nested parametrized types (manual)", t => {
  let schema = nestedSchema(S.string)
  t->Assert.deepEqual(
    %raw(`{"inner": {"value": "deep"}}`)->S.parseOrThrow(~to=schema),
    {inner: {value: "deep"}},
  )
})

type withOption<'a> = {data: option<'a>}
let withOptionSchema = (aSchema: S.t<'a>): S.t<withOption<'a>> =>
  S.schema(s => {data: s.matches(S.option(aSchema))})

test("Parametrized type with option field (manual)", t => {
  let schema = withOptionSchema(S.int)
  t->Assert.deepEqual(
    %raw(`{"data": 5}`)->S.parseOrThrow(~to=schema),
    {data: Some(5)},
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(~to=schema),
    {data: %raw(`undefined`)},
  )
})

type withArray<'a> = {items: array<'a>}
let withArraySchema = (aSchema: S.t<'a>): S.t<withArray<'a>> =>
  S.schema(s => {items: s.matches(S.array(aSchema))})

test("Parametrized type with array field (manual)", t => {
  let schema = withArraySchema(S.string)
  t->Assert.deepEqual(
    %raw(`{"items": ["a", "b", "c"]}`)->S.parseOrThrow(~to=schema),
    {items: ["a", "b", "c"]},
  )
})

type alias<'a> = 'a
let aliasSchema = (aSchema: S.t<'a>): S.t<alias<'a>> => aSchema

test("Parametrized type alias (manual)", t => {
  let schema = aliasSchema(S.int)
  t->Assert.deepEqual(
    %raw(`42`)->S.parseOrThrow(~to=schema),
    42,
  )
})
