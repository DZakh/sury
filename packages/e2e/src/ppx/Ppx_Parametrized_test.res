open Ava
open U

// ============================================================================
// Issue #162: PPX support for parametrized types (single type parameter)
// https://github.com/DZakh/sury/issues/162
// ============================================================================

// --- Single type parameter: record ---
@schema
type wrapper<'a> = {value: 'a}

test("Parametrized record with single type param — int", t => {
  let schema = wrapperSchema(S.int)
  t->assertEqualSchemas(
    schema,
    S.schema(s => {value: s.matches(S.int)}),
  )
  t->Assert.deepEqual(
    %raw(`{"value": 42}`)->S.parseOrThrow(~to=schema),
    {value: 42},
  )
})

test("Parametrized record with single type param — string", t => {
  let schema = wrapperSchema(S.string)
  t->assertEqualSchemas(
    schema,
    S.schema(s => {value: s.matches(S.string)}),
  )
  t->Assert.deepEqual(
    %raw(`{"value": "hello"}`)->S.parseOrThrow(~to=schema),
    {value: "hello"},
  )
})

// --- Using a parametrized type as a field ---
@schema
type parent = {wrapped: wrapper<int>}

test("Record using parametrized type as field", t => {
  t->assertEqualSchemas(
    parentSchema,
    S.schema(s => {
      wrapped: s.matches(wrapperSchema(S.int)),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{"wrapped": {"value": 42}}`)->S.parseOrThrow(~to=parentSchema),
    {wrapped: {value: 42}},
  )
})

// --- Nested: forwarding the type param ---
@schema
type nested<'a> = {inner: wrapper<'a>}

test("Nested parametrized types", t => {
  let schema = nestedSchema(S.string)
  t->Assert.deepEqual(
    %raw(`{"inner": {"value": "deep"}}`)->S.parseOrThrow(~to=schema),
    {inner: {value: "deep"}},
  )
})

// --- Parametrized type with option field ---
@schema
type withOption<'a> = {data: option<'a>}

test("Parametrized type with option field", t => {
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

// --- Parametrized type with array field ---
@schema
type withArray<'a> = {items: array<'a>}

test("Parametrized type with array field", t => {
  let schema = withArraySchema(S.string)
  t->Assert.deepEqual(
    %raw(`{"items": ["a", "b", "c"]}`)->S.parseOrThrow(~to=schema),
    {items: ["a", "b", "c"]},
  )
})

// --- Parametrized type alias ---
@schema
type alias<'a> = 'a

test("Parametrized type alias", t => {
  let schema = aliasSchema(S.int)
  t->Assert.deepEqual(
    %raw(`42`)->S.parseOrThrow(~to=schema),
    42,
  )
})

// --- Round-trip (parse + reverse) ---
test("Parametrized type round-trip", t => {
  let schema = wrapperSchema(S.int)
  t->Assert.deepEqual(
    {value: 42}
    ->S.decodeOrThrow(~from=schema->S.reverse, ~to=S.unknown)
    ->S.parseOrThrow(~to=schema),
    {value: 42},
  )
})

// --- Multiple type parameters: not supported by default ---
// The PPX errors on 2+ type parameters:
//   [sury-ppx] Parametrized types with more than one type parameter are not supported yet
//
// @schema
// type pair<'a, 'b> = {first: 'a, second: 'b}

// --- Multiple type parameters: supported via @s.matches override ---
// When the user provides the schema explicitly with @s.matches, the PPX
// short-circuits before the 2+ params check, so multi-param types work as
// long as the user supplies the schema.

type result2<'a, 'b> = R2A('a) | R2B('b)
let result2Schema = (aSchema, bSchema) =>
  S.union([
    S.schema(s => R2A(s.matches(aSchema))),
    S.schema(s => R2B(s.matches(bSchema))),
  ])

@schema
type holder = {
  res: @s.matches(result2Schema(S.int, S.string)) result2<int, string>,
}

test("Record field with @s.matches override for a 2-param type", t => {
  t->assertEqualSchemas(
    holderSchema,
    S.schema(s => {
      res: s.matches(result2Schema(S.int, S.string)),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{"res": {"TAG": "R2A", "_0": 1}}`)->S.parseOrThrow(~to=holderSchema),
    {res: R2A(1)},
  )
  t->Assert.deepEqual(
    %raw(`{"res": {"TAG": "R2B", "_0": "boom"}}`)->S.parseOrThrow(~to=holderSchema),
    {res: R2B("boom")},
  )
})

// --- Phantom type parameter (unused) ---
@schema
type id<'a> = string

test("Parametrized type alias whose param is unused (phantom)", t => {
  let schema = idSchema(S.int)
  t->assertEqualSchemas(schema, S.string)
  t->Assert.deepEqual(
    %raw(`"hello"`)->S.parseOrThrow(~to=schema),
    "hello",
  )
})
