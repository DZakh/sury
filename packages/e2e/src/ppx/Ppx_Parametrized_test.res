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
  t->Assert.deepEqual(
    %raw(`{"value": 42}`)->S.parseOrThrow(~to=schema),
    {value: 42},
  )
})

test("Parametrized record with single type param — string", t => {
  let schema = wrapperSchema(S.string)
  t->Assert.deepEqual(
    %raw(`{"value": "hello"}`)->S.parseOrThrow(~to=schema),
    {value: "hello"},
  )
})

// --- Using a parametrized type as a field ---
@schema
type parent = {wrapped: wrapper<int>}

test("Record using parametrized type as field", t => {
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

// --- Multiple type parameters: not supported by default ---
// The PPX errors on 2+ type parameters:
//   [sury-ppx] Parametrized types with more than one type parameter are not supported yet
//
// @schema
// type pair<'a, 'b> = {first: 'a, second: 'b}
