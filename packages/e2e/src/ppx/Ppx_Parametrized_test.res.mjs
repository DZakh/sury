// Reproduction test for Issue #162: PPX support for parametrized types
// https://github.com/DZakh/sury/issues/162
//
// This file is HAND-WRITTEN (not PPX-generated) because the PPX does not yet
// support parametrized types. It demonstrates the desired generated code.
//
// When the PPX is updated, `@schema type wrapper<'a> = { value: 'a }` should
// produce the equivalent of `wrapperSchema` below, and field usages like
// `wrapper<int>` should emit `wrapperSchema(S.int)`.

import * as S from "sury/src/S.res.mjs";
import Ava from "ava";

// --- Single type parameter ---
// @schema type wrapper<'a> = { value: 'a }
// Expected PPX output:
let wrapperSchema = (aSchema) => S.schema(s => ({
  value: s.m(aSchema)
}));

Ava("Parametrized record with single type param — int", t => {
  let schema = wrapperSchema(S.int);
  t.deepEqual(S.parseOrThrow({"value": 42}, schema), { value: 42 });
  t.deepEqual(S.decodeOrThrow({ value: 42 }, S.reverse(schema)), {"value": 42});
});

Ava("Parametrized record with single type param — string", t => {
  let schema = wrapperSchema(S.string);
  t.deepEqual(S.parseOrThrow({"value": "hello"}, schema), { value: "hello" });
});

// --- Multiple type parameters ---
// @schema type pair<'a, 'b> = { first: 'a, second: 'b }
// Expected PPX output:
let pairSchema = (aSchema, bSchema) => S.schema(s => ({
  first: s.m(aSchema),
  second: s.m(bSchema)
}));

Ava("Parametrized record with multiple type params", t => {
  let schema = pairSchema(S.string, S.int);
  t.deepEqual(
    S.parseOrThrow({"first": "hello", "second": 42}, schema),
    { first: "hello", second: 42 }
  );
});

// --- Using parametrized types as fields ---
// @schema type parent = { wrapped: wrapper<int>, paired: pair<string, bool> }
// Expected PPX output (call-site must apply schema args):
let parentSchema = S.schema(s => ({
  wrapped: s.m(wrapperSchema(S.int)),
  paired: s.m(pairSchema(S.string, S.bool))
}));

Ava("Record using parametrized types as fields", t => {
  t.deepEqual(
    S.parseOrThrow(
      {"wrapped": {"value": 42}, "paired": {"first": "hello", "second": true}},
      parentSchema
    ),
    { wrapped: { value: 42 }, paired: { first: "hello", second: true } }
  );
});

// --- Nested parametrized types ---
// @schema type nested<'a> = { inner: wrapper<'a> }
// Expected PPX output (type param forwarded to inner schema):
let nestedSchema = (aSchema) => S.schema(s => ({
  inner: s.m(wrapperSchema(aSchema))
}));

Ava("Nested parametrized types", t => {
  let schema = nestedSchema(S.string);
  t.deepEqual(
    S.parseOrThrow({"inner": {"value": "deep"}}, schema),
    { inner: { value: "deep" } }
  );
});

// --- Parametrized type with option field ---
// @schema type withOption<'a> = { data: option<'a> }
// Expected PPX output:
let withOptionSchema = (aSchema) => S.schema(s => ({
  data: s.m(S.option(aSchema))
}));

Ava("Parametrized type with option field", t => {
  let schema = withOptionSchema(S.int);
  t.deepEqual(S.parseOrThrow({"data": 5}, schema), { data: 5 });
  t.deepEqual(S.parseOrThrow({}, schema), { data: undefined });
});

// --- Parametrized type with array field ---
// @schema type withArray<'a> = { items: array<'a> }
// Expected PPX output:
let withArraySchema = (aSchema) => S.schema(s => ({
  items: s.m(S.array(aSchema))
}));

Ava("Parametrized type with array field", t => {
  let schema = withArraySchema(S.string);
  t.deepEqual(
    S.parseOrThrow({"items": ["a", "b", "c"]}, schema),
    { items: ["a", "b", "c"] }
  );
});

// --- Parametrized type alias ---
// @schema type alias<'a> = 'a
// Expected PPX output:
let aliasSchema = (aSchema) => aSchema;

Ava("Parametrized type alias", t => {
  let schema = aliasSchema(S.int);
  t.deepEqual(S.parseOrThrow(42, schema), 42);
});

export {
  wrapperSchema,
  pairSchema,
  parentSchema,
  nestedSchema,
  withOptionSchema,
  withArraySchema,
  aliasSchema,
}
