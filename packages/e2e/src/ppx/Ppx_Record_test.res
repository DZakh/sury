open Ava
open U

@schema
type simpleRecord = {
  label: string,
  value: int,
}
test("Simple record schema", t => {
  t->assertEqualSchemas(
    simpleRecordSchema,
    S.object(s => {
      label: s.field("label", S.string),
      value: s.field("value", S.int),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{label:"foo",value:1}`)->S.parseOrThrow(simpleRecordSchema),
    {label: "foo", value: 1},
    (),
  )
})

@schema
type recordWithAlias = {
  @as("aliased-label") label: string,
  value: int,
}
test("Record schema with alias for field name", t => {
  t->assertEqualSchemas(
    recordWithAliasSchema,
    S.object(s => {
      label: s.field("aliased-label", S.string),
      value: s.field("value", S.int),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{"aliased-label":"foo",value:1}`)->S.parseOrThrow(recordWithAliasSchema),
    {label: "foo", value: 1},
    (),
  )
})

@schema
type recordWithOptional = {
  label: option<string>,
  value?: int,
}
test("Record schema with optional fields", t => {
  t->assertEqualSchemas(
    recordWithOptionalSchema,
    S.object(s => {
      label: s.field("label", S.option(S.string)),
      value: ?s.field("value", S.option(S.int)),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{"label":"foo",value:1}`)->S.parseOrThrow(recordWithOptionalSchema),
    {label: Some("foo"), value: 1},
    (),
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(recordWithOptionalSchema),
    {label: %raw(`undefined`), value: %raw(`undefined`)},
    (),
  )
})

@schema
type recordWithNullableField = {
  subscription: @s.matches(S.option(S.null(S.string))) option<option<string>>,
}
test("Record schema with nullable field", t => {
  t->assertEqualSchemas(
    recordWithNullableFieldSchema,
    S.object(s => {
      subscription: s.field("subscription", S.option(S.null(S.string))),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(recordWithNullableFieldSchema),
    {subscription: None},
    (),
  )
  t->Assert.deepEqual(
    %raw(`{"subscription":null}`)->S.parseOrThrow(recordWithNullableFieldSchema),
    {subscription: Some(None)},
    (),
  )
})
