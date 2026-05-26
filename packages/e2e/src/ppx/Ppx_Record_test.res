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
    S.schema(s => {
      label: s.matches(S.string),
      value: s.matches(S.int),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{label:"foo",value:1}`)->S.parseOrThrow(~to=simpleRecordSchema),
    {label: "foo", value: 1},
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
    S.schema(s => {
      label: s.matches(S.string),
      value: s.matches(S.int),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{"aliased-label":"foo",value:1}`)->S.parseOrThrow(~to=recordWithAliasSchema),
    {label: "foo", value: 1},
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
    S.schema(s => {
      label: s.matches(S.option(S.string)),
      value: ?s.matches(S.option(S.int)),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{"label":"foo",value:1}`)->S.parseOrThrow(~to=recordWithOptionalSchema),
    {label: Some("foo"), value: 1},
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(~to=recordWithOptionalSchema),
    {label: %raw(`undefined`), value: %raw(`undefined`)},
  )
})

@schema
type recordWithNullableField = {
  subscription: @s.matches(S.option(S.nullAsOption(S.string))) option<option<string>>,
}
test("Record schema with nullable field", t => {
  t->assertEqualSchemas(
    recordWithNullableFieldSchema,
    S.schema(s => {
      subscription: s.matches(S.option(S.nullAsOption(S.string))),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(~to=recordWithNullableFieldSchema),
    {subscription: None},
  )
  t->Assert.deepEqual(
    %raw(`{"subscription":null}`)->S.parseOrThrow(~to=recordWithNullableFieldSchema),
    {subscription: Some(None)},
  )
})

// Issue #161: @s.nullable on optional record field should apply
@schema
type recordWithOptionalNullableField = {
  foo?: @s.nullable string,
}
test("@s.nullable on optional record field (issue #161)", t => {
  t->assertEqualSchemas(
    recordWithOptionalNullableFieldSchema,
    S.schema(s => {
      foo: ?s.matches(S.option(S.nullableAsOption(S.string))),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(~to=recordWithOptionalNullableFieldSchema),
    {foo: ?None},
  )
  t->Assert.deepEqual(
    %raw(`{"foo":"hello"}`)->S.parseOrThrow(~to=recordWithOptionalNullableFieldSchema),
    {foo: "hello"},
  )
  t->Assert.deepEqual(
    %raw(`{"foo":null}`)->S.parseOrThrow(~to=recordWithOptionalNullableFieldSchema),
    {foo: ?None},
  )
})

// Issue #161: @s.null on optional record field should apply
@schema
type recordWithOptionalNullField = {
  bar?: @s.null string,
}
test("@s.null on optional record field (issue #161)", t => {
  t->assertEqualSchemas(
    recordWithOptionalNullFieldSchema,
    S.schema(s => {
      bar: ?s.matches(S.option(S.nullAsOption(S.string))),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(~to=recordWithOptionalNullFieldSchema),
    {bar: ?None},
  )
  t->Assert.deepEqual(
    %raw(`{"bar":"hello"}`)->S.parseOrThrow(~to=recordWithOptionalNullFieldSchema),
    {bar: "hello"},
  )
  t->Assert.deepEqual(
    %raw(`{"bar":null}`)->S.parseOrThrow(~to=recordWithOptionalNullFieldSchema),
    {bar: ?None},
  )
})
