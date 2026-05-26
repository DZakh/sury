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

module Meta = {
  @schema
  type t = {
    id: string,
    summary: option<string>,
  }
}

@schema
type recordWithSpread = {
  ...Meta.t,
  messages: array<string>,
}
test("Record schema with type spread", t => {
  t->Assert.deepEqual(
    %raw(`{id:"abc",summary:"hello",messages:["a","b"]}`)->S.parseOrThrow(
      ~to=recordWithSpreadSchema,
    ),
    {id: "abc", summary: Some("hello"), messages: ["a", "b"]},
  )
  t->assertReverseReversesBack(recordWithSpreadSchema)
})

module Extra = {
  @schema
  type t = {
    score: float,
  }
}

@schema
type recordWithMultipleSpreads = {
  ...Meta.t,
  ...Extra.t,
  active: bool,
}
test("Record schema with multiple type spreads", t => {
  t->Assert.deepEqual(
    %raw(`{id:"abc",summary:"hello",score:9.5,active:true}`)->S.parseOrThrow(
      ~to=recordWithMultipleSpreadsSchema,
    ),
    {id: "abc", summary: Some("hello"), score: 9.5, active: true},
  )
  t->assertReverseReversesBack(recordWithMultipleSpreadsSchema)
})
