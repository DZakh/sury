open Vitest
open U

@schema @s.strict
type strictRecord = {
  strictEmail: string,
  strictName: string,
}

@schema @s.strip
type stripRecord = {
  stripEmail: string,
  stripName: string,
}

@schema @s.deepStrict
type deepStrictRecord = {
  deepStrictEmail: string,
  deepStrictName: string,
}

@schema @s.deepStrip
type deepStripRecord = {
  deepStripEmail: string,
  deepStripName: string,
}

@schema @s.noValidation
type noValidationRecord = {
  noValEmail: string,
  noValName: string,
}

@schema @s.meta({description: "A user record"})
type metaRecord = {
  metaEmail: string,
  metaName: string,
}

@schema @s.strict @s.meta({description: "Combined attributes"})
type combinedRecord = {
  combinedEmail: string,
  combinedName: string,
}

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
type emptyRecord = {}
test("Empty record schema", t => {
  t->Assert.deepEqual(
    %raw(`{}`)->S.parseOrThrow(~to=emptyRecordSchema),
    ({}: emptyRecord),
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

test("@s.strict on root record type", t => {
  t->assertEqualSchemas(
    strictRecordSchema,
    S.strict(
      S.schema(s => {
        strictEmail: s.matches(S.string),
        strictName: s.matches(S.string),
      }),
    ),
  )
})

test("@s.strip on root record type", t => {
  t->assertEqualSchemas(
    stripRecordSchema,
    S.strip(
      S.schema(s => {
        stripEmail: s.matches(S.string),
        stripName: s.matches(S.string),
      }),
    ),
  )
})

test("@s.deepStrict on root record type", t => {
  t->assertEqualSchemas(
    deepStrictRecordSchema,
    S.deepStrict(
      S.schema(s => {
        deepStrictEmail: s.matches(S.string),
        deepStrictName: s.matches(S.string),
      }),
    ),
  )
})

test("@s.deepStrip on root record type", t => {
  t->assertEqualSchemas(
    deepStripRecordSchema,
    S.deepStrip(
      S.schema(s => {
        deepStripEmail: s.matches(S.string),
        deepStripName: s.matches(S.string),
      }),
    ),
  )
})

test("@s.noValidation on root record type", t => {
  t->assertEqualSchemas(
    noValidationRecordSchema,
    S.noValidation(
      S.schema(s => {
        noValEmail: s.matches(S.string),
        noValName: s.matches(S.string),
      }),
      true,
    ),
  )
})

test("@s.meta on root record type", t => {
  t->assertEqualSchemas(
    metaRecordSchema,
    S.meta(
      S.schema(s => {
        metaEmail: s.matches(S.string),
        metaName: s.matches(S.string),
      }),
      {description: "A user record"},
    ),
  )
})

test("Multiple @s.* attributes on root record type", t => {
  t->assertEqualSchemas(
    combinedRecordSchema,
    S.meta(
      S.strict(
        S.schema(s => {
          combinedEmail: s.matches(S.string),
          combinedName: s.matches(S.string),
        }),
      ),
      {description: "Combined attributes"},
    ),
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
  t->Assert.deepEqual(
    %raw(`{id:"abc",messages:[]}`)->S.parseOrThrow(~to=recordWithSpreadSchema),
    {id: "abc", summary: None, messages: []},
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

@schema
type recordWithOnlySpread = {
  ...Meta.t,
}
test("Record schema with only a spread (no own fields)", t => {
  t->Assert.deepEqual(
    %raw(`{id:"abc",summary:"hi"}`)->S.parseOrThrow(~to=recordWithOnlySpreadSchema),
    {id: "abc", summary: Some("hi")},
  )
  t->Assert.deepEqual(
    %raw(`{id:"abc"}`)->S.parseOrThrow(~to=recordWithOnlySpreadSchema),
    {id: "abc", summary: None},
  )
  t->assertReverseReversesBack(recordWithOnlySpreadSchema)
})

@schema
type recordWithMultipleSpreadsNoFields = {
  ...Meta.t,
  ...Extra.t,
}
test("Record schema with multiple spreads and no own fields", t => {
  t->Assert.deepEqual(
    %raw(`{id:"abc",summary:"hi",score:5.0}`)->S.parseOrThrow(
      ~to=recordWithMultipleSpreadsNoFieldsSchema,
    ),
    {id: "abc", summary: Some("hi"), score: 5.0},
  )
  t->assertReverseReversesBack(recordWithMultipleSpreadsNoFieldsSchema)
})

@schema
type recordWithSpreadAndAliasedField = {
  ...Meta.t,
  @as("ms") messages: array<string>,
}
test("Record schema with spread and @as-aliased own field", t => {
  t->Assert.deepEqual(
    %raw(`{id:"abc",summary:"hi",ms:["a"]}`)->S.parseOrThrow(
      ~to=recordWithSpreadAndAliasedFieldSchema,
    ),
    {id: "abc", summary: Some("hi"), messages: ["a"]},
  )
  t->assertReverseReversesBack(recordWithSpreadAndAliasedFieldSchema)
})

@schema
type recordWithSpreadAndOptionalField = {
  ...Meta.t,
  extra: option<int>,
}
test("Record schema with spread and optional own field", t => {
  t->Assert.deepEqual(
    %raw(`{id:"abc",summary:"hi",extra:5}`)->S.parseOrThrow(
      ~to=recordWithSpreadAndOptionalFieldSchema,
    ),
    {id: "abc", summary: Some("hi"), extra: Some(5)},
  )
  t->Assert.deepEqual(
    %raw(`{id:"abc"}`)->S.parseOrThrow(~to=recordWithSpreadAndOptionalFieldSchema),
    {id: "abc", summary: None, extra: None},
  )
  t->assertReverseReversesBack(recordWithSpreadAndOptionalFieldSchema)
})
