open Ava

test("Successfully parses and reverse converts a simple object with compactColumns", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.int),
          }
        ),
      ),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{Array.isArray(i)&&i.length===2&&Array.isArray(i[0])&&Array.isArray(i[1])||e[2](i);let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){try{let v2=i[0][v0];typeof v2==="string"||e[0](v2);let v3=i[1][v0];typeof v3==="number"&&v3<=2147483647&&v3>=-2147483648&&v3%1===0||e[1](v3);v1[v0]={"foo":v2,"bar":v3,};}catch(v4){v4.path='["'+v0+'"]'+v4.path;throw v4}}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];}catch(v2){v2.path='["'+v0+'"]'+v2.path;throw v2}}let v4=[new Array(i.length),new Array(i.length),];for(let v3=0;v3<i.length;++v3){v4[0][v3]=i[v3]["foo"];v4[1][v3]=i[v3]["bar"];}return v4}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, 1]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, 1]]`),
  )
})

test("Transforms nullable fields", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.nullAsOption(S.int)),
          }
        ),
      ),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{Array.isArray(i)&&i.length===2&&Array.isArray(i[0])&&Array.isArray(i[1])||e[2](i);let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){try{let v2=i[0][v0];typeof v2==="string"||e[0](v2);let v3=i[1][v0];if(v3===null){v3=void 0}else if(!(typeof v3==="number"&&!Number.isNaN(v3)&&(v3<=2147483647&&v3>=-2147483648&&v3%1===0))){e[1](v3)}v1[v0]={"foo":v2,"bar":v3,};}catch(v4){v4.path='["'+v0+'"]'+v4.path;throw v4}}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];let v2=v1["bar"];if(v2===void 0){v2=null}else if(!(typeof v2==="number"&&!Number.isNaN(v2)&&(v2<=2147483647&&v2>=-2147483648&&v2%1===0))){e[0](v2)}v4[v0]={"foo":v1["foo"],"bar":v2,}}catch(v3){v3.path='["'+v0+'"]'+v3.path;throw v3}}let v6=[new Array(v4.length),new Array(v4.length),];for(let v5=0;v5<v4.length;++v5){v6[0][v5]=v4[v5]["foo"];v6[1][v5]=v4[v5]["bar"];}return v6}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, null]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": undefined}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": undefined}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, null]]`),
  )
})

test("Case with missing item at the end", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.option(S.string)),
            "bar": s.matches(S.bool),
          }
        ),
      ),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{Array.isArray(i)&&i.length===2&&Array.isArray(i[0])&&Array.isArray(i[1])||e[2](i);let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){try{let v2=i[0][v0];if(!(typeof v2==="string"||v2===void 0)){e[0](v2)}let v3=i[1][v0];typeof v3==="boolean"||e[1](v3);v1[v0]={"foo":v2,"bar":v3,};}catch(v4){v4.path='["'+v0+'"]'+v4.path;throw v4}}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];}catch(v2){v2.path='["'+v0+'"]'+v2.path;throw v2}}let v4=[new Array(i.length),new Array(i.length),];for(let v3=0;v3<i.length;++v3){v4[0][v3]=i[v3]["foo"];v4[1][v3]=i[v3]["bar"];}return v4}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [true, true, false]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": true}, {"foo": "b", "bar": true}, {"foo": undefined, "bar": false}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": true}, {"foo": "b", "bar": true}, {"foo": undefined, "bar": false}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b", undefined], [true, true, false]]`),
  )
})

test("Handles empty objects", t => {
  let schema = S.compactColumns(S.unknown)->S.to(S.array(S.object(_ => ())))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{Array.isArray(i)&&i.length===0||e[0](i);return []}`,
  )

  // Parse empty columnar input to empty array
  t->Assert.deepEqual(%raw(`[]`)->S.parseOrThrow(schema), %raw(`[]`))
})

test("Handles non-object schemas", t => {
  let schema = S.compactColumns(S.unknown)->S.to(S.array(S.tuple2(S.string, S.int)))
  t->Assert.throws(
    () => {
      %raw(`[["a"], [0]]`)->S.parseOrThrow(schema)
    },
    ~expectations={
      message: "[Sury] S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(S.array(objectSchema)).",
    },
  )
})

test("Schema has format field set to compactColumns", t => {
  let schema = S.compactColumns(S.unknown)
  t->Assert.deepEqual((schema->S.untag).format, Some(CompactColumns))
})

test("Typed input schema (non-unknown inputSchema branch)", t => {
  // Exercises the non-unknown branch of itemSchema derivation,
  // where input.schema.additionalItems is walked twice.
  let schema =
    S.compactColumns(S.string)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.string),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], ["c", "d"]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": "c"}, {"foo": "b", "bar": "d"}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": "c"}, {"foo": "b", "bar": "d"}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], ["c", "d"]]`),
  )
})

test("Invalid field value reports error with path", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.int),
          }
        ),
      ),
    )

  // Second row, bar column contains a non-int value.
  t->U.assertThrowsMessage(
    () => %raw(`[["a", "b"], [0, "not-an-int"]]`)->S.parseOrThrow(schema),
    `Failed at ["1"]["bar"]: Expected int32, received "not-an-int"`,
  )
})

test("Error path reporting for invalid column value", t => {
  // Asserts that validation errors carry a useful path to the offending cell.
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.int),
          }
        ),
      ),
    )

  t->U.assertThrowsMessage(
    () => %raw(`[["a"], ["not-an-int"]]`)->S.parseOrThrow(schema),
    `Failed at ["0"]["bar"]: Expected int32, received "not-an-int"`,
  )
})

asyncTest("Async field schema", async t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string->S.transform(_ => {asyncParser: async i => i})),
            "bar": s.matches(S.int),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    await %raw(`[["a", "b"], [0, 1]]`)->S.parseAsyncOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`),
  )
})

test("Field schema with S.transform", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string->S.transform(_ => {parser: v => v->Js.String2.toUpperCase})),
            "bar": s.matches(S.int),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, 1]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "A", "bar": 0}, {"foo": "B", "bar": 1}]`),
  )
})

test("Nullable field (null | undefined)", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.nullable(S.string)),
            "bar": s.matches(S.int),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", null], [0, 1]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": null, "bar": 1}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": null, "bar": 1}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", null], [0, 1]]`),
  )
})

test("More than 2 fields", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "a": s.matches(S.string),
            "b": s.matches(S.int),
            "c": s.matches(S.bool),
            "d": s.matches(S.float),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["x", "y"], [1, 2], [true, false], [1.5, 2.5]]`)->S.parseOrThrow(schema),
    %raw(`[{"a": "x", "b": 1, "c": true, "d": 1.5}, {"a": "y", "b": 2, "c": false, "d": 2.5}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"a": "x", "b": 1, "c": true, "d": 1.5}, {"a": "y", "b": 2, "c": false, "d": 2.5}]`)->S.reverseConvertOrThrow(
      schema,
    ),
    %raw(`[["x", "y"], [1, 2], [true, false], [1.5, 2.5]]`),
  )
})

test("Single-field object", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "only": s.matches(S.string),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b", "c"]]`)->S.parseOrThrow(schema),
    %raw(`[{"only": "a"}, {"only": "b"}, {"only": "c"}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"only": "a"}, {"only": "b"}, {"only": "c"}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b", "c"]]`),
  )
})

test("Union field", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.union([S.int->S.castToUnknown, S.string->S.castToUnknown])),
            "bar": s.matches(S.bool),
          }
        ),
      ),
    )

  t->Assert.deepEqual(
    %raw(`[[1, "two"], [true, false]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": 1, "bar": true}, {"foo": "two", "bar": false}]`),
  )
})

test("Field with S.refine", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "age": s.matches(S.int->S.refine(age => age >= 0, ~error="Age must be non-negative")),
            "name": s.matches(S.string),
          }
        ),
      ),
    )

  // Valid row parses successfully.
  t->Assert.deepEqual(
    %raw(`[[10, 20], ["alice", "bob"]]`)->S.parseOrThrow(schema),
    %raw(`[{"age": 10, "name": "alice"}, {"age": 20, "name": "bob"}]`),
  )

  // Negative age triggers the refinement error.
  t->U.assertThrowsMessage(
    () => %raw(`[[-5], ["bad"]]`)->S.parseOrThrow(schema),
    `Failed at ["0"]["age"]: Age must be non-negative`,
  )
})

test("reverseConvertToJsonOrThrow with nullable field", t => {
  S.enableJson()
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.nullAsOption(S.int)),
          }
        ),
      ),
    )

  let value = %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": undefined}]`)
  t->Assert.deepEqual(
    value->S.reverseConvertToJsonOrThrow(schema),
    %raw(`[["a", "b"], [0, null]]`),
  )
})

test("Roundtrip: parse -> reverseConvert -> parse", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.nullAsOption(S.int)),
          }
        ),
      ),
    )

  let columnar = %raw(`[["a", "b", "c"], [0, null, 2]]`)
  let rows = columnar->S.parseOrThrow(schema)
  let roundtripped = rows->S.reverseConvertOrThrow(schema)->S.parseOrThrow(schema)
  t->Assert.deepEqual(rows, roundtripped)
})

test("reverseConvertToJsonOrThrow validates non-JSON-able unknown field values", t => {
  S.enableJson()
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.unknown),
          }
        ),
      ),
    )

  // JSON-compatible values round-trip through the columnar form unchanged.
  t->Assert.deepEqual(
    %raw(`[{"foo": "hello"}, {"foo": 42}]`)->S.reverseConvertToJsonOrThrow(schema),
    %raw(`[["hello", 42]]`),
  )

  // Non-JSON-able values (e.g. bigint) are rejected by the json step that
  // runs after the rows → columnar conversion. The path ["0"]["0"] points
  // at column 0, row 0 of the columnar output (i.e. the "foo" value of the
  // first row).
  t->U.assertThrowsMessage(
    () => %raw(`[{"foo": 123n}]`)->S.reverseConvertToJsonOrThrow(schema),
    `Failed at ["0"]["0"]: Expected JSON, received 123n`,
  )
})

test("Json source with bigint field converts string↔bigint", t => {
  S.enableJson()
  let schema =
    S.compactColumns(S.json)->S.to(
      S.array(
        S.schema(s =>
          {
            "id": s.matches(S.string),
            "amount": s.matches(S.bigint),
          }
        ),
      ),
    )

  // Forward: json strings are converted to bigint via BigInt()
  t->Assert.deepEqual(
    %raw(`[["0", "1"], ["12345678901234567890", "98765432109876543210"]]`)->S.parseOrThrow(schema),
    %raw(`[{"id": "0", "amount": 12345678901234567890n}, {"id": "1", "amount": 98765432109876543210n}]`),
  )

  // Reverse: bigint values are converted back to strings for json
  t->Assert.deepEqual(
    %raw(`[{"id": "0", "amount": 12345678901234567890n}, {"id": "1", "amount": 98765432109876543210n}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["0", "1"], ["12345678901234567890", "98765432109876543210"]]`),
  )
})

test("Json source roundtrip with bigint", t => {
  S.enableJson()
  let schema =
    S.compactColumns(S.json)->S.to(
      S.array(
        S.schema(s =>
          {
            "id": s.matches(S.string),
            "amount": s.matches(S.bigint),
          }
        ),
      ),
    )

  let columnar = %raw(`[["0", "1"], ["12345678901234567890", "98765432109876543210"]]`)
  let rows = columnar->S.parseOrThrow(schema)
  let roundtripped = rows->S.reverseConvertOrThrow(schema)->S.parseOrThrow(schema)
  t->Assert.deepEqual(rows, roundtripped)
})
