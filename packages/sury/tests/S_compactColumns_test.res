open Ava

test("Successfully parses and reverse converts a simple object with compactColumns", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i[0])||!Array.isArray(i[1])){e[0](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){v1[v0]={"foo":i[0][v0],"bar":i[1][v0],};}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){v1[0][v0]=i[v0]["foo"];v1[1][v0]=i[v0]["bar"];}return v1}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, 1]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, 1]]`),
  )

  let example =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "id": s.matches(S.string),
          "name": s.matches(S.nullAsOption(S.string)),
          "deleted": s.matches(S.bool),
        }
      ),
    )
  t->U.assertCompiledCode(
    ~schema=example,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){v1[0][v0]=i[v0]["id"];v1[1][v0]=i[v0]["name"];v1[2][v0]=i[v0]["deleted"];}return v1}`,
  )
})

test("Transforms nullable fields", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.nullAsOption(S.int)),
        }
      ),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i[0])||!Array.isArray(i[1])){e[0](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){v1[v0]={"foo":i[0][v0],"bar":i[1][v0],};}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){v1[0][v0]=i[v0]["foo"];v1[1][v0]=i[v0]["bar"];}return v1}`,
  )

  // Note: compactColumns creates raw objects, field-level transformations like nullAsOption
  // are not applied (null stays as null, not transformed to undefined)
  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, null]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": null}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": null}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, null]]`),
  )
})

test("Case with missing item at the end", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.option(S.string)),
          "bar": s.matches(S.bool),
        }
      ),
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i[0])||!Array.isArray(i[1])){e[0](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){v1[v0]={"foo":i[0][v0],"bar":i[1][v0],};}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){v1[0][v0]=i[v0]["foo"];v1[1][v0]=i[v0]["bar"];}return v1}`,
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
  let schema = S.compactColumns(S.unknown)->S.to(S.object(_ => ()))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)||i.length!==0){e[0](i)}return []}`,
  )

  // Parse empty columnar input to empty array
  t->Assert.deepEqual(%raw(`[]`)->S.parseOrThrow(schema), %raw(`[]`))
})

test("Handles non-object schemas", t => {
  let schema = S.compactColumns(S.unknown)->S.to(S.tuple2(S.string, S.int))
  t->Assert.throws(
    () => {
      %raw(`[["a"], [0]]`)->S.parseOrThrow(schema)
    },
    ~expectations={
      message: "[Sury] S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(objectSchema).",
    },
  )
})

test("Schema has format field set to compactColumns", t => {
  let schema = S.compactColumns(S.unknown)
  t->Assert.deepEqual((schema->S.untag).format, Some(CompactColumns))
})
