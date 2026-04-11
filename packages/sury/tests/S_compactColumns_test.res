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
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i[0])||!Array.isArray(i[1])){e[2](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){let v2=i[0][v0];if(typeof v2!=="string"){e[0](v2)}let v3=i[1][v0];if(typeof v3!=="number"||v3>2147483647||v3<-2147483648||v3%1!==0){e[1](v3)}v1[v0]={"foo":v2,"bar":v3,};}return v1}`,
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
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i[0])||!Array.isArray(i[1])){e[2](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){let v2=i[0][v0];if(typeof v2!=="string"){e[0](v2)}let v3=i[1][v0];if(v3===null){v3=void 0}else if(!(typeof v3==="number"&&!Number.isNaN(v3)&&(v3<2147483647&&v3>-2147483648&&v3%1===0))){e[1](v3)}v1[v0]={"foo":v2,"bar":v3,};}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];let v2=v1["bar"];if(v2===void 0){v2=null}else if(!(typeof v2==="number"&&!Number.isNaN(v2)&&(v2<2147483647&&v2>-2147483648&&v2%1===0))){e[0](v2)}v4[v0]={"foo":v1["foo"],"bar":v2,}}catch(v3){v3.path='["'+v0+'"]'+v3.path;throw v3}}let v6=[new Array(v4.length),new Array(v4.length),];for(let v5=0;v5<v4.length;++v5){v6[0][v5]=v4[v5]["foo"];v6[1][v5]=v4[v5]["bar"];}return v6}`,
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
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i[0])||!Array.isArray(i[1])){e[2](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){let v2=i[0][v0];if(!(typeof v2==="string"||v2===void 0)){e[0](v2)}let v3=i[1][v0];if(typeof v3!=="boolean"){e[1](v3)}v1[v0]={"foo":v2,"bar":v3,};}return v1}`,
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
    `i=>{if(!Array.isArray(i)||i.length!==0){e[0](i)}return []}`,
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
