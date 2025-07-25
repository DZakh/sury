open Ava

test("Successfully parses and reverse converts a simple object with unnest", t => {
  let schema = S.unnest(
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
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i["0"])||!Array.isArray(i["1"])){e[0](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){try{let v3=i[0][v0],v4=i[1][v0];if(typeof v3!=="string"){e[1](v3)}if(typeof v4!=="number"||v4>2147483647||v4<-2147483648||v4%1!==0){e[2](v4)}v1[v0]={"foo":v3,"bar":v4,};}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v0+\'"]\'+v2.path}throw v2}}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){v1[v0]={"foo":i[0][v0],"bar":i[1][v0],};}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){v1[0][v0]=i[v0]["foo"];v1[1][v0]=i[v0]["bar"];}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(!Array.isArray(i)){e[0](i)}let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){try{if(typeof i[v0]!=="object"||!i[v0]){e[1](i[v0])}let v3=i[v0]["foo"],v4=i[v0]["bar"];if(typeof v3!=="string"){e[2](v3)}if(typeof v4!=="number"||v4>2147483647||v4<-2147483648||v4%1!==0){e[3](v4)}v1[0][v0]=v3;v1[1][v0]=v4;}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v0+\'"]\'+v2.path}throw v2}}return v1}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, 1]]`)->S.parseOrThrow(schema),
    [{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}],
  )

  t->Assert.deepEqual(
    [{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, 1]]`),
  )

  let example = S.unnest(
    S.schema(s =>
      {
        "id": s.matches(S.string),
        "name": s.matches(S.null(S.string)),
        "deleted": s.matches(S.bool),
      }
    ),
  )
  t->U.assertCompiledCode(
    ~schema=example,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){try{let v3=i[v0]["name"];if(v3===void 0){v3=null}v1[0][v0]=i[v0]["id"];v1[1][v0]=v3;v1[2][v0]=i[v0]["deleted"];}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v0+\'"]\'+v2.path}throw v2}}return v1}`,
  )
})

test("Transforms nullable fields", t => {
  let schema = S.unnest(
    S.schema(s =>
      {
        "foo": s.matches(S.string),
        "bar": s.matches(S.null(S.int)),
      }
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i["0"])||!Array.isArray(i["1"])){e[0](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){try{let v3=i[0][v0];if(typeof v3!=="string"){e[1](v3)}let v4=i[1][v0];if(v4===null){v4=void 0}else if(!(typeof v4==="number"&&v4<2147483647&&v4>-2147483648&&v4%1===0)){e[2](v4)}v1[v0]={"foo":v3,"bar":v4,};}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v0+\'"]\'+v2.path}throw v2}}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){try{let v3=i[v0]["bar"];if(v3===void 0){v3=null}v1[0][v0]=i[v0]["foo"];v1[1][v0]=v3;}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v0+\'"]\'+v2.path}throw v2}}return v1}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, null]]`)->S.parseOrThrow(schema),
    [{"foo": "a", "bar": Some(0)}, {"foo": "b", "bar": None}],
  )

  t->Assert.deepEqual(
    [{"foo": "a", "bar": Some(0)}, {"foo": "b", "bar": None}]->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, null]]`),
  )
})

test("Case with missing item at the end", t => {
  let schema = S.unnest(
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
    `i=>{if(!Array.isArray(i)||i.length!==2||!Array.isArray(i["0"])||!Array.isArray(i["1"])){e[0](i)}let v1=new Array(Math.max(i[0].length,i[1].length,));for(let v0=0;v0<v1.length;++v0){try{let v4=i[1][v0];let v3=i[0][v0];if(!(typeof v3==="string"||v3===void 0)){e[1](v3)}if(typeof v4!=="boolean"){e[2](v4)}v1[v0]={"foo":v3,"bar":v4,};}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v0+\'"]\'+v2.path}throw v2}}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v1=[new Array(i.length),new Array(i.length),];for(let v0=0;v0<i.length;++v0){v1[0][v0]=i[v0]["foo"];v1[1][v0]=i[v0]["bar"];}return v1}`,
  )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [true, true, false]]`)->S.parseOrThrow(schema),
    [{"foo": Some("a"), "bar": true}, {"foo": Some("b"), "bar": true}, {"foo": None, "bar": false}],
  )

  t->Assert.deepEqual(
    [
      {"foo": Some("a"), "bar": true},
      {"foo": Some("b"), "bar": true},
      {"foo": None, "bar": false},
    ]->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b", undefined], [true, true, false]]`),
  )
})

test("Handles empty objects", t => {
  t->Assert.throws(
    () => {
      S.unnest(S.object(_ => ()))
    },
    ~expectations={
      message: "[Sury] Invalid empty object for S.unnest schema.",
    },
  )
})

test("Handles non-object schemas", t => {
  t->Assert.throws(
    () => {
      S.unnest(S.tuple2(S.string, S.int))
    },
    ~expectations={
      message: "[Sury] S.unnest supports only object schemas.",
    },
  )
})
