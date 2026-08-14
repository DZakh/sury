open Vitest

type user = {id: string, extra: dict<int>}
type row = {head: string, tail: array<bool>}

test("Object with s.rest collects the undeclared keys", t => {
  let schema = S.object(s => {
    id: s.field("USER_ID", S.string),
    extra: s.rest(S.int),
  })

  t->U.assertReverseReversesBack(schema)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[2](i);let v0=i["USER_ID"];typeof v0==="string"||e[0](v0);let v4={};for(let v1 in i){if(v1!=="USER_ID"){try{let v2=i[v1];typeof v2==="number"&&v2<=2147483647&&v2>=-2147483648&&v2%1===0||e[1](v2);v4[v1]=v2}catch(v3){v3.path='["'+v1+'"]'+v3.path;throw v3}}}return {"id":v0,"extra":v4,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0=i["extra"];return {...v0,"USER_ID":i["id"],}}`,
  )

  t->Assert.deepEqual(
    %raw(`{"USER_ID": "u1", "a": 1, "b": 2}`)->S.parseOrThrow(~to=schema),
    {id: "u1", extra: Dict.fromArray([("a", 1), ("b", 2)])},
  )
  t->Assert.deepEqual(
    {id: "u1", extra: Dict.fromArray([("a", 1)])}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"a": 1, "USER_ID": "u1"}`),
  )
})

test("Object with s.rest validates every undeclared key", t => {
  let schema = S.object(s => {
    id: s.field("USER_ID", S.string),
    extra: s.rest(S.int),
  })

  t->U.assertThrowsMessage(
    () => %raw(`{"USER_ID": "u1", "a": "nope"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["a"]: Expected int32, received "nope"`,
  )
})

test("Tuple with s.rest collects the items past the declared ones", t => {
  let schema = S.tuple(s => {
    head: s.item(0, S.string),
    tail: s.rest(S.bool),
  })

  t->U.assertReverseReversesBack(schema)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{Array.isArray(i)&&i.length>=1||e[2](i);let v0=i["0"];typeof v0==="string"||e[0](v0);let v4=[];for(let v1=1;v1<i.length;++v1){try{let v2=i[v1];typeof v2==="boolean"||e[1](v2);v4[v1-1]=v2}catch(v3){v3.path='["'+v1+'"]'+v3.path;throw v3}}return {"head":v0,"tail":v4,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{let v0=i["tail"];return [i["head"],...v0]}`)

  t->Assert.deepEqual(
    %raw(`["a", true, false]`)->S.parseOrThrow(~to=schema),
    {head: "a", tail: [true, false]},
  )
  t->Assert.deepEqual(
    {head: "a", tail: [true]}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`["a", true]`),
  )
})

test("Tuple with s.rest still requires the declared prefix", t => {
  let schema = S.tuple(s => {
    head: s.item(0, S.string),
    tail: s.rest(S.bool),
  })

  t->U.assertThrowsMessage(
    () => %raw(`[]`)->S.parseOrThrow(~to=schema),
    `Expected [string, ...boolean[]], received []`,
  )
})
