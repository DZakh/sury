open Vitest

test("Union of tagged objects wrapped in S.option as a matches field", t => {
  let itemSchema = S.union([
    S.object(s => {
      s.tag("type", "a")
      true
    }),
    S.object(s => {
      s.tag("type", "b")
      false
    }),
  ])

  let schema = S.schema(s =>
    {
      "foo": s.matches(S.option(itemSchema)),
    }
  )

  t->U.assertCompiledCode(
    ~schema=S.json->S.to(schema),
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[2](i);let v0=(i["foo"]??null);if(typeof v0==="object"&&v0&&!Array.isArray(v0)){if(v0["type"]==="a"){v0=true}else if(v0["type"]==="b"){v0=false}else{e[0](v0)}}else if(v0===null){v0=void 0}else{e[1](v0)}return {"foo":v0,}}`,
  )

  t->Assert.deepEqual(
    %raw("{}")->S.decodeOrThrow(~from=S.json, ~to=schema),
    %raw(`{"foo": undefined}`),
  )

  t->Assert.deepEqual(
    %raw(`{"foo": null}`)->S.decodeOrThrow(~from=S.json, ~to=schema),
    %raw(`{"foo": undefined}`),
  )

  t->Assert.deepEqual(
    %raw(`{"foo": {"type": "a"}}`)->S.decodeOrThrow(~from=S.json, ~to=schema),
    %raw(`{"foo": true}`),
  )
})
