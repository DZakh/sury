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
    `i=>{try{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);let v0=(i.foo??null);for(;;){if(typeof v0==="object"&&v0&&!Array.isArray(v0)){for(;;){if(v0.type==="a"){v0=true;break}if(v0.type==="b"){v0=false;break}throw e[0](v0)};break}if(v0===null){v0=void 0;break}throw e[0](v0)}return {foo:v0}}catch(v1){e[2](v1)}}`,
  )

  t->Assert.deepEqual(
    %raw("{}")->S.convertOrThrow(~from=S.json, ~to=schema),
    %raw(`{"foo": undefined}`),
  )

  t->Assert.deepEqual(
    %raw(`{"foo": null}`)->S.convertOrThrow(~from=S.json, ~to=schema),
    %raw(`{"foo": undefined}`),
  )

  t->Assert.deepEqual(
    %raw(`{"foo": {"type": "a"}}`)->S.convertOrThrow(~from=S.json, ~to=schema),
    %raw(`{"foo": true}`),
  )
})
