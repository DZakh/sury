open Ava

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
