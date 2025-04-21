open Ava
open U

@schema
type simpleObject = {"label": string, "value": int}
test("Simple object schema", t => {
  t->assertEqualSchemas(
    simpleObjectSchema,
    S.object(s =>
      {
        "label": s.field("label", S.string),
        "value": s.field("value", S.int),
      }
    ),
  )
  t->Assert.deepEqual(
    %raw(`{label:"foo",value:1}`)->S.parseOrThrow(simpleObjectSchema),
    {"label": "foo", "value": 1},
    (),
  )
})
