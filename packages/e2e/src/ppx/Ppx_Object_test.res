open Ava
open U

@schema
type simpleObject = {"label": string, "value": int}
test("Simple object schema", t => {
  t->assertEqualSchemas(
    simpleObjectSchema,
    S.schema(s =>
      {
        "label": s.matches(S.string),
        "value": s.matches(S.int),
      }
    ),
  )
  t->Assert.deepEqual(
    %raw(`{label:"foo",value:1}`)->S.parseOrThrow(simpleObjectSchema),
    {"label": "foo", "value": 1},
  )
})

// @schema
// type objectWithAlias = {@as("aliased-label") "label": string, "value": int}
// test("The @as attribute for the object schema is ignored since it doesn't work", t => {
//   t->assertEqualSchemas(
//     objectWithAliasSchema,
//     S.schema(s =>
//       {
//         "label": s.matches(S.string),
//         "value": s.matches(S.int),
//       }
//     ),
//   )
//   t->Assert.deepEqual(
//     %raw(`{"label":"foo",value:1}`)->S.parseOrThrow(objectWithAliasSchema),
//     {"label": "foo", "value": 1},
//   )
// })
