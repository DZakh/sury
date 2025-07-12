open Ava
open RescriptCore

@dead
type rating =
  | @as("G") GeneralAudiences
  | @as("PG") ParentalGuidanceSuggested
  | @as("PG13") ParentalStronglyCautioned
  | @as("R") Restricted

@dead
type film = {
  id: float,
  title: string,
  tags: array<string>,
  rating: rating,
  deprecatedAgeRestriction: option<int>,
}

let filmSchema = S.object(s => {
  id: s.field("Id", S.float),
  title: s.field("Title", S.string),
  tags: s.fieldOr("Tags", S.array(S.string), []),
  rating: s.field(
    "Rating",
    S.union([
      S.literal(GeneralAudiences),
      S.literal(ParentalGuidanceSuggested),
      S.literal(ParentalStronglyCautioned),
      S.literal(Restricted),
    ]),
  ),
  deprecatedAgeRestriction: s.field(
    "Age",
    S.option(S.int)->S.meta({description: "Use rating instead", deprecated: true}),
  ),
})

test("Example", t => {
  t->Assert.deepEqual(
    %raw(`{"Id": 1, "Title": "My first film", "Rating": "R", "Age": 17}`)->S.parseOrThrow(
      filmSchema,
    ),
    {
      id: 1.,
      title: "My first film",
      tags: [],
      rating: Restricted,
      deprecatedAgeRestriction: Some(17),
    },
  )
  t->Assert.deepEqual(
    {
      id: 2.,
      tags: ["Loved"],
      title: "Sad & sed",
      rating: ParentalStronglyCautioned,
      deprecatedAgeRestriction: None,
    }->S.reverseConvertToJsonOrThrow(filmSchema),
    %raw(`{
        "Id": 2,
        "Title": "Sad & sed",
        "Rating": "PG13",
        "Tags": ["Loved"],
        "Age": undefined,
      }`),
  )
})

test("Compiled parse code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["Id"],v1=i["Title"];if(typeof v0!=="number"||Number.isNaN(v0)){e[1](v0)}if(typeof v1!=="string"){e[2](v1)}let v6=i["Tags"];if(Array.isArray(v6)){let v2=i["Tags"];for(let v3=0;v3<v2.length;++v3){try{let v5=v2[v3];if(typeof v5!=="string"){e[3](v5)}}catch(v4){if(v4&&v4.s===s){v4.path="[\\"Tags\\"]"+\'["\'+v3+\'"]\'+v4.path}throw v4}}}else if(!(v6===void 0)){e[4](v6)}let v7=i["Rating"];if(!(typeof v7==="string"&&(v7==="G"||v7==="PG"||v7==="PG13"||v7==="R"))){e[6](v7)}let v8=i["Age"];if(!(typeof v8==="number"&&v8<2147483647&&v8>-2147483648&&v8%1===0||v8===void 0)){e[7](v8)}return {"id":v0,"title":v1,"tags":v6===void 0?e[5]:v6,"rating":v7,"deprecatedAgeRestriction":v8,}}`,
  )
})

test("Compiled serialize code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["tags"];let v5=i["rating"];return {"Id":i["id"],"Title":i["title"],"Tags":v0,"Rating":v5,"Age":i["deprecatedAgeRestriction"],}}`,
  )
})

test("Custom schema", t => {
  let mySet = itemSchema => {
    S.instance(%raw(`Set`))
    ->S.transform(_ => {
      parser: input => {
        let output = Set.make()
        input
        ->Obj.magic
        ->Set.forEach(
          item => {
            output->Set.add(S.parseOrThrow(item, itemSchema))
          },
        )
        output
      },
    })
    ->S.meta({name: `Set.t<${S.toExpression(itemSchema)}>`})
  }

  let intSetSchema = mySet(S.int)

  t->Assert.deepEqual(
    S.parseOrThrow(%raw(`new Set([1, 2, 3])`), intSetSchema),
    Set.fromArray([1, 2, 3]),
  )
  t->U.assertThrowsMessage(
    () => S.parseOrThrow(%raw(`new Set([1, 2, "3"])`), intSetSchema),
    `Failed parsing: Expected int32, received "3"`,
  )
  t->U.assertThrowsMessage(
    () => S.parseOrThrow(%raw(`[1, 2, 3]`), intSetSchema),
    `Failed parsing: Expected Set.t<int32>, received [1, 2, 3]`,
  )
})
