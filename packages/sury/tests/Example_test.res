open Ava

S.enableJson()

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
    `i=>{if(typeof i!=="object"||!i){e[7](i)}let v0=i["Id"],v1=i["Title"],v2=i["Tags"],v6=i["Rating"],v7=i["Age"];if(typeof v0!=="number"||Number.isNaN(v0)){e[0](v0)}if(typeof v1!=="string"){e[1](v1)}if(Array.isArray(v2)){for(let v3=0;v3<v2.length;++v3){try{let v4=v2[v3];if(typeof v4!=="string"){e[2](v4)}}catch(v5){v5.path="[\\"Tags\\"]"+\'["\'+v3+\'"]\'+v5.path;throw v5}}}else if(!(v2===void 0)){e[3](v2)}if(!(typeof v6==="string"&&(v6==="G"||v6==="PG"||v6==="PG13"||v6==="R"))){e[5](v6)}if(!(typeof v7==="number"&&!Number.isNaN(v7)&&(v7<2147483647&&v7>-2147483648&&v7%1===0)||v7===void 0)){e[6](v7)}return {"id":v0,"title":v1,"tags":v2===void 0?e[4]:v2,"rating":v6,"deprecatedAgeRestriction":v7,}}`,
  )
})

test("Compiled serialize code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#ReverseConvert,
    `i=>{return {"Id":i["id"],"Title":i["title"],"Tags":i["tags"],"Rating":i["rating"],"Age":i["deprecatedAgeRestriction"],}}`,
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
    `Expected int32, received "3"`,
  )
  t->U.assertThrowsMessage(
    () => S.parseOrThrow(%raw(`[1, 2, 3]`), intSetSchema),
    `Expected Set.t<int32>, received [1, 2, 3]`,
  )
})
