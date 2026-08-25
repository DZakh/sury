open Vitest

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
      ~to=filmSchema,
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
    }->S.decodeOrThrow(~from=filmSchema, ~to=S.json),
    %raw(`{
        "Id": 2,
        "Title": "Sad & sed",
        "Rating": "PG13",
        "Tags": ["Loved"],
      }`),
  )
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#EncodeToJson,
    `i=>{let v0=i["tags"],v6=i["deprecatedAgeRestriction"];Array.isArray(v0)||e[1](v0);for(let v2=0;v2<v0.length;++v2){try{let v3=v0[v2];typeof v3==="string"||e[0](v3);}catch(v4){v4.path="[\\"tags\\"]"+'["'+v2+'"]'+v4.path;throw v4}}let v5={Id:i["id"],Title:i["title"],Tags:v0,Rating:i["rating"]};if(v6!==void 0){v5["Age"]=v6}return v5}`,
  )
})

test("Compiled parse code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[7](i);let v0=i["Id"],v1=i["Title"],v2=i["Tags"],v6=i["Rating"],v7=i["Age"];typeof v0==="number"&&v0===v0||e[0](v0);typeof v1==="string"||e[1](v1);if(v2===void 0){v2=e[2]}else{Array.isArray(v2)||e[4](v2);for(let v3=0;v3<v2.length;++v3){try{let v4=v2[v3];typeof v4==="string"||e[3](v4);}catch(v5){v5.path="[\\"Tags\\"]"+\'["\'+v3+\'"]\'+v5.path;throw v5}}}typeof v6==="string"&&(v6==="G"||v6==="PG"||v6==="PG13"||v6==="R")||e[5](v6);(typeof v7==="number"&&v7===v7&&v7<=2147483647&&v7>=-2147483648&&v7%1===0||v7===void 0)||e[6](v7);return {id:v0,title:v1,tags:v2,rating:v6,deprecatedAgeRestriction:v7}}`,
  )
})

test("Compiled serialize code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Encode,
    `i=>{let v0=i["tags"];Array.isArray(v0)||e[1](v0);for(let v2=0;v2<v0.length;++v2){try{let v3=v0[v2];typeof v3==="string"||e[0](v3);}catch(v4){v4.path="[\\"tags\\"]"+'["'+v2+'"]'+v4.path;throw v4}}return {Id:i["id"],Title:i["title"],Tags:v0,Rating:i["rating"],Age:i["deprecatedAgeRestriction"]}}`,
  )
})

test("Custom schema", t => {
  let mySet = itemSchema => {
    S.instance(%raw(`Set`))
    ->S.to(
      S.any,
      ~custom={
        decode: Sync(
          input => {
            let output = Set.make()
            input
            ->Obj.magic
            ->Set.forEach(item => {
              output->Set.add(S.parseOrThrow(item, ~to=itemSchema))
            })
            output
          },
        ),
        encode: Never,
      },
    )
    ->S.meta({name: `Set.t<${S.inputExpression(itemSchema)}>`})
  }

  let intSetSchema = mySet(S.int)

  t->Assert.deepEqual(
    S.parseOrThrow(%raw(`new Set([1, 2, 3])`), ~to=intSetSchema),
    Set.fromArray([1, 2, 3]),
  )
  t->U.assertThrowsMessage(
    () => S.parseOrThrow(%raw(`new Set([1, 2, "3"])`), ~to=intSetSchema),
    `Expected int32, received "3"`,
  )
  t->U.assertThrowsMessage(
    () => S.parseOrThrow(%raw(`[1, 2, 3]`), ~to=intSetSchema),
    `Expected Set.t<int32>, received [1, 2, 3]`,
  )
})
