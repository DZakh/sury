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
    %raw(`{"Id": 1, "Title": "My first film", "Rating": "R", "Age": 17}`)->S.parseOrThrow(~to=
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
    `i=>{let v0=i["tags"],v5=i["deprecatedAgeRestriction"];if(Array.isArray(v0)){for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];typeof v2==="string"||e[0](v2);}catch(v3){v3.path="[\\"tags\\"]"+'["'+v1+'"]'+v3.path;throw v3}}}else{e[1](v0)}let v4={"Id":i["id"],"Title":i["title"],"Tags":v0,"Rating":i["rating"],};if(v5!==void 0){v4["Age"]=v5}return v4}`,
  )
})

test("Compiled parse code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[7](i);let v0=i["Id"],v1=i["Title"],v2=i["Tags"],v7=i["Rating"],v8=i["Age"];typeof v0==="number"&&!Number.isNaN(v0)||e[0](v0);typeof v1==="string"||e[1](v1);for(;;){if(Array.isArray(v2)){for(let v3=0;v3<v2.length;++v3){try{let v4=v2[v3];typeof v4==="string"||e[2](v4);}catch(v5){v5.path="[\\"Tags\\"]"+\'["\'+v3+\'"]\'+v5.path;throw v5}};break}if(v2===void 0){v2=e[3];break}e[4](v2)}typeof v7==="string"&&(v7==="G"||v7==="PG"||v7==="PG13"||v7==="R")||e[5](v7);(typeof v8==="number"&&!Number.isNaN(v8)&&v8<=2147483647&&v8>=-2147483648&&v8%1===0||v8===void 0)||e[6](v8);return {"id":v0,"title":v1,"tags":v2,"rating":v7,"deprecatedAgeRestriction":v8,}}`,
  )
})

test("Compiled serialize code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Encode,
    `i=>{let v0=i["tags"];if(Array.isArray(v0)){for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];typeof v2==="string"||e[0](v2);}catch(v3){v3.path="[\\"tags\\"]"+'["'+v1+'"]'+v3.path;throw v3}}}else{e[1](v0)}return {"Id":i["id"],"Title":i["title"],"Tags":v0,"Rating":i["rating"],"Age":i["deprecatedAgeRestriction"],}}`,
  )
})

test("Custom schema", t => {
  let mySet = itemSchema => {
    S.instance(%raw(`Set`))
    ->S.to(S.any, ~custom={decode: Sync(input => {
        let output = Set.make()
        input
        ->Obj.magic
        ->Set.forEach(
          item => {
            output->Set.add(S.parseOrThrow(item, ~to=itemSchema))
          },
        )
        output
      }), encode: Never})
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
