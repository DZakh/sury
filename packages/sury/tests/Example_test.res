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
    }->S.convertOrThrow(~from=filmSchema, ~to=S.json),
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
    `i=>{try{let v0=i.tags,v5=i.deprecatedAgeRestriction;Array.isArray(v0)||e[1](v0);for(let v2=0;v2<v0.length;++v2){let v3=v0[v2];typeof v3==="string"||e[0](v3,["tags",v2]);}let v4={Id:i.id,Title:i.title,Tags:v0,Rating:i.rating};if(v5!==void 0){v4.Age=v5}return v4}catch(v6){e[2](v6)}}`,
  )
})

test("Compiled parse code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Parse,
    `i=>{try{typeof i==="object"&&i&&!Array.isArray(i)||e[7](i);let v0=i.Id,v1=i.Title,v2=i.Tags,v5=i.Rating,v6=i.Age;typeof v0==="number"&&v0==v0||e[0](v0);typeof v1==="string"||e[1](v1);if(v2===void 0){v2=e[2]}else{Array.isArray(v2)||e[4](v2);for(let v3=0;v3<v2.length;++v3){let v4=v2[v3];typeof v4==="string"||e[3](v4,["Tags",v3]);}}typeof v5==="string"&&(v5==="G"||v5==="PG"||v5==="PG13"||v5==="R")||e[5](v5);(typeof v6==="number"&&v6==v6&&v6<=2147483647&&v6>=-2147483648&&v6%1==0||v6===void 0)||e[6](v6);return {id:v0,title:v1,tags:v2,rating:v5,deprecatedAgeRestriction:v6}}catch(v7){e[8](v7)}}`,
  )
})

test("Compiled serialize code snapshot", t => {
  t->U.assertCompiledCode(
    ~schema=filmSchema,
    ~op=#Encode,
    `i=>{try{let v0=i.tags;Array.isArray(v0)||e[1](v0);for(let v2=0;v2<v0.length;++v2){let v3=v0[v2];typeof v3==="string"||e[0](v3,["tags",v2]);}return {Id:i.id,Title:i.title,Tags:v0,Rating:i.rating,Age:i.deprecatedAgeRestriction}}catch(v4){e[2](v4)}}`,
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
    ->S.meta({name: `Set.t<${S.toInputExpression(itemSchema)}>`})
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
