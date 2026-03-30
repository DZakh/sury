open Ava

test("Literal schema", t => {
  t->U.assertEqualSchemas(S.schema(_ => 1), S.literal(1))
  t->U.assertEqualSchemas(S.schema(_ => ()), S.literal())
  t->U.assertEqualSchemas(S.schema(_ => "foo"), S.literal("foo"))
})

test("Tuple of literals schema", t => {
  t->U.assertEqualSchemas(
    S.schema(_ => (1, (), "bar")),
    S.tuple3(S.literal(1), S.literal(), S.literal("bar")),
  )
})

test("Object with embeded schema", t => {
  let schema = S.schema(s =>
    {
      "foo": "bar",
      "zoo": s.matches(S.int),
    }
  )
  let objectSchema = S.object(s =>
    {
      "foo": s.field("foo", S.literal("bar")),
      "zoo": s.field("zoo", S.int),
    }
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    objectSchema->U.getCompiledCodeString(~op=#Parse),
    ~message=`i=>{if(typeof i!=="object"||!i||i["foo"]!=="bar"){e[0](i)}let v0=i["zoo"];if(typeof v0!=="number"||v0>2147483647||v0<-2147483648||v0%1!==0){e[1](v0)}return {"foo":"bar","zoo":v0,}}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  t->Assert.is(
    objectSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{return {"foo":"bar","zoo":i["zoo"],}}`,
  )
})

test("Object with embeded transformed schema", t => {
  let schema = S.schema(s =>
    {
      "foo": "bar",
      "zoo": s.matches(S.nullAsOption(S.int)),
    }
  )
  let objectSchema = S.object(s =>
    {
      "foo": s.field("foo", S.literal("bar")),
      "zoo": s.field("zoo", S.nullAsOption(S.int)),
    }
  )
  // t->U.assertEqualSchemas(schema, objectSchema)
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    objectSchema->U.getCompiledCodeString(~op=#Parse),
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["zoo"];if(v0===void 0){v0=null}else if(!(typeof v0==="number"&&!Number.isNaN(v0)&&(v0<2147483647&&v0>-2147483648&&v0%1===0))){e[0](v0)}return {"foo":"bar","zoo":v0,}}`,
  )
  t->Assert.is(
    objectSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["zoo"];if(v0===void 0){v0=null}else if(!(typeof v0==="number"&&!Number.isNaN(v0)&&(v0<2147483647&&v0>-2147483648&&v0%1===0))){e[0](v0)}return {"foo":"bar","zoo":v0,}}`,
  )
})

test("Strict object with embeded returns input without object recreation", t => {
  S.global({
    defaultAdditionalItems: Strict,
  })
  let schema = S.schema(s =>
    {
      "foo": "bar",
      "zoo": s.matches(S.int),
    }
  )
  S.global({})

  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    `i=>{if(typeof i!=="object"||!i||Array.isArray(i)){e[3](i)}let v0=i["foo"],v1=i["zoo"],v2;if(v0!=="bar"){e[0](v0)}if(typeof v1!=="number"||v1>2147483647||v1<-2147483648||v1%1!==0){e[1](v1)}for(v2 in i){if(v2!=="foo"&&v2!=="zoo"){e[2](v2)}}return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Tuple with embeded schema", t => {
  let schema = S.schema(s => (s.matches(S.string), (), "bar"))
  let tupleSchema = S.tuple(s => (
    s.item(0, S.string),
    s.item(1, S.literal()),
    s.item(2, S.literal("bar")),
  ))

  // t->U.assertEqualSchemas(schema, tupleSchema)
  // S.schema does return i without tuple recreation
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    `i=>{if(!Array.isArray(i)||i.length!==3){e[3](i)}let v0=i["0"],v1=i["1"],v2=i["2"];if(typeof v0!=="string"){e[0](v0)}if(v1!==void 0){e[1](v1)}if(v2!=="bar"){e[2](v2)}return i}`,
  )
  t->Assert.is(
    tupleSchema->U.getCompiledCodeString(~op=#Parse),
    `i=>{if(!Array.isArray(i)||i.length!==3){e[3](i)}let v0=i["0"],v1=i["1"],v2=i["2"];if(typeof v0!=="string"){e[0](v0)}if(v1!==void 0){e[1](v1)}if(v2!=="bar"){e[2](v2)}return [v0,v1,v2,]}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  t->Assert.is(
    tupleSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{return [i["0"],void 0,"bar",]}`,
  )
})

test("Tuple with embeded transformed schema", t => {
  let schema = S.schema(s => (s.matches(S.nullAsOption(S.string)), (), "bar"))
  let tupleSchema = S.tuple(s => (
    s.item(0, S.nullAsOption(S.string)),
    s.item(1, S.literal()),
    s.item(2, S.literal("bar")),
  ))

  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    tupleSchema->U.getCompiledCodeString(~op=#Parse),
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["0"];if(v0===void 0){v0=null}else if(!(typeof v0==="string")){e[0](v0)}return [v0,void 0,"bar",]}`,
  )
  t->Assert.is(
    tupleSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["0"];if(v0===void 0){v0=null}else if(!(typeof v0==="string")){e[0](v0)}return [v0,void 0,"bar",]}`,
  )
})

test("Nested object with embeded schema", t => {
  let schema = S.schema(s =>
    {
      "nested": {
        "foo": "bar",
        "zoo": s.matches(S.int),
      },
    }
  )
  let objectSchema = S.object(s =>
    {
      "nested": s.field(
        "nested",
        S.object(
          s =>
            {
              "foo": s.field("foo", S.literal("bar")),
              "zoo": s.field("zoo", S.int),
            },
        ),
      ),
    }
  )

  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    objectSchema->U.getCompiledCodeString(~op=#Parse),
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["nested"];return i}`,
  )
  t->Assert.is(
    objectSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["nested"];return {"nested":{"foo":"bar","zoo":v0["zoo"],},}}`,
  )
})

@unboxed
type answer =
  | Text(string)
  | MultiSelect(array<string>)
  | Other({value: string, @as("description") maybeDescription: option<string>})

test("Example", t => {
  t->U.assertEqualSchemas(S.schema(s => Text(s.matches(S.string))), S.string->S.castToAny)
  t->U.assertEqualSchemas(
    S.schema(s => MultiSelect(s.matches(S.array(S.string)))),
    S.array(S.string)->S.castToAny,
  )
  t->U.assertReverseReversesBack(
    S.schema(s => Other({
      value: s.matches(S.string),
      maybeDescription: s.matches(S.option(S.string)),
    })),
  )
  t->U.assertReverseReversesBack(S.schema(s => (#id, s.matches(S.string))))
})

test(
  "Strict object schema should also check that object is not Array. Otherwise it will incorrectly return array input",
  t => {
    let schema = S.schema(s =>
      {
        "0": s.matches(S.string),
        "1": s.matches(S.bool),
      }
    )

    t->Assert.deepEqual(%raw(`["foo", true]`)->S.parseOrThrow(schema), {"0": "foo", "1": true})

    t->U.assertThrowsMessage(
      () => %raw(`["foo", true]`)->S.parseOrThrow(schema->S.strict),
      `Expected { 0: string; 1: boolean; }, received ["foo", true]`,
    )
  },
)

test(
  "Strict tuple schema should check the exact number of items, but it can optimize input recreation",
  t => {
    let schema = S.schema(s => (s.matches(S.string), s.matches(S.bool)))->S.strict

    t->Assert.deepEqual(%raw(`["foo", true]`)->S.parseOrThrow(schema), ("foo", true))

    t->U.assertThrowsMessage(
      () => %raw(`["foo", true, 1]`)->S.parseOrThrow(schema),
      `Expected [string, boolean], received ["foo", true, 1]`,
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)||i.length!==2){e[2](i)}let v0=i["0"],v1=i["1"];if(typeof v0!=="string"){e[0](v0)}if(typeof v1!=="boolean"){e[1](v1)}return i}`,
    )
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  },
)

test("Object schema with empty object field", t => {
  let schema = S.schema(_ =>
    {
      "foo": Dict.make(),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{"foo": "bar"}`)->S.parseOrThrow(schema),
    `Failed at ["foo"]: Expected {}, received "bar"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[1](i)}let v0=i["foo"];if(typeof v0!=="object"||!v0){e[0](v0)}return {"foo":{},}}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Object schema with nested object field containing only literal", t => {
  let schema = S.schema(_ =>
    {
      "foo": %raw(`{"bar": "baz"}`),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{"foo": {"bar": "bap"}}`)->S.parseOrThrow(schema),
    `Failed at ["foo"]["bar"]: Expected "baz", received "bap"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foo"];if(typeof v0!=="object"||!v0){e[1](v0)}let v1=v0["bar"];if(v1!=="baz"){e[0](v1)}return {"foo":{"bar":v1,},}}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("https://github.com/DZakh/sury/issues/131", t => {
  let testSchema = S.schema(s =>
    {
      "foobar": s.matches(S.array(S.option(S.string))),
    }
  )

  let json = (%raw(`{"weird": true}`): JSON.t)
  t->U.assertThrowsMessage(
    () => json->S.parseOrThrow(testSchema),
    `Failed at ["foobar"]: Expected (string | undefined)[], received undefined`,
  )

  t->U.assertCompiledCode(
    ~schema=testSchema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foobar"];if(!Array.isArray(v0)){e[1](v0)}for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];if(!(typeof v2==="string"||v2===void 0)){e[0](v2)}}catch(v3){v3.path="[\\"foobar\\"]"+\'["\'+v1+\'"]\'+v3.path;throw v3}}return {"foobar":v0,}}`,
  )
})
