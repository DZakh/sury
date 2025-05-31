open Ava
open RescriptCore

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
  // t->U.assertEqualSchemas(schema, objectSchema)
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    objectSchema->U.getCompiledCodeString(~op=#Parse),
    ~message=`i=>{if(typeof i!=="object"||!i||i["foo"]!=="bar"){e[1](i)}let v0=i["zoo"];if(typeof v0!=="number"||v0>2147483647||v0<-2147483648||v0%1!==0){e[0](v0)}return {"foo":i["foo"],"zoo":v0,}}`,
    (),
  )
  t->Assert.is(schema->U.getCompiledCodeString(~op=#ReverseConvert), `i=>{return i}`, ())
  t->Assert.is(
    objectSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{return {"foo":i["foo"],"zoo":i["zoo"],}}`,
    (),
  )
})

test("Object with embeded transformed schema", t => {
  let schema = S.schema(s =>
    {
      "foo": "bar",
      "zoo": s.matches(S.null(S.int)),
    }
  )
  let objectSchema = S.object(s =>
    {
      "foo": s.field("foo", S.literal("bar")),
      "zoo": s.field("zoo", S.null(S.int)),
    }
  )
  // t->U.assertEqualSchemas(schema, objectSchema)
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    objectSchema->U.getCompiledCodeString(~op=#Parse),
    (),
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["zoo"];if(v0===void 0){v0=null}return {"foo":i["foo"],"zoo":v0,}}`,
    (),
  )
  t->Assert.is(
    objectSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["zoo"];if(v0===void 0){v0=null}return {"foo":i["foo"],"zoo":v0,}}`,
    (),
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
    `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||i["foo"]!=="bar"){e[2](i)}let v0=i["zoo"],v1;if(typeof v0!=="number"||v0>2147483647||v0<-2147483648||v0%1!==0){e[0](v0)}for(v1 in i){if(v1!=="foo"&&v1!=="zoo"){e[1](v1)}}return i}`,
    (),
  )
  t->Assert.is(schema->U.getCompiledCodeString(~op=#ReverseConvert), `i=>{return i}`, ())
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
    `i=>{if(!Array.isArray(i)||i.length!==3||i["1"]!==void 0||i["2"]!=="bar"){e[1](i)}let v0=i["0"];if(typeof v0!=="string"){e[0](v0)}return i}`,
    (),
  )
  t->Assert.is(
    tupleSchema->U.getCompiledCodeString(~op=#Parse),
    `i=>{if(!Array.isArray(i)||i.length!==3||i["1"]!==void 0||i["2"]!=="bar"){e[1](i)}let v0=i["0"];if(typeof v0!=="string"){e[0](v0)}return [v0,i["1"],i["2"],]}`,
    (),
  )
  t->Assert.is(schema->U.getCompiledCodeString(~op=#ReverseConvert), `i=>{return i}`, ())
  t->Assert.is(
    tupleSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{return [i["0"],i["1"],i["2"],]}`,
    (),
  )
})

test("Tuple with embeded transformed schema", t => {
  let schema = S.schema(s => (s.matches(S.null(S.string)), (), "bar"))
  let tupleSchema = S.tuple(s => (
    s.item(0, S.null(S.string)),
    s.item(1, S.literal()),
    s.item(2, S.literal("bar")),
  ))

  // t->U.assertEqualSchemas(schema, tupleSchema)
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    tupleSchema->U.getCompiledCodeString(~op=#Parse),
    (),
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["0"];if(v0===void 0){v0=null}return [v0,i["1"],i["2"],]}`,
    (),
  )
  t->Assert.is(
    tupleSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["0"];if(v0===void 0){v0=null}return [v0,i["1"],i["2"],]}`,
    (),
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
  // t->U.assertEqualSchemas(schema, objectSchema)

  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#Parse),
    objectSchema->U.getCompiledCodeString(~op=#Parse),
    (),
  )
  t->Assert.is(
    schema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["nested"];return i}`,
    (),
  )
  t->Assert.is(
    objectSchema->U.getCompiledCodeString(~op=#ReverseConvert),
    `i=>{let v0=i["nested"];return {"nested":{"foo":v0["foo"],"zoo":v0["zoo"],},}}`,
    (),
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

    t->Assert.deepEqual(%raw(`["foo", true]`)->S.parseOrThrow(schema), {"0": "foo", "1": true}, ())

    t->U.assertThrows(
      () => %raw(`["foo", true]`)->S.parseOrThrow(schema->S.strict),
      {
        code: InvalidType({
          expected: schema->S.strict->S.toUnknown,
          received: %raw(`["foo", true]`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  },
)

test(
  "Strict tuple schema should check the exact number of items, but it can optimize input recreation",
  t => {
    let schema = S.schema(s => (s.matches(S.string), s.matches(S.bool)))->S.strict

    t->Assert.deepEqual(%raw(`["foo", true]`)->S.parseOrThrow(schema), ("foo", true), ())

    t->U.assertThrows(
      () => %raw(`["foo", true, 1]`)->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: schema->S.strict->S.toUnknown,
          received: %raw(`["foo", true, 1]`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)||i.length!==2){e[2](i)}let v0=i["0"],v1=i["1"];if(typeof v0!=="string"){e[0](v0)}if(typeof v1!=="boolean"){e[1](v1)}return i}`,
    )
    // FIXME: Make it noop
    t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{return i}`)
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
    `Failed parsing: Expected { foo: {}; }, received { foo: "bar"; }`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["foo"]!=="object"||!i["foo"]){e[0](i)}return {"foo":{},}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return i}`)
})

test("Object schema with nested object field containing only literal", t => {
  let schema = S.schema(_ =>
    {
      "foo": %raw(`{"bar": "baz"}`),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{"foo": {"bar": "bap"}}`)->S.parseOrThrow(schema),
    `Failed parsing: Expected { foo: { bar: "baz"; }; }, received { foo: { bar: "bap"; }; }`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["foo"]!=="object"||!i["foo"]||i["foo"]["bar"]!=="baz"){e[0](i)}let v0=i["foo"];return {"foo":{"bar":v0["bar"],},}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{let v0=i["foo"];return i}`)
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
    `Failed parsing at ["foobar"]: Expected (string | undefined)[], received undefined`,
  )

  t->U.assertCompiledCode(
    ~schema=testSchema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foobar"],v5=[];if(!Array.isArray(v0)){e[0](v0)}for(let v1=0;v1<v0.length;++v1){let v3=v0[v1],v4;try{if(!(typeof v3==="string"||v3===void 0)){e[1](v3)}v4=v3}catch(v2){if(v2&&v2.s===s){v2.path="[\\"foobar\\"]"+\'["\'+v1+\'"]\'+v2.path}throw v2}v5[v1]=v4}return {"foobar":v5,}}`,
  )
})
