open Vitest

module CommonWithNested = {
  let value = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
  let any = %raw(`{"key1":"value1","key2":"value2"}`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`{"key1":"value1","key2":true}`)
  let factory = () => S.dict(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.convertOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected { [key: string]: string; }, received true`,
    )
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => nestedInvalidAny->S.parseOrThrow(~to=schema),
      `Failed at key2: Expected string, received true`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{try{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);for(let v0 in i){let v1=i[v0];typeof v1==="string"||e[0](v1,[v0]);}return i}catch(v2){e[2](v2)}}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.dict(
      S.unknown->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never}),
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{try{try{typeof i==="object"&&i&&!Array.isArray(i)||e[2](i);let v2={};for(let v0 in i){let v1;try{v1=e[0](i[v0]).catch(x=>e[1](x,[v0]))}catch(x){e[1](x,[v0])}v2[v0]=v1}return new Promise((v3,v4)=>{let v6=Object.keys(v2).length;if(!v6){v3(v2)}for(let v0 in v2){v2[v0].then(v5=>{v2[v0]=v5;if(v6--===1){v3(v2)}},v4)}})}catch(v7){e[3](v7)}}catch(v8){return Promise.reject(v8)}}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = S.dict(S.string)
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)

    let schema = S.dict(S.option(S.string))
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
  })

  test("Compiled serialize code snapshot with transform", t => {
    let schema = S.dict(S.nullAsOption(S.string))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{try{let v2={};for(let v0 in i){let v1=i[v0];for(;;){if(typeof v1==="string")break;if(v1===void 0){v1=null;break}e[0](v1,[v0])}v2[v0]=v1}return v2}catch(v3){e[1](v3)}}`,
    )
  })

  test("Reverse to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, value)
  })
}

test("Reverse child schema", t => {
  let schema = S.dict(S.nullAsOption(S.string))
  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.dict(S.union([S.string->S.castToUnknown, S.nullAsUnit->S.reverse]))->S.castToUnknown,
  )
})

test("Successfully parses dict with int keys", t => {
  let schema = S.dict(S.string)

  t->Assert.deepEqual(
    %raw(`{1:"b",2:"d"}`)->S.parseOrThrow(~to=schema),
    Dict.fromArray([("1", "b"), ("2", "d")]),
  )
})

test("Applies operation for each item on serializing", t => {
  let schema = S.dict(S.jsonString->S.to(S.int))

  t->Assert.deepEqual(
    Dict.fromArray([("a", 1), ("b", 2)])->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{
        "a": "1",
        "b": "2",
      }`),
  )
})

test("Fails to serialize dict item", t => {
  let schema = S.dict(S.string->S.refine(_ => false, ~error="User error"))

  t->U.assertThrowsMessage(
    () => Dict.fromArray([("a", "aa"), ("b", "bb")])->S.convertOrThrow(~from=schema, ~to=S.unknown),
    `Failed at a: User error`,
  )
})

test("Successfully parses dict with optional items", t => {
  let schema = S.dict(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`{"key1":"value1","key2":undefined}`)->S.parseOrThrow(~to=schema),
    Dict.fromArray([("key1", Some("value1")), ("key2", None)]),
  )
})
