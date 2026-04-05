open Ava

module CommonWithNested = {
  let value = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
  let any = %raw(`{"key1":"value1","key2":"value2"}`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`{"key1":"value1","key2":true}`)
  let factory = () => S.dict(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(schema),
      `Expected { [key: string]: string; }, received true`,
    )
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => nestedInvalidAny->S.parseOrThrow(schema),
      `Failed at ["key2"]: Expected string, received true`,
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)){e[1](i)}for(let v0 in i){try{let v1=i[v0];if(typeof v1!=="string"){e[0](v1)}}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.dict(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)){e[2](i)}let v3={};for(let v0 in i){try{let v1;try{v1=e[0](i[v0]).catch(x=>e[1](x))}catch(x){e[1](x)}v3[v0]=v1.catch(v2=>{v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2})}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}let v8=new Promise((v4,v5)=>{let v7=Object.keys(v3).length;for(let v0 in v3){v3[v0].then(v6=>{v3[v0]=v6;if(v7--===1){v4(v3)}},v5)}});return v8}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = S.dict(S.string)
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)

    let schema = S.dict(S.option(S.string))
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Compiled serialize code snapshot with transform", t => {
    let schema = S.dict(S.nullAsOption(S.string))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v3={};for(let v0 in i){try{let v1=i[v0];if(v1===void 0){v1=null}else if(!(typeof v1==="string")){e[0](v1)}v3[v0]=v1}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}return v3}`,
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
    %raw(`{1:"b",2:"d"}`)->S.parseOrThrow(schema),
    Dict.fromArray([("1", "b"), ("2", "d")]),
  )
})

test("Applies operation for each item on serializing", t => {
  S.enableJsonString()

  let schema = S.dict(S.jsonString->S.to(S.int))

  t->Assert.deepEqual(
    Dict.fromArray([("a", 1), ("b", 2)])->S.reverseConvertOrThrow(schema),
    %raw(`{
        "a": "1",
        "b": "2",
      }`),
  )
})

test("Fails to serialize dict item", t => {
  let schema = S.dict(S.string->S.refine(_ => false, ~error="User error"))

  t->U.assertThrowsMessage(
    () => Dict.fromArray([("a", "aa"), ("b", "bb")])->S.reverseConvertOrThrow(schema),
    `Failed at ["a"]: User error`,
  )
})

test("Successfully parses dict with optional items", t => {
  let schema = S.dict(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`{"key1":"value1","key2":undefined}`)->S.parseOrThrow(schema),
    Dict.fromArray([("key1", Some("value1")), ("key2", None)]),
  )
})
