open Ava

module CommonWithNested = {
  let value = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
  let any = %raw(`{"key1":"value1","key2":"value2"}`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`{"key1":"value1","key2":true}`)
  let factory = () => S.dict(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertRaised(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.toUnknown, received: invalidAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertRaised(
      () => nestedInvalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.string->S.toUnknown, received: %raw(`true`)}),
        operation: Parse,
        path: S.Path.fromArray(["key2"]),
      },
    )
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[1](i)}for(let v0 in i){let v2=i[v0];try{if(typeof v2!=="string"){e[0](v2)}}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.dict(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[1](i)}let v3={};for(let v0 in i){let v2;try{v2=e[0](i[v0]).catch(v1=>{if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1})}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}v3[v0]=v2}return new Promise((v4,v5)=>{let v7=Object.keys(v3).length;for(let v0 in v3){v3[v0].then(v6=>{v3[v0]=v6;if(v7--===1){v4(v3)}},v5)}})}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = S.dict(S.string)
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)

    let schema = S.dict(S.option(S.string))
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Compiled serialize code snapshot with transform", t => {
    let schema = S.dict(S.null(S.string))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v5={};for(let v0 in i){let v2=i[v0],v4;try{let v3;if(v2!==void 0){v3=v2}else{v3=null}v4=v3}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}v5[v0]=v4}return v5}`,
    )
  })

  test("Reverse to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, value)
  })
}

test("Reverse child schema", t => {
  let schema = S.dict(S.null(S.string))
  t->U.assertEqualSchemas(schema->S.reverse, S.dict(S.option(S.string))->S.toUnknown)
})

test("Successfully parses dict with int keys", t => {
  let schema = S.dict(S.string)

  t->Assert.deepEqual(
    %raw(`{1:"b",2:"d"}`)->S.parseOrThrow(schema),
    Dict.fromArray([("1", "b"), ("2", "d")]),
    (),
  )
})

test("Applies operation for each item on serializing", t => {
  let schema = S.dict(S.jsonString(S.int))

  t->Assert.deepEqual(
    Dict.fromArray([("a", 1), ("b", 2)])->S.reverseConvertOrThrow(schema),
    %raw(`{
        "a": "1",
        "b": "2",
      }`),
    (),
  )
})

test("Fails to serialize dict item", t => {
  let schema = S.dict(S.string->S.refine(s => _ => s.fail("User error")))

  t->U.assertRaised(
    () => Dict.fromArray([("a", "aa"), ("b", "bb")])->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: ReverseConvert,
      path: S.Path.fromLocation("a"),
    },
  )
})

test("Successfully parses dict with optional items", t => {
  let schema = S.dict(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`{"key1":"value1","key2":undefined}`)->S.parseOrThrow(schema),
    Dict.fromArray([("key1", Some("value1")), ("key2", None)]),
    (),
  )
})
