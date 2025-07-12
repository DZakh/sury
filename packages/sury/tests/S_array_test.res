open Ava

module CommonWithNested = {
  let value = ["Hello world!", ""]
  let any = %raw(`["Hello world!", ""]`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`["Hello world!", 1]`)
  let factory = () => S.array(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.castToUnknown, received: invalidAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertThrows(
      () => nestedInvalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: S.string->S.castToUnknown, received: 1->Obj.magic}),
        operation: Parse,
        path: S.Path.fromArray(["1"]),
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any)
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)){e[0](i)}for(let v0=0;v0<i.length;++v0){try{let v2=i[v0];if(typeof v2!=="string"){e[1](v2)}}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.array(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{if(!Array.isArray(i)){e[0](i)}let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v2;try{v2=e[1](i[v0]).catch(v1=>{if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1})}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}v3[v0]=v2}return Promise.all(v3)}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = S.array(S.string)
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)

    let schema = S.array(S.option(S.string))
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Compiled serialize code snapshot with transform", t => {
    let schema = S.array(S.null(S.string))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v3;try{let v2=i[v0];if(v2===void 0){v2=null}v3=v2}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}v4[v0]=v3}return v4}`,
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
  let schema = S.array(S.null(S.string))

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.array(S.union([S.string->S.castToUnknown, S.nullAsUnit->S.reverse]))->S.castToUnknown,
  )
})

test("Successfully parses matrix", t => {
  let schema = S.array(S.array(S.string))

  t->Assert.deepEqual(
    %raw(`[["a", "b"], ["c", "d"]]`)->S.parseOrThrow(schema),
    [["a", "b"], ["c", "d"]],
  )
})

test("Fails to parse matrix", t => {
  let schema = S.array(S.array(S.string))

  t->U.assertThrows(
    () => %raw(`[["a", 1], ["c", "d"]]`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.castToUnknown, received: %raw(`1`)}),
      operation: Parse,
      path: S.Path.fromArray(["0", "1"]),
    },
  )
})

test("Successfully parses array of optional items", t => {
  let schema = S.array(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`["a", undefined, undefined, "b"]`)->S.parseOrThrow(schema),
    [Some("a"), None, None, Some("b")],
  )
})
