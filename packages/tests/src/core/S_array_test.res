open Ava

module CommonWithNested = {
  let value = ["Hello world!", ""]
  let any = %raw(`["Hello world!", ""]`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`["Hello world!", 1]`)
  let factory = () => S.array(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
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
        code: InvalidType({expected: S.string->S.toUnknown, received: 1->Obj.magic}),
        operation: Parse,
        path: S.Path.fromArray(["1"]),
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)){e[1](i)}for(let v0=0;v0<i.length;++v0){let v2=i[v0];try{if(typeof v2!=="string"){e[0](v2)}}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.array(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!Array.isArray(i)){e[1](i)}let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v2;try{v2=e[0](i[v0]).catch(v1=>{if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1})}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}v3[v0]=v2}return Promise.all(v3)}`,
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
      `i=>{let v5=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v2=i[v0],v4;try{let v3;if(v2!==void 0){v3=v2}else{v3=null}v4=v3}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}v5[v0]=v4}return v5}`,
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
  let schema = S.array(S.null(S.string))
  t->U.assertEqualSchemas(schema->S.reverse, S.array(S.option(S.string))->S.toUnknown)
})

test("Successfully parses matrix", t => {
  let schema = S.array(S.array(S.string))

  t->Assert.deepEqual(
    %raw(`[["a", "b"], ["c", "d"]]`)->S.parseOrThrow(schema),
    [["a", "b"], ["c", "d"]],
    (),
  )
})

test("Fails to parse matrix", t => {
  let schema = S.array(S.array(S.string))

  t->U.assertRaised(
    () => %raw(`[["a", 1], ["c", "d"]]`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.toUnknown, received: %raw(`1`)}),
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
    (),
  )
})
