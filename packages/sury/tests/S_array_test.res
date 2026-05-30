open Vitest

module CommonWithNested = {
  let value = ["Hello world!", ""]
  let any = %raw(`["Hello world!", ""]`)
  let invalidAny = %raw(`true`)
  let nestedInvalidAny = %raw(`["Hello world!", 1]`)
  let factory = () => S.array(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected string[], received true`,
    )
  })

  test("Fails to parse nested", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => nestedInvalidAny->S.parseOrThrow(~to=schema),
      `Failed at ["1"]: Expected string, received 1`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{Array.isArray(i)||e[1](i);for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];typeof v1==="string"||e[0](v1);}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.array(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{Array.isArray(i)||e[2](i);let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v1;try{v1=e[0](i[v0]).catch(x=>e[1](x))}catch(x){e[1](x)}v3[v0]=v1.catch(v2=>{v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2})}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}return Promise.all(v3)}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = S.array(S.string)
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)

    let schema = S.array(S.option(S.string))
    t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
  })

  test("Compiled serialize code snapshot with transform", t => {
    let schema = S.array(S.nullAsOption(S.string))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];if(v1===void 0){v1=null}else if(!(typeof v1==="string")){e[0](v1)}v3[v0]=v1}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}return v3}`,
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
  let schema = S.array(S.nullAsOption(S.string))

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.array(S.union([S.string->S.castToUnknown, S.nullAsUnit->S.reverse]))->S.castToUnknown,
  )
})

test("Successfully parses matrix", t => {
  let schema = S.array(S.array(S.string))

  t->Assert.deepEqual(
    %raw(`[["a", "b"], ["c", "d"]]`)->S.parseOrThrow(~to=schema),
    [["a", "b"], ["c", "d"]],
  )
})

test("Fails to parse matrix", t => {
  let schema = S.array(S.array(S.string))

  t->U.assertThrowsMessage(
    () => %raw(`[["a", 1], ["c", "d"]]`)->S.parseOrThrow(~to=schema),
    `Failed at ["0"]["1"]: Expected string, received 1`,
  )
})

test("Successfully parses array of optional items", t => {
  let schema = S.array(S.option(S.string))

  t->Assert.deepEqual(
    %raw(`["a", undefined, undefined, "b"]`)->S.parseOrThrow(~to=schema),
    [Some("a"), None, None, Some("b")],
  )
})
