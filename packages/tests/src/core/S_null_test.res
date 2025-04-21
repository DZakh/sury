open Ava

module Common = {
  let value = None
  let any = %raw(`null`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.null(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrows(
      () => invalidAny->S.parseOrThrow(schema),
      {
        code: InvalidType({expected: schema->S.toUnknown, received: invalidAny}),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), any, ())
  })

  test("Compiled code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(i===null){i=void 0}else if(!(typeof i==="string")){e[0](i)}return i}`,
    )
    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i===void 0){i=null}return i}`)
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.null(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{try{i=e[0](i)}catch(e0){if(i===null){i=void 0}else{e[1](i,e0)}}return Promise.resolve(i)}`,
    )
  })

  test("Reverse schema to option", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, S.option(S.string)->S.toUnknown)
  })

  test("Reverse of reverse returns the original schema", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, Some("abc"))
    t->U.assertReverseParsesBack(schema, None)
  })
}

test("Successfully parses primitive", t => {
  let schema = S.null(S.bool)

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(schema), Some(true), ())
})

test("Fails to parse JS undefined", t => {
  let schema = S.null(S.bool)

  t->U.assertThrows(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`undefined`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse object with missing field that marked as null", t => {
  let fieldSchema = S.null(S.string)
  let schema = S.object(s => s.field("nullableField", fieldSchema))

  t->U.assertThrows(
    () => %raw(`{}`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: fieldSchema->S.toUnknown, received: %raw(`undefined`)}),
      operation: Parse,
      path: S.Path.fromArray(["nullableField"]),
    },
  )
})

test("Fails to parse JS null when schema doesn't allow optional data", t => {
  let schema = S.bool

  t->U.assertThrows(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`null`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully parses null and serializes it back for deprecated nullable schema", t => {
  let schema = S.null(S.bool)->S.meta({description: "Deprecated", deprecated: true})

  t->Assert.deepEqual(
    %raw(`null`)->S.parseOrThrow(schema)->S.reverseConvertOrThrow(schema),
    %raw(`null`),
    (),
  )
})

test("Serializes Some(None) to null for null nested in option", t => {
  let schema = S.option(S.null(S.bool))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), Some(None), ())
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())

  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===null){i={"BS_PRIVATE_NESTED_SOME_NONE":0}}else if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=null}return i}`,
  )
})

test("Serializes Some(None) to null for null nested in null", t => {
  let schema = S.null(S.null(S.bool))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None, ())

  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===null){i=void 0}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i===void 0){i=null}else if(typeof i==="object"&&i&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=null}return i}`,
  )
})
