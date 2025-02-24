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

    t->U.assertRaised(
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

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(i!==null&&(typeof i!=="string")){e[0](i)}let v0;if(i!==null){v0=i}else{v0=void 0}return v0}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.null(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{let v0;if(i!==null){v0=e[0](i)}else{v0=Promise.resolve(void 0)}return v0}`,
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

  test("Compiled serialize code snapshot", t => {
    let schema = factory()
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v0;if(i!==void 0){v0=i}else{v0=null}return v0}`,
    )
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

  t->U.assertRaised(
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

  t->U.assertRaised(
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

  t->U.assertRaised(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`null`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully parses null and serializes it back for deprecated nullable schema", t => {
  let schema = S.null(S.bool)->S.deprecate("Deprecated")

  t->Assert.deepEqual(
    %raw(`null`)->S.parseOrThrow(schema)->S.reverseConvertOrThrow(schema),
    %raw(`null`),
    (),
  )
})

test("Parses null nested in option as None instead of Some(None)", t => {
  let schema = S.option(S.null(S.bool))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None, ())
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
})

test("Serializes Some(None) to null for null nested in option", t => {
  let schema = S.option(S.null(S.bool))

  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v2;if(i!==void 0){let v0=e[0](i),v1;if(v0!==void 0){v1=v0}else{v1=null}v2=v1}return v2}`,
  )
})

test("Serializes Some(None) to null for null nested in null", t => {
  let schema = S.null(S.null(S.bool))

  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v2;if(i!==void 0){let v0=e[0](i),v1;if(v0!==void 0){v1=v0}else{v1=null}v2=v1}else{v2=null}return v2}`,
  )
})
