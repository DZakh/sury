open Ava
open RescriptCore

module Common = {
  let value = None
  let any = %raw(`undefined`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.option(S.string)

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

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(!(typeof i==="string"||i===void 0)){e[0](i)}return i}`,
    )
  })

  // Undefined check should be first
  test("Compiled async parse code snapshot", t => {
    let schema = S.option(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{try{i=e[0](i)}catch(e0){if(!(i===void 0)){e[1](i,e0)}}return Promise.resolve(i)}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
  })

  test("Reverse to self", t => {
    let schema = factory()
    t->Assert.is(schema->S.reverse, schema->S.toUnknown, ())
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, Some("abc"))
    t->U.assertReverseParsesBack(schema, None)
  })
}

test("Classify schema", t => {
  let schema = S.option(S.null(S.string))

  t->U.assertEqualSchemas(
    schema->S.toUnknown,
    S.union([S.string->S.toUnknown, S.unit->S.toUnknown, S.literal(%raw(`null`))->S.toUnknown]),
  )

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.union([
      S.string->S.toUnknown,
      S.unit->S.toUnknown,
      S.literal({"BS_PRIVATE_NESTED_SOME_NONE": 0})->S.toUnknown,
    ]),
  )
})

test("Successfully parses primitive", t => {
  let schema = S.option(S.bool)

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(schema), Some(true), ())
})

test("Fails to parse JS null", t => {
  let schema = S.option(S.bool)

  t->U.assertThrows(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`null`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse JS undefined when schema doesn't allow optional data", t => {
  let schema = S.bool

  t->U.assertThrows(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`undefined`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Serializes Some(None) to undefined for option nested in null", t => {
  let schema = S.null(S.option(S.bool))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), Some(None), ())
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None, ())

  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===null){i=void 0}else if(i===void 0){i={"BS_PRIVATE_NESTED_SOME_NONE":0}}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i===void 0){i=null}else if(typeof i==="object"&&i&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}return i}`,
  )
})

test("Applies valFromOption for Some()", t => {
  let schema = S.option(S.literal())

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
  t->Assert.deepEqual(Some()->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(i===void 0)){e[0](i)}return i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}return i}`,
  )
})

test("Nested option support", t => {
  let schema = S.option(S.option(S.bool))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
  t->Assert.deepEqual(Some(Some(true))->S.reverseConvertOrThrow(schema), %raw(`true`), ())
  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}return i}`,
  )
})

test("Triple nested option support", t => {
  let schema = S.option(S.option(S.option(S.bool)))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
  t->Assert.deepEqual(Some(Some(Some(true)))->S.reverseConvertOrThrow(schema), %raw(`true`), ())
  t->Assert.deepEqual(Some(Some(None))->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}else if(i["BS_PRIVATE_NESTED_SOME_NONE"]===1){i=void 0}}return i}`,
  )
})

test(
  "Empty object in option: S.option(S.object(_ => ())) https://github.com/DZakh/rescript-schema/issues/110",
  t => {
    let schema = S.option(S.object(_ => ()))

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), None, ())
    t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(schema), Some(), ())
    t->Assert.deepEqual(Some()->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
    t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i){i={"BS_PRIVATE_NESTED_SOME_NONE":0}}else if(!(i===void 0)){e[0](i)}return i}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(typeof i==="object"&&i&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=undefined}return i}`,
    )
  },
)

test("Doesn't apply valFromOption for non-undefined literals in option", t => {
  let schema: S.t<option<Null.t<unknown>>> = S.option(S.literal(%raw(`null`)))

  // Note: It'll fail without a type annotation, but we can't do anything here
  t->Assert.deepEqual(Some(%raw(`null`))->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Option with unknown", t => {
  let schema = S.option(S.unknown)

  t->Assert.deepEqual(
    Some(%raw(`undefined`))->S.reverseConvertOrThrow(schema),
    %raw(`{BS_PRIVATE_NESTED_SOME_NONE: 0}`),
    (),
  )
  t->Assert.deepEqual(Some(%raw(`"foo"`))->S.reverseConvertOrThrow(schema), %raw(`"foo"`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Parse)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Option with transformed unknown", t => {
  let schema = S.option(S.unknown->S.shape(v => {"field": v}))

  t->Assert.deepEqual(
    Some(%raw(`undefined`))->S.reverseConvertOrThrow(schema),
    %raw(`undefined`),
    (),
  )
  t->Assert.deepEqual(
    Some({"field": %raw(`"foo"`)})->S.reverseConvertOrThrow(schema),
    %raw(`"foo"`),
    (),
  )
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // TODO: Validation can be removed
    `i=>{try{let v0={"field":i,};if(typeof v0!=="object"||!v0){e[0](v0)}i=v0}catch(e0){if(!(i===void 0)){e[1](i,e0)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i){i=i["field"]}return i}`,
  )
})
