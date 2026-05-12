open Ava

module Common = {
  let value = None
  let any = %raw(`undefined`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.option(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected string | undefined, received 123.45`,
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
      `i=>{if(!(typeof i==="string"||i===void 0)){e[0](i)}return i}`,
    )
  })

  // Undefined check should be first ?
  test("Compiled async parse code snapshot", t => {
    let schema = S.option(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{try{let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}i=v0}catch(e0){if(!(i===void 0)){e[2](i,e0)}}return Promise.resolve(i)}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(!(typeof i==="string"||i===void 0)){e[0](i)}return i}`,
    )
  })

  test("Reverse to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, Some("abc"))
    t->U.assertReverseParsesBack(schema, None)
  })
}

test("Classify schema", t => {
  let schema = S.option(S.nullAsOption(S.string))

  t->U.assertEqualSchemas(
    schema->S.castToUnknown,
    S.union([
      S.string->S.castToUnknown,
      S.unit->S.castToUnknown,
      S.nullAsUnit->S.to(S.literal({"BS_PRIVATE_NESTED_SOME_NONE": 0}))->S.castToUnknown,
    ]),
  )

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.union([
      S.string->S.castToUnknown,
      S.unit->S.castToUnknown,
      S.literal({"BS_PRIVATE_NESTED_SOME_NONE": 0})->S.to(S.nullAsUnit->S.reverse)->S.castToUnknown,
    ]),
  )
})

test("Successfully parses primitive", t => {
  let schema = S.option(S.bool)

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(~to=schema), Some(true))
})

test("Fails to parse JS null", t => {
  let schema = S.option(S.bool)

  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined, received null`,
  )
})

test("Fails to parse JS undefined when schema doesn't allow optional data", t => {
  let schema = S.bool

  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(~to=schema),
    `Expected boolean, received undefined`,
  )
})

test("Serializes Some(None) to undefined for option nested in null", t => {
  let schema = S.nullAsOption(S.option(S.bool))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Some(None))
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)

  t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===null){i=void 0}else if(i===void 0){i={BS_PRIVATE_NESTED_SOME_NONE:0}}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i===void 0){i=null}else if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}else{e[0](i)}}else if(!(typeof i==="boolean")){e[1](i)}return i}`,
  )
})

test("Applies valFromOption for Some()", t => {
  let schema = S.option(S.literal())

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(Some()->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(i===void 0)){e[0](i)}return i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}else{e[0](i)}}else if(!(i===void 0)){e[1](i)}return i}`,
  )
})

test("Nested option support", t => {
  let schema = S.option(S.option(S.bool))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(Some(Some(true))->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))
  t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}else{e[0](i)}}else if(!(typeof i==="boolean"||i===void 0)){e[1](i)}return i}`,
  )
})

test("Triple nested option support", t => {
  let schema = S.option(S.option(S.option(S.bool)))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(Some(Some(Some(true)))->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))
  t->Assert.deepEqual(Some(Some(None))->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===1){i=void 0}else if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=void 0}else{e[0](i)}}else if(!(typeof i==="boolean"||i===void 0)){e[1](i)}return i}`,
  )
})

test(
  "Empty object in option: S.option(S.object(_ => ())) https://github.com/DZakh/rescript-schema/issues/110",
  t => {
    let schema = S.option(S.object(_ => ()))

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(~to=schema), Some())
    t->Assert.deepEqual(Some()->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{}`))
    t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){i={BS_PRIVATE_NESTED_SOME_NONE:0}}else if(!(i===void 0)){e[0](i)}return i}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i={}}else{e[0](i)}}else if(!(i===void 0)){e[1](i)}return i}`,
    )
  },
)

test("Doesn't apply valFromOption for non-undefined literals in option", t => {
  let schema: S.t<option<Null.t<unknown>>> = S.option(S.literal(%raw(`null`)))

  // Note: It'll fail without a type annotation, but we can't do anything here
  t->Assert.deepEqual(Some(%raw(`null`))->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(!(i===null||i===void 0)){e[0](i)}return i}`,
  )
})

test("Option with unknown", t => {
  let schema = S.option(S.unknown)

  t->Assert.deepEqual(
    Some(%raw(`undefined`))->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{BS_PRIVATE_NESTED_SOME_NONE: 0}`),
  )
  t->Assert.deepEqual(Some(%raw(`"foo"`))->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"foo"`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Parse)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Option with transformed unknown", t => {
  let schema = S.option(S.unknown->S.shape(v => {"field": v}))

  t->Assert.deepEqual(Some(%raw(`undefined`))->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(
    Some({"field": %raw(`"foo"`)})->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"foo"`),
  )
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{i={"field":i,}}catch(e0){if(!(i===void 0)){e[0](i,e0)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){i=i["field"]}else if(!(i===void 0)){e[0](i)}return i}`,
  )
})
