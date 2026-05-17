open Ava

module Common = {
  let value = None
  let any = %raw(`null`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.nullAsOption(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected string | null, received 123.45`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Compiled code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(i===null){i=void 0}else if(!(typeof i==="string")){e[0](i)}return i}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{if(i===void 0){i=null}else if(!(typeof i==="string")){e[0](i)}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.nullAsOption(S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{try{let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}i=v0}catch(e0){if(i===null){i=void 0}else{e[2](i,e0)}}return Promise.resolve(i)}`,
    )
  })

  test("Reverses schema to option", t => {
    let schema = factory()
    t->U.assertEqualSchemas(
      schema->S.reverse,
      S.union([S.string->S.castToUnknown, S.nullAsUnit->S.reverse]),
    )
  })

  test("Reverse of reverse returns the original schema", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse->S.reverse, schema->S.castToUnknown)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, Some("abc"))
    t->U.assertReverseParsesBack(schema, None)
  })
}

test("Successfully parses primitive", t => {
  let schema = S.nullAsOption(S.bool)

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(~to=schema), Some(true))
})

test("Fails to parse JS undefined", t => {
  let schema = S.nullAsOption(S.bool)

  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(~to=schema),
    `Expected boolean | null, received undefined`,
  )
})

test("Fails to parse object with missing field that marked as null", t => {
  let fieldSchema = S.nullAsOption(S.string)
  let schema = S.object(s => s.field("nullableField", fieldSchema))

  t->U.assertThrowsMessage(
    () => %raw(`{}`)->S.parseOrThrow(~to=schema),
    `Failed at ["nullableField"]: Expected string | null, received undefined`,
  )
})

test("Fails to parse JS null when schema doesn't allow optional data", t => {
  let schema = S.bool

  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Expected boolean, received null`,
  )
})

test("Successfully parses null and serializes it back for deprecated nullable schema", t => {
  let schema = S.nullAsOption(S.bool)->S.meta({description: "Deprecated", deprecated: true})

  t->Assert.deepEqual(
    %raw(`null`)->S.parseOrThrow(~to=schema)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`null`),
  )
})

test("Serializes Some(None) to null for null nested in option", t => {
  let schema = S.option(S.nullAsOption(S.bool))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), Some(None))
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)

  t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===null){i={BS_PRIVATE_NESTED_SOME_NONE:0}}else if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=null}}else if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )
})

test("Serializes Some(None) to null for null nested in null", t => {
  let schema = S.nullAsOption(S.nullAsOption(S.bool))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)

  t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===null){i=void 0}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i===void 0){i=null}else if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=null}}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
})

// https://github.com/DZakh/sury/issues/150
module OuterRecord = {
  module Inner = {
    type t = {k?: option<int>}

    let schema = S.schema((s): t => {
      k: ?s.matches(S.option(S.nullAsOption(S.int))),
    })
  }

  type t = {record?: option<Inner.t>}

  let schema = S.schema(s => {
    record: ?s.matches(S.option(S.nullAsOption(Inner.schema))),
  })

  test("Record schema with optional nullable field", t => {
    let record = {record: None}

    t->Assert.deepEqual(record, %raw(`{ record: { BS_PRIVATE_NESTED_SOME_NONE: 0 } }`))
    t->Assert.deepEqual(record->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{ record: null }`))
    t->Assert.deepEqual(record->S.decodeOrThrow(~from=schema, ~to=S.jsonString), `{"record":null}`)

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{let v0=i["record"];if(typeof v0==="object"&&v0&&!Array.isArray(v0)){if(v0["BS_PRIVATE_NESTED_SOME_NONE"]===0){v0=null}else{try{let v1=v0["k"];if(typeof v1==="object"&&v1&&!Array.isArray(v1)){if(v1["BS_PRIVATE_NESTED_SOME_NONE"]===0){v1=null}}else if(!(typeof v1==="number"&&!Number.isNaN(v1)&&(v1<=2147483647&&v1>=-2147483648&&v1%1===0)||v1===void 0)){e[0](v1)}v0={"k":v1,}}catch(e1){e[1](v0,e1)}}}else if(!(v0===void 0)){e[2](v0)}return {"record":v0,}}`,
    )
  })
}
