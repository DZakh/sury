open Vitest

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
      `i=>{for(;;){if(typeof i==="string")break;if(i===null){i=void 0;break}e[0](i)}return i}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{for(;;){if(typeof i==="string")break;if(i===void 0){i=null;break}e[0](i)}return i}`,
    )
  })

  test("Compiled async parse code snapshot", t => {
    let schema = S.nullAsOption(
      S.unknown->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never}),
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{return Promise.resolve((async(i)=>{for(;;){let r;try{let v0=e[0](i);i=await v0;break}catch(x){(r||(r=[])).push(e[1](x))}if(i===null){i=void 0;break}e[2](i,...(r||[]))};return i})(i))}`,
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
    `i=>{for(;;){if(typeof i==="boolean")break;if(i===void 0)break;if(i===null){i={BS_PRIVATE_NESTED_SOME_NONE:0};break}e[0](i)}return i}`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="boolean")break;if(i===void 0)break;if(typeof i==="object"&&i&&!Array.isArray(i)&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=null;break}e[0](i)}return i}`,
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
    `i=>{for(;;){if(typeof i==="boolean")break;if(i===null){i=void 0;break}e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="boolean")break;if(i===void 0){i=null;break}if(typeof i==="object"&&i&&!Array.isArray(i)&&i["BS_PRIVATE_NESTED_SOME_NONE"]===0){i=null;break}e[0](i)}return i}`,
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
    t->Assert.deepEqual(
      record->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      %raw(`{ record: null }`),
    )
    t->Assert.deepEqual(record->S.decodeOrThrow(~from=schema, ~to=S.jsonString), `{"record":null}`)

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{let v0=i["record"];for(;;){if(typeof v0==="object"&&v0&&!Array.isArray(v0)&&v0["BS_PRIVATE_NESTED_SOME_NONE"]===0){v0=null;break}if(typeof v0==="object"&&v0&&!Array.isArray(v0)){let v1=v0["k"];for(;;){if(typeof v1==="number"&&!Number.isNaN(v1)&&v1<=2147483647&&v1>=-2147483648&&v1%1===0)break;if(v1===void 0)break;if(typeof v1==="object"&&v1&&!Array.isArray(v1)&&v1["BS_PRIVATE_NESTED_SOME_NONE"]===0){v1=null;break}e[0](v1)}v0={k:v1};break}if(v0===void 0)break;e[1](v0)}return {record:v0}}`,
    )
  })
}
