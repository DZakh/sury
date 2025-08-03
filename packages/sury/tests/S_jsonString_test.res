open Ava
open RescriptCore

S.enableJsonString()

test("Parses JSON string without transformation", t => {
  let schema = S.jsonString

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(schema), `"Foo"`)
  t->U.assertThrowsMessage(
    () => `Foo`->S.parseOrThrow(schema),
    `Failed parsing: Expected JSON string, received "Foo"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}try{JSON.parse(i)}catch(t){e[1](i)}return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Parses JSON string to string", t => {
  let schema = S.jsonString->S.to(S.string)

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(schema), "Foo")
  t->U.assertThrowsMessage(
    () => `Foo`->S.parseOrThrow(schema),
    `Failed parsing: Expected JSON string, received "Foo"`,
  )
  t->U.assertThrowsMessage(
    () => `123`->S.parseOrThrow(schema),
    `Failed parsing: Expected string, received 123`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="string"){e[2](v0)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    "i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}return v0}",
  )

  t->Assert.deepEqual(`"Foo`->S.reverseConvertOrThrow(schema), %raw(`'"\\"Foo"'`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return JSON.stringify(i)}`)
})

test("Parses JSON string to string literal", t => {
  let schema = S.jsonString->S.to(S.literal("Foo"))

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(schema), "Foo")
  t->U.assertThrowsMessage(
    () => `123`->S.parseOrThrow(schema),
    `Failed parsing: Expected "Foo", received "123"`,
  )

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="\\"Foo\\""||e[0](i);return "Foo"}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{i==="\\"Foo\\""||e[0](i);return "Foo"}`)

  t->Assert.deepEqual(`Foo`->S.reverseConvertOrThrow(schema), %raw(`'"Foo"'`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!=="Foo"){e[0](i)}return "\\"Foo\\""}`,
  )

  let schema = S.jsonString->S.to(S.literal("\"Foo"))
  t->Assert.deepEqual(`"Foo`->S.reverseConvertOrThrow(schema), %raw(`'"\\"Foo"'`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!=="\\"Foo"){e[0](i)}return "\\"\\\\\\"Foo\\""}`,
  )
})

test("Parses JSON string to float", t => {
  let schema = S.jsonString->S.to(S.float)

  t->Assert.deepEqual(`1.23`->S.parseOrThrow(schema), 1.23)
  t->U.assertThrowsMessage(
    () => `null`->S.parseOrThrow(schema),
    `Failed parsing: Expected number, received null`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="number"||Number.isNaN(v0)){e[2](v0)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    "i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}return v0}",
  )

  t->Assert.deepEqual(1.23->S.reverseConvertOrThrow(schema), %raw(`"1.23"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Parses JSON string to float literal", t => {
  let schema = S.jsonString->S.to(S.literal(1.23))

  t->Assert.deepEqual(`1.23`->S.parseOrThrow(schema), 1.23)
  t->U.assertThrowsMessage(
    () => `null`->S.parseOrThrow(schema),
    `Failed parsing: Expected 1.23, received "null"`,
  )

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="1.23"||e[0](i);return 1.23}`)

  t->Assert.deepEqual(1.23->S.reverseConvertOrThrow(schema), %raw(`"1.23"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==1.23){e[0](i)}return "1.23"}`)
})

test("Parses JSON string to bool", t => {
  let schema = S.jsonString->S.to(S.bool)

  t->Assert.deepEqual(`true`->S.parseOrThrow(schema), true)
  t->U.assertThrowsMessage(
    () => `"t"`->S.parseOrThrow(schema),
    `Failed parsing: Expected boolean, received "t"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="boolean"){e[2](v0)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    "i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}return v0}",
  )

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Parses JSON string to bool literal", t => {
  let schema = S.jsonString->S.to(S.literal(true))

  t->Assert.deepEqual(`true`->S.parseOrThrow(schema), true)
  t->U.assertThrowsMessage(
    () => `null`->S.parseOrThrow(schema),
    `Failed parsing: Expected true, received "null"`,
  )

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="true"||e[0](i);return true}`)

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==true){e[0](i)}return "true"}`)
})

test("Parses JSON string to bigint", t => {
  let schema = S.jsonString->S.to(S.bigint)

  t->U.assertThrowsMessage(
    () => `123`->S.parseOrThrow(schema),
    `Failed parsing: bigint is not valid JSON`,
  )

  // t->Assert.deepEqual(`123`->S.parseOrThrow(schema), 123n)

  // t->U.assertCompiledCode(
  //   ~schema,
  //   ~op=#Parse,
  //   `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="boolean"){e[2](v0)}return v0}`,
  // )
  // t->U.assertCompiledCode(
  //   ~schema,
  //   ~op=#Convert,
  //   "i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}return v0}",
  // )

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"\"123\""`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return "\\""+i+"\\""}`)
})

test("Parses JSON string to bigint literal", t => {
  let schema = S.jsonString->S.to(S.literal(123n))

  t->Assert.deepEqual(`"123"`->S.parseOrThrow(schema), 123n)
  t->U.assertThrowsMessage(
    () => `123`->S.parseOrThrow(schema),
    `Failed parsing: Expected 123n, received "123"`,
  )

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="\\"123\\""||e[0](i);return 123n}`)

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`'"123"'`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==123n){e[0](i)}return "\\"123\\""}`,
  )
})

test("Parses JSON string to symbol literal", t => {
  let symbol = %raw(`Symbol("foo")`)

  let schema = S.jsonString->S.to(S.literal(symbol))

  t->U.assertThrowsMessage(
    () => `true`->S.parseOrThrow(schema),
    `Failed parsing: Unsupported transformation from Symbol(foo) to JSON string`,
  )

  t->U.assertThrowsMessage(
    () => symbol->S.reverseConvertOrThrow(schema),
    `Failed converting: Unsupported transformation from Symbol(foo) to JSON string`,
  )
})

test("Parses JSON string to null literal", t => {
  let nullVal = %raw(`null`)
  let schema = S.jsonString->S.to(S.literal(nullVal))

  t->Assert.deepEqual("null"->S.parseOrThrow(schema), nullVal)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="null"||e[0](i);return null}`)

  t->Assert.deepEqual(nullVal->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return "null"}`)
})

test("Parses JSON string to nullAsUnit", t => {
  let schema = S.jsonString->S.to(S.nullAsUnit)

  t->Assert.deepEqual(`null`->S.parseOrThrow(schema), ())

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="null"||e[0](i);return void 0}`)

  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==void 0){e[0](i)}return "null"}`)
})

test("Parses JSON string to unit", t => {
  let schema = S.jsonString->S.to(S.unit)

  t->Assert.deepEqual(`null`->S.parseOrThrow(schema), ())

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i==="null"||e[0](i);return void 0}`)

  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==void 0){e[0](i)}return "null"}`)
})

test("Parses JSON string to dict", t => {
  let value = Dict.fromArray([("foo", true)])
  let schema = S.jsonString->S.to(S.dict(S.bool))

  t->Assert.deepEqual(`{"foo": true}`->S.parseOrThrow(schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="object"||!v0||Array.isArray(v0)){e[2](v0)}for(let v1 in v0){try{let v3=v0[v1];if(typeof v3!=="boolean"){e[3](v3)}}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v1+\'"]\'+v2.path}throw v2}}return v0}`,
  )

  t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), `{"foo":true}`->Obj.magic)

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return JSON.stringify(i)}`)
})

test("Parses JSON string to array", t => {
  let value = [true, false]
  let schema = S.jsonString->S.to(S.array(S.bool))

  t->Assert.deepEqual(`[true, false]`->S.parseOrThrow(schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(!Array.isArray(v0)){e[2](v0)}for(let v1=0;v1<v0.length;++v1){try{let v3=v0[v1];if(typeof v3!=="boolean"){e[3](v3)}}catch(v2){if(v2&&v2.s===s){v2.path=""+\'["\'+v1+\'"]\'+v2.path}throw v2}}return v0}`,
  )

  t->Assert.deepEqual(value->S.reverseConvertOrThrow(schema), `[true,false]`->Obj.magic)

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return JSON.stringify(i)}`)
})

test("A chain of JSON string schemas should do nothing", t => {
  let schema = S.jsonString->S.to(S.jsonString)->S.to(S.jsonString)->S.to(S.bool)

  t->Assert.deepEqual(`true`->S.parseOrThrow(schema), true)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="boolean"){e[2](v0)}return v0}`,
  )

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

Failing.test("Nested JSON string", t => {
  let schema = S.jsonString->S.to(S.unknown)->S.to(S.jsonString)->S.to(S.bool)

  t->Assert.deepEqual(`"true"`->S.parseOrThrow(schema), true)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="boolean"){e[2](v0)}return v0}`,
  )

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`'"true"'`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Parses JSON string to object", t => {
  let value = {
    "foo": "bar",
    "bar": [1, 3],
  }

  let schema = S.jsonString->S.to(
    S.schema(_ =>
      {
        "foo": "bar",
        "bar": [1, 3],
      }
    ),
  )

  t->Assert.deepEqual(`{"foo":"bar","bar":[1,3]}`->S.parseOrThrow(schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="object"||!v0||v0["foo"]!=="bar"||!Array.isArray(v0["bar"])||v0["bar"].length!==2||v0["bar"]["0"]!==1||v0["bar"]["1"]!==3){e[2](v0)}return {"foo":"bar","bar":v0["bar"],}}`,
  )

  t->Assert.deepEqual(
    value->S.reverseConvertOrThrow(schema),
    `{"foo":"bar","bar":[1,3]}`->Obj.magic,
  )

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return JSON.stringify(i)}`)
})

test("Parses JSON string to option", t => {
  let schema = S.jsonString->S.to(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => `"foo"`->S.parseOrThrow(schema),
    `Failed parsing: boolean | undefined is not valid JSON`,
  )

  // t->Assert.deepEqual(`null`->S.parseOrThrow(schema), None)
  // t->Assert.deepEqual(`true`->S.parseOrThrow(schema), Some(true))

  // t->U.assertCompiledCode(
  //   ~schema,
  //   ~op=#Parse,
  //   `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="object"||!v0||v0["foo"]!=="bar"||!Array.isArray(v0["bar"])||v0["bar"].length!==2||v0["bar"]["0"]!==1||v0["bar"]["1"]!==3){e[2](v0)}return {"foo":"bar","bar":v0["bar"],}}`,
  // )

  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), `null`->Obj.magic)
  t->Assert.deepEqual(Some(true)->S.reverseConvertOrThrow(schema), `true`->Obj.magic)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(i===void 0){i="null"}return i}`,
  )
})

test("Successfully serializes JSON object with space", t => {
  let schema = S.schema(_ =>
    {
      "foo": "bar",
      "baz": [1, 3],
    }
  )

  t->Assert.deepEqual(
    {
      "foo": "bar",
      "baz": [1, 3],
    }->S.reverseConvertOrThrow(S.jsonStringWithSpace(2)->S.to(schema)),
    %raw(`'{\n  "foo": "bar",\n  "baz": [\n    1,\n    3\n  ]\n}'`),
  )
})

test(
  "Create schema when passing non-jsonable schema to S.jsonString, but fails to serialize",
  t => {
    let schema = S.jsonString->S.to(S.object(s => s.field("foo", S.unknown)))

    t->U.assertThrowsMessage(
      () => %raw(`"foo"`)->S.reverseConvertOrThrow(S.jsonString->S.to(schema)),
      `Failed converting: { foo: unknown; } is not valid JSON`,
    )
  },
)

test("Compiled async parse code snapshot", t => {
  let schema = S.jsonString->S.to(S.bool->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{if(typeof i!=="string"){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="boolean"){e[2](v0)}return e[3](v0)}`,
  )
})
