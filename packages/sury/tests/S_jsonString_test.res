open Ava

S.enableJsonString()

test("Parses JSON string without transformation", t => {
  let schema = S.jsonString

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(schema), `"Foo"`)
  t->U.assertThrowsMessage(
    () => `Foo`->S.parseOrThrow(schema),
    `Expected JSON string, received "Foo"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}try{JSON.parse(i)}catch(t){e[0](i)}return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Parses JSON string to string", t => {
  let schema = S.jsonString->S.to(S.string)

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(schema), "Foo")
  t->U.assertThrowsMessage(
    () => `Foo`->S.parseOrThrow(schema),
    `Expected JSON string, received "Foo"`,
  )
  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(schema), `Expected string, received 123`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="string"){e[1](v0)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="string"){e[1](v0)}return v0}`,
  )

  t->Assert.deepEqual(`"Foo`->S.reverseConvertOrThrow(schema), %raw(`'"\\"Foo"'`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return JSON.stringify(i)}`)
})

test("Parses JSON string to string literal", t => {
  let schema = S.jsonString->S.to(S.literal("Foo"))

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(schema), "Foo")
  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(schema), `Expected ""Foo"", received "123"`)
  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(schema), `Expected JSON string, received 123`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="\\"Foo\\""){e[0](i)}return "Foo"}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{if(i!=="\\"Foo\\""){e[0](i)}return "Foo"}`)

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
  t->U.assertThrowsMessage(() => `null`->S.parseOrThrow(schema), `Expected number, received null`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="number"||Number.isNaN(v0)){e[1](v0)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="number"||Number.isNaN(v0)){e[1](v0)}return v0}`,
  )

  t->Assert.deepEqual(1.23->S.reverseConvertOrThrow(schema), %raw(`"1.23"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Parses JSON string to float literal", t => {
  let schema = S.jsonString->S.to(S.literal(1.23))

  t->Assert.deepEqual(`1.23`->S.parseOrThrow(schema), 1.23)
  t->U.assertThrowsMessage(() => `null`->S.parseOrThrow(schema), `Expected "1.23", received "null"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="1.23"){e[0](i)}return 1.23}`,
  )

  t->Assert.deepEqual(1.23->S.reverseConvertOrThrow(schema), %raw(`"1.23"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==1.23){e[0](i)}return "1.23"}`)
})

test("Parses JSON string to bool", t => {
  let schema = S.jsonString->S.to(S.bool)

  t->Assert.deepEqual(`true`->S.parseOrThrow(schema), true)
  t->U.assertThrowsMessage(() => `"t"`->S.parseOrThrow(schema), `Expected boolean, received "t"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="boolean"){e[1](v0)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="boolean"){e[1](v0)}return v0}`,
  )

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Parses JSON string to bool literal", t => {
  let schema = S.jsonString->S.to(S.literal(true))

  t->Assert.deepEqual(`true`->S.parseOrThrow(schema), true)
  t->U.assertThrowsMessage(() => `null`->S.parseOrThrow(schema), `Expected "true", received "null"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="true"){e[0](i)}return true}`,
  )

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==true){e[0](i)}return "true"}`)
})

test("Parses JSON string to bigint", t => {
  let schema = S.jsonString->S.to(S.bigint)

  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(schema), `Expected string, received 123`)

  t->Assert.deepEqual(`"123"`->S.parseOrThrow(schema), 123n)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="string"){e[2](v0)}let v1;try{v1=BigInt(v0)}catch(_){e[1](v0)}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="string"){e[2](v0)}let v1;try{v1=BigInt(v0)}catch(_){e[1](v0)}return v1}`,
  )

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"\"123\""`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return "\\""+i+"\\""}`)
})

test("Parses JSON string to bigint literal", t => {
  let schema = S.jsonString->S.to(S.literal(123n))

  t->Assert.deepEqual(`"123"`->S.parseOrThrow(schema), 123n)
  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(schema), `Expected ""123"", received "123"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="\\"123\\""){e[0](i)}return 123n}`,
  )

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
    `Unsupported conversion from Symbol(foo) to JSON string`,
  )

  t->U.assertThrowsMessage(
    () => symbol->S.reverseConvertOrThrow(schema),
    `Unsupported conversion from Symbol(foo) to JSON string`,
  )
})

test("Parses JSON string to null literal", t => {
  let nullVal = %raw(`null`)
  let schema = S.jsonString->S.to(S.literal(nullVal))

  t->Assert.deepEqual("null"->S.parseOrThrow(schema), nullVal)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="null"){e[0](i)}return null}`,
  )

  t->Assert.deepEqual(nullVal->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return "null"}`)
})

test("Parses JSON string to nullAsUnit", t => {
  let schema = S.jsonString->S.to(S.nullAsUnit)

  t->Assert.deepEqual(`null`->S.parseOrThrow(schema), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="null"){e[0](i)}return void 0}`,
  )

  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==void 0){e[0](i)}return "null"}`)
})

test("Parses JSON string to unit", t => {
  let schema = S.jsonString->S.to(S.unit)

  t->Assert.deepEqual(`null`->S.parseOrThrow(schema), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="null"){e[0](i)}return void 0}`,
  )

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
    `i=>{if(typeof i!=="string"){e[3](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="object"||!v0||Array.isArray(v0)){e[2](v0)}for(let v1 in v0){try{let v2=v0[v1];if(typeof v2!=="boolean"){e[1](v2)}}catch(v3){v3.path=\'["\'+v1+\'"]\'+v3.path;throw v3}}return v0}`,
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
    `i=>{if(typeof i!=="string"){e[3](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(!Array.isArray(v0)){e[2](v0)}for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];if(typeof v2!=="boolean"){e[1](v2)}}catch(v3){v3.path=\'["\'+v1+\'"]\'+v3.path;throw v3}}return v0}`,
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
    `i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="boolean"){e[1](v0)}return v0}`,
  )

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("A S.unknown in the S.jsonString chain should do nothing", t => {
  let schema = S.jsonString->S.to(S.unknown)->S.to(S.jsonString)->S.to(S.bool)

  t->Assert.deepEqual("true"->S.parseOrThrow(schema), true)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}try{JSON.parse(i)}catch(t){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}if(typeof v0!=="boolean"){e[2](v0)}return v0}`,
  )

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`"true"`))
})

test("Parses JSON string to object with bigint", t => {
  let value = {
    "foo": "bar",
    "bar": (1n, true),
  }

  let schema = S.jsonString->S.to(
    S.schema(s =>
      {
        "foo": "bar",
        "bar": (s.matches(S.bigint), s.matches(S.bool)),
      }
    ),
  )

  t->Assert.deepEqual(`{"foo":"bar","bar":["1",true]}`->S.parseOrThrow(schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[8](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="object"||!v0||Array.isArray(v0)){e[7](v0)}let v1=v0["foo"],v2=v0["bar"];if(v1!=="bar"){e[1](v1)}if(!Array.isArray(v2)){e[6](v2)}if(v2.length!==2){e[5](v2)}let v4=v2["0"],v5=v2["1"];if(typeof v4!=="string"){e[3](v4)}let v3;try{v3=BigInt(v4)}catch(_){e[2](v4)}if(typeof v5!=="boolean"){e[4](v5)}return {"foo":v1,"bar":[v3,v5,],}}`,
  )

  t->Assert.deepEqual(
    value->S.reverseConvertOrThrow(schema),
    `{"foo":"bar","bar":["1",true]}`->Obj.magic,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["bar"];return JSON.stringify({"foo":"bar","bar":[""+v0["0"],v0["1"],],})}`,
  )
})

test("Parses JSON string to option", t => {
  let schema = S.jsonString->S.to(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => `"foo"`->S.parseOrThrow(schema),
    `Expected boolean | undefined, received "foo"`,
  )

  t->Assert.deepEqual(`null`->S.parseOrThrow(schema), None)
  t->Assert.deepEqual(`true`->S.parseOrThrow(schema), Some(true))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(v0===null){v0=void 0}else if(!(typeof v0==="boolean")){e[1](v0)}return v0}`,
  )

  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), `null`->Obj.magic)
  t->Assert.deepEqual(Some(true)->S.reverseConvertOrThrow(schema), `true`->Obj.magic)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(i===void 0){i="null"}else{e[0](i)}return i}`,
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

test("Converts JSON string to object with unknown field", t => {
  let schema = S.jsonString->S.to(S.object(s => s.field("foo", S.unknown)))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="object"||!v0||Array.isArray(v0)){e[1](v0)}return v0["foo"]}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{e[0](i);return JSON.stringify({"foo":i,})}`,
  )

  t->Assert.deepEqual(%raw(`"foo"`)->S.reverseConvertOrThrow(schema), %raw(`'{"foo":"foo"}'`))
  t->U.assertThrowsMessage(() => {
    %raw(`123n`)->S.reverseConvertOrThrow(schema)
  }, `Expected JSON, received 123n`)
})

test("Compiled async parse code snapshot", t => {
  let schema = S.jsonString->S.to(S.bool->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{if(typeof i!=="string"){e[4](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="boolean"){e[3](v0)}let v1;try{v1=e[1](v0).catch(x=>e[2](x))}catch(x){e[2](x)}return v1}`,
  )
})

test("Can apply refinement to JSON string", t => {
  let schema = S.jsonString->S.refine(v => v === "123", ~error="Expected 123")

  t->U.assertThrowsMessage(() => `124`->S.parseOrThrow(schema), `Expected 123`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}try{JSON.parse(i)}catch(t){e[0](i)}if(!e[2](i)){e[1]()}return i}`,
  )
})

test("Can apply refinement to JSON string with S.to after", t => {
  let schema =
    S.jsonString
    ->S.refine(v => v === "123", ~error="Expected 123")
    ->S.to(S.int)

  t->U.assertThrowsMessage(() => `124`->S.parseOrThrow(schema), `Expected 123`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // TODO: Can be improved
    `i=>{if(typeof i!=="string"){e[4](i)}let v0;try{JSON.parse(i)}catch(t){e[0](i)}if(!e[1](i)){e[5]()}try{v0=JSON.parse(i)}catch(t){e[2](i)}if(typeof v0!=="number"||v0>2147483647||v0<-2147483648||v0%1!==0){e[3](v0)}return v0}`,
  )
})

test("Can apply refinement to JSON string with S.to before", t => {
  let schema = S.int->S.to(
    S.jsonString->S.refine(v => v === "123", ~error="Expected 123"),
  )

  t->U.assertThrowsMessage(() => 124->S.parseOrThrow(schema), `Expected 123`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="number"||i>2147483647||i<-2147483648||i%1!==0){e[2](i)}let v0=""+i;if(!e[1](v0)){e[0]()}return v0}`,
  )
})
