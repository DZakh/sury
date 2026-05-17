open Ava

test("Parses JSON string without transformation", t => {
  let schema = S.jsonString

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(~to=schema), `"Foo"`)
  t->U.assertThrowsMessage(
    () => `Foo`->S.parseOrThrow(~to=schema),
    `Expected JSON string, received "Foo"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);try{JSON.parse(i)}catch(t){e[0](i)}return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
})

test("Parses JSON string to string", t => {
  let schema = S.jsonString->S.to(S.string)

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(~to=schema), "Foo")
  t->U.assertThrowsMessage(
    () => `Foo`->S.parseOrThrow(~to=schema),
    `Expected JSON string, received "Foo"`,
  )
  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(~to=schema), `Expected string, received 123`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="string"||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="string"||e[1](v0);return v0}`,
  )

  t->Assert.deepEqual(`"Foo`->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`'"\\"Foo"'`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return JSON.stringify(i)}`)
})

test("Parses JSON string to string literal", t => {
  let schema = S.jsonString->S.to(S.literal("Foo"))

  t->Assert.deepEqual(`"Foo"`->S.parseOrThrow(~to=schema), "Foo")
  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(~to=schema), `Expected ""Foo"", received "123"`)
  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `Expected JSON string, received 123`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="\\"Foo\\""||e[0](i);return "Foo"}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{i==="\\"Foo\\""||e[0](i);return "Foo"}`)

  t->Assert.deepEqual(`Foo`->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`'"Foo"'`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{i==="Foo"||e[0](i);return "\\"Foo\\""}`,
  )

  let schema = S.jsonString->S.to(S.literal("\"Foo"))
  t->Assert.deepEqual(`"Foo`->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`'"\\"Foo"'`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{i==="\\"Foo"||e[0](i);return "\\"\\\\\\"Foo\\""}`,
  )
})

test("Parses JSON string to float", t => {
  let schema = S.jsonString->S.to(S.float)

  t->Assert.deepEqual(`1.23`->S.parseOrThrow(~to=schema), 1.23)
  t->U.assertThrowsMessage(() => `null`->S.parseOrThrow(~to=schema), `Expected number, received null`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="number"&&!Number.isNaN(v0)||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="number"&&!Number.isNaN(v0)||e[1](v0);return v0}`,
  )

  t->Assert.deepEqual(1.23->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1.23"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("Parses JSON string to float literal", t => {
  let schema = S.jsonString->S.to(S.literal(1.23))

  t->Assert.deepEqual(`1.23`->S.parseOrThrow(~to=schema), 1.23)
  t->U.assertThrowsMessage(() => `null`->S.parseOrThrow(~to=schema), `Expected "1.23", received "null"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="1.23"||e[0](i);return 1.23}`,
  )

  t->Assert.deepEqual(1.23->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1.23"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===1.23||e[0](i);return "1.23"}`)
})

test("Parses JSON string to bool", t => {
  let schema = S.jsonString->S.to(S.bool)

  t->Assert.deepEqual(`true`->S.parseOrThrow(~to=schema), true)
  t->U.assertThrowsMessage(() => `"t"`->S.parseOrThrow(~to=schema), `Expected boolean, received "t"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="boolean"||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="boolean"||e[1](v0);return v0}`,
  )

  t->Assert.deepEqual(true->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("Parses JSON string to bool literal", t => {
  let schema = S.jsonString->S.to(S.literal(true))

  t->Assert.deepEqual(`true`->S.parseOrThrow(~to=schema), true)
  t->U.assertThrowsMessage(() => `null`->S.parseOrThrow(~to=schema), `Expected "true", received "null"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="true"||e[0](i);return true}`,
  )

  t->Assert.deepEqual(true->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===true||e[0](i);return "true"}`)
})

test("Parses JSON string to bigint", t => {
  let schema = S.jsonString->S.to(S.bigint)

  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(~to=schema), `Expected string, received 123`)

  t->Assert.deepEqual(`"123"`->S.parseOrThrow(~to=schema), 123n)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[3](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="string"||e[2](v0);let v1;try{v1=BigInt(v0)}catch(_){e[1](v0)}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="string"||e[2](v0);let v1;try{v1=BigInt(v0)}catch(_){e[1](v0)}return v1}`,
  )

  t->Assert.deepEqual(123n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"\"123\""`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return "\\""+i+"\\""}`)
})

test("Parses JSON string to bigint literal", t => {
  let schema = S.jsonString->S.to(S.literal(123n))

  t->Assert.deepEqual(`"123"`->S.parseOrThrow(~to=schema), 123n)
  t->U.assertThrowsMessage(() => `123`->S.parseOrThrow(~to=schema), `Expected ""123"", received "123"`)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="\\"123\\""||e[0](i);return 123n}`,
  )

  t->Assert.deepEqual(123n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`'"123"'`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{i===123n||e[0](i);return "\\"123\\""}`,
  )
})

test("Parses JSON string to symbol literal", t => {
  let symbol = %raw(`Symbol("foo")`)

  // TODO: Test that it works with literal having noValidation
  let schema = S.jsonString->S.to(S.literal(symbol))

  t->U.assertThrowsMessage(
    () => `true`->S.parseOrThrow(~to=schema),
    `Can't decode JSON string to Symbol(foo). Use S.to to define a custom decoder`,
  )

  t->U.assertThrowsMessage(
    () => symbol->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Can't decode Symbol(foo) to JSON string. Use S.to to define a custom decoder`,
  )
})

test("Parses JSON string to null literal", t => {
  let nullVal = %raw(`null`)
  let schema = S.jsonString->S.to(S.literal(nullVal))

  t->Assert.deepEqual("null"->S.parseOrThrow(~to=schema), nullVal)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="null"||e[0](i);return null}`,
  )

  t->Assert.deepEqual(nullVal->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===null||e[0](i);return "null"}`)
})

test("Parses JSON string to nullAsUnit", t => {
  let schema = S.jsonString->S.to(S.nullAsUnit)

  t->Assert.deepEqual(`null`->S.parseOrThrow(~to=schema), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="null"||e[0](i);return void 0}`,
  )

  t->Assert.deepEqual(()->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===void 0||e[0](i);return "null"}`)
})

test("Parses JSON string to unit", t => {
  let schema = S.jsonString->S.to(S.unit)

  t->Assert.deepEqual(`null`->S.parseOrThrow(~to=schema), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="null"||e[0](i);return void 0}`,
  )

  t->Assert.deepEqual(()->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"null"`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===void 0||e[0](i);return "null"}`)
})

test("Parses JSON string to dict", t => {
  let value = Dict.fromArray([("foo", true)])
  let schema = S.jsonString->S.to(S.dict(S.bool))

  t->Assert.deepEqual(`{"foo": true}`->S.parseOrThrow(~to=schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[3](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="object"&&v0&&!Array.isArray(v0)||e[2](v0);for(let v1 in v0){try{let v2=v0[v1];typeof v2==="boolean"||e[1](v2);}catch(v3){v3.path=\'["\'+v1+\'"]\'+v3.path;throw v3}}return v0}`,
  )

  t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), `{"foo":true}`->Obj.magic)

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return JSON.stringify(i)}`)
})

test("Parses JSON string to array", t => {
  let value = [true, false]
  let schema = S.jsonString->S.to(S.array(S.bool))

  t->Assert.deepEqual(`[true, false]`->S.parseOrThrow(~to=schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[3](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}Array.isArray(v0)||e[2](v0);for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];typeof v2==="boolean"||e[1](v2);}catch(v3){v3.path=\'["\'+v1+\'"]\'+v3.path;throw v3}}return v0}`,
  )

  t->Assert.deepEqual(value->S.decodeOrThrow(~from=schema, ~to=S.unknown), `[true,false]`->Obj.magic)

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return JSON.stringify(i)}`)
})

test("A chain of JSON string schemas should do nothing", t => {
  let schema = S.jsonString->S.to(S.jsonString)->S.to(S.jsonString)->S.to(S.bool)

  t->Assert.deepEqual(`true`->S.parseOrThrow(~to=schema), true)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="boolean"||e[1](v0);return v0}`,
  )

  t->Assert.deepEqual(true->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("A S.unknown in the S.jsonString chain should do nothing", t => {
  let schema = S.jsonString->S.to(S.unknown)->S.to(S.jsonString)->S.to(S.bool)

  t->Assert.deepEqual("true"->S.parseOrThrow(~to=schema), true)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[3](i);try{JSON.parse(i)}catch(t){e[0](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[1](i)}typeof v0==="boolean"||e[2](v0);return v0}`,
  )

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
  t->Assert.deepEqual(true->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))
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

  t->Assert.deepEqual(`{"foo":"bar","bar":["1",true]}`->S.parseOrThrow(~to=schema), value)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[8](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="object"&&v0&&!Array.isArray(v0)||e[7](v0);let v1=v0["foo"],v2=v0["bar"];v1==="bar"||e[1](v1);Array.isArray(v2)||e[6](v2);v2.length===2||e[5](v2);let v4=v2["0"],v5=v2["1"];typeof v4==="string"||e[3](v4);let v3;try{v3=BigInt(v4)}catch(_){e[2](v4)}typeof v5==="boolean"||e[4](v5);return {"foo":v1,"bar":[v3,v5,],}}`,
  )

  t->Assert.deepEqual(
    value->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `{"foo":"bar","bar":["1",true]}`->Obj.magic,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0=i["bar"];return JSON.stringify({"foo":"bar","bar":[""+v0["0"],v0["1"],],})}`,
  )
})

test("Parses JSON string to option", t => {
  let schema = S.jsonString->S.to(S.option(S.bool))

  t->U.assertThrowsMessage(
    () => `"foo"`->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined, received "foo"`,
  )

  t->Assert.deepEqual(`null`->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(`true`->S.parseOrThrow(~to=schema), Some(true))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(v0===null){v0=void 0}else if(!(typeof v0==="boolean")){e[1](v0)}return v0}`,
  )

  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), `null`->Obj.magic)
  t->Assert.deepEqual(Some(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), `true`->Obj.magic)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
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
    }->S.decodeOrThrow(~from=S.jsonStringWithSpace(2)->S.to(schema), ~to=S.unknown),
    %raw(`'{\n  "foo": "bar",\n  "baz": [\n    1,\n    3\n  ]\n}'`),
  )
})

test("Converts JSON string to object with unknown field", t => {
  let schema = S.jsonString->S.to(S.object(s => s.field("foo", S.unknown)))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="object"&&v0&&!Array.isArray(v0)||e[1](v0);return v0["foo"]}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{e[0](i);return JSON.stringify({"foo":i,})}`,
  )

  t->Assert.deepEqual(%raw(`"foo"`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`'{"foo":"foo"}'`))
  t->U.assertThrowsMessage(() => {
    %raw(`123n`)->S.decodeOrThrow(~from=schema, ~to=S.unknown)
  }, `Expected JSON, received 123n`)
})

test("Compiled async parse code snapshot", t => {
  let schema = S.jsonString->S.to(S.bool->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{typeof i==="string"||e[4](i);let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}typeof v0==="boolean"||e[3](v0);let v1;try{v1=e[1](v0).catch(x=>e[2](x))}catch(x){e[2](x)}return v1}`,
  )
})

test("Can apply refinement to JSON string", t => {
  let schema = S.jsonString->S.refine(v => v === "123", ~error="Expected 123")

  t->U.assertThrowsMessage(() => `124`->S.parseOrThrow(~to=schema), `Expected 123`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[3](i);try{JSON.parse(i)}catch(t){e[0](i)}e[1](i)||e[2](i);return i}`,
  )
})

test("Can apply refinement to JSON string with S.to after", t => {
  let schema =
    S.jsonString
    ->S.refine(v => v === "123", ~error="Expected 123")
    ->S.to(S.int)

  t->U.assertThrowsMessage(() => `124`->S.parseOrThrow(~to=schema), `Expected 123`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // TODO: Can be improved to perform JSON.parse only once
    `i=>{typeof i==="string"||e[5](i);try{JSON.parse(i)}catch(t){e[0](i)}e[1](i)||e[4](i);let v0;try{v0=JSON.parse(i)}catch(t){e[2](i)}typeof v0==="number"&&v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[3](v0);return v0}`,
  )
})

test("Can apply refinement to JSON string with S.to before", t => {
  let schema = S.int->S.to(S.jsonString->S.refine(v => v === "123", ~error="Expected 123"))

  t->U.assertThrowsMessage(() => 124->S.parseOrThrow(~to=schema), `Expected 123`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="number"&&i<=2147483647&&i>=-2147483648&&i%1===0||e[2](i);let v0=""+i;e[0](v0)||e[1](v0);return v0}`,
  )
})
