open Ava

S.enableJson()

test("Coerce from string to string", t => {
  let schema = S.string->S.to(S.string)
  t->Assert.is(schema, S.string)
})

test("Coerce from string to bool", t => {
  let schema = S.string->S.to(S.bool)

  t->Assert.deepEqual("false"->S.parseOrThrow(schema), false)
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), true)
  t->U.assertThrowsMessage(() => "tru"->S.parseOrThrow(schema), `Expected boolean, received "tru"`)
  t->Assert.deepEqual(false->S.reverseConvertOrThrow(schema), %raw(`"false"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;(v0=i==="true")||i==="false"||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;(v0=i==="true")||i==="false"||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce from bool to string", t => {
  let schema = S.bool->S.to(S.string)

  t->Assert.deepEqual(false->S.parseOrThrow(schema), "false")
  t->Assert.deepEqual(true->S.parseOrThrow(schema), "true")
  t->U.assertThrowsMessage(
    () => "tru"->S.reverseConvertOrThrow(schema),
    `Expected boolean, received "tru"`,
  )
  t->Assert.deepEqual("false"->S.reverseConvertOrThrow(schema), %raw(`false`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(typeof i!=="boolean"){e[0](i)}return ""+i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{return \"\"+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;(v0=i===\"true\")||i===\"false\"||e[0](i);return v0}`,
  )
})

test("Coerce from string to bool literal", t => {
  let schema = S.string->S.to(S.literal(false))

  t->Assert.deepEqual("false"->S.parseOrThrow(schema), false)
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(schema),
    `Expected "false", received "true"`,
  )
  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(schema), `Expected string, received 123`)
  t->Assert.deepEqual(false->S.reverseConvertOrThrow(schema), %raw(`"false"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="false"){e[0](i)}return false}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==false){e[0](i)}return "false"}`)
})

test("Coerce from string to null literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual("null"->S.parseOrThrow(schema), %raw(`null`))
  t->U.assertThrowsMessage(() => "true"->S.parseOrThrow(schema), `Expected "null", received "true"`)
  t->Assert.deepEqual(%raw(`null`)->S.reverseConvertOrThrow(schema), %raw(`"null"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="null"){e[0](i)}return null}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return "null"}`)
})

test("Coerce from string to undefined literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`undefined`)))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(schema), %raw(`undefined`))
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(schema),
    `Expected "undefined", received "true"`,
  )
  t->Assert.deepEqual(%raw(`undefined`)->S.reverseConvertOrThrow(schema), %raw(`"undefined"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="undefined"){e[0](i)}return void 0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==void 0){e[0](i)}return "undefined"}`,
  )
})

test("Coerce from string to NaN literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual("NaN"->S.parseOrThrow(schema), %raw(`NaN`))
  t->U.assertThrowsMessage(() => "true"->S.parseOrThrow(schema), `Expected "NaN", received "true"`)
  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertOrThrow(schema), %raw(`"NaN"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="NaN"){e[0](i)}return NaN}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(!Number.isNaN(i)){e[0](i)}return "NaN"}`,
  )
})

test("Coerce from string to string literal", t => {
  let quotedString = `"'\``
  let schema = S.string->S.to(S.literal(quotedString))

  t->Assert.deepEqual(quotedString->S.parseOrThrow(schema), quotedString)
  t->U.assertThrowsMessage(
    () => "bar"->S.parseOrThrow(schema),
    `Expected "${quotedString}", received "bar"`,
  )
  t->Assert.deepEqual(quotedString->S.reverseConvertOrThrow(schema), %raw(`quotedString`))
  t->U.assertThrowsMessage(
    () => "bar"->S.reverseConvertOrThrow(schema),
    `Expected "${quotedString}", received "bar"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="\\"\'\`"){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!=="\\"\'\`"){e[0](i)}return i}`)
})

test("Coerce from object shaped as string to float", t => {
  let schema = S.object(s => s.field("foo", S.string))->S.to(S.float)

  t->Assert.deepEqual({"foo": "123"}->S.parseOrThrow(schema), 123.)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foo"];if(typeof v0!=="string"){e[0](v0)}let v1=+v0;if(Number.isNaN(v1)){e[1](v0)}return v1}`,
  )

  t->Assert.deepEqual(123.->S.reverseConvertOrThrow(schema), %raw(`{"foo": "123"}`))
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":""+i,}}`)
})

test("Coerce to literal can be used as tag and automatically embeded on reverse operation", t => {
  let schema = S.object(s => {
    let _ = s.field("tag", S.string->S.to(S.literal(true)))
  })

  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`{"tag": "true"}`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==void 0){e[0](i)}return {"tag":"true",}}`,
  )

  t->Assert.deepEqual({"tag": "true"}->S.parseOrThrow(schema), ())
  t->U.assertThrowsMessage(
    () => {"tag": "false"}->S.parseOrThrow(schema),
    `Failed at ["tag"]: Expected "true", received "false"`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // TODO: Test that it'll work with S.refine on S.string
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["tag"];if(typeof v0!=="string"){e[1](v0)}if(v0!=="true"){e[0](v0)}return void 0}`,
  )
})

test("Coerce from string to float", t => {
  let schema = S.string->S.to(S.float)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10.)
  t->Assert.deepEqual("10.2"->S.parseOrThrow(schema), 10.2)
  t->U.assertThrowsMessage(() => "tru"->S.parseOrThrow(schema), `Expected number, received "tru"`)
  t->Assert.deepEqual(10.->S.reverseConvertOrThrow(schema), %raw(`"10"`))
  t->Assert.deepEqual(10.2->S.reverseConvertOrThrow(schema), %raw(`"10.2"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0=+i;if(Number.isNaN(v0)){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;if(Number.isNaN(v0)){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce from string to int32", t => {
  let schema = S.string->S.to(S.int)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10)
  t->U.assertThrowsMessage(
    () => "2147483648"->S.parseOrThrow(schema),
    `Expected int32, received "2147483648"`,
  )
  t->U.assertThrowsMessage(() => "10.2"->S.parseOrThrow(schema), `Expected int32, received "10.2"`)
  t->Assert.deepEqual(10->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0=+i;if(v0>2147483647||v0<-2147483648||v0%1!==0){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;if(v0>2147483647||v0<-2147483648||v0%1!==0){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce from string to port", t => {
  let schema = S.string->S.to(S.int->S.port)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10)
  t->U.assertThrowsMessage(
    () => "2147483648"->S.parseOrThrow(schema),
    `Expected port, received 2147483648`,
  )
  t->U.assertThrowsMessage(() => "10.2"->S.parseOrThrow(schema), `Expected port, received 10.2`)
  t->Assert.deepEqual(10->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0=+i;if(Number.isNaN(v0)){e[1](i)}v0>0&&v0<65536&&v0%1===0||e[0](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;if(Number.isNaN(v0)){e[1](i)}v0>0&&v0<65536&&v0%1===0||e[0](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{i>0&&i<65536&&i%1===0||e[0](i);return ""+i}`,
  )
})

test("Coerce from true to bool", t => {
  let schema = S.literal(true)->S.to(S.bool)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==true){e[0](i)}return i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{if(i!==true){e[0](i)}return i}`)
})

test("Coerce from string to bigint literal", t => {
  let schema = S.string->S.to(S.literal(10n))

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10n)
  t->U.assertThrowsMessage(() => "11"->S.parseOrThrow(schema), `Expected "10", received "11"`)
  t->Assert.deepEqual(10n->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="10"){e[0](i)}return 10n}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{if(i!=="10"){e[0](i)}return 10n}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==10n){e[0](i)}return "10"}`)
})

test("Coerce from string to bigint", t => {
  let schema = S.string->S.to(S.bigint)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10n)
  t->U.assertThrowsMessage(() => "10.2"->S.parseOrThrow(schema), `Expected bigint, received "10.2"`)
  t->Assert.deepEqual(10n->S.reverseConvertOrThrow(schema), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

test("Coerce string after a transform", t => {
  let schema = S.string->S.transform(_ => {parser: v => v, serializer: v => v})->S.to(S.bool)

  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(schema),
    `Expected boolean, received "true"`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}let v0;try{v0=e[0](i)}catch(x){e[1](x)}if(typeof v0!=="boolean"){e[2](v0)}return v0}`,
  )

  t->U.assertThrowsMessage(
    () => true->S.parseOrThrow(S.reverse(schema)),
    `Expected string, received true`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="boolean"){e[3](i)}let v0;try{v0=e[0](i)}catch(x){e[1](x)}if(typeof v0!=="string"){e[2](v0)}return v0}`,
  )
})

@unboxed
type numberOrBoolean = Number(float) | Boolean(bool)

// FIXME: Test nested union
// FIXME: Test transformed union
test("Coerce string to unboxed union (each item separately)", t => {
  let schema =
    S.string->S.to(
      S.union([
        S.schema(s => Number(s.matches(S.float))),
        S.schema(s => Boolean(s.matches(S.bool))),
      ]),
    )

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), Number(10.))
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), Boolean(true))

  t->Assert.throws(
    () => {
      "t"->S.parseOrThrow(schema)
    },
    ~expectations={
      message: `Expected number | boolean, received "t"
- Expected number, received "t"
- Expected boolean, received "t"`,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}try{let v0=+i;if(Number.isNaN(v0)){e[1](i)}i=v0}catch(e1){try{let v1;(v1=i==="true")||i==="false"||e[0](i);i=v1}catch(e2){e[2](i,e1,e2)}}return i}`,
  )

  t->Assert.deepEqual(Number(10.)->S.reverseConvertOrThrow(schema), %raw(`"10"`))
  t->Assert.deepEqual(Boolean(true)->S.reverseConvertOrThrow(schema), %raw(`"true"`))

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="number"&&!Number.isNaN(i)){i=""+i}else if(typeof i==="boolean"){i=""+i}else{e[0](i)}return i}`,
  )
})

test("Coerce string to custom JSON schema", t => {
  let schema = S.string->S.to(
    S.recursive("CustomJSON", self => {
      S.union([
        S.schema(_ => JSON.Null),
        S.schema(s => JSON.Number(s.matches(S.float))),
        S.schema(s => JSON.Boolean(s.matches(S.bool))),
        S.schema(s => JSON.String(s.matches(S.string))),
        S.schema(s => JSON.Object(s.matches(S.dict(self)))),
        S.schema(s => JSON.Array(s.matches(S.array(self)))),
      ])
    }),
  )

  t->U.assertThrowsMessage(
    () => S.reverseConvertOrThrow(JSON.Boolean(true), schema),
    `Expected string, received true`,
    ~message="I don't know what we expect here, but currently it works this way",
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=e[0](i);if(typeof v0!=="string"){e[1](v0)}return v0}`,
  )
})

test("Keeps description of the schema we are coercing to (not working)", t => {
  // Fix it later if it's needed
  let schema = S.string->S.to(S.string->S.meta({description: "To descr"}))
  t->Assert.is((schema->S.untag).description, None)

  // let schema = S.string->S.description("From descr")->S.to(S.string->S.description("To descr"))
  // t->Assert.is((schema->S.untag).description, Some("To descr"))

  // There's no specific reason for it. Just wasn't needed for cases S.to initially designed
  let schema = S.string->S.meta({description: "From descr"})->S.to(S.string)
  t->Assert.is((schema->S.untag).description, Some("From descr"))
})

test("Coerce from unit to null literal", t => {
  let schema = S.unit->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual(()->S.parseOrThrow(schema), %raw(`null`))
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(schema),
    // FIXME: It fails because we overwrite expected name with string version
    `Expected undefined, received null`,
  )
  t->Assert.deepEqual(%raw(`null`)->S.reverseConvertOrThrow(schema), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==void 0){e[0](i)}return null}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[0](i)}return void 0}`)
})

test("Coerce from string to optional bool", t => {
  let schema = S.string->S.to(S.option(S.bool))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(schema), None)
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), Some(true))

  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(schema),
    `Expected string, received null`,
  )

  t->Assert.deepEqual(Some(true)->S.reverseConvertOrThrow(schema), %raw(`"true"`))
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`"undefined"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}try{let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}catch(e0){if(i==="undefined"){i=void 0}else{e[1](i,e0)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(i===void 0){i="undefined"}else{e[0](i)}return i}`,
  )
})

test("Coerce from object to string", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.string),
    }
  )->S.to(S.string)

  t->U.assertThrowsMessage(() => {
    %raw(`{"foo": "bar"}`)->S.parseOrThrow(schema)
  }, `Unsupported conversion from { foo: string; } to string`)
  t->U.assertThrowsMessage(() => {
    %raw(`{"foo": "bar"}`)->S.reverseConvertOrThrow(schema)
  }, `Unsupported conversion from string to { foo: string; }`)
})

test("Coerce from string to JSON and then to bigint", t => {
  let schema = S.string->S.to(S.json)->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`123n`))
  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="bigint"){e[0](i)}return ""+i}`,
  )
})

test("Coerce from JSON to bigint", t => {
  let schema = S.json->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`123n`))
  t->U.assertThrowsMessage(() => {
    123->S.parseOrThrow(schema)
  }, "Expected string, received 123")
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Expected string, received true")

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="bigint"){e[0](i)}return ""+i}`,
  )
})

test("Coerce from JSON to unit", t => {
  let schema = S.json->S.to(S.unit)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), ())
  t->U.assertThrowsMessage(() => {
    %raw(`undefined`)->S.parseOrThrow(schema)
  }, "Expected null, received undefined")
  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`null`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==null){e[0](i)}return void 0}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==void 0){e[0](i)}return null}`)
})

test("Coerce from JSON to NaN", t => {
  let schema = S.json->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), %raw(`NaN`))
  t->U.assertThrowsMessage(() => {
    %raw(`undefined`)->S.parseOrThrow(schema)
  }, "Expected null, received undefined")
  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertOrThrow(schema), %raw(`null`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==null){e[0](i)}return NaN}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(!Number.isNaN(i)){e[0](i)}return null}`,
  )
})

test("Coerce from JSON to optional bigint", t => {
  let schema = S.json->S.to(S.option(S.bigint))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), None)
  t->Assert.deepEqual(%raw(`"123"`)->S.parseOrThrow(schema), Some(123n))
  t->U.assertThrowsMessage(() => {
    %raw(`123`)->S.parseOrThrow(schema)
  }, `Expected bigint | undefined, received 123`)
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`))
  t->Assert.deepEqual(Some(123n)->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(i===null){i=void 0}else{e[1](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="bigint"){i=""+i}else if(i===void 0){i=null}else{e[0](i)}return i}`,
  )
})

test("Coerce from JSON to array of bigint", t => {
  let schema = S.json->S.to(S.array(S.bigint))

  t->Assert.deepEqual(%raw(`["123"]`)->S.parseOrThrow(schema), [123n])
  t->U.assertThrowsMessage(() => {
    %raw(`[123]`)->S.parseOrThrow(schema)
  }, `Failed at ["0"]: Expected string, received 123`)
  t->Assert.deepEqual([123n]->S.reverseConvertOrThrow(schema), %raw(`["123"]`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)){e[2](i)}let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v2=i[v0];if(typeof v2!=="string"){e[1](v2)}let v1;try{v1=BigInt(v2)}catch(_){e[0](v2)}v4[v0]=v1}catch(v3){v3.path=\'["\'+v0+\'"]\'+v3.path;throw v3}}return v4}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v2=new Array(i.length);for(let v1=0;v1<i.length;++v1){v2[v1]=""+i[v1]}return v2}`,
  )
})

test("Coerce from JSON to tuple with bigint", t => {
  let schema = S.json->S.to(S.schema(s => (s.matches(S.string), s.matches(S.bigint))))

  t->Assert.deepEqual(%raw(`["foo", "123"]`)->S.parseOrThrow(schema), ("foo", 123n))
  t->U.assertThrowsMessage(() => {
    %raw(`["foo"]`)->S.parseOrThrow(schema)
  }, `Expected [string, bigint], received ["foo"]`)
  t->Assert.deepEqual(("foo", 123n)->S.reverseConvertOrThrow(schema), %raw(`["foo", "123"]`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!Array.isArray(i)){e[4](i)}if(i.length!==2){e[3](i)}let v0=i["0"],v2=i["1"];if(typeof v0!=="string"){e[0](v0)}if(typeof v2!=="string"){e[2](v2)}let v1;try{v1=BigInt(v2)}catch(_){e[1](v2)}return [v0,v1,]}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return [i["0"],""+i["1"],]}`)
})

// test("Coerce from JSON to object with optional field", t => {
//   let schema = S.json->S.to(
//     S.schema(s =>
//       {
//         "id": s.matches(S.bigint),
//         "isDeleted": s.matches(S.option(S.string)),
//       }
//     ),
//   )

//   // t->Assert.deepEqual(
//   //   {
//   //     "id": "123",
//   //   }->S.parseOrThrow(schema),
//   //   {
//   //     "id": 123n,
//   //     "isDeleted": None,
//   //   },
//   // )
//   // t->U.assertThrowsMessage(() => {
//   //   123->S.parseOrThrow(schema)
//   // }, "Expected string, received 123")
//   // t->U.assertThrowsMessage(() => {
//   //   true->S.parseOrThrow(schema)
//   // }, "Expected string, received true")

//   // t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

//   t->U.assertCompiledCode(
//     ~schema,
//     ~op=#Parse,
//     `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
//   )
//   // t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
//   // t->U.assertCompiledCode(
//   //   ~schema,
//   //   ~op=#ReverseParse,
//   //   `i=>{if(typeof i!=="bigint"){e[0](i)}return ""+i}`,
//   // )
// })

test("Coerce from union to bigint", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])->S.to(
      S.bigint,
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`123n`))
  t->Assert.deepEqual(123->S.parseOrThrow(schema), %raw(`123n`))
  t->U.assertThrowsMessage(
    () => {
      true->S.parseOrThrow(schema)
    },
    `Expected string | number | boolean, received true
- Unsupported conversion from boolean to bigint`,
  )
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(schema)
  }, "Expected string | number | boolean, received 123n")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=BigInt(i)}else if(typeof i==="boolean"){e[2](i,e[1])}else{e[3](i)}return i}`,
  )

  t->Assert.deepEqual(123n->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{i=""+i}catch(e0){try{throw e[0]}catch(e1){try{throw e[1]}catch(e2){e[2](i,e0,e1,e2)}}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="bigint"){e[3](i)}try{i=""+i}catch(e0){try{throw e[0]}catch(e1){try{throw e[1]}catch(e2){e[2](i,e0,e1,e2)}}}return i}`,
  )
})

test("Coerce from union to bigint with refinement on union", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
    ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
    ->S.to(S.bigint)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=BigInt(i)}else if(typeof i==="boolean"){e[2](i,e[1])}else{e[3](i)}if(!e[5](i)){e[4]()}return i}`,
  )
})

test("Coerce from union to bigint with refinement on union (with an item transformed to)", t => {
  let schema =
    S.union([
      S.string->S.castToUnknown,
      S.float->S.to(S.string)->S.castToUnknown,
      S.bool->S.castToUnknown,
    ])
    ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
    ->S.to(S.bigint)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){let v2=""+i;let v1;try{v1=BigInt(v2)}catch(_){e[1](v2)}i=v1}else if(typeof i==="boolean"){e[3](i,e[2])}else{e[4](i)}if(!e[6](i)){e[5]()}return i}`,
    ~message="Should apply refinement after the item transformation",
  )
})

test("Coerce from union to bigint and then to string", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
    ->S.to(S.bigint)
    ->S.to(S.string)

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(schema), %raw(`"123"`))
  t->U.assertThrowsMessage(
    () => {
      true->S.parseOrThrow(schema)
    },
    `Expected string | number | boolean, received true
- Unsupported conversion from boolean to bigint`,
  )
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(schema)
  }, "Expected string | number | boolean, received 123n")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=""+v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=""+BigInt(i)}else if(typeof i==="boolean"){e[2](i,e[1])}else{e[3](i)}return i}`,
  )

  t->Assert.deepEqual("123"->S.reverseConvertOrThrow(schema), %raw(`"123"`))
  t->U.assertThrowsMessage(() => {
    "abc"->S.reverseConvertOrThrow(schema)
  }, `Expected bigint, received "abc"`)

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;try{v0=BigInt(i)}catch(_){e[0](i)}try{v0=""+v0}catch(e0){try{throw e[1]}catch(e1){try{throw e[2]}catch(e2){e[3](v0,e0,e1,e2)}}}return v0}`,
  )
})

test("Coerce from union to wider union should keep the original value type", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]),
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(schema), %raw(`123`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Expected string | number, received true")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="string"||typeof i==="number"&&!Number.isNaN(i))){e[0](i)}return i}`,
  )
})

test("Fails to transform union to union to string", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])
    ->S.to(S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]))
    ->S.to(S.string)

  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(schema)
  }, "Expected string | number, received true")
})

test(
  "Transform from union to wider union with different items order (applies decoder to both one at a time)",
  t => {
    let schema =
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
        S.union([S.float->S.castToUnknown, S.string->S.castToUnknown, S.bool->S.castToUnknown]),
      )

    t->U.assertThrowsMessage(() => {
      true->S.parseOrThrow(schema)
    }, "Expected string | number, received true")
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      // TODO: Can be optimized to remove the second check
      `i=>{if(!(typeof i==="string"||typeof i==="number"&&!Number.isNaN(i))){e[0](i)}if(!(typeof i==="number"&&!Number.isNaN(i)||typeof i==="string"||typeof i==="boolean")){e[1](i)}return i}`,
    )
  },
)
