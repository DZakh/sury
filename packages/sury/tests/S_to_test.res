open Vitest

test("Coerce from string to string", t => {
  let schema = S.string->S.to(S.string)
  t->Assert.is(schema, S.string)
})

test("Coerce from string to bool", t => {
  let schema = S.string->S.to(S.bool)

  t->Assert.deepEqual("false"->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual("true"->S.parseOrThrow(~to=schema), true)
  t->U.assertThrowsMessage(
    () => "tru"->S.parseOrThrow(~to=schema),
    `Expected boolean, received "tru"`,
  )
  t->Assert.deepEqual(false->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"false"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);let v0;(v0=i==="true")||i==="false"||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;(v0=i==="true")||i==="false"||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("Coerce from bool to string", t => {
  let schema = S.bool->S.to(S.string)

  t->Assert.deepEqual(false->S.parseOrThrow(~to=schema), "false")
  t->Assert.deepEqual(true->S.parseOrThrow(~to=schema), "true")
  t->U.assertThrowsMessage(
    () => "tru"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected boolean, received "tru"`,
  )
  t->Assert.deepEqual("false"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`false`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{typeof i==="boolean"||e[0](i);return ""+i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{return \"\"+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;(v0=i===\"true\")||i===\"false\"||e[0](i);return v0}`,
  )
})

test("Coerce from string to bool literal", t => {
  let schema = S.string->S.to(S.literal(false))

  t->Assert.deepEqual("false"->S.parseOrThrow(~to=schema), false)
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Expected "false", received "true"`,
  )
  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `Expected string, received 123`)
  t->Assert.deepEqual(false->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"false"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="false"||e[0](i);return false}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===false||e[0](i);return "false"}`)
})

test("S.string->S.refine->S.to(S.literal) reports type error before refinement error", t => {
  let schema =
    S.string
    ->S.refine(v => v->String.length > 0, ~error="non-empty")
    ->S.to(S.literal(false))

  t->Assert.deepEqual("false"->S.parseOrThrow(~to=schema), false)
  t->U.assertThrowsMessage(() => ""->S.parseOrThrow(~to=schema), "non-empty")
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Expected "false", received "true"`,
  )
  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `Expected string, received 123`)
})

test("Coerce from string to null literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual("null"->S.parseOrThrow(~to=schema), %raw(`null`))
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Expected "null", received "true"`,
  )
  t->Assert.deepEqual(%raw(`null`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"null"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="null"||e[0](i);return null}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===null||e[0](i);return "null"}`)
})

test("Coerce from string to undefined literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`undefined`)))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(~to=schema), %raw(`undefined`))
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Expected "undefined", received "true"`,
  )
  t->Assert.deepEqual(
    %raw(`undefined`)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"undefined"`),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="undefined"||e[0](i);return void 0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{i===void 0||e[0](i);return "undefined"}`,
  )
})

test("Coerce from string to NaN literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual("NaN"->S.parseOrThrow(~to=schema), %raw(`NaN`))
  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Expected "NaN", received "true"`,
  )
  t->Assert.deepEqual(%raw(`NaN`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"NaN"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="NaN"||e[0](i);return NaN}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{Number.isNaN(i)||e[0](i);return "NaN"}`,
  )
})

test("Coerce from string to string literal", t => {
  let quotedString = `"'\``
  let schema = S.string->S.to(S.literal(quotedString))

  t->Assert.deepEqual(quotedString->S.parseOrThrow(~to=schema), quotedString)
  t->U.assertThrowsMessage(
    () => "bar"->S.parseOrThrow(~to=schema),
    `Expected "${quotedString}", received "bar"`,
  )
  t->Assert.deepEqual(
    quotedString->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`quotedString`),
  )
  t->U.assertThrowsMessage(
    () => "bar"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected "${quotedString}", received "bar"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="\\"\'\`"||e[0](i);return i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i==="\\"\'\`"||e[0](i);return i}`)
})

test("Coerce from object shaped as string to float", t => {
  let schema = S.object(s => s.field("foo", S.string))->S.to(S.float)

  t->Assert.deepEqual({"foo": "123"}->S.parseOrThrow(~to=schema), 123.)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);let v1=+v0;!Number.isNaN(v1)||e[1](v0);return v1}`,
  )

  t->Assert.deepEqual(123.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"foo": "123"}`))
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return {"foo":""+i,}}`)
})

test("Coerce to literal can be used as tag and automatically embeded on reverse operation", t => {
  let schema = S.object(s => {
    let _ = s.field("tag", S.string->S.to(S.literal(true)))
  })

  t->Assert.deepEqual(()->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"tag": "true"}`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{i===void 0||e[0](i);return {"tag":"true",}}`,
  )

  t->Assert.deepEqual({"tag": "true"}->S.parseOrThrow(~to=schema), ())
  t->U.assertThrowsMessage(
    () => {"tag": "false"}->S.parseOrThrow(~to=schema),
    `Failed at ["tag"]: Expected "true", received "false"`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // FIXME: Test that it'll work with S.refine on S.string
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["tag"];typeof v0==="string"||e[1](v0);v0==="true"||e[0](v0);return void 0}`,
  )
})

test("Coerce from string to float", t => {
  let schema = S.string->S.to(S.float)

  t->Assert.deepEqual("10"->S.parseOrThrow(~to=schema), 10.)
  t->Assert.deepEqual("10.2"->S.parseOrThrow(~to=schema), 10.2)
  t->U.assertThrowsMessage(
    () => "tru"->S.parseOrThrow(~to=schema),
    `Expected number, received "tru"`,
  )
  t->Assert.deepEqual(10.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))
  t->Assert.deepEqual(10.2->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10.2"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);let v0=+i;!Number.isNaN(v0)||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;!Number.isNaN(v0)||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("Coerce from string to int32", t => {
  let schema = S.string->S.to(S.int)

  t->Assert.deepEqual("10"->S.parseOrThrow(~to=schema), 10)
  t->U.assertThrowsMessage(
    () => "2147483648"->S.parseOrThrow(~to=schema),
    `Expected int32, received "2147483648"`,
  )
  t->U.assertThrowsMessage(
    () => "10.2"->S.parseOrThrow(~to=schema),
    `Expected int32, received "10.2"`,
  )
  t->Assert.deepEqual(10->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);let v0=+i;v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("Coerce from string to port", t => {
  let schema = S.string->S.to(S.port)

  t->Assert.deepEqual("10"->S.parseOrThrow(~to=schema), 10)
  t->U.assertThrowsMessage(
    () => "2147483648"->S.parseOrThrow(~to=schema),
    `Expected port, received 2147483648`,
  )
  t->U.assertThrowsMessage(() => "10.2"->S.parseOrThrow(~to=schema), `Expected port, received 10.2`)
  t->Assert.deepEqual(10->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);let v0=+i;!Number.isNaN(v0)||e[1](i);v0>0&&v0<65536&&v0%1===0||e[0](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;!Number.isNaN(v0)||e[1](i);v0>0&&v0<65536&&v0%1===0||e[0](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{i>0&&i<65536&&i%1===0||e[0](i);return ""+i}`,
  )
})

test("Coerce from true to bool", t => {
  let schema = S.literal(true)->S.to(S.bool)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i===true||e[0](i);return i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{i===true||e[0](i);return i}`)
})

test("Coerce from string to bigint literal", t => {
  let schema = S.string->S.to(S.literal(10n))

  t->Assert.deepEqual("10"->S.parseOrThrow(~to=schema), 10n)
  t->U.assertThrowsMessage(() => "11"->S.parseOrThrow(~to=schema), `Expected "10", received "11"`)
  t->Assert.deepEqual(10n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);i==="10"||e[0](i);return 10n}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{i==="10"||e[0](i);return 10n}`)
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===10n||e[0](i);return "10"}`)
})

test("Coerce from string to bigint", t => {
  let schema = S.string->S.to(S.bigint)

  t->Assert.deepEqual("10"->S.parseOrThrow(~to=schema), 10n)
  t->U.assertThrowsMessage(
    () => "10.2"->S.parseOrThrow(~to=schema),
    `Expected bigint, received "10.2"`,
  )
  t->Assert.deepEqual(10n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
})

test("Coerce string after a transform", t => {
  let schema = S.string->S.transform(_ => {parser: v => v, serializer: v => v})->S.to(S.bool)

  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Expected boolean, received "true"`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[3](i);let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="boolean"||e[2](v0);return v0}`,
  )

  t->U.assertThrowsMessage(
    () => true->S.parseOrThrow(~to=S.reverse(schema)),
    `Expected string, received true`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{typeof i==="boolean"||e[3](i);let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="string"||e[2](v0);return v0}`,
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

  t->Assert.deepEqual("10"->S.parseOrThrow(~to=schema), Number(10.))
  t->Assert.deepEqual("true"->S.parseOrThrow(~to=schema), Boolean(true))

  t->Assert.throws(
    () => {
      "t"->S.parseOrThrow(~to=schema)
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
    `i=>{typeof i==="string"||e[3](i);try{let v0=+i;!Number.isNaN(v0)||e[1](i);i=v0}catch(e1){try{let v1;(v1=i==="true")||i==="false"||e[0](i);i=v1}catch(e2){e[2](i,e1,e2)}}return i}`,
  )

  t->Assert.deepEqual(Number(10.)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))
  t->Assert.deepEqual(Boolean(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
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
    () => S.decodeOrThrow(JSON.Boolean(true), ~from=schema, ~to=S.unknown),
    `Can't decode CustomJSON to string. Use S.to to define a custom decoder`,
    // `Expected string, received true`, FIXME: Should be this error
  )

  // t->U.assertCompiledCode(
  //   ~schema,
  //   ~op=#Encode,
  //   `i=>{let v0=e[0](i);if(typeof v0!=="string"){e[1](v0)}return v0}`,
  // )
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

  t->Assert.deepEqual(()->S.parseOrThrow(~to=schema), %raw(`null`))
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    // FIXME: It fails because we overwrite expected name with string version
    `Expected undefined, received null`,
  )
  t->Assert.deepEqual(%raw(`null`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i===void 0||e[0](i);return null}`)
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{i===null||e[0](i);return void 0}`)
})

test("Coerce from string to optional bool", t => {
  let schema = S.string->S.to(S.option(S.bool))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual("true"->S.parseOrThrow(~to=schema), Some(true))

  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Expected string, received null`,
  )

  t->Assert.deepEqual(Some(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"undefined"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[2](i);try{let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}catch(e0){if(i==="undefined"){i=void 0}else{e[1](i,e0)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
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
    %raw(`{"foo": "bar"}`)->S.parseOrThrow(~to=schema)
  }, `Can't decode { foo: string; } to string. Use S.to to define a custom decoder`)
  t->U.assertThrowsMessage(() => {
    %raw(`{"foo": "bar"}`)->S.decodeOrThrow(~from=schema, ~to=S.unknown)
  }, `Can't decode string to { foo: string; }. Use S.to to define a custom decoder`)
})

test("Coerce from string to JSON and then to bigint", t => {
  let schema = S.string->S.to(S.json)->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))
  t->Assert.deepEqual(123n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[1](i);let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{typeof i==="bigint"||e[0](i);return ""+i}`,
  )
})

test("Coerce from JSON to bigint", t => {
  let schema = S.json->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))
  t->U.assertThrowsMessage(() => {
    123->S.parseOrThrow(~to=schema)
  }, "Expected string, received 123")
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(~to=schema)
  }, "Expected string, received true")

  t->Assert.deepEqual(123n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    ~embedded=[],
    `i=>{typeof i==="string"||e[1](i);let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Encode, ~embedded=[], `i=>{return ""+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    ~embedded=[],
    `i=>{typeof i==="bigint"||e[0](i);return ""+i}`,
  )
})

test("Coerce from JSON to unit", t => {
  let schema = S.json->S.to(S.unit)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), ())
  t->U.assertThrowsMessage(() => {
    %raw(`undefined`)->S.parseOrThrow(~to=schema)
  }, "Expected null, received undefined")
  t->Assert.deepEqual(()->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, ~embedded=[], `i=>{i===null||e[0](i);return void 0}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    ~embedded=[],
    `i=>{i===void 0||e[0](i);return null}`,
  )
})

test("Coerce from JSON to NaN", t => {
  let schema = S.json->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), %raw(`NaN`))
  t->U.assertThrowsMessage(() => {
    %raw(`undefined`)->S.parseOrThrow(~to=schema)
  }, "Expected null, received undefined")
  t->Assert.deepEqual(%raw(`NaN`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, ~embedded=[], `i=>{i===null||e[0](i);return NaN}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    ~embedded=[],
    `i=>{Number.isNaN(i)||e[0](i);return null}`,
  )
})

test("Coerce from JSON to optional bigint", t => {
  let schema = S.json->S.to(S.option(S.bigint))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`"123"`)->S.parseOrThrow(~to=schema), Some(123n))
  t->U.assertThrowsMessage(() => {
    %raw(`123`)->S.parseOrThrow(~to=schema)
  }, `Expected bigint | undefined, received 123`)
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(Some(123n)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~embedded=[],
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(i===null){i=void 0}else{e[1](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~embedded=[],
    ~op=#Encode,
    `i=>{if(typeof i==="bigint"){i=""+i}else if(i===void 0){i=null}else{e[0](i)}return i}`,
  )
})

test("Coerce from JSON to array of bigint", t => {
  let schema = S.json->S.to(S.array(S.bigint))

  t->Assert.deepEqual(%raw(`["123"]`)->S.parseOrThrow(~to=schema), [123n])
  t->U.assertThrowsMessage(() => {
    %raw(`[123]`)->S.parseOrThrow(~to=schema)
  }, `Failed at ["0"]: Expected string, received 123`)
  t->Assert.deepEqual([123n]->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`["123"]`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    ~embedded=[],
    `i=>{Array.isArray(i)||e[2](i);let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){try{let v2=i[v0];typeof v2==="string"||e[1](v2);let v1;try{v1=BigInt(v2)}catch(_){e[0](v2)}v4[v0]=v1}catch(v3){v3.path=\'["\'+v0+\'"]\'+v3.path;throw v3}}return v4}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    ~embedded=[],
    `i=>{let v2=new Array(i.length);for(let v1=0;v1<i.length;++v1){v2[v1]=""+i[v1]}return v2}`,
  )
})

test("Coerce from JSON to tuple with bigint", t => {
  let schema = S.json->S.to(S.schema(s => (s.matches(S.string), s.matches(S.bigint))))

  t->Assert.deepEqual(%raw(`["foo", "123"]`)->S.parseOrThrow(~to=schema), ("foo", 123n))
  t->U.assertThrowsMessage(() => {
    %raw(`["foo"]`)->S.parseOrThrow(~to=schema)
  }, `Expected [string, bigint], received ["foo"]`)
  t->Assert.deepEqual(
    ("foo", 123n)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`["foo", "123"]`),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    ~embedded=[],
    `i=>{Array.isArray(i)||e[4](i);i.length===2||e[3](i);let v0=i["0"],v2=i["1"];typeof v0==="string"||e[0](v0);typeof v2==="string"||e[2](v2);let v1;try{v1=BigInt(v2)}catch(_){e[1](v2)}return [v0,v1,]}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    ~embedded=[],
    `i=>{return [i["0"],""+i["1"],]}`,
  )
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
//   //   }->S.parseOrThrow(~to=schema),
//   //   {
//   //     "id": 123n,
//   //     "isDeleted": None,
//   //   },
//   // )
//   // t->U.assertThrowsMessage(() => {
//   //   123->S.parseOrThrow(~to=schema)
//   // }, "Expected string, received 123")
//   // t->U.assertThrowsMessage(() => {
//   //   true->S.parseOrThrow(~to=schema)
//   // }, "Expected string, received true")

//   // t->Assert.deepEqual(123n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

//   t->U.assertCompiledCode(
//     ~schema,
//     ~op=#Parse,
//     `i=>{if(typeof i!=="string"){e[1](i)}let v0;try{v0=BigInt(i)}catch(_){e[0](i)}return v0}`,
//   )
//   // t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return ""+i}`)
//   // t->U.assertCompiledCode(
//   //   ~schema,
//   //   ~op=#ReverseParse,
//   //   `i=>{typeof i==="bigint"||e[0](i);return ""+i}`,
//   // )
// })

test("Coerce from union to bigint", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])->S.to(
      S.bigint,
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))
  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), %raw(`123n`))
  t->U.assertThrowsMessage(
    () => {
      true->S.parseOrThrow(~to=schema)
    },
    `Expected string | number | boolean, received true
- Can't decode boolean to bigint. Use S.to to define a custom decoder`,
  )
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(~to=schema)
  }, "Expected string | number | boolean, received 123n")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=BigInt(i)}else if(typeof i==="boolean"){e[2](i,e[1])}else{e[3](i)}return i}`,
  )

  t->Assert.deepEqual(123n->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{i=""+i}catch(e0){try{throw e[0]}catch(e1){try{throw e[1]}catch(e2){e[2](i,e0,e1,e2)}}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{typeof i==="bigint"||e[3](i);try{i=""+i}catch(e0){try{throw e[0]}catch(e1){try{throw e[1]}catch(e2){e[2](i,e0,e1,e2)}}}return i}`,
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
    `i=>{if(typeof i==="string"){e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){e[0](i)||e[3](i);i=BigInt(i)}else if(typeof i==="boolean"){e[5](i,e[4])}else{e[6](i)}return i}`,
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
    `i=>{if(typeof i==="string"){e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0}else if(typeof i==="number"&&!Number.isNaN(i)){let v2=""+i;e[0](v2)||e[4](v2);let v1;try{v1=BigInt(v2)}catch(_){e[3](v2)}i=v1}else if(typeof i==="boolean"){e[6](i,e[5])}else{e[7](i)}return i}`,
    ~message="Should apply refinement after the item transformation",
  )
})

test("Coerce from union to bigint and then to string", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
    ->S.to(S.bigint)
    ->S.to(S.string)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), %raw(`"123"`))
  t->U.assertThrowsMessage(
    () => {
      true->S.parseOrThrow(~to=schema)
    },
    `Expected string | number | boolean, received true
- Can't decode boolean to bigint. Use S.to to define a custom decoder`,
  )
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(~to=schema)
  }, "Expected string | number | boolean, received 123n")

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=""+v0}else if(typeof i==="number"&&!Number.isNaN(i)){i=""+BigInt(i)}else if(typeof i==="boolean"){e[2](i,e[1])}else{e[3](i)}return i}`,
  )

  t->Assert.deepEqual("123"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))
  t->U.assertThrowsMessage(() => {
    "abc"->S.decodeOrThrow(~from=schema, ~to=S.unknown)
  }, `Expected bigint, received "abc"`)

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;try{v0=BigInt(i)}catch(_){e[0](i)}try{v0=""+v0}catch(e0){try{throw e[1]}catch(e1){try{throw e[2]}catch(e2){e[3](v0,e0,e1,e2)}}}return v0}`,
  )
})

test("Coerce from union to wider union should keep the original value type", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]),
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), %raw(`123`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(~to=schema)
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
    true->S.parseOrThrow(~to=schema)
  }, "Expected string | number, received true")
})

test("Transform from union to wider union with different items order keeps source type", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([S.float->S.castToUnknown, S.string->S.castToUnknown, S.bool->S.castToUnknown]),
    )

  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(~to=schema)
  }, "Expected string | number, received true")
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="string"||typeof i==="number"&&!Number.isNaN(i))){e[0](i)}return i}`,
  )
})

test("Tier 1: source tag matches a target variant — identity wins, no cross-type coercion", t => {
  let schema = S.string->S.to(S.union([S.bool->S.castToUnknown, S.string->S.castToUnknown]))

  // String input flows through as a string. The bool variant is never tried,
  // so "true"/"false" are NOT coerced to bool.
  t->Assert.deepEqual("true"->S.parseOrThrow(~to=schema), %raw(`"true"`))
  t->Assert.deepEqual("false"->S.parseOrThrow(~to=schema), %raw(`"false"`))
  t->Assert.deepEqual("anything"->S.parseOrThrow(~to=schema), %raw(`"anything"`))
  t->U.assertThrowsMessage(() => true->S.parseOrThrow(~to=schema), `Expected string, received true`)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{typeof i==="string"||e[0](i);return i}`)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(!(typeof i==="string")){e[0](i)}return i}`,
  )
})

test(
  "Tier 2: nullish bridge — null source uses opposite undefined target when no null target",
  t => {
    let schema =
      S.literal(%raw(`null`))->S.to(S.union([S.string->S.castToUnknown, S.unit->S.castToUnknown]))

    // null bridges to undefined (the opposite-nullish target). The string
    // variant is never compiled into the dispatch.
    t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), %raw(`undefined`))
    t->U.assertThrowsMessage(
      () => "hello"->S.parseOrThrow(~to=schema),
      `Expected null, received "hello"`,
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{i===null||e[1](i);try{i=void 0}catch(e1){e[0](i,e1)}return i}`,
    )
  },
)

test("Tier 3: no source-tag match — coercion fallback retained", t => {
  let schema = S.bool->S.to(S.union([S.string->S.castToUnknown, S.float->S.castToUnknown]))

  // Source tag (boolean) matches no target tag → fall through to today's
  // trial-coercion behavior. bool→string via `""+i` succeeds.
  t->Assert.deepEqual(true->S.parseOrThrow(~to=schema), %raw(`"true"`))
  t->Assert.deepEqual(false->S.parseOrThrow(~to=schema), %raw(`"false"`))
  t->U.assertThrowsMessage(
    () => "hello"->S.parseOrThrow(~to=schema),
    `Expected boolean, received "hello"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="boolean"||e[2](i);try{i=""+i}catch(e0){try{throw e[0]}catch(e1){e[1](i,e0,e1)}}return i}`,
  )
})

test(
  "Tier 2: nullish bridge — undefined source uses opposite null target when no undefined target",
  t => {
    let schema =
      S.unit->S.to(S.union([S.string->S.castToUnknown, S.literal(%raw(`null`))->S.castToUnknown]))

    // undefined bridges to null. The string variant is never compiled into the dispatch.
    t->Assert.deepEqual(()->S.parseOrThrow(~to=schema), %raw(`null`))
    t->U.assertThrowsMessage(
      () => "hello"->S.parseOrThrow(~to=schema),
      `Expected undefined, received "hello"`,
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{i===void 0||e[1](i);try{i=null}catch(e1){e[0](i,e1)}return i}`,
    )
  },
)

test("Tier 1 instance: same class wins, other instance branch never compiled", t => {
  let schema =
    S.instance(%raw(`Set`))->S.to(
      S.union([S.instance(%raw(`Map`))->Obj.magic, S.instance(%raw(`Set`))->Obj.magic]),
    )

  t->Assert.deepEqual(%raw(`new Set(["a"])`)->S.parseOrThrow(~to=schema), %raw(`new Set(["a"])`))
  t->U.assertThrowsMessage(
    () => %raw(`new Map()`)->S.parseOrThrow(~to=schema),
    `Expected Set, received [object Map]`,
  )

  // Generated dispatch only checks `i instanceof Set` — Map branch absent.
  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i instanceof e[0]||e[1](i);return i}`)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
})

test("Tier 3 instance: source class absent from target — coercion fallback retained", t => {
  let schema =
    S.instance(%raw(`Set`))->S.to(
      S.union([S.string->S.castToUnknown, S.instance(%raw(`Map`))->Obj.magic]),
    )

  // Set source matches neither string nor Map by class name → tier-3 fallback.
  t->U.assertThrowsMessage(
    () => %raw(`new Set()`)->S.parseOrThrow(~to=schema),
    `Expected string | Map, received [object Set]
- Can't decode Set to string. Use S.to to define a custom decoder
- Can't decode Set to Map. Use S.to to define a custom decoder`,
  )
  t->U.assertThrowsMessage(
    () => "hello"->S.parseOrThrow(~to=schema),
    `Expected Set, received "hello"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{i instanceof e[3]||e[4](i);try{throw e[0]}catch(e0){try{throw e[1]}catch(e1){e[2](i,e0,e1)}}return i}`,
  )
})

test("Tier 1 instance: S.date -> S.union([S.string, S.date]) keeps Date identity", t => {
  let schema = S.date->S.to(S.union([S.string->S.castToUnknown, S.date->S.castToUnknown]))

  let d = Date.fromString("2024-01-01T00:00:00Z")
  t->Assert.deepEqual(d->S.parseOrThrow(~to=schema), d->Obj.magic)
  t->U.assertThrowsMessage(
    () => %raw(`"2024-01-01"`)->S.parseOrThrow(~to=schema),
    `Expected Date, received "2024-01-01"`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`new Date("invalid")`)->S.parseOrThrow(~to=schema),
    `Expected Date, received [object Date]`,
  )

  // Forward dispatch only checks the Date branch; the string variant is absent.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{i instanceof e[1]||e[2](i);!Number.isNaN(i.getTime())||e[0](i);return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  // Reverse handles both source variants: parse string as Date, or pass Date through.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="string"){let v0=new Date(i);!Number.isNaN(v0.getTime())||e[0](v0);i=new Date(i)}else if(!(i instanceof e[1])){e[2](i)}return i}`,
  )
})

test("Tier 1 over tier 2: undefined -> [null, undefined] keeps undefined (tier-1 wins)", t => {
  let schema =
    S.unit->S.to(S.union([S.literal(%raw(`null`))->S.castToUnknown, S.unit->S.castToUnknown]))

  // The undefined target is present, so tier-1 must win — undefined stays undefined.
  // The null bridge must NOT be applied even though null is also a target.
  t->Assert.deepEqual(()->S.parseOrThrow(~to=schema), %raw(`undefined`))
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Expected undefined, received null`,
  )

  // Generated dispatch only checks `i===void 0` — the null branch is absent.
  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i===void 0||e[0](i);return i}`)
})

test("Tier 3 fallback for unknown source — transform on unknown variant still runs", t => {
  // Source is S.unknown. Even though the target has an `unknown` variant
  // (the second one, with a transform), tier-1 must NOT fire here: an
  // unknown source has no derived tag, so dispatch falls through to
  // tier-3 trial and tries `string` first, then the transformed unknown.
  let schema = S.unknown->S.to(
    S.union([
      S.string->S.castToUnknown,
      S.unknown
      ->S.transform(_ => {
        parser: v => Some(v),
        serializer: v => v->Obj.magic,
      })
      ->S.castToUnknown,
    ]),
  )

  // String input matches the string variant — passes through as-is.
  t->Assert.deepEqual("abc"->S.parseOrThrow(~to=schema), %raw(`"abc"`))
  // Non-string input fails the string check, falls through to the unknown
  // variant, which applies the transform (wraps in Some).
  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), Some(123)->Obj.magic)

  // Generated code is the tier-3 trial chain — string check first, then
  // the transformed unknown branch in a catch.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{typeof i==="string"||e[0](i);}catch(e1){try{let v0;try{v0=e[1](i)}catch(x){e[2](x)}i=v0}catch(e2){e[3](i,e1,e2)}}return i}`,
  )
})

test(
  "Tier 1: union with refine+to as target — downstream refine/to only see narrowed variant",
  t => {
    // Source S.string matches the string variant in the target union.
    // Tier 1 narrows the target union to just [string], so the union's refiner
    // and the chained .to(bigint) should compile only for the string case.
    let target =
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
      ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
      ->S.to(S.bigint)
    let schema = S.string->S.to(target)

    t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))

    // FIXME: ideally the only surviving case should compile inline as
    // `typeof i==="string"||e(i);if(!e(i)){e()}let v0=BigInt(i);return v0` —
    // outer typecheck, per-case refine, string→bigint, no exhaustive wrapper.
    // Today the union still wraps the single surviving case in try/catch.
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="string"||e[4](i);try{e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0}catch(e0){e[3](i,e0)}return i}`,
    )
  },
)

test(
  "Tier 1: S.string -> S.to(union[str,bigint] -> refine -> to(bigint)) — union output schema is S.string, downstream sees only refine + BigInt",
  t => {
    // The target is a 2-variant union [string, bigint] with a refine and a
    // .to(S.bigint) chained on it. Source is S.string, so tier-1 narrows the
    // union to just [string]; the union decoder's output schema becomes S.string.
    // Downstream the refine and .to(S.bigint) compile against that single
    // surviving variant — no bigint case, no fallback.
    let target =
      S.union([S.string->S.castToUnknown, S.bigint->S.castToUnknown])
      ->S.refine(_ => true)
      ->S.to(S.bigint)
    let schema = S.string->S.to(target)

    t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))

    // FIXME: ideally the surviving string case should compile inline without
    // the exhaustive try/catch wrapper around it. Locked to current output
    // until the union codegen learns to bypass dispatch for single-case
    // narrowing.
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="string"||e[4](i);try{e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0}catch(e0){e[3](i,e0)}return i}`,
    )
  },
)
