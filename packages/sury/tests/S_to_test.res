open Vitest

test("Coerce from string to string", t => {
  let schema = S.string->S.to(S.string)
  t->Assert.is(schema, S.string)
})

test("Coerce a one-directional transform to itself relies on the same-instance shortcut", t => {
  // `S.to` returns `from` untouched when both arguments are the same instance.
  // Without that shortcut this would chain the transform's int output back into
  // the target's string decoder, which the missing serializer can't bridge — as
  // the two-instances case below shows.
  let makeSchema = () => S.string->S.transform(_ => {parser: String.length})

  let schema = makeSchema()
  t->Assert.is(schema->S.to(schema), schema)
  t->Assert.deepEqual("hello"->S.parseOrThrow(~to=schema->S.to(schema)), 5)

  t->U.assertThrowsMessage(
    () => "hello"->S.parseOrThrow(~to=makeSchema()->S.to(makeSchema())),
    `Expected string, received 5`,
  )
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

test("Coerce from string to option of int (union dispatch over a converted value)", t => {
  let schema = S.string->S.to(S.option(S.int))

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), Some(123))
  t->Assert.deepEqual("undefined"->S.parseOrThrow(~to=schema), None)
  t->U.assertThrowsMessage(
    () => "1.5"->S.parseOrThrow(~to=schema),
    `Expected int32 | undefined, received "1.5"
- Expected int32, received 1.5`,
  )

  // Regression (v0 is not defined): the union discriminant must not be hoisted
  // above the `let v0 = +i` conversion it reads. The string->number coercion is
  // a self-contained codeFromPrev unit, so the int branch dispatches via
  // try/catch with its declaration intact.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"||e[4](i);for(;;){let r;try{let v0=+i;!Number.isNaN(v0)||e[1](i);v0<=2147483647&&v0>=-2147483648&&v0%1===0||e[0](v0);i=v0;;break}catch(x){(r||(r=[])).push(e[2](x))}if(i==="undefined"){i=void 0;break}e[3](i,...(r||[]))}return i}`,
  )

  t->Assert.deepEqual(Some(123)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"undefined"`))
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="number"&&!Number.isNaN(i)&&i<=2147483647&&i>=-2147483648&&i%1===0){i=""+i;break}if(i===void 0){i="undefined";break}e[0](i)}return i}`,
  )
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
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[2](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);let v1=+v0;!Number.isNaN(v1)||e[1](v0);return v1}`,
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
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[2](i);let v0=i["tag"];typeof v0==="string"||e[1](v0);v0==="true"||e[0](v0);return void 0}`,
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
    `i=>{typeof i==="string"||e[4](i);for(;;){let r;try{let v0=+i;!Number.isNaN(v0)||e[0](i);i=v0;;break}catch(x){(r||(r=[])).push(e[2](x))}try{let v1;(v1=i==="true")||i==="false"||e[1](i);i=v1;;break}catch(x){(r||(r=[])).push(e[2](x))}e[3](i,...(r||[]))}return i}`,
  )

  t->Assert.deepEqual(Number(10.)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"10"`))
  t->Assert.deepEqual(Boolean(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"true"`))

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="number"&&!Number.isNaN(i)){i=""+i;break}if(typeof i==="boolean"){i=""+i;break}e[0](i)}return i}`,
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
    `i=>{typeof i==="string"||e[3](i);for(;;){let r;try{let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0;;break}catch(x){(r||(r=[])).push(e[1](x))}if(i==="undefined"){i=void 0;break}e[2](i,...(r||[]))}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="boolean"){i=""+i;break}if(i===void 0){i="undefined";break}e[0](i)}return i}`,
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
    `i=>{for(;;){if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0;;break}if(i===null){i=void 0;break}e[1](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~embedded=[],
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="bigint"){i=""+i;break}if(i===void 0){i=null;break}e[0](i)}return i}`,
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
  let schema = S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(S.bigint)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))
  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), %raw(`123n`))
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(~to=schema)
  }, "Expected string | number, received 123n")

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{for(;;){if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=v0;break}if(typeof i==="number"&&!Number.isNaN(i)){i=BigInt(i);break}e[1](i)}return i}`)
})

test("Rejects a union -> bigint conversion whose member has no decoder", t => {
  // A member the built-in decoder can't be built for is an error in the
  // operation, raised where it's written — never a branch that throws per value.
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])->S.to(
      S.bigint,
    )

  t->U.assertThrowsMessage(
    () => "123"->S.parseOrThrow(~to=schema),
    `Can't decode boolean to bigint. Use S.to to define a custom decoder`,
  )

  // S.never marks the member deliberately unreachable instead.
  let explicit =
    S.union([
      S.string->S.castToUnknown,
      S.float->S.castToUnknown,
      S.bool->S.to(S.never)->S.castToUnknown,
    ])->S.to(S.bigint)
  t->Assert.deepEqual("123"->S.parseOrThrow(~to=explicit), %raw(`123n`))
  t->U.assertThrowsMessage(
    () => true->S.parseOrThrow(~to=explicit),
    `Expected never, received true`,
  )
})

test("Rejects reversing a union -> bigint conversion with no single way back", t => {
  // Both members decode into bigint, so reversing has to pick one — and
  // bigint -> number has no built-in decoder, which makes the choice an error
  // rather than a silent preference for the string member.
  let schema = S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(S.bigint)

  t->U.assertThrowsMessage(
    () => 123n->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Can't decode bigint to number. Use S.to to define a custom decoder`,
  )

  let explicit =
    S.union([S.string->S.castToUnknown, S.float->S.to(S.never)->S.castToUnknown])->S.to(S.bigint)
  t->Assert.deepEqual(123n->S.decodeOrThrow(~from=explicit, ~to=S.unknown), %raw(`"123"`))
})

test("Coerce from union to bigint with refinement on union", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])
    ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
    ->S.to(S.bigint)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{for(;;){if(typeof i==="string"){e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0;break}if(typeof i==="number"&&!Number.isNaN(i)){e[0](i)||e[3](i);i=BigInt(i);break}e[4](i)}return i}`)
})

test("Coerce from union to bigint with refinement on union (with an item transformed to)", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.to(S.string)->S.castToUnknown])
    ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
    ->S.to(S.bigint)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(typeof i==="string"){e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0;break}if(typeof i==="number"&&!Number.isNaN(i)){let v2=""+i;e[0](v2)||e[4](v2);let v1;try{v1=BigInt(v2)}catch(_){e[3](v2)}i=v1;break}e[5](i)}return i}`,
    ~message="Should apply refinement after the item transformation",
  )
})

test("Coerce from union to bigint and then to string", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(S.bigint)->S.to(S.string)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), %raw(`"123"`))
  t->U.assertThrowsMessage(() => {
    123n->S.parseOrThrow(~to=schema)
  }, "Expected string | number, received 123n")

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{for(;;){if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i=""+v0;break}if(typeof i==="number"&&!Number.isNaN(i)){i=""+BigInt(i);break}e[1](i)}return i}`)
})

test("Rejects widening a union into one with an uncovered member", t => {
  // A union-to-union conversion coerces nothing, so the two unions have to cover
  // each other — the extra boolean target has no source member to come from.
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]),
    )

  t->U.assertThrowsMessage(
    () => "123"->S.parseOrThrow(~to=schema),
    `Invalid operation: can't convert string | number to string | number | boolean — boolean has no same-type variant on the other side. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )

  // S.never marks the extra member unreachable, and the rest passes through.
  let explicit =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([
        S.string->S.castToUnknown,
        S.float->S.castToUnknown,
        S.never->S.to(S.bool)->S.castToUnknown,
      ]),
    )
  t->Assert.deepEqual("123"->S.parseOrThrow(~to=explicit), %raw(`"123"`))
  t->Assert.deepEqual(123->S.parseOrThrow(~to=explicit), %raw(`123`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(~to=explicit)
  }, "Expected string | number, received true")

  t->U.assertCompiledCode(~schema=explicit, ~op=#Parse, `i=>{(typeof i==="string"||typeof i==="number"&&!Number.isNaN(i))||e[0](i);return i}`)
})

test("Fails to transform union to union to string", t => {
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])
    ->S.to(S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown]))
    ->S.to(S.string)

  // Each member converts to the chained target on its own, so the string member
  // meets `string | number | boolean` — where it matches one member and not the
  // others, which is the ambiguity rule 2 rejects.
  t->U.assertThrowsMessage(
    () => true->S.parseOrThrow(~to=schema),
    `Invalid operation: can't convert string to string | number | boolean — string has the same type as the source and the others don't. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )
})

test("Transform from union to reordered union keeps source type", t => {
  // Member order doesn't matter to rule 4 — both unions cover each other, so
  // every value passes through unchanged.
  let schema =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
      S.union([S.float->S.castToUnknown, S.string->S.castToUnknown]),
    )

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`"123"`))
  t->U.assertThrowsMessage(() => {
    true->S.parseOrThrow(~to=schema)
  }, "Expected string | number, received true")
  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{(typeof i==="string"||typeof i==="number"&&!Number.isNaN(i))||e[0](i);return i}`)
})

test("Rejects a source matching some but not all target members", t => {
  // For "true" — keep the string, or decode it to the boolean member? Sury can't
  // tell, so the operation is rejected where it's written.
  let schema = S.string->S.to(S.union([S.bool->S.castToUnknown, S.string->S.castToUnknown]))

  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(~to=schema),
    `Invalid operation: can't convert string to boolean | string — string has the same type as the source and the others don't. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )

  // Pass strings through, never producing a boolean:
  let passThrough =
    S.string->S.to(
      S.union([S.never->S.to(S.bool)->S.castToUnknown, S.string->S.castToUnknown]),
    )
  t->Assert.deepEqual("true"->S.parseOrThrow(~to=passThrough), %raw(`"true"`))
  t->Assert.deepEqual("anything"->S.parseOrThrow(~to=passThrough), %raw(`"anything"`))

  // Or try decoding to a boolean first, keeping the string otherwise:
  let decodeFirst =
    S.string->S.to(
      S.union([S.string->S.to(S.bool)->S.castToUnknown, S.string->S.castToUnknown]),
    )
  t->Assert.deepEqual("true"->S.parseOrThrow(~to=decodeFirst), %raw(`true`))
  t->Assert.deepEqual("anything"->S.parseOrThrow(~to=decodeFirst), %raw(`"anything"`))
})

test("No nullish bridge for a non-union source — members are tried in order", t => {
  // The nullish bridge belongs to union-to-union conversion. A plain `null`
  // source goes through rule 2 instead: the first member that accepts wins, and
  // `S.string` decodes null to the "null" sentinel.
  let schema =
    S.literal(%raw(`null`))->S.to(S.union([S.string->S.castToUnknown, S.unit->S.castToUnknown]))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), %raw(`"null"`))
  t->U.assertThrowsMessage(
    () => "hello"->S.parseOrThrow(~to=schema),
    `Expected null, received "hello"`,
  )

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{i===null||e[0](i);for(;;){i="null";break;}return i}`)

  // Reach the undefined member by marking the string one unreachable.
  let toUndefined =
    S.literal(%raw(`null`))->S.to(
      S.union([S.never->S.to(S.string)->S.castToUnknown, S.unit->S.castToUnknown]),
    )
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=toUndefined), %raw(`undefined`))
})

test("No source-tag match — every member must still be decodable", t => {
  // boolean matches neither member, so both are attempted — and boolean has no
  // built-in decoder to number, which rejects the whole operation.
  let schema = S.bool->S.to(S.union([S.string->S.castToUnknown, S.float->S.castToUnknown]))

  t->U.assertThrowsMessage(
    () => true->S.parseOrThrow(~to=schema),
    `Can't decode boolean to number. Use S.to to define a custom decoder`,
  )

  let explicit =
    S.bool->S.to(
      S.union([S.string->S.castToUnknown, S.never->S.to(S.float)->S.castToUnknown]),
    )
  t->Assert.deepEqual(true->S.parseOrThrow(~to=explicit), %raw(`"true"`))
  t->Assert.deepEqual(false->S.parseOrThrow(~to=explicit), %raw(`"false"`))
  t->U.assertThrowsMessage(
    () => "hello"->S.parseOrThrow(~to=explicit),
    `Expected boolean, received "hello"`,
  )

  t->U.assertCompiledCode(~schema=explicit, ~op=#Parse, `i=>{typeof i==="boolean"||e[0](i);i=""+i;return i}`)
})

test("No nullish bridge for an undefined source either", t => {
  let schema =
    S.unit->S.to(
      S.union([S.string->S.castToUnknown, S.literal(%raw(`null`))->S.castToUnknown]),
    )

  t->Assert.deepEqual(()->S.parseOrThrow(~to=schema), %raw(`"undefined"`))
  t->U.assertThrowsMessage(
    () => "hello"->S.parseOrThrow(~to=schema),
    `Expected undefined, received "hello"`,
  )

  let toNull =
    S.unit->S.to(
      S.union([
        S.never->S.to(S.string)->S.castToUnknown,
        S.literal(%raw(`null`))->S.castToUnknown,
      ]),
    )
  t->Assert.deepEqual(()->S.parseOrThrow(~to=toNull), %raw(`null`))
})

test("Instance source matching one of two instance members is ambiguous", t => {
  let schema =
    S.instance(%raw(`Set`))->S.to(
      S.union([S.instance(%raw(`Map`))->Obj.magic, S.instance(%raw(`Set`))->Obj.magic]),
    )

  t->U.assertThrowsMessage(
    () => %raw(`new Set(["a"])`)->S.parseOrThrow(~to=schema),
    `Invalid operation: can't convert Set to Map | Set — Set has the same type as the source and the others don't. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )

  let explicit =
    S.instance(%raw(`Set`))->S.to(
      S.union([
        S.never->S.to(S.instance(%raw(`Map`)))->Obj.magic,
        S.instance(%raw(`Set`))->Obj.magic,
      ]),
    )
  t->Assert.deepEqual(%raw(`new Set(["a"])`)->S.parseOrThrow(~to=explicit), %raw(`new Set(["a"])`))
  t->U.assertCompiledCode(~schema=explicit, ~op=#Parse, `i=>{i instanceof e[0]||e[1](i);return i}`)
})

test("Instance source absent from the target union has no decoder to it", t => {
  // Set matches neither member, and there is no built-in Set -> string decoder,
  // so this can never work — it's rejected instead of compiling into an
  // operation that throws for every input.
  let schema =
    S.instance(%raw(`Set`))->S.to(
      S.union([S.string->S.castToUnknown, S.instance(%raw(`Map`))->Obj.magic]),
    )

  t->U.assertThrowsMessage(
    () => %raw(`new Set()`)->S.parseOrThrow(~to=schema),
    `Can't decode Set to string. Use S.to to define a custom decoder`,
  )
})

test("S.date -> S.union([S.string, S.date]) is an ambiguous widening", t => {
  let schema = S.date->S.to(S.union([S.string->S.castToUnknown, S.date->S.castToUnknown]))
  let d = Date.fromString("2024-01-01T00:00:00Z")

  t->U.assertThrowsMessage(
    () => d->S.parseOrThrow(~to=schema),
    `Invalid operation: can't convert Date to string | Date — Date has the same type as the source and the others don't. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )

  let explicit =
    S.date->S.to(
      S.union([S.never->S.to(S.string)->S.castToUnknown, S.date->S.castToUnknown]),
    )
  t->Assert.deepEqual(d->S.parseOrThrow(~to=explicit), d->Obj.magic)
  t->U.assertThrowsMessage(
    () => %raw(`"2024-01-01"`)->S.parseOrThrow(~to=explicit),
    `Expected Date, received "2024-01-01"`,
  )
  t->U.assertCompiledCode(~schema=explicit, ~op=#Parse, `i=>{i instanceof e[1]||e[2](i);!Number.isNaN(i.getTime())||e[0](i);return i}`)
})

test("A const source the target spells out exactly reaches only that member", t => {
  let schema =
    S.unit->S.to(
      S.union([S.literal(%raw(`null`))->S.castToUnknown, S.unit->S.castToUnknown]),
    )

  // The value already *is* undefined, so the null member is dead code and the
  // bridge never applies.
  t->Assert.deepEqual(()->S.parseOrThrow(~to=schema), %raw(`undefined`))
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Expected undefined, received null`,
  )

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
    `i=>{for(;;){if(typeof i==="string")break;let v0=e[0](i);i=v0;break;}return i}`,
  )
})

test("Refined+converted target union is still an ambiguous widening", t => {
  let target =
    S.union([S.string->S.castToUnknown, S.float->S.castToUnknown, S.bool->S.castToUnknown])
    ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
    ->S.to(S.bigint)
  let schema = S.string->S.to(target)

  t->U.assertThrowsMessage(
    () => "123"->S.parseOrThrow(~to=schema),
    `Invalid operation: can't convert string to string | number | boolean — string has the same type as the source and the others don't. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )

  // Narrow the target to the reachable member and both the refinement and the
  // conversion still run.
  let explicitTarget =
    S.union([
      S.string->S.castToUnknown,
      S.never->S.to(S.float)->S.castToUnknown,
      S.never->S.to(S.bool)->S.castToUnknown,
    ])
    ->S.refine(v => typeof(v) !== #bigint, ~error="Unsupported bigint")
    ->S.to(S.bigint)
  let explicit = S.string->S.to(explicitTarget)
  t->Assert.deepEqual("123"->S.parseOrThrow(~to=explicit), %raw(`123n`))
  t->U.assertCompiledCode(~schema=explicit, ~op=#Parse, `i=>{typeof i==="string"||e[3](i);e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0;return i}`)
})

test("A narrowed target union runs its refine and chained .to on the one member", t => {
  // [string, bigint] with a refine and a .to(S.bigint) chained on it. The bigint
  // member is marked unreachable, so only the string member compiles — with the
  // refinement and the conversion.
  let target =
    S.union([S.string->S.castToUnknown, S.never->S.to(S.bigint)->S.castToUnknown])
    ->S.refine(_ => true)
    ->S.to(S.bigint)
  let schema = S.string->S.to(target)

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), %raw(`123n`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{typeof i==="string"||e[3](i);e[0](i)||e[2](i);let v0;try{v0=BigInt(i)}catch(_){e[1](i)}i=v0;return i}`)
})

// Union schema as decoder input: the conversion runs for each source variant
// separately (see "Decoding into / out of a union" in the docs)

test("Converts union nested in object into another union (per member)", t => {
  // Rule 4 coerces nothing, so a member that needs converting says so with its
  // own S.to; null <-> undefined still bridges on its own.
  let schema =
    S.schema(s =>
      {
        "f": s.matches(
          S.union([
            S.bigint->S.to(S.string)->S.castToUnknown,
            S.literal(%raw(`null`))->S.castToUnknown,
          ]),
        ),
      }
    )->S.to(
      S.schema(s =>
        {
          "f": s.matches(S.union([S.string->S.castToUnknown, S.unit->S.castToUnknown])),
        }
      ),
    )

  t->Assert.deepEqual({"f": %raw(`123n`)}->S.parseOrThrow(~to=schema), {"f": %raw(`"123"`)})
  t->Assert.deepEqual({"f": %raw(`null`)}->S.parseOrThrow(~to=schema), {"f": %raw(`undefined`)})

  // Reverse direction
  t->Assert.deepEqual(
    {"f": %raw(`"123"`)}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{f: 123n}`),
  )
  t->Assert.deepEqual(
    {"f": %raw(`undefined`)}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{f: null}`),
  )
})

test("Rejects a nested union whose member has no same-type target member", t => {
  let schema =
    S.schema(s =>
      {
        "f": s.matches(
          S.union([S.bigint->S.castToUnknown, S.literal(%raw(`null`))->S.castToUnknown]),
        ),
      }
    )->S.to(
      S.schema(s =>
        {
          "f": s.matches(S.union([S.string->S.castToUnknown, S.unit->S.castToUnknown])),
        }
      ),
    )

  t->U.assertThrowsMessage(
    () => {"f": %raw(`123n`)}->S.parseOrThrow(~to=schema),
    `Failed at ["f"]: Invalid operation: can't convert bigint | null to string | undefined — bigint has no same-type variant on the other side. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )
})

test("Converts union nested in object into a single schema (per member)", t => {
  let schema =
    S.schema(s =>
      {
        "f": s.matches(
          S.union([S.string->S.castToUnknown, S.float->S.to(S.string)->S.castToUnknown]),
        ),
      }
    )->S.to(S.schema(s => {"f": s.matches(S.string)}))

  t->Assert.deepEqual({"f": %raw(`123`)}->S.parseOrThrow(~to=schema), {"f": %raw(`"123"`)})
  t->Assert.deepEqual({"f": %raw(`"abc"`)}->S.parseOrThrow(~to=schema), {"f": %raw(`"abc"`)})
})

test("Rejects a nested union where only some members match the single target", t => {
  let schema =
    S.schema(s =>
      {
        "f": s.matches(S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])),
      }
    )->S.to(S.schema(s => {"f": s.matches(S.string)}))

  t->U.assertThrowsMessage(
    () => {"f": %raw(`123`)}->S.parseOrThrow(~to=schema),
    `Failed at ["f"]: Invalid operation: can't convert string | number to string — string has the same type as the target and the others don't. Use S.to to say what you mean, or S.never to mark a variant unreachable`,
  )
})

test("Union member with no decoder to the target rejects the operation", t => {
  let schema =
    S.schema(s =>
      {
        "f": s.matches(S.union([S.string->S.castToUnknown, S.bool->S.castToUnknown])),
      }
    )->S.to(S.schema(s => {"f": s.matches(S.bigint)}))

  t->U.assertThrowsMessage(
    () => {"f": %raw(`"12"`)}->S.parseOrThrow(~to=schema),
    `Failed at ["f"]: Can't decode boolean to bigint. Use S.to to define a custom decoder`,
  )

  let explicit =
    S.schema(s =>
      {
        "f": s.matches(
          S.union([S.string->S.castToUnknown, S.bool->S.to(S.never)->S.castToUnknown]),
        ),
      }
    )->S.to(S.schema(s => {"f": s.matches(S.bigint)}))
  t->Assert.deepEqual({"f": %raw(`"12"`)}->S.parseOrThrow(~to=explicit), {"f": %raw(`12n`)})
  t->U.assertThrowsMessage(
    () => {"f": %raw(`true`)}->S.parseOrThrow(~to=explicit),
    `Failed at ["f"]: Expected never, received true`,
  )
})

test("Tier 1: matching const variant wins over an earlier mapped literal", t => {
  let schema =
    S.union([S.literal("a"), S.literal("b")])->S.to(
      S.union([S.literal("b"), S.literal("a"), S.literal("c")]),
    )

  t->Assert.deepEqual("a"->S.parseOrThrow(~to=schema), "a")
  t->Assert.deepEqual("b"->S.parseOrThrow(~to=schema), "b")
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="string"&&(i==="a"||i==="b")||e[0](i);return i}`,
  )
})

test("Tier 1: object source picks the matching object variant in target union", t => {
  let objA = S.schema(s => {"k": s.matches(S.literal("a")), "x": s.matches(S.float)})
  let objB = S.schema(s => {"k": s.matches(S.literal("b")), "y": s.matches(S.string)})
  let schema = objA->S.to(S.union([objB->Obj.magic, objA->Obj.magic]))

  t->Assert.deepEqual(
    {"k": "a", "x": 1.}->S.parseOrThrow(~to=schema),
    {"k": "a", "x": 1.}->Obj.magic,
  )
})

test("Converts union of objects into reordered union of objects", t => {
  let objA = S.schema(s => {"k": s.matches(S.literal("a")), "x": s.matches(S.float)})
  let objB = S.schema(s => {"k": s.matches(S.literal("b")), "y": s.matches(S.string)})
  let schema =
    S.union([objA->Obj.magic, objB->Obj.magic])->S.to(
      S.union([objB->Obj.magic, objA->Obj.magic]),
    )

  t->Assert.deepEqual(
    %raw(`{k: "a", x: 1}`)->S.parseOrThrow(~to=schema),
    %raw(`{k: "a", x: 1}`),
  )
  t->Assert.deepEqual(
    %raw(`{k: "b", y: "hi"}`)->S.parseOrThrow(~to=schema),
    %raw(`{k: "b", y: "hi"}`),
  )
  t->U.assertThrowsMessage(
    () => %raw(`{k: "c"}`)->S.parseOrThrow(~to=schema),
    `Expected { k: "a"; x: number; } | { k: "b"; y: string; }, received { k: "c"; }`,
  )
})

test("Converts union nested in array into another union (per member)", t => {
  let schema =
    S.array(
      S.union([
        S.bigint->S.to(S.string)->S.castToUnknown,
        S.literal(%raw(`null`))->S.castToUnknown,
      ]),
    )->S.to(S.array(S.union([S.string->S.castToUnknown, S.unit->S.castToUnknown])))

  t->Assert.deepEqual(
    %raw(`[123n, null]`)->S.parseOrThrow(~to=schema),
    %raw(`["123", undefined]`),
  )
  t->U.assertThrowsMessage(
    () => %raw(`[true]`)->S.parseOrThrow(~to=schema),
    `Failed at ["0"]: Expected bigint | null, received true`,
  )
})

test("Converts union nested in tuple into a single schema (per member)", t => {
  let schema =
    S.schema(s => (
      s.matches(S.union([S.string->S.castToUnknown, S.float->S.to(S.string)->S.castToUnknown])),
      s.matches(S.bool),
    ))->S.to(S.schema(s => (s.matches(S.string), s.matches(S.bool))))

  t->Assert.deepEqual(%raw(`[123, true]`)->S.parseOrThrow(~to=schema), %raw(`["123", true]`))
  t->Assert.deepEqual(%raw(`["abc", false]`)->S.parseOrThrow(~to=schema), %raw(`["abc", false]`))
})

asyncTest("Converts union nested in object into an async target (per member)", async t => {
  let schema =
    S.schema(s =>
      {
        "f": s.matches(
          S.union([S.string->S.castToUnknown, S.float->S.to(S.string)->S.castToUnknown]),
        ),
      }
    )->S.to(
      S.schema(s =>
        {
          "f": s.matches(S.string->S.transform(_ => {asyncParser: v => Promise.resolve(v)})),
        }
      ),
    )

  t->Assert.deepEqual(
    await %raw(`{f: 123}`)->S.parseAsyncOrThrow(~to=schema),
    {"f": "123"},
  )
  t->Assert.deepEqual(
    await %raw(`{f: "abc"}`)->S.parseAsyncOrThrow(~to=schema),
    {"f": "abc"},
  )
})

test("Union variant with a transformed field — parse and encode roundtrip", t => {
  let variantA = S.schema(s =>
    {
      "k": s.matches(S.literal("a")),
      "n": s.matches(S.string->S.to(S.float)),
    }
  )
  let variantB = S.schema(s => {"k": s.matches(S.literal("b"))})
  let schema = S.schema(s =>
    {
      "u": s.matches(S.union([variantA->Obj.magic, variantB->Obj.magic])),
    }
  )

  t->Assert.deepEqual(
    %raw(`{u: {k: "a", n: "12"}}`)->S.parseOrThrow(~to=schema),
    %raw(`{u: {k: "a", n: 12}}`),
  )
  t->Assert.deepEqual(
    %raw(`{u: {k: "a", n: 12}}`)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{u: {k: "a", n: "12"}}`),
  )
})

test("Two fused .to stages over a composite field round-trip (no phantom var)", t => {
  // A nested object whose own field transforms, fed through a second `.to` whose
  // input re-reads that transformed value. The second stage materialises the
  // first stage's already-emitted output late; before the `_notVar` `finalized`
  // re-read this dropped the declaration and emitted an undeclared var, throwing
  // `ReferenceError` at runtime.
  let inner = () => S.schema(s => {"a": s.matches(S.int->S.to(S.string))})
  let schema = S.schema(s => {"foo": s.matches(inner())})->S.to(
    S.schema(s => {"foo": s.matches(inner())}),
  )

  t->Assert.deepEqual(
    %raw(`{"foo":{"a":5}}`)->S.parseOrThrow(~to=schema),
    %raw(`{"foo":{"a":"5"}}`),
  )
  t->U.assertReverseParsesBack(schema, %raw(`{"foo":{"a":"5"}}`))
})
