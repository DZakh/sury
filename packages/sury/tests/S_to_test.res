open Ava

Only.test("Coerce from string to string", t => {
  let schema = S.string->S.to(S.string)
  t->Assert.is(schema, S.string, ())
})

Only.test("Coerce from string to bool", t => {
  let schema = S.string->S.to(S.bool)

  t->Assert.deepEqual("false"->S.parseOrThrow(schema), false, ())
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), true, ())
  t->U.assertThrows(
    () => "tru"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.bool->S.toUnknown,
        received: %raw(`"tru"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(false->S.reverseConvertOrThrow(schema), %raw(`"false"`), ())

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

Only.test("Coerce from bool to string", t => {
  let schema = S.bool->S.to(S.string)

  t->Assert.deepEqual(false->S.parseOrThrow(schema), "false", ())
  t->Assert.deepEqual(true->S.parseOrThrow(schema), "true", ())
  t->U.assertThrows(
    () => "tru"->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({
        expected: S.bool->S.toUnknown,
        received: %raw(`"tru"`),
      }),
      path: S.Path.empty,
      operation: ReverseConvert,
    },
  )
  t->Assert.deepEqual("false"->S.reverseConvertOrThrow(schema), %raw(`false`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="boolean"){e[1](i)}return \"\"+i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{return \"\"+i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0;(v0=i===\"true\")||i===\"false\"||e[0](i);return v0}`,
  )
})

Only.test("Coerce from string to bool literal", t => {
  let schema = S.string->S.to(S.literal(false))

  t->Assert.deepEqual("false"->S.parseOrThrow(schema), false, ())
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(false)->S.toUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(false->S.reverseConvertOrThrow(schema), %raw(`"false"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}i==="false"||e[0](i);return false}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==false){e[1](i)}return "false"}`)
})

Only.test("Coerce from string to null literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual("null"->S.parseOrThrow(schema), %raw(`null`), ())
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(%raw(`null`))->S.toUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`null`)->S.reverseConvertOrThrow(schema), %raw(`"null"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}i==="null"||e[0](i);return null}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[1](i)}return "null"}`)
})

Only.test("Coerce from string to undefined literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`undefined`)))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(schema), %raw(`undefined`), ())
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(%raw(`undefined`))->S.toUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`undefined`)->S.reverseConvertOrThrow(schema), %raw(`"undefined"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}i==="undefined"||e[0](i);return void 0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==void 0){e[1](i)}return "undefined"}`,
  )
})

Only.test("Coerce from string to NaN literal", t => {
  let schema = S.string->S.to(S.literal(%raw(`NaN`)))

  t->Assert.deepEqual("NaN"->S.parseOrThrow(schema), %raw(`NaN`), ())
  t->U.assertThrows(
    () => "true"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(%raw(`NaN`))->S.toUnknown,
        received: %raw(`"true"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`NaN`)->S.reverseConvertOrThrow(schema), %raw(`"NaN"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}i==="NaN"||e[0](i);return NaN}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(!Number.isNaN(i)){e[1](i)}return "NaN"}`,
  )
})

Only.test("Coerce from string to string literal", t => {
  let quotedString = `"'\``
  let schema = S.string->S.to(S.literal(quotedString))

  t->Assert.deepEqual(quotedString->S.parseOrThrow(schema), quotedString, ())
  t->U.assertThrows(
    () => "bar"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(quotedString)->S.toUnknown,
        received: %raw(`"bar"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(quotedString->S.reverseConvertOrThrow(schema), %raw(`quotedString`), ())
  t->U.assertThrows(
    () => "bar"->S.reverseConvertOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(quotedString)->S.toUnknown,
        received: %raw(`"bar"`),
      }),
      path: S.Path.empty,
      operation: ReverseConvert,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}if(i!=="\\"\'\`"){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!=="\\"\'\`"){e[0](i)}return i}`)
})

Only.test("Coerce from object shaped as string to float", t => {
  let schema = S.object(s => s.field("foo", S.string))->S.to(S.float)

  t->Assert.deepEqual({"foo": "123"}->S.parseOrThrow(schema), 123., ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foo"];if(typeof v0!=="string"){e[0](v0)}let v1=+v0;Number.isNaN(v1)&&e[1](v0);return v1}`,
  )

  t->Assert.deepEqual(123.->S.reverseConvertOrThrow(schema), %raw(`{"foo": "123"}`), ())
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"foo":""+i,}}`)
})

Only.test(
  "Coerce to literal can be used as tag and automatically embeded on reverse operation",
  t => {
    let schema = S.object(s => {
      let _ = s.field("tag", S.string->S.to(S.literal(true)))
    })

    t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`{"tag": "true"}`), ())
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(i!==void 0){e[2](i)}return {"tag":"true",}}`,
    )

    t->Assert.deepEqual({"tag": "true"}->S.parseOrThrow(schema), (), ())
    t->U.assertThrows(
      () => {"tag": "false"}->S.parseOrThrow(schema),
      {
        code: InvalidType({
          expected: S.literal(true)->S.toUnknown,
          received: %raw(`"false"`),
        }),
        path: S.Path.fromLocation("tag"),
        operation: Parse,
      },
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[3](i)}let v0=i["tag"];if(typeof v0!=="string"){e[0](v0)}v0==="true"||e[1](v0);return e[2]}`,
    )
  },
)

Only.test("Coerce from string to float", t => {
  let schema = S.string->S.to(S.float)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10., ())
  t->Assert.deepEqual("10.2"->S.parseOrThrow(schema), 10.2, ())
  t->U.assertThrows(
    () => "tru"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.float->S.toUnknown,
        received: %raw(`"tru"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10.->S.reverseConvertOrThrow(schema), %raw(`"10"`), ())
  t->Assert.deepEqual(10.2->S.reverseConvertOrThrow(schema), %raw(`"10.2"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0=+i;Number.isNaN(v0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;Number.isNaN(v0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

Only.test("Coerce from string to int32", t => {
  let schema = S.string->S.to(S.int)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10, ())
  t->U.assertThrows(
    () => "2147483648"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.int->S.toUnknown,
        received: %raw(`"2147483648"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->U.assertThrows(
    () => "10.2"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.int->S.toUnknown,
        received: %raw(`"10.2"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10->S.reverseConvertOrThrow(schema), %raw(`"10"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}let v0=+i;(v0>2147483647||v0<-2147483648||v0%1!==0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;(v0>2147483647||v0<-2147483648||v0%1!==0)&&e[0](i);return v0}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return ""+i}`)
})

Only.test("Coerce from string to port", t => {
  let schema = S.string->S.to(S.int->S.port)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10, ())
  t->U.assertThrowsMessage(
    () => "2147483648"->S.parseOrThrow(schema),
    `Failed parsing: Expected port, received 2147483648`,
  )
  t->U.assertThrowsMessage(
    () => "10.2"->S.parseOrThrow(schema),
    `Failed parsing: Expected port, received 10.2`,
  )
  t->Assert.deepEqual(10->S.reverseConvertOrThrow(schema), %raw(`"10"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0=+i;(Number.isNaN(v0))&&e[0](i);v0>0&&v0<65536&&v0%1===0||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=+i;(Number.isNaN(v0))&&e[0](i);v0>0&&v0<65536&&v0%1===0||e[1](v0);return v0}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{i>0&&i<65536&&i%1===0||e[0](i);return ""+i}`,
  )
})

Only.test("Coerce from true to bool", t => {
  let schema = S.literal(true)->S.to(S.bool)

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==true){e[0](i)}return i}`)
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{if(i!==true){e[0](i)}return i}`)
})

Only.test("Coerce from string to bigint literal", t => {
  let schema = S.string->S.to(S.literal(10n))

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10n, ())
  t->U.assertThrows(
    () => "11"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.literal(10n)->S.toUnknown,
        received: %raw(`"11"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10n->S.reverseConvertOrThrow(schema), %raw(`"10"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[1](i)}i==="10"||e[0](i);return 10n}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#Convert, `i=>{i==="10"||e[0](i);return 10n}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==10n){e[1](i)}return "10"}`)
})

Only.test("Coerce from string to bigint", t => {
  let schema = S.string->S.to(S.bigint)

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), 10n, ())
  t->U.assertThrows(
    () => "10.2"->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.bigint->S.toUnknown,
        received: %raw(`"10.2"`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(10n->S.reverseConvertOrThrow(schema), %raw(`"10"`), ())

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

Only.test("Coerce string after a transform", t => {
  let schema = S.string->S.transform(_ => {parser: v => v, serializer: v => v})->S.to(S.bool)

  t->U.assertThrowsMessage(
    () => "true"->S.parseOrThrow(schema),
    `Failed parsing: Expected boolean, received "true"`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[2](i)}let v0=e[0](i);if(typeof v0!=="boolean"){e[1](v0)}return v0}`,
  )

  // FIXME: This is not correct. Should be fixed after S.transform is removed by S.to
  t->Assert.deepEqual(true->S.parseOrThrow(S.reverse(schema)), %raw(`true`), ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(typeof i!=="boolean"){e[1](i)}return e[0](i)}`,
  )
})

@unboxed
type numberOrBoolean = Number(float) | Boolean(bool)

// FIXME: Test nested union
// FIXME: Test transformed union
Only.test("Coerce string to unboxed union (each item separately)", t => {
  let schema =
    S.string->S.to(
      S.union([
        S.schema(s => Number(s.matches(S.float))),
        S.schema(s => Boolean(s.matches(S.bool))),
      ]),
    )

  t->Assert.deepEqual("10"->S.parseOrThrow(schema), Number(10.), ())
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), Boolean(true), ())

  t->Assert.throws(
    () => {
      "t"->S.parseOrThrow(schema)
    },
    ~expectations={
      message: `Failed parsing: Expected string | string, received "t"
- Expected number, received "t"
- Expected boolean, received "t"`,
    },
    (),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{let v0=+i;Number.isNaN(v0)&&e[0](i);i=v0}catch(e0){try{let v1;(v1=i==="true")||i==="false"||e[1](i);i=v1}catch(e1){e[2](i,e0,e1)}}}else{e[3](i)}return i}`,
  )

  t->Assert.deepEqual(Number(10.)->S.reverseConvertOrThrow(schema), %raw(`"10"`), ())
  t->Assert.deepEqual(Boolean(true)->S.reverseConvertOrThrow(schema), %raw(`"true"`), ())

  // TODO: Can be improved
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="number"&&!Number.isNaN(i)){i=""+i}else if(typeof i==="boolean"){i=""+i}return i}`,
  )
})

// Only.test("Coerce string to JSON schema", t => {
//   let schema = S.string->S.to(
//     S.recursive(self => {
//       S.union([
//         S.schema(_ => Json.Null),
//         S.schema(s => Json.Number(s.matches(S.float))),
//         S.schema(s => Json.Boolean(s.matches(S.bool))),
//         S.schema(s => Json.String(s.matches(S.string))),
//         S.schema(s => Json.Object(s.matches(S.dict(self)))),
//         S.schema(s => Json.Array(s.matches(S.array(self)))),
//       ])
//     }),
//   )

//   t->U.assertCompiledCode(
//     ~schema,
//     ~op=#ReverseConvert,
//     ``,
//   )
// })

Only.test("Keeps description of the schema we are coercing to (not working)", t => {
  // Fix it later if it's needed
  let schema = S.string->S.to(S.string->S.meta({description: "To descr"}))
  t->Assert.is((schema->S.untag).description, None, ())

  // let schema = S.string->S.description("From descr")->S.to(S.string->S.description("To descr"))
  // t->Assert.is((schema->S.untag).description, Some("To descr"), ())

  // There's no specific reason for it. Just wasn't needed for cases S.to initially designed
  let schema = S.string->S.meta({description: "From descr"})->S.to(S.string)
  t->Assert.is((schema->S.untag).description, Some("From descr"), ())
})

Only.test("Coerce from unit to null literal", t => {
  let schema = S.unit->S.to(S.literal(%raw(`null`)))

  t->Assert.deepEqual(()->S.parseOrThrow(schema), %raw(`null`), ())
  t->U.assertThrows(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: S.unit->S.toUnknown,
        received: %raw(`null`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )
  t->Assert.deepEqual(%raw(`null`)->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==void 0){e[1](i)}return null}`)
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!==null){e[1](i)}return void 0}`)
})

Only.test("Coerce from string to optional bool", t => {
  let schema = S.string->S.to(S.option(S.bool))

  t->Assert.deepEqual("undefined"->S.parseOrThrow(schema), None, ())
  t->Assert.deepEqual("true"->S.parseOrThrow(schema), Some(true), ())
  t->U.assertThrows(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: schema->S.toUnknown,
        received: %raw(`null`),
      }),
      path: S.Path.empty,
      operation: Parse,
    },
  )

  t->Assert.deepEqual(Some(true)->S.reverseConvertOrThrow(schema), %raw(`"true"`), ())
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`"undefined"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}catch(e0){try{i==="undefined"||e[1](i);i=void 0}catch(e1){e[2](i,e0,e1)}}}else{e[3](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(i===void 0){i="undefined"}return i}`,
  )
})
