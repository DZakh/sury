open Ava
open RescriptCore

test("Supports String", t => {
  let schema = S.json
  let data = JSON.Encode.string("Foo")

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Number", t => {
  let schema = S.json
  let data = JSON.Encode.float(123.)

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Bool", t => {
  let schema = S.json
  let data = JSON.Encode.bool(true)

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Null", t => {
  let schema = S.json
  let data = JSON.Encode.null

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Array", t => {
  let schema = S.json
  let data = JSON.Encode.array([JSON.Encode.string("foo"), JSON.Encode.null])

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Supports Object", t => {
  let schema = S.json
  let data = JSON.Encode.object(
    [("bar", JSON.Encode.string("foo")), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(data->S.reverseConvertToJsonOrThrow(schema), data)
})

test("Fails to parse Object field", t => {
  let schema = S.json
  let data = JSON.Encode.object(
    [("bar", %raw(`undefined`)), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->U.assertThrows(
    () => data->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`undefined`), expected: schema->S.castToUnknown}),
      operation: Parse,
      path: S.Path.fromLocation("bar"),
    },
  )
})

test("Fails to parse matrix field", t => {
  let schema = S.json
  let data = %raw(`[1,[undefined]]`)

  t->U.assertThrows(
    () => data->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`undefined`), expected: schema->S.castToUnknown}),
      operation: Parse,
      path: S.Path.fromArray(["1", "0"]),
    },
  )
})

test("Fails to parse NaN", t => {
  let schema = S.json
  Js.log(schema)
  t->U.assertThrows(
    () => %raw(`NaN`)->S.parseOrThrow(schema),
    {
      code: InvalidType({received: %raw(`NaN`), expected: schema->S.castToUnknown}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Fails to parse undefined", t => {
  let schema = S.json
  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    `Failed parsing: Expected JSON, received undefined`,
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.json

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{return e[0](i)}
JSON: i=>{if(Array.isArray(i)){let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v2;try{v2=e[0][1](i[v0])}catch(v1){if(v1&&v1.s===s){v1.path=""+'["'+v0+'"]'+v1.path}throw v1}v3[v0]=v2}i=v3}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v7={};for(let v4 in i){let v6;try{v6=e[1][1](i[v4])}catch(v5){if(v5&&v5.s===s){v5.path=""+'["'+v4+'"]'+v5.path}throw v5}v7[v4]=v6}i=v7}else if(!(typeof i==="string"||typeof i==="boolean"||typeof i==="number"&&!Number.isNaN(i)||i===null)){e[2](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{return e[0](i)}
JSON: i=>{if(Array.isArray(i)){let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v2;try{v2=e[0][0](i[v0])}catch(v1){if(v1&&v1.s===s){v1.path=""+'["'+v0+'"]'+v1.path}throw v1}v3[v0]=v2}i=v3}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v7={};for(let v4 in i){let v6;try{v6=e[1][0](i[v4])}catch(v5){if(v5&&v5.s===s){v5.path=""+'["'+v4+'"]'+v5.path}throw v5}v7[v4]=v6}i=v7}return i}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.json
  t->U.assertCompiledCode(
    ~schema=schema->S.reverse,
    ~op=#Convert,
    `i=>{return e[0](i)}
JSON: i=>{if(Array.isArray(i)){let v3=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v2;try{v2=e[0][0](i[v0])}catch(v1){if(v1&&v1.s===s){v1.path=""+'["'+v0+'"]'+v1.path}throw v1}v3[v0]=v2}i=v3}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v7={};for(let v4 in i){let v6;try{v6=e[1][0](i[v4])}catch(v5){if(v5&&v5.s===s){v5.path=""+'["'+v4+'"]'+v5.path}throw v5}v7[v4]=v6}i=v7}return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Reverse schema to S.json", t => {
  let schema = S.json
  t->U.assertEqualSchemas(schema->S.reverse, S.json->S.castToUnknown)
})

test("Succesfully uses reversed schema with validate=true for parsing back to initial value", t => {
  let schema = S.json
  t->U.assertReverseParsesBack(schema, %raw(`{"foo":"bar"}`))
})
