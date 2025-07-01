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
    `i=>{let v0=e[0](i);return v0}
JSON: i=>{if(Array.isArray(i)){let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v3;try{let v2=e[0][1](i[v0]);v3=v2}catch(v1){if(v1&&v1.s===s){v1.path=""+'["'+v0+'"]'+v1.path}throw v1}v4[v0]=v3}i=v4}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v9={};for(let v5 in i){let v8;try{let v7=e[1][1](i[v5]);v8=v7}catch(v6){if(v6&&v6.s===s){v6.path=""+'["'+v5+'"]'+v6.path}throw v6}v9[v5]=v8}i=v9}else if(!(typeof i==="string"||typeof i==="boolean"||typeof i==="number"&&!Number.isNaN(i)||i===null)){e[2](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=e[0](i);return v0}
JSON: i=>{if(Array.isArray(i)){let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v3;try{let v2=e[0][0](i[v0]);v3=v2}catch(v1){if(v1&&v1.s===s){v1.path=""+'["'+v0+'"]'+v1.path}throw v1}v4[v0]=v3}i=v4}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v9={};for(let v5 in i){let v8;try{let v7=e[1][0](i[v5]);v8=v7}catch(v6){if(v6&&v6.s===s){v6.path=""+'["'+v5+'"]'+v6.path}throw v6}v9[v5]=v8}i=v9}return i}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.json
  t->U.assertCompiledCode(
    ~schema=schema->S.reverse,
    ~op=#Convert,
    `i=>{let v0=e[0](i);return v0}
JSON: i=>{if(Array.isArray(i)){let v4=new Array(i.length);for(let v0=0;v0<i.length;++v0){let v3;try{let v2=e[0][0](i[v0]);v3=v2}catch(v1){if(v1&&v1.s===s){v1.path=""+'["'+v0+'"]'+v1.path}throw v1}v4[v0]=v3}i=v4}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v9={};for(let v5 in i){let v8;try{let v7=e[1][0](i[v5]);v8=v7}catch(v6){if(v6&&v6.s===s){v6.path=""+'["'+v5+'"]'+v6.path}throw v6}v9[v5]=v8}i=v9}return i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{let v0=e[0](i);return v0}`)
})

test("Reverse schema to S.json", t => {
  let schema = S.json
  t->U.assertEqualSchemas(schema->S.reverse, S.json->S.castToUnknown)
})

test("Succesfully uses reversed schema with validate=true for parsing back to initial value", t => {
  let schema = S.json
  t->U.assertReverseParsesBack(schema, %raw(`{"foo":"bar"}`))
})
