open Ava

S.enableJson()

test("Supports String", t => {
  let schema = S.json
  let data = JSON.Encode.string("Foo")

  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(data->S.decodeOrThrow(~from=schema, ~to=S.json), data)
})

test("Supports Number", t => {
  let schema = S.json
  let data = JSON.Encode.float(123.)

  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(data->S.decodeOrThrow(~from=schema, ~to=S.json), data)
})

test("Supports Bool", t => {
  let schema = S.json
  let data = JSON.Encode.bool(true)

  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(data->S.decodeOrThrow(~from=schema, ~to=S.json), data)
})

test("Supports Null", t => {
  let schema = S.json
  let data = JSON.Encode.null

  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(data->S.decodeOrThrow(~from=schema, ~to=S.json), data)
})

test("Supports Array", t => {
  let schema = S.json
  let data = JSON.Encode.array([JSON.Encode.string("foo"), JSON.Encode.null])

  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(data->S.decodeOrThrow(~from=schema, ~to=S.json), data)
})

test("Supports Object", t => {
  let schema = S.json
  let data = JSON.Encode.object(
    [("bar", JSON.Encode.string("foo")), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(data->S.decodeOrThrow(~from=schema, ~to=S.json), data)
})

test("Fails to parse Object field", t => {
  let schema = S.json
  let data = JSON.Encode.object(
    [("bar", %raw(`undefined`)), ("baz", JSON.Encode.null)]->Dict.fromArray,
  )

  t->U.assertThrowsMessage(
    () => data->S.parseOrThrow(~to=schema),
    `Failed at ["bar"]: Expected JSON, received undefined`,
  )
})

test("Fails to parse matrix field", t => {
  let schema = S.json
  let data = %raw(`[1,[undefined]]`)

  t->U.assertThrowsMessage(
    () => data->S.parseOrThrow(~to=schema),
    `Failed at ["1"]["0"]: Expected JSON, received undefined`,
  )
})

test("Fails to parse NaN", t => {
  let schema = S.json
  t->U.assertThrowsMessage(() => %raw(`NaN`)->S.parseOrThrow(~to=schema), `Expected JSON, received NaN`)
})

test("Fails to parse undefined", t => {
  let schema = S.json
  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(~to=schema),
    `Expected JSON, received undefined`,
  )
})

let jsonParseCode = `i=>{e[0](i);return i}
JSON: i=>{if(Array.isArray(i)){for(let v0=0;v0<i.length;++v0){try{e[0]["unknown->JSON--0"](i[v0]);}catch(v1){v1.path='["'+v0+'"]'+v1.path;throw v1}}}else if(typeof i==="object"&&i&&!Array.isArray(i)){for(let v2 in i){try{e[1]["unknown->JSON--0"](i[v2]);}catch(v3){v3.path='["'+v2+'"]'+v3.path;throw v3}}}else if(!(typeof i==="string"||typeof i==="boolean"||typeof i==="number"&&!Number.isNaN(i)||i===null)){e[2](i)}return i}`
test("Compiled parse code snapshot", t => {
  let schema = S.json

  t->U.assertCompiledCode(~schema, ~op=#Parse, jsonParseCode)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
})

test("Compiled serialize code snapshot", t => {
  let schema = S.json
  t->U.assertCompiledCodeIsNoop(~schema=schema->S.reverse, ~op=#Convert)
  t->U.assertCompiledCode(~schema, ~op=#ReverseParse, jsonParseCode)
})

test("Reverse schema to S.json", t => {
  let schema = S.json
  t->U.assertEqualSchemas(schema->S.reverse, S.json->S.castToUnknown)
})

test("Succesfully uses reversed schema with validate=true for parsing back to initial value", t => {
  let schema = S.json
  t->U.assertReverseParsesBack(schema, %raw(`{"foo":"bar"}`))
})
