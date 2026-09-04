open Vitest

let assertCode = (t, fn: 'a => 'b, code) => {
  t->Assert.is((fn->Obj.magic)["toString"](), code)
}

test("Schema with empty code optimised to use precompiled noop function", t => {
  let fn = S.compileConvertOrThrow(~from=S.string, ~to=S.unknown)
  t->assertCode(fn, U.noopOpCode)
})

test("Doesn't compile primitive unknown with assert output to noop", t => {
  let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=S.unknown->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Doesn't compile to noop when primitive converted to json string", t => {
  let fn = S.compileConvertOrThrow(~from=S.bool, ~to=S.jsonString)
  t->assertCode(fn, `i=>{return ""+i}`)
})

test("JsonString output with Async mode", t => {
  let fn = S.compileConvertAsyncOrThrow(~from=S.string, ~to=S.jsonString)
  t->assertCode(fn, `i=>{return Promise.resolve(e[0](i))}`)
})

test("TypeValidation=false works with assert output", t => {
  let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return void 0}`)
  let fn = S.compileConvertOrThrow(~from=S.string, ~to=S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Assert output with Async mode", t => {
  let fn = S.compileConvertAsyncOrThrow(~from=S.unknown, ~to=S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return Promise.resolve(void 0)}`)
})

test("Immitate assert returning true with S.to and literal", t => {
  let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=S.string->S.to(S.literal(true)->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return true}`)
})

test("compileConvertOrThrow with ~via chains through the middle schema", t => {
  let schema = S.object(s => s.field("n", S.int))
  let fn = S.compileConvertOrThrow(~from=S.jsonString, ~via=S.json, ~to=schema)
  t->Assert.deepEqual(fn(S.JsonString(`{"n":1}`)), 1)
  t->Assert.deepEqual(S.JsonString(`{"n":2}`)->S.convertOrThrow(~from=S.jsonString, ~via=S.json, ~to=schema), 2)
  t->U.assertThrowsMessage(
    () => S.JsonString(`{"n":"x"}`)->S.convertOrThrow(~from=S.jsonString, ~via=S.json, ~to=schema),
    `Failed at n: Expected int32, received "x"`,
  )
})

test("validate and compileValidate answer with a bool", t => {
  t->Assert.deepEqual("abc"->S.validate(~to=S.string), true)
  t->Assert.deepEqual(%raw(`1`)->S.validate(~to=S.string), false)
  let isString = S.compileValidate(~to=S.string)
  t->Assert.deepEqual(isString("abc"), true)
  t->Assert.deepEqual(isString(%raw(`1`)), false)
})
