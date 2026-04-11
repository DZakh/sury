open Ava

S.enableJsonString()

let assertCode = (t, fn: 'a => 'b, code) => {
  t->Assert.is((fn->Obj.magic)["toString"](), code)
}

test("Schema with empty code optimised to use precompiled noop function", t => {
  let fn = S.makeConvertOrThrow(S.string, S.unknown)
  t->assertCode(fn, U.noopOpCode)
})

test("Doesn't compile primitive unknown with assert output to noop", t => {
  let fn = S.makeConvertOrThrow(S.unknown, S.unknown->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Doesn't compile to noop when primitive converted to json string", t => {
  let fn = S.makeConvertOrThrow(S.bool, S.jsonString)
  t->assertCode(fn, `i=>{return ""+i}`)
})

test("JsonString output with Async mode", t => {
  let fn = S.makeAsyncConvertOrThrow(S.string, S.jsonString)
  t->assertCode(fn, `i=>{return Promise.resolve(JSON.stringify(i))}`)
})

test("TypeValidation=false works with assert output", t => {
  let fn = S.makeConvertOrThrow(S.unknown, S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return void 0}`)
  let fn = S.makeConvertOrThrow(S.string, S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Assert output with Async mode", t => {
  let fn = S.makeAsyncConvertOrThrow(S.unknown, S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return Promise.resolve(void 0)}`)
})

test("Immitate assert returning true with S.to and literal", t => {
  let fn = S.makeConvertOrThrow(S.unknown, S.string->S.to(S.literal(true)->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return true}`)
})
