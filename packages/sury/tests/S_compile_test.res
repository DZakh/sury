open Ava

S.enableJsonString()

let assertCode = (t, fn: 'a => 'b, code) => {
  t->Assert.is((fn->Obj.magic)["toString"](), code)
}

test("Schema with empty code optimised to use precompiled noop function", t => {
  let fn = S.decoder(~from=S.string, ~to=S.unknown)
  t->assertCode(fn, U.noopOpCode)
})

test("Doesn't compile primitive unknown with assert output to noop", t => {
  let fn = S.decoder(~from=S.unknown, ~to=S.unknown->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Doesn't compile to noop when primitive converted to json string", t => {
  let fn = S.decoder(~from=S.bool, ~to=S.jsonString)
  t->assertCode(fn, `i=>{return ""+i}`)
})

test("JsonString output with Async mode", t => {
  let fn = S.asyncDecoder(~from=S.string, ~to=S.jsonString)
  t->assertCode(fn, `i=>{return Promise.resolve(JSON.stringify(i))}`)
})

test("TypeValidation=false works with assert output", t => {
  let fn = S.decoder(~from=S.unknown, ~to=S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return void 0}`)
  let fn = S.decoder(~from=S.string, ~to=S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Assert output with Async mode", t => {
  let fn = S.asyncDecoder(~from=S.unknown, ~to=S.string->S.to(S.literal()->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return Promise.resolve(void 0)}`)
})

test("Immitate assert returning true with S.to and literal", t => {
  let fn = S.decoder(~from=S.unknown, ~to=S.string->S.to(S.literal(true)->S.noValidation(true)))
  t->assertCode(fn, `i=>{typeof i==="string"||e[0](i);return true}`)
})
