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

test("compileAssertOrThrow emits validation only", t => {
  let fn = S.compileAssertOrThrow(~to=S.string)
  t->U.assertCompiledCode(~schema=S.string, ~op=#Assert, `i=>{typeof i==="string"||e[0](i);return void 0}`)
  t->Assert.deepEqual(fn("abc"), ())
  t->U.assertThrowsMessage(() => fn(%raw(`1`)), `Expected string, received 1`)
})

test("compileConvert3OrThrow chains through the middle schema", t => {
  let schema = S.object(s => s.field("n", S.int))
  let fn = S.compileConvert3OrThrow(~from=S.jsonString, ~through=S.json, ~to=schema)
  t->Assert.deepEqual(fn(`{"n":1}`), 1)
  t->Assert.deepEqual(`{"n":2}`->S.convert3OrThrow(~from=S.jsonString, ~through=S.json, ~to=schema), 2)
})

test("convert1OrThrow goes from a schema's input to its output", t => {
  let schema = S.string->S.to(S.float)
  t->Assert.deepEqual(%raw(`"1.5"`)->S.convert1OrThrow(~to=schema), 1.5)
})

asyncTest("compileAssertAsyncOrThrow resolves to unit", async t => {
  let fn = S.compileAssertAsyncOrThrow(~to=S.string)
  t->Assert.deepEqual(await fn("abc"), ())
})
