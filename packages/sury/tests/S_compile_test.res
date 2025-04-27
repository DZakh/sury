open Ava

let assertCode = (t, fn: 'a => 'b, code) => {
  t->Assert.is((fn->Obj.magic)["toString"](), code, ())
}

test("Schema with empty code optimised to use precompiled noop function", t => {
  let schema = S.string
  let fn = schema->S.compile(~input=Any, ~output=Unknown, ~mode=Sync, ~typeValidation=false)
  t->assertCode(fn, U.noopOpCode)
})

test("Doesn't compile primitive unknown with assert output to noop", t => {
  let schema = S.unknown
  let fn = schema->S.compile(~input=Any, ~output=Assert, ~mode=Sync, ~typeValidation=true)
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Doesn't compile to noop when primitive converted to json string", t => {
  let schema = S.string
  let fn = schema->S.compile(~input=Any, ~output=JsonString, ~mode=Sync, ~typeValidation=false)
  t->assertCode(fn, `i=>{return JSON.stringify(i)}`)
})

test("JsonString output with Async mode", t => {
  let schema = S.string
  let fn = schema->S.compile(~input=Any, ~output=JsonString, ~mode=Async, ~typeValidation=false)
  t->assertCode(fn, `i=>{return Promise.resolve(JSON.stringify(i))}`)
})

test("TypeValidation=false works with assert output", t => {
  let schema = S.string
  let fn = schema->S.compile(~input=Any, ~output=Assert, ~mode=Sync, ~typeValidation=true)
  t->assertCode(fn, `i=>{if(typeof i!=="string"){e[0](i)}return void 0}`)
  let fn = schema->S.compile(~input=Any, ~output=Assert, ~mode=Sync, ~typeValidation=false)
  t->assertCode(fn, `i=>{return void 0}`)
})

test("Assert output with Async mode", t => {
  let schema = S.string
  let fn = schema->S.compile(~input=Any, ~output=Assert, ~mode=Async, ~typeValidation=true)
  t->assertCode(fn, `i=>{if(typeof i!=="string"){e[0](i)}return Promise.resolve(void 0)}`)
})
