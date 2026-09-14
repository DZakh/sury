open Vitest

let schema = S.string->S.to(S.float)

test("parse returns a result", t => {
  t->Assert.deepEqual(%raw(`"1.5"`)->S.parseAsResult(~to=schema), Ok(1.5))
  switch %raw(`1`)->S.parseAsResult(~to=schema) {
  | Ok(_) => t->Assert.fail("Expected Error")
  | Error(error) => t->Assert.is(error.message, `Expected string, received 1`)
  }
})

asyncTest("parseAsync returns a promise of a result", async t => {
  let asyncSchema =
    S.string->S.to(S.float, ~custom={decode: Async(s => Promise.resolve(Float.parseFloat(s))), encode: Auto})
  t->Assert.deepEqual(await "2.5"->S.parseAsResultPromise(~to=asyncSchema), Ok(2.5))
  t->Assert.deepEqual((await %raw(`1`)->S.parseAsResultPromise(~to=asyncSchema))->Result.isError, true)
})

test("convert returns a result", t => {
  t->Assert.deepEqual(S.JsonString(`"a"`)->S.convertAsResult(~from=S.jsonString, ~via=S.json, ~to=S.string), Ok("a"))
  t->Assert.deepEqual(1.5->S.convertAsResult(~from=schema, ~to=S.string), Ok("1.5"))
  t->Assert.deepEqual(S.JsonString(`1`)->S.convertAsResult(~from=S.jsonString, ~to=S.string)->Result.isError, true)
})

asyncTest("convertAsync returns a promise of a result", async t => {
  t->Assert.deepEqual(await 1.5->S.convertAsResultPromise(~from=schema, ~to=S.string), Ok("1.5"))
})

test("make returns a result", t => {
  t->Assert.deepEqual(1.->S.makeAsResult(~schema=schema), Ok(1.))
  t->Assert.deepEqual(%raw(`"1"`)->S.makeAsResult(~schema=schema)->Result.isError, true)
})

asyncTest("makeAsync returns a promise of a result", async t => {
  t->Assert.deepEqual(await 1.->S.makeAsResultPromise(~schema=schema), Ok(1.))
})

test("A refine that throws is that refinement failing, so the exception comes back as an Error", t => {
  let throwing = S.string->S.refine(_ => throw(Not_found))
  switch "x"->S.parseAsResult(~to=throwing) {
  | Ok(_) => t->Assert.fail("Expected an Error")
  | Error(error) => t->Assert.is(error.reason->String.includes("Not_found"), true)
  }
})
