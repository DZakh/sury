open Vitest

let schema = S.string->S.to(S.float)

test("parse returns a result", t => {
  t->Assert.deepEqual(%raw(`"1.5"`)->S.parse(~to=schema), Ok(1.5))
  switch %raw(`1`)->S.parse(~to=schema) {
  | Ok(_) => t->Assert.fail("Expected Error")
  | Error(error) => t->Assert.is(error.message, `Expected string, received 1`)
  }
})

test("compileParse returns a result", t => {
  let fn = S.compileParse(~to=S.string)
  t->Assert.deepEqual(fn("abc"), Ok("abc"))
  t->Assert.deepEqual(fn(%raw(`1`))->Result.isError, true)
})

asyncTest("parseAsync returns a promise of a result", async t => {
  let asyncSchema =
    S.string->S.to(S.float, ~custom={decode: Async(s => Promise.resolve(Float.parseFloat(s))), encode: Auto})
  t->Assert.deepEqual(await "2.5"->S.parseAsync(~to=asyncSchema), Ok(2.5))
  t->Assert.deepEqual((await %raw(`1`)->S.parseAsync(~to=asyncSchema))->Result.isError, true)
  t->Assert.deepEqual((await S.compileParseAsync(~to=asyncSchema)("3"))->Result.isOk, true)
})

test("convert returns a result", t => {
  t->Assert.deepEqual(`"a"`->S.convert(~from=S.jsonString, ~via=S.json, ~to=S.string), Ok("a"))
  t->Assert.deepEqual(1.5->S.convert(~from=schema, ~to=S.string), Ok("1.5"))
  t->Assert.deepEqual(`1`->S.convert(~from=S.jsonString, ~to=S.string)->Result.isError, true)
  t->Assert.deepEqual(S.compileConvert(~from=schema, ~to=S.string)(2.), Ok("2"))
})

asyncTest("convertAsync returns a promise of a result", async t => {
  t->Assert.deepEqual(await 1.5->S.convertAsync(~from=schema, ~to=S.string), Ok("1.5"))
  t->Assert.deepEqual(await S.compileConvertAsync(~from=schema, ~to=S.string)(2.), Ok("2"))
})

test("make returns a result", t => {
  t->Assert.deepEqual(1.->S.make(~schema), Ok(1.))
  t->Assert.deepEqual(%raw(`"1"`)->S.make(~schema)->Result.isError, true)
  t->Assert.deepEqual(S.compileMake(~schema)(3.), Ok(3.))
})

asyncTest("makeAsync returns a promise of a result", async t => {
  t->Assert.deepEqual(await 1.->S.makeAsync(~schema), Ok(1.))
  t->Assert.deepEqual(await S.compileMakeAsync(~schema)(2.), Ok(2.))
})

test("A non-Sury exception is not turned into Error", t => {
  let throwing = S.string->S.refine(_ => throw(Not_found))
  t->Assert.throws(() => "x"->S.parse(~to=throwing))
})
