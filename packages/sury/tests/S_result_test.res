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

let signup = S.object(s =>
  {
    "name": s.field("name", S.string),
    "age": s.field("age", S.int),
  }
)

test("parseAsStandardResult reports every issue", t => {
  t->Assert.deepEqual(
    (%raw(`{name: 1, age: "x"}`)->S.parseAsStandardResult(~to=signup)).issues,
    Some([
      {message: "Expected string, received 1", path: [String("name")]},
      {message: `Expected int32, received "x"`, path: [String("age")]},
    ]),
  )
  t->Assert.deepEqual(
    (%raw(`{name: "a", age: 1}`)->S.parseAsStandardResult(~to=signup)).value,
    Some({"name": "a", "age": 1}),
  )
})

asyncTest("parseAsStandardResultPromise reports every issue", async t => {
  let result = await %raw(`{name: 1, age: "x"}`)->S.parseAsStandardResultPromise(~to=signup)
  t->Assert.is(result.issues->Option.map(Array.length), Some(2))
})

test("convertAsStandardResult and makeAsStandardResult answer the standard shape", t => {
  t->Assert.deepEqual((1.5->S.convertAsStandardResult(~from=schema, ~to=S.string)).value, Some("1.5"))
  t->Assert.is(
    (S.JsonString(`1`)->S.convertAsStandardResult(~from=S.jsonString, ~to=S.string)).issues->Option.isSome,
    true,
  )
  t->Assert.deepEqual(S.compileMakeAsStandardResult(~schema)(1.).value, Some(1.))
  t->Assert.is((%raw(`"1"`)->S.makeAsStandardResult(~schema)).issues->Option.isSome, true)
})
