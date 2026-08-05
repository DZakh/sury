open Vitest

test("Returns false for schema with NoOperation", t => {
  t->Assert.is(S.unknown->S.isAsync, false)
})

test("Returns false for sync schema", t => {
  t->Assert.is(S.string->S.isAsync, false)
})

test("Returns true for async schema", t => {
  let schema = S.string->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never})

  t->Assert.is(schema->S.isAsync, true)
})

test("Returns true for async schema after running a serializer", t => {
  let schema =
    S.string->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Sync(i => i)})
  t->Assert.deepEqual("abc"->S.decodeOrThrow(~from=schema, ~to=S.json), %raw(`"abc"`))
  t->Assert.is(schema->S.isAsync, true)
})

test("Returns true for schema with nested async", t => {
  let schema = S.tuple1(S.string->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never}))

  t->Assert.is(schema->S.isAsync, true)
})
