open Ava

test("Returns false for schema with NoOperation", t => {
  t->Assert.is(S.unknown->S.isAsync, false, ())
})

test("Returns false for sync schema", t => {
  t->Assert.is(S.string->S.isAsync, false, ())
})

test("Returns true for async schema", t => {
  let schema = S.string->S.transform(_ => {asyncParser: i => Promise.resolve(i)})

  t->Assert.is(schema->S.isAsync, true, ())
})

test("Returns true for async schema after running a serializer", t => {
  let schema =
    S.string->S.transform(_ => {asyncParser: i => Promise.resolve(i), serializer: i => i})
  t->Assert.deepEqual("abc"->S.reverseConvertToJsonOrThrow(schema), %raw(`"abc"`), ())
  t->Assert.is(schema->S.isAsync, true, ())
})

test("Returns true for schema with nested async", t => {
  let schema = S.tuple1(S.string->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))

  t->Assert.is(schema->S.isAsync, true, ())
})
