open Ava

test("Works with empty", t => {
  t->Assert.deepEqual(S.Path.empty->S.Path.toArray, [], ())
})

test("Works with single location", t => {
  t->Assert.deepEqual(S.Path.fromLocation(`123`)->S.Path.toArray, ["123"], ())
})

test("Works with nested location", t => {
  t->Assert.deepEqual(S.Path.fromArray(["1", "2", "3"])->S.Path.toArray, ["1", "2", "3"], ())
})

test("Works with path like location", t => {
  let pathLikeLocation = S.Path.fromArray(["1", "2"])->S.Path.toString
  t->Assert.deepEqual(S.Path.fromLocation(pathLikeLocation)->S.Path.toArray, [pathLikeLocation], ())
})
