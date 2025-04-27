open Ava

test("Works with empty", t => {
  t->Assert.deepEqual(S.Path.fromArray([]), S.Path.empty, ())
})

test("Works with single location", t => {
  t->Assert.deepEqual(["123"]->S.Path.fromArray, S.Path.fromLocation(`123`), ())
})

test("Works with nested location", t => {
  t->Assert.deepEqual(
    ["1", "2"]->S.Path.fromArray,
    S.Path.fromLocation(`1`)->S.Path.concat(S.Path.fromLocation("2")),
    (),
  )
})

test("Works with path like location", t => {
  let pathLikeLocation = S.Path.fromArray(["1", "2"])->S.Path.toString
  t->Assert.deepEqual(
    S.Path.fromArray([pathLikeLocation]),
    S.Path.fromLocation(pathLikeLocation),
    (),
  )
})
