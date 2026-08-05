open Vitest

test("Works", t => {
  t->Assert.deepEqual(S.Path.fromLocation("123"), S.Path.fromArray(["123"]))
})

test("Keeps a path-like location a single segment", t => {
  t->Assert.deepEqual(S.Path.fromLocation(`["1"]["2"]`)->S.Path.toText, `["[\\"1\\"][\\"2\\"]"]`)
})
