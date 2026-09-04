open Vitest

test("Works with empty", t => {
  t->Assert.deepEqual(S.Path.fromArray([]), S.Path.empty)
})

test("Keeps locations as-is", t => {
  t->Assert.deepEqual(S.Path.fromArray(["1", "my key"]), [String("1"), String("my key")])
})
