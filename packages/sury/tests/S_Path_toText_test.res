open Vitest

test("Renders empty path as empty text", t => {
  t->Assert.deepEqual(S.Path.empty->S.Path.toText, "")
})

test("Renders identifier-safe locations with dots", t => {
  t->Assert.deepEqual(S.Path.fromArray(["user", "name"])->S.Path.toText, "user.name")
})

test("Renders numeric locations with brackets", t => {
  t->Assert.deepEqual(S.Path.fromArray(["items", "2", "id"])->S.Path.toText, "items[2].id")
})

test("Renders non-identifier locations with quoted brackets", t => {
  t->Assert.deepEqual(S.Path.fromArray(["my key", "b"])->S.Path.toText, `["my key"].b`)
})

test("Renders dynamic marker verbatim", t => {
  t->Assert.deepEqual(
    S.Path.fromArray(["[]", "id"])->S.Path.toText,
    "[].id",
  )
})

test("Escapes quotes in locations", t => {
  t->Assert.deepEqual(S.Path.fromArray([`"123"`])->S.Path.toText, `["\\"123\\""]`)
})
