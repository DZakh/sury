open Ava

test("Arrayable", t => {
  t->Assert.deepEqual(
    JSONSchema.Arrayable.array([1, 2])->JSONSchema.Arrayable.classify,
    JSONSchema.Arrayable.Array([1, 2]),
  )
  t->Assert.deepEqual(
    JSONSchema.Arrayable.single(1)->JSONSchema.Arrayable.classify,
    JSONSchema.Arrayable.Single(1),
  )
})

test("Definition", t => {
  t->Assert.deepEqual(JSONSchema.Schema({title: "foo"}), %raw(`{title: "foo"}`))
  t->Assert.deepEqual(JSONSchema.Never, %raw(`false`))
  t->Assert.deepEqual(JSONSchema.Any, %raw(`true`))
})

test("Dependency", t => {
  t->Assert.deepEqual(JSONSchema.RequiredSchema({title: "foo"}), %raw(`{title: "foo"}`))
  t->Assert.deepEqual(
    JSONSchema.RequiredProperties(["field1", "field2"]),
    %raw(`["field1", "field2"]`),
  )
})
