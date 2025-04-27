open Ava

test("Arrayable", t => {
  t->Assert.deepEqual(
    JSONSchema.Arrayable.array([1, 2])->JSONSchema.Arrayable.classify,
    JSONSchema.Arrayable.Array([1, 2]),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.Arrayable.single(1)->JSONSchema.Arrayable.classify,
    JSONSchema.Arrayable.Single(1),
    (),
  )
})

test("Definition", t => {
  t->Assert.deepEqual(
    JSONSchema.Definition.schema({title: "foo"})->JSONSchema.Definition.classify,
    JSONSchema.Definition.Schema({title: "foo"}),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.Definition.boolean(true)->JSONSchema.Definition.classify,
    JSONSchema.Definition.Boolean(true),
    (),
  )
})

test("Dependency", t => {
  t->Assert.deepEqual(
    JSONSchema.Dependency.schema({title: "foo"})->JSONSchema.Dependency.classify,
    JSONSchema.Dependency.Schema({title: "foo"}),
    (),
  )
  t->Assert.deepEqual(
    JSONSchema.Dependency.required(["field1", "field2"])->JSONSchema.Dependency.classify,
    JSONSchema.Dependency.Required(["field1", "field2"]),
    (),
  )
})
