open Ava

type objectWithOneField = {key: string}

test("Successfully parses Object with unknown keys by default", t => {
  let any = %raw(`{key: "value", unknownKey: "value2"}`)

  let schema = S.object(s => s.field("key", S.string))

  t->Assert.deepEqual(any->S.parseOrThrow(schema), "value")
})

test("Fails fast and shows only one excees key in the error message", t => {
  let schema = S.object(s =>
    {
      "key": s.field("key", S.string),
    }
  )->S.strict

  t->U.assertThrows(
    () =>
      %raw(`{key: "value", unknownKey: "value2", unknownKey2: "value2"}`)->S.parseOrThrow(schema),
    {code: ExcessField("unknownKey"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully parses Object with unknown keys when Strip strategy applyed", t => {
  let value = "value"
  let any = %raw(`{key: "value", unknownKey: "value2"}`)

  let schema = S.object(s => s.field("key", S.string))->S.strip

  t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
})

test("Works correctly when the same unknown keys strategy applyed multiple times", t => {
  let value = "value"
  let any = %raw(`{key: "value", unknownKey: "value2"}`)

  let schema = S.object(s => s.field("key", S.string))->S.strip->S.strip->S.strip

  t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
})

test("Doesn't raise an error when unknown keys strategy applyed to a non Object schema", t => {
  t->Assert.notThrows(() => {
    S.string->S.strip->ignore
  })
  t->Assert.notThrows(() => {
    S.string->S.strict->ignore
  })
})

test("Can reset unknown keys strategy applying Strict strategy", t => {
  let any = %raw(`{key: "value", unknownKey: "value2"}`)

  let schema = S.object(s => s.field("key", S.string))->S.strip->S.strict

  t->U.assertThrows(
    () => any->S.parseOrThrow(schema),
    {code: ExcessField("unknownKey"), operation: Parse, path: S.Path.empty},
  )
})

test("Ignores additional items override for S.array and S.dict", t => {
  let schema = S.array(S.string)
  t->Assert.is(schema->S.strict, schema)
  let schema = S.dict(S.string)
  t->Assert.is(schema->S.strict, schema)
  let schema = S.array(S.string)->S.strict
  t->Assert.is(schema->S.strip, schema)
})
