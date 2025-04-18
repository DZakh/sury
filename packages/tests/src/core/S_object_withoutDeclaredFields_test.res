open Ava

test("Successfully parses empty object", t => {
  let schema = S.object(_ => ())

  t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(schema), (), ())
})

test("Successfully parses object with excess keys", t => {
  let schema = S.object(_ => ())

  t->Assert.deepEqual(%raw(`{field:"bar"}`)->S.parseOrThrow(schema), (), ())
})

test("Successfully parses empty object when UnknownKeys are strict", t => {
  let schema = S.object(_ => ())->S.strict

  t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(schema), (), ())
})

test("Fails to parse object with excess keys when UnknownKeys are strict", t => {
  let schema = S.object(_ => ())->S.strict

  t->U.assertThrows(
    () => %raw(`{field:"bar"}`)->S.parseOrThrow(schema),
    {code: ExcessField("field"), operation: Parse, path: S.Path.empty},
  )
})

test("Successfully parses object with excess keys and returns transformed value", t => {
  let transformedValue = {"bas": true}
  let schema = S.object(_ => transformedValue)

  t->Assert.deepEqual(%raw(`{field:"bar"}`)->S.parseOrThrow(schema), transformedValue, ())
})

test("Successfully serializes transformed value to empty object", t => {
  let transformedValue = {"bas": true}
  let schema = S.object(_ => transformedValue)

  t->Assert.deepEqual(transformedValue->S.reverseConvertOrThrow(schema), %raw("{}"), ())
})

test("Allows to pass array to object value", t => {
  let schema = S.object(_ => ())

  t->Assert.deepEqual(%raw(`[]`)->S.parseOrThrow(schema), (), ())
})
