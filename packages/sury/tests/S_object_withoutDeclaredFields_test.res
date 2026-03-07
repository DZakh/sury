open Ava

test("Successfully parses empty object", t => {
  let schema = S.object(_ => ())

  t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(schema), ())
})

test("Successfully parses object with excess keys", t => {
  let schema = S.object(_ => ())

  t->Assert.deepEqual(%raw(`{field:"bar"}`)->S.parseOrThrow(schema), ())
})

test("Successfully parses empty object when UnknownKeys are strict", t => {
  let schema = S.object(_ => ())->S.strict

  t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(schema), ())
})

test("Fails to parse object with excess keys when UnknownKeys are strict", t => {
  let schema = S.object(_ => ())->S.strict

  t->U.assertThrowsMessage(
    () => %raw(`{field:"bar"}`)->S.parseOrThrow(schema),
    `Unrecognized key "field"`,
  )
})

test("Successfully parses object with excess keys and returns transformed value", t => {
  // FIXME: S.object mutatate `let transformedValue = {"bas": true}`
  let schema = S.object(_ => {"bas": true})

  t->Assert.deepEqual(%raw(`{field:"bar"}`)->S.parseOrThrow(schema), {"bas": true})
})

test("Successfully serializes transformed value to empty object", t => {
  let transformedValue = {"bas": true}
  let schema = S.object(_ => transformedValue)

  t->Assert.deepEqual(transformedValue->S.reverseConvertOrThrow(schema), %raw("{}"))
})

test("Allows to pass array to object value", t => {
  let schema = S.object(_ => ())

  t->Assert.deepEqual(%raw(`[]`)->S.parseOrThrow(schema), ())
})
