open Ava

S.enableEmail()

test("Catch-all _ overrides any constraint message", t => {
  let schema = S.email->S.meta({errorMessage: {catchAll: "Bad input"}})

  t->U.assertThrowsMessage(() => "invalid"->S.parseOrThrow(~to=schema), `Bad input`)
})

test("Specific key takes precedence over catch-all _", t => {
  let schema = S.email->S.meta({errorMessage: {catchAll: "Fallback", format: "Must be email"}})

  t->U.assertThrowsMessage(() => "invalid"->S.parseOrThrow(~to=schema), `Must be email`)
})

test("Empty errorMessage deletes the field from schema", t => {
  let schema = S.string->S.min(1)->S.meta({errorMessage: {}})

  switch schema {
  | String({?errorMessage}) => t->Assert.deepEqual(errorMessage, None)
  | _ => t->Assert.fail("Expected String")
  }
})

test("S.meta errorMessage overwrites, not merges", t => {
  let schema = S.string->S.min(1)->S.max(10)->S.meta({errorMessage: {minLength: "Custom min"}})

  switch schema {
  | String({errorMessage}) =>
    t->Assert.deepEqual(errorMessage, {minLength: "Custom min"})
  | _ => t->Assert.fail("Expected String")
  }
  // maxLength message is gone since we overwrote
  t->U.assertThrowsMessage(() => ""->S.parseOrThrow(~to=schema), `Custom min`)
})

test("Override constraint message via S.meta on string min", t => {
  let schema = S.string->S.min(5)->S.meta({errorMessage: {minLength: "Too short"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Too short`)
})

test("Override constraint message via S.meta on int max", t => {
  let schema = S.int->S.max(10)->S.meta({errorMessage: {maximum: "Number too large"}})

  t->U.assertThrowsMessage(() => 100->S.parseOrThrow(~to=schema), `Number too large`)
})

test("Override works on serialization path too", t => {
  let schema = S.email->S.meta({errorMessage: {format: "Bad email"}})

  t->U.assertThrowsMessage(
    () => "invalid"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Bad email`,
  )
})

test("Catch-all _ works on constraint refiners", t => {
  let schema = S.string->S.min(5)->S.meta({errorMessage: {catchAll: "Bad"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Bad`)
})

test("Override pattern message via S.meta", t => {
  let schema = S.string->S.pattern(~message="Original", /^\d+$/)->S.meta({errorMessage: {pattern: "Override"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Override`)
})

test("Override type error message via S.meta on S.string", t => {
  let schema = S.string->S.meta({errorMessage: {type_: "Must be a string"}})

  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `Must be a string`)
})

test("Override type error message via S.meta on S.int", t => {
  let schema = S.int->S.meta({errorMessage: {type_: "Must be an integer"}})

  t->U.assertThrowsMessage(() => "abc"->S.parseOrThrow(~to=schema), `Must be an integer`)
})

test("Catch-all _ is used for type error when no specific type_ is set", t => {
  let schema = S.string->S.meta({errorMessage: {catchAll: "Bad value"}})

  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `Bad value`)
})

test("S.meta does not mutate the original schema", t => {
  let original = S.string->S.min(1)
  let _ = original->S.meta({errorMessage: {minLength: "Custom"}})

  // Original still uses default message
  t->U.assertThrowsMessage(() => ""->S.parseOrThrow(~to=original), `String must be 1 or more characters long`)
})
