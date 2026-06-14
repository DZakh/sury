open Vitest

// Tests that InvalidInput.received reports the pre-narrowing schema,
// not the post-narrowing target. Without the fix, received === expected
// because B.refine sets val.schema = expected before checks run.

test("InvalidInput error has correct received schema for unknown-to-string type mismatch", t => {
  switch 123->S.parseOrThrow(~to=S.string) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, "string", ~message="expected schema")
      t->Assert.is(received->S.toExpression, "unknown", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error has correct received schema for unknown-to-float type mismatch", t => {
  switch "hello"->S.parseOrThrow(~to=S.float) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, "number", ~message="expected schema")
      t->Assert.is(received->S.toExpression, "unknown", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error has correct received schema for unknown-to-bool type mismatch", t => {
  switch 42->S.parseOrThrow(~to=S.bool) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, "boolean", ~message="expected schema")
      t->Assert.is(received->S.toExpression, "unknown", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error has correct received schema for unknown-to-object type mismatch", t => {
  switch "not an object"->S.parseOrThrow(~to=S.object(s => s.field("x", S.string))) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected: _, received}) =>
      t->Assert.is(received->S.toExpression, "unknown", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error received differs from expected (not equal)", t => {
  switch true->S.parseOrThrow(~to=S.string) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.notDeepEqual(
        expected->S.toExpression,
        received->S.toExpression,
        ~message="received should differ from expected",
      )
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error has correct received schema for nested field type mismatch", t => {
  let schema = S.object(s => s.field("age", S.float))
  switch {"age": true}->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, "number", ~message="expected schema")
      t->Assert.is(received->S.toExpression, "unknown", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

// Cases where received is a specific type (not unknown)

test("InvalidInput error reports number as received when float fails int32 format check", t => {
  switch 1.5->S.decodeOrThrow(~from=S.float, ~to=S.int) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, "int32", ~message="expected schema")
      t->Assert.is(received->S.toExpression, "number", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error reports string as received when string-to-number coercion produces NaN", t => {
  switch "abc"->S.decodeOrThrow(~from=S.string, ~to=S.float) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, "number", ~message="expected schema")
      t->Assert.is(received->S.toExpression, "string", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("InvalidInput error reports string as received when string doesn't match literal", t => {
  switch "wrong"->S.decodeOrThrow(~from=S.string, ~to=S.literal("apple")) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({expected, received}) =>
      t->Assert.is(expected->S.toExpression, `"apple"`, ~message="expected schema")
      t->Assert.is(received->S.toExpression, "string", ~message="received schema")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})
