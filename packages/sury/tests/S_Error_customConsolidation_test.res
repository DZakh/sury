open Vitest

// Custom is gone — paths that used to produce Custom now produce
// InvalidInput with the user-provided reason and a populated
// expected/received pair derived from the failing schema position.

let assertInvalidInput = (t, error: S.error, ~reason, ~expected, ~received) => {
  switch error->S.Error.classify {
  | InvalidInput({reason: r, expected: e, received: rcv}) =>
    t->Assert.is(r, reason, ~message="reason")
    t->Assert.is(e->S.inputExpression, expected, ~message="expected")
    t->Assert.is(rcv->S.inputExpression, received, ~message="received")
  | _ => t->Assert.fail("Expected InvalidInput error, got something else")
  }
}

test("errorMessage.type override produces InvalidInput with custom reason", t => {
  let schema = S.string->S.meta({errorMessage: {type_: "must be a string"}})
  switch 123->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    t->assertInvalidInput(
      error,
      ~reason="must be a string",
      ~expected="string",
      ~received="unknown",
    )
  }
})

test("errorMessage catch-all fallback override produces InvalidInput with custom reason", t => {
  let schema = S.string->S.meta({errorMessage: {catchAll: "anything wrong here"}})
  switch 123->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    t->assertInvalidInput(
      error,
      ~reason="anything wrong here",
      ~expected="string",
      ~received="unknown",
    )
  }
})

test("errorMessage.minLength override produces InvalidInput with custom reason", t => {
  let schema = S.string->S.minLength(3)->S.meta({errorMessage: {minLength: "too short"}})
  switch "hi"->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    t->assertInvalidInput(
      error,
      ~reason="too short",
      // Both sides of a bound failure carry the same schema, so the bound
      // renders on each — the user-facing message here is the custom reason.
      ~expected="string.length >= 3",
      ~received="string.length >= 3",
    )
  }
})

test("S.refine with ~error produces InvalidInput with custom reason", t => {
  let schema = S.int->S.refine(n => n > 0, ~error="must be positive")
  switch -1->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    t->assertInvalidInput(error, ~reason="must be positive", ~expected="int32", ~received="int32")
  }
})

test("S.refine with ~error and ~path applies path correctly", t => {
  let schema = S.string->S.refine(_ => false, ~error="bad", ~path=S.Path.fromArray(["a", "b"]))
  switch "hi"->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({reason, path}) =>
      t->Assert.is(reason, "bad", ~message="reason")
      t->Assert.is(path->S.Path.toText, "a.b", ~message="path")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("S.refine ~path takes an array index as a number segment", t => {
  let schema = S.string->S.refine(_ => false, ~error="bad", ~path=[String("items"), Number(0.)])
  switch "hi"->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({path}) =>
      t->Assert.deepEqual(path, [String("items"), Number(0.)], ~message="segments keep their kind")
      t->Assert.is(path->S.Path.toText, "items[0]", ~message="path")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("S.transform parser ctx.fail produces InvalidInput with custom reason", t => {
  let schema =
    S.string->S.to(
      S.any,
      ~custom={decode: Sync(str => str === "" ? U.fail("empty not allowed") : str), encode: Never},
    )
  switch ""->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({reason}) => t->Assert.is(reason, "empty not allowed", ~message="reason")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("S.transform serializer ctx.fail produces InvalidInput with custom reason", t => {
  let schema = S.string->S.to(
    S.any,
    ~custom={
      decode: Sync(str => str),
      encode: Sync(str => str === "" ? U.fail("empty not allowed") : str),
    },
  )
  switch ""->S.decodeOrThrow(~from=schema, ~to=S.unknown) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({reason}) => t->Assert.is(reason, "empty not allowed", ~message="reason")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("ctx.fail with ~path is concatenated to current location", t => {
  let schema = S.object(s =>
    s.field(
      "field",
      S.string->S.to(
        S.any,
        ~custom={
          decode: Sync(_ => U.fail("oops", ~path=S.Path.fromArray(["nested"]))),
          encode: Never,
        },
      ),
    )
  )
  switch {"field": "x"}->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({reason, path}) =>
      t->Assert.is(reason, "oops", ~message="reason")
      t->Assert.is(path->S.Path.toText, "field.nested", ~message="path")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("error.code is invalid_input for every consolidated path", t => {
  let assertCode = (schema, input) =>
    switch input->S.parseOrThrow(~to=schema) {
    | _ => t->Assert.fail("Should have thrown")
    | exception S.Exn(error) =>
      t->Assert.is((error->Obj.magic)["code"], "invalid_input", ~message="code")
    }

  assertCode(S.string->S.meta({errorMessage: {type_: "x"}}), 1->Obj.magic)
  assertCode(S.string->S.minLength(2)->S.meta({errorMessage: {minLength: "x"}}), "a"->Obj.magic)
  assertCode(S.int->S.refine(_ => false, ~error="x"), 1->Obj.magic)
  assertCode(
    S.string->S.to(S.any, ~custom={decode: Sync(_ => U.fail("x")), encode: Never}),
    "anything"->Obj.magic,
  )
})
