open Vitest

// Custom is gone — paths that used to produce Custom now produce
// InvalidInput with the user-provided reason and a populated
// expected/received pair derived from the failing schema position.

let assertInvalidInput = (t, error: S.error, ~reason, ~expected, ~received) => {
  switch error->S.Error.classify {
  | InvalidInput({reason: r, expected: e, received: rcv}) =>
    t->Assert.is(r, reason, ~message="reason")
    t->Assert.is(e->S.toExpression, expected, ~message="expected")
    t->Assert.is(rcv->S.toExpression, received, ~message="received")
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
  let schema = S.string->S.min(3)->S.meta({errorMessage: {minLength: "too short"}})
  switch "hi"->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    t->assertInvalidInput(
      error,
      ~reason="too short",
      ~expected="string",
      ~received="string",
    )
  }
})

test("S.refine with ~error produces InvalidInput with custom reason", t => {
  let schema = S.int->S.refine(n => n > 0, ~error="must be positive")
  switch (-1)->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    t->assertInvalidInput(error, ~reason="must be positive", ~expected="int32", ~received="int32")
  }
})

test("S.refine with ~error and ~path applies path correctly", t => {
  let schema = S.string->S.refine(_ => false, ~error="bad", ~path=["a", "b"])
  switch "hi"->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Should have thrown")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidInput({reason, path}) =>
      t->Assert.is(reason, "bad", ~message="reason")
      t->Assert.is(path->S.Path.toString, `["a"]["b"]`, ~message="path")
    | _ => t->Assert.fail("Expected InvalidInput error")
    }
  }
})

test("S.transform parser ctx.fail produces InvalidInput with custom reason", t => {
  let schema = S.string->S.transform(s => {
    parser: str => str === "" ? s.fail("empty not allowed") : str,
  })
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
  let schema = S.string->S.transform(s => {
    parser: str => str,
    serializer: str => str === "" ? s.fail("empty not allowed") : str,
  })
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
      S.string->S.transform(
        s => {
          parser: _ => s.fail("oops", ~path=S.Path.fromArray(["nested"])),
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
      t->Assert.is(path->S.Path.toString, `["field"]["nested"]`, ~message="path")
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
  assertCode(S.string->S.min(2)->S.meta({errorMessage: {minLength: "x"}}), "a"->Obj.magic)
  assertCode(S.int->S.refine(_ => false, ~error="x"), 1->Obj.magic)
  assertCode(
    S.string->S.transform(s => {parser: _ => s.fail("x")}),
    "anything"->Obj.magic,
  )
})
