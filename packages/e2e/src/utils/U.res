open Vitest

// Read the real toString() instead of hardcoding it, so coverage/instrumentation
// tooling that rewrites function source (and would otherwise change the literal
// output) can't desync this from the actual generated code.
let noopOpCode: string = (S.compileConvertOrThrow(~from=S.unknown, ~to=S.unknown)->Obj.magic)["toString"]()

external magic: 'a => 'b = "%identity"
external castAnyToUnknown: 'any => unknown = "%identity"
external castUnknownToAny: unknown => 'any = "%identity"

let throwError = (error: S.error) => throw(error->Obj.magic)

%%private(
  @val @scope("JSON")
  external unsafeStringify: 'a => string = "stringify"
)

let unsafeGetVariantPayload = variant => (variant->Obj.magic)["_0"]

exception Test
let throwTestException = () => throw(Test)

let assertThrowsTestException = {
  (t, fn, ~message=?) => {
    try {
      let _ = fn()
      t->Assert.fail("Didn't throw")
    } catch {
    | Test => t->Assert.pass(~message?)
    | _ => t->Assert.fail("Thrown another exception")
    }
  }
}

let assertThrows = (t, cb, errorPayload) => {
  switch cb() {
  | any => t->Assert.fail("Asserted result is not Error. Recieved: " ++ any->unsafeStringify)
  | exception S.Exn({message}) => t->Assert.is(message, S.Error.make(errorPayload).message)
  }
}

let assertThrowsMessage = (t, cb, errorMessage, ~message=?) => {
  switch cb() {
  | any =>
    t->Assert.fail(
      `Asserted result is not S.Exn "${errorMessage}". Instead got: ${any->unsafeStringify}`,
    )
  | exception S.Exn({message: actualErrorMessage}) =>
    t->Assert.is(actualErrorMessage, errorMessage, ~message?)
  }
}

let assertThrowsAsync = async (t, cb, errorPayload) => {
  switch await cb() {
  | any => t->Assert.fail("Asserted result is not Error. Recieved: " ++ any->unsafeStringify)
  | exception S.Exn({message}) => t->Assert.is(message, S.Error.make(errorPayload).message)
  }
}

let getCompiledCodeString = (
  schema,
  ~op: [
    | #Parse
    | #Parse
    | #ParseAsync
    | #Convert
    | #ConvertAsync
    | #EncodeAsync
    | #Encode
    | #ReverseParse
    | #Assert
    | #EncodeToJson
  ],
) => {
  let toFn = schema =>
    switch op {
    | #Parse =>
      let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=schema)
      fn->magic
    | #ParseAsync =>
      let fn = S.compileConvertAsyncOrThrow(~from=S.unknown, ~to=schema)
      fn->magic
    | #Convert =>
      let fn = S.compileConvertOrThrow(~from=schema->S.reverse, ~to=S.unknown)
      fn->magic
    | #ConvertAsync =>
      let fn = S.compileConvertAsyncOrThrow(~from=schema->S.reverse, ~to=S.unknown)
      fn->magic
    | #Assert =>
      let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=schema->S.to(S.literal()->S.noValidation(true)))
      fn->magic
    | #ReverseParse => {
        let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=schema->S.reverse)
        fn->magic
      }
    | #Encode => {
        let fn = S.compileConvertOrThrow(~from=schema, ~to=S.unknown)
        fn->magic
      }
    | #EncodeAsync => {
        let fn = S.compileConvertAsyncOrThrow(~from=schema, ~to=S.unknown)
        fn->magic
      }
    | #EncodeToJson => {
        let fn = S.compileConvertOrThrow(~from=schema, ~to=S.json)
        fn->magic
      }
    }

  let fn = schema->toFn
  let code = ref(fn["toString"]())

  switch (schema->S.untag).defs {
  | Some(defs) if code.contents !== noopOpCode =>
    defs->Dict.forEachWithKey((schema, key) =>
      try {
        let defFn = schema->toFn
        code := code.contents ++ "\n" ++ `${key}: ${defFn["toString"]()}`
      } catch {
      | _exn => ()
      }
    )
  | _ => ()
  }

  code.contents
}

let rec cleanUpSchema = schema => {
  let new = Dict.make()
  schema
  ->(magic: S.t<'a> => Dict.t<unknown>)
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    switch key {
    | "output"
    | "isAsync"
    | "hasTransform"
    | "seq" => ()
    // ditemToItem leftovers FIXME:
    | "k" | "p" | "of" | "r" => ()
    | _ =>
      if typeof(value) === #function {
        ()
      } else if typeof(value) === #object && value !== %raw(`null`) {
        new->Dict.set(
          key,
          cleanUpSchema(value->(magic: unknown => S.t<'a>))->(magic: S.t<'a> => unknown),
        )
      } else {
        new->Dict.set(key, value)
      }
    }
  })
  new->(magic: Dict.t<unknown> => S.t<'a>)
}

let unsafeAssertEqualSchemas = (t, s1: S.t<'v1>, s2: S.t<'v2>, ~message=?) => {
  t->Assert.unsafeDeepEqual(s1->cleanUpSchema, s2->cleanUpSchema, ~message?)
}

let assertCompiledCode = (t, ~schema, ~op, code, ~message=?) => {
  t->Assert.is(schema->getCompiledCodeString(~op), code, ~message?)
}

let assertCompiledCodeIsNoop = (t, ~schema, ~op, ~message=?) => {
  t->assertCompiledCode(~schema, ~op, noopOpCode, ~message?)
}

let assertEqualSchemas: (
  Vitest.ExecutionContext.t<'a>,
  S.t<'value>,
  S.t<'value>,
  ~message: string=?,
) => unit = unsafeAssertEqualSchemas

let assertReverseParsesBack = (t, schema: S.t<'value>, value: 'value) => {
  t->Assert.unsafeDeepEqual(
    value
    ->S.convertOrThrow(~from=schema, ~to=S.unknown)
    ->S.parseOrThrow(~to=schema),
    value,
  )
}

let assertReverseReversesBack = (t, schema: S.t<'value>) => {
  t->assertEqualSchemas(schema->S.castToUnknown, schema->S.reverse->S.reverse)
}
