open Ava

// The hack to bypass wallaby adding tags
// and turning the function into:
// function noopOperation(i) {␊ 
//     var $_$c = $_$wf(3);␊ 
//     return $_$w(3, 444, $_$c), i;␊ 
// }
let noopOpCode: string = (S.makeConvertOrThrow(S.unknown, S.unknown)->Obj.magic)["toString"]()

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

let asyncAssertThrowsMessage = async (t, cb, errorMessage, ~message=?) => {
  switch await cb() {
  | any =>
    t->Assert.fail(
      `Asserted result is not S.Exn "${errorMessage}". Instead got: ${any->unsafeStringify}`,
    )
  | exception S.Exn({message: actualErrorMessage}) =>
    t->Assert.is(actualErrorMessage, errorMessage, ~message?)
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
    | #ReverseConvertAsync
    | #ReverseConvert
    | #ReverseParse
    | #Assert
    | #ReverseConvertToJson
  ],
) => {
  let toCode = schema =>
    (
      switch op {
      | #Parse =>
        let fn = S.makeConvertOrThrow(S.unknown, schema)
        fn->magic
      | #ParseAsync =>
        let fn = S.makeAsyncConvertOrThrow(S.unknown, schema)
        fn->magic
      | #Convert =>
        let fn = S.makeConvertOrThrow(schema->S.reverse, S.unknown)
        fn->magic
      | #ConvertAsync =>
        let fn = S.makeAsyncConvertOrThrow(schema->S.reverse, S.unknown)
        fn->magic
      | #Assert =>
        let fn = S.makeConvertOrThrow(S.unknown, schema->S.to(S.literal()->S.noValidation(true)))
        fn->magic
      | #ReverseParse => {
          let fn = S.makeConvertOrThrow(S.unknown, schema->S.reverse)
          fn->magic
        }
      | #ReverseConvert => {
          let fn = S.makeConvertOrThrow(schema, S.unknown)
          fn->magic
        }
      | #ReverseConvertAsync => {
          let fn = S.makeAsyncConvertOrThrow(schema, S.unknown)
          fn->magic
        }
      | #ReverseConvertToJson => {
          let fn = S.makeConvertOrThrow(schema, S.json)
          fn->magic
        }
      }
    )["toString"]()

  let code = ref(schema->toCode)

  switch (schema->S.untag).defs {
  | Some(defs) if code.contents !== noopOpCode =>
    defs->Dict.forEachWithKey((schema, key) =>
      try {
        let defCode =
          schema
          ->toCode
          ->String.replaceAll(
            `${(S.unknown->S.untag).seq->Float.toString}-${(schema->S.untag).seq->Float.toString}`,
            `unknown->${key}`,
          )

        code := code.contents ++ "\n" ++ `${key}: ${defCode}`
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
  Ava.ExecutionContext.t<'a>,
  S.t<'value>,
  S.t<'value>,
  ~message: string=?,
) => unit = unsafeAssertEqualSchemas

let assertReverseParsesBack = (t, schema: S.t<'value>, value: 'value) => {
  t->Assert.unsafeDeepEqual(
    value
    ->S.reverseConvertOrThrow(schema)
    ->S.parseOrThrow(schema),
    value,
  )
}

let assertReverseReversesBack = (t, schema: S.t<'value>) => {
  t->assertEqualSchemas(schema->S.castToUnknown, schema->S.reverse->S.reverse)
}
