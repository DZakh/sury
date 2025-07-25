open Ava
open RescriptCore

// The hack to bypass wallaby adding tags
// and turning the function into:
// function noopOperation(i) {␊ 
//     var $_$c = $_$wf(3);␊ 
//     return $_$w(3, 444, $_$c), i;␊ 
// }
let noopOpCode = (
  S.unknown->S.compile(~input=Any, ~output=Unknown, ~mode=Sync, ~typeValidation=false)->Obj.magic
)["toString"]()

external magic: 'a => 'b = "%identity"
external castAnyToUnknown: 'any => unknown = "%identity"
external castUnknownToAny: unknown => 'any = "%identity"

let raiseError = (error: S.error) => raise(error->Obj.magic)

%%private(
  @val @scope("JSON")
  external unsafeStringify: 'a => string = "stringify"
)

let unsafeGetVariantPayload = variant => (variant->Obj.magic)["_0"]

exception Test
let raiseTestException = () => raise(Test)

type taggedFlag =
  | Parse
  | ParseAsync
  | ReverseConvertToJson
  | ReverseParse
  | ReverseConvert
  | Assert

type errorPayload = {operation: taggedFlag, code: S.errorCode, path: S.Path.t}

// TODO: Get rid of the helper
let error = ({operation, code, path}: errorPayload): S.error => {
  S.ErrorClass.constructor(
    ~code,
    ~flag=switch operation {
    | Parse => S.Flag.typeValidation
    | ReverseParse => S.Flag.reverse->S.Flag.with(S.Flag.typeValidation)
    | ReverseConvertToJson => S.Flag.reverse->S.Flag.with(S.Flag.jsonableOutput)
    | ReverseConvert => S.Flag.reverse
    | ParseAsync => S.Flag.typeValidation->S.Flag.with(S.Flag.async)
    | Assert => S.Flag.typeValidation->S.Flag.with(S.Flag.assertOutput)
    },
    ~path,
  )
}

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
  | exception S.Error({message}) => t->Assert.is(message, error(errorPayload).message)
  }
}

let assertThrowsMessage = (t, cb, errorMessage, ~message=?) => {
  switch cb() {
  | any =>
    t->Assert.fail(
      `Asserted result is not S.Error "${errorMessage}". Instead got: ${any->unsafeStringify}`,
    )
  | exception S.Error({message: actualErrorMessage}) =>
    t->Assert.is(actualErrorMessage, errorMessage, ~message?)
  }
}

let assertThrowsAsync = async (t, cb, errorPayload) => {
  switch await cb() {
  | any => t->Assert.fail("Asserted result is not Error. Recieved: " ++ any->unsafeStringify)
  | exception S.Error({message}) => t->Assert.is(message, error(errorPayload).message)
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
        let fn = schema->S.compile(~input=Any, ~output=Value, ~mode=Sync, ~typeValidation=true)
        fn->magic
      | #ParseAsync =>
        let fn = schema->S.compile(~input=Any, ~output=Value, ~mode=Async, ~typeValidation=true)
        fn->magic
      | #Convert =>
        let fn = schema->S.compile(~input=Any, ~output=Value, ~mode=Sync, ~typeValidation=false)
        fn->magic
      | #ConvertAsync =>
        let fn = schema->S.compile(~input=Any, ~output=Value, ~mode=Async, ~typeValidation=false)
        fn->magic
      | #Assert =>
        let fn = schema->S.compile(~input=Any, ~output=Assert, ~mode=Sync, ~typeValidation=true)
        fn->magic
      | #ReverseParse => {
          let fn =
            schema->S.compile(~input=Value, ~output=Unknown, ~mode=Sync, ~typeValidation=true)
          fn->magic
        }
      | #ReverseConvert => {
          let fn =
            schema->S.compile(~input=Value, ~output=Unknown, ~mode=Sync, ~typeValidation=false)
          fn->magic
        }
      | #ReverseConvertAsync => {
          let fn =
            schema->S.compile(~input=Value, ~output=Unknown, ~mode=Async, ~typeValidation=false)
          fn->magic
        }
      | #ReverseConvertToJson => {
          let fn = schema->S.compile(~input=Value, ~output=Json, ~mode=Sync, ~typeValidation=false)
          fn->magic
        }
      }
    )["toString"]()

  let code = ref(schema->toCode)

  switch (schema->S.untag).defs {
  | Some(defs) =>
    defs->Dict.forEachWithKey((schema, key) =>
      try {
        code := code.contents ++ "\n" ++ `${key}: ${schema->toCode}`
      } catch {
      | exn => Console.error(exn)
      }
    )
  | None => ()
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
    | "isAsync" => ()
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
