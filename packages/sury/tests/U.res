open Vitest

// Read the real toString() instead of hardcoding it, so coverage/instrumentation
// tooling that rewrites function source (and would otherwise change the literal
// output) can't desync this from the actual generated code.
let noopOpCode: string = (S.compileConvertOrThrow(~from=S.unknown, ~to=S.unknown)->Obj.magic)["toString"]()

external magic: 'a => 'b = "%identity"
external castAnyToUnknown: 'any => unknown = "%identity"
external castUnknownToAny: unknown => 'any = "%identity"

let throwError = (error: S.error) => throw(error->Obj.magic)

// Stands in for the removed transform effect ctx's `fail`. A transform now
// fails by throwing, and this is that throw with the boilerplate named once -
// the same shape a caller writes, kept here so the tests read as assertions
// rather than as error construction.
let fail = (message, ~path=S.Path.empty) =>
  S.Error.throw(
    S.Error.make(
      InvalidInput({
        reason: message,
        path,
        expected: S.unknown,
        received: S.unknown,
      }),
    ),
  )

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

// The operation a def compiled to, read off the def's own memo (OpNode in
// parse.ts: `c` is the node list, `a` the schema arguments, `v` the function).
// A def is only ever compiled nested - cold, it has no `$defs` to resolve - so
// the node is the one place its code exists. Newest first, so the most recent
// operation's nested node answers.
let defOperationCode: S.t<'a> => option<string> = %raw(`(def) => {
  for (let node = def.c; node; node = node.n) {
    if (node.a.length === 2 && node.a[1] === def && node.v) return node.v.toString()
  }
}`)

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
  ~embedded=?,
) => {
  let toFn = schema =>
    switch op {
    | #Parse =>
      let fn = S.compileConvertOrThrow(~from=S.unknown, ~to=schema)
      fn->magic
    | #ParseAsync =>
      let fn = S.compileConvertAsPromiseOrReject(~from=S.unknown, ~to=schema)
      fn->magic
    | #Convert =>
      let fn = S.compileConvertOrThrow(~from=schema->S.reverse, ~to=S.unknown)
      fn->magic
    | #ConvertAsync =>
      let fn = S.compileConvertAsPromiseOrReject(~from=schema->S.reverse, ~to=S.unknown)
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
        let fn = S.compileConvertAsPromiseOrReject(~from=schema, ~to=S.unknown)
        fn->magic
      }
    | #EncodeToJson => {
        let fn = S.compileConvertOrThrow(~from=schema, ~to=S.json)
        fn->magic
      }
    }

  let fn = schema->toFn
  let code = ref(fn["toString"]())

  switch embedded {
  | Some(embedded) =>
    embedded->Array.forEach(((name, index)) => {
      code := code.contents ++ "\n" ++ `${name}: ${fn["embedded"]->Array.getUnsafe(index)}`
    })
  | None =>
    switch (schema->S.untag).defs {
    | Some(defs) if code.contents !== noopOpCode =>
      defs->Dict.forEachWithKey((schema, key) =>
        switch schema->defOperationCode {
        | Some(defCode) => code := code.contents ++ "\n" ++ `${key}: ${defCode}`
        | None => ()
        }
      )
    | _ => ()
    }
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
    | "ia"
    | "ht"
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

let assertCompiledCode = (t, ~schema, ~op, code, ~embedded=?, ~message=?) => {
  t->Assert.is(schema->getCompiledCodeString(~op, ~embedded?), code, ~message?)
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
