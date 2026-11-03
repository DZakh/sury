open Vitest

// Read the real toString() instead of hardcoding it, so coverage/instrumentation
// tooling that rewrites function source (and would otherwise change the literal
// output) can't desync this from the actual generated code.
let noopOpCode: string = (
  S.compileConvertOrThrow(~from=S.unknown, ~to=S.unknown)->Obj.magic
)["toString"]()

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
      let fn = S.compileConvertAsPromiseOrReject(~from=S.unknown, ~to=schema)
      fn->magic
    | #Convert =>
      let fn = S.compileConvertOrThrow(~from=schema->S.reverse, ~to=S.unknown)
      fn->magic
    | #ConvertAsync =>
      let fn = S.compileConvertAsPromiseOrReject(~from=schema->S.reverse, ~to=S.unknown)
      fn->magic
    | #Assert =>
      let fn = S.compileConvertOrThrow(
        ~from=S.unknown,
        ~to=schema->S.to(S.literal()->S.noValidation(true)),
      )
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

// What Serde does with JSON, per ppx test type: packages/serde-compat mirrors
// each one as the Rust type Serde would read the same JSON into, and writes its
// verdicts here. `output` is what Serde writes back for the value it read,
// `error` why it refused the text, `gap` a disagreement Sury is known to have.
type serdeCase = {input: string, output?: string, error?: string, gap?: string}
type serdeMirror = {cases?: array<serdeCase>, skipped?: string}
@module("../../../serde-compat/fixtures.json")
external serdeFixtures: dict<dict<serdeMirror>> = "default"

// toEqual would let -0 pass for 0, which Serde tells apart.
let rec jsonEqual = (a: JSON.t, b: JSON.t) =>
  switch (a, b) {
  | (Number(a), Number(b)) => Object.is(a, b)
  | (String(a), String(b)) => a === b
  | (Boolean(a), Boolean(b)) => a === b
  | (Null, Null) => true
  | (Array(a), Array(b)) =>
    a->Array.length === b->Array.length &&
      a->Array.everyWithIndex((item, idx) => jsonEqual(item, b->Array.getUnsafe(idx)))
  | (Object(a), Object(b)) =>
    let keys = a->Dict.keysToArray
    keys->Array.length === b->Dict.keysToArray->Array.length &&
      keys->Array.every(key =>
        switch b->Dict.get(key) {
        | Some(value) => jsonEqual(a->Dict.getUnsafe(key), value)
        | None => false
        }
      )
  | _ => false
  }

type jsonVerdict = Wrote(JSON.t) | WroteUndefined | Refused(string)

let describeVerdict = verdict =>
  switch verdict {
  | Wrote(json) => `writes ${json->JSON.stringify}`
  | WroteUndefined => "writes undefined, which isn't JSON"
  | Refused(reason) => `refuses it (${reason})`
  }

// What the README does with JSON: parse the value, convert it back to unknown
// and stringify that. The written text is parsed again rather than compared as
// a value, so whatever JSON.stringify loses on the wire is lost here too.
let suryJsonVerdict = (schema: S.t<unknown>, input) =>
  switch input->JSON.parseOrThrow->S.parseOrThrow(~to=schema) {
  | value =>
    switch value->S.convertOrThrow(~from=schema, ~to=S.unknown)->JSON.stringifyAny {
    | Some(text) => Wrote(text->JSON.parseOrThrow)
    | None => WroteUndefined
    }
  | exception S.Exn(error) => Refused(error.message)
  }

let assertSerdeCase = (t, schema, case: serdeCase, ~label) => {
  let serde = switch (case.output, case.error) {
  | (Some(output), _) => Wrote(output->JSON.parseOrThrow)
  | (None, Some(error)) => Refused(error)
  | (None, None) => JsError.throwWithMessage(`${label} ${case.input}: a case without a verdict`)
  }
  let sury = suryJsonVerdict(schema, case.input)
  let agree = switch (serde, sury) {
  | (Wrote(a), Wrote(b)) => jsonEqual(a, b)
  | (Refused(_), Refused(_)) => true
  | _ => false
  }
  switch (case.gap, agree) {
  | (None, true) | (Some(_), false) => ()
  | (None, false) =>
    t->Assert.fail(
      `${label} ${case.input}: Serde ${serde->describeVerdict}, Sury ${sury->describeVerdict}`,
    )
  | (Some(gap), true) =>
    t->Assert.fail(
      `${label} ${case.input}: Sury now agrees with Serde, closing the gap "${gap}". Move the case out of \`diverges\` in packages/serde-compat.`,
    )
  }
}

// One test per schema, plus one holding the list to the mirror's: a type the
// mirror skips has to be left out here too.
let assertSerdeCompatible = (group, schemas: array<(string, S.t<unknown>)>) => {
  let mirrors = switch serdeFixtures->Dict.get(group) {
  | Some(mirrors) => mirrors
  | None => JsError.throwWithMessage(`packages/serde-compat/fixtures.json has no group "${group}"`)
  }
  test(`Every ${group} type has a Serde mirror and every mirror a schema`, t => {
    let mirrored =
      mirrors
      ->Dict.toArray
      ->Array.filterMap(((name, mirror)) => mirror.cases->Option.map(_ => name))
      ->Array.toSorted(String.compare)
    let tested = schemas->Array.map(((name, _)) => name)->Array.toSorted(String.compare)
    t->Assert.deepEqual(tested, mirrored)
  })
  schemas->Array.forEach(((name, schema)) =>
    test(`${group}.${name} reads and writes JSON the way Serde does`, t => {
      let label = `${group}.${name}`
      switch mirrors->Dict.get(name) {
      | Some({cases}) => cases->Array.forEach(case => t->assertSerdeCase(schema, case, ~label))
      | _ => t->Assert.fail(`packages/serde-compat has no cases for ${label}`)
      }
    })
  )
}
