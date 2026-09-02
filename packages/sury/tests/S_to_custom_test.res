open Vitest

// Custom codecs on S.to: the ReScript surface of what used to be S.transform.
// The coders land on the target's output side, so their results are trusted
// (see the toOutput slots in S.res).

test("Parses with a custom decode to the same type and a validating Auto encode", t => {
  let schema = S.string->S.to(S.string, ~custom={decode: Sync(String.trim), encode: Auto})

  t->Assert.deepEqual("  Hello world!"->S.parseOrThrow(~to=schema), "Hello world!")
  // Auto encode is the built-in string -> string validating pass-through.
  t->Assert.deepEqual(
    "Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"Hello world!"`),
  )
})

test("Parses with a custom decode to another type", t => {
  let schema = S.int->S.to(S.float, ~custom={decode: Sync(Int.toFloat), encode: Never})

  t->Assert.deepEqual(123->S.parseOrThrow(~to=schema), 123.)
})

asyncTest("Parses with an async decode to another type", async t => {
  let schema = S.int->S.to(
    S.float,
    ~custom={
      decode: Async(value => Promise.resolve()->Promise.thenResolve(() => value->Int.toFloat)),
      encode: Never,
    },
  )

  t->Assert.deepEqual(await 123->S.parseAsyncOrThrow(~to=schema), 123.)
})

test("A never decode rejects the parse operation at creation", t => {
  let schema = S.string->S.to(S.any, ~custom={decode: Never, encode: Sync(value => value)})

  // The target converting on its own is the case a reading exists for, so the
  // output-seam guard has to let one past: it places no coder to claim a result.
  let carried = S.uint8Array->S.to(
    S.jsonString->S.to(S.string),
    ~custom={decode: Unpack, encode: Pack},
  )
  t->Assert.deepEqual(
    %raw(`new TextEncoder().encode('"hi"')`)->S.parseOrThrow(~to=carried),
    "hi",
  )

  t->U.assertThrowsMessage(
    () => "Hello world!"->S.parseOrThrow(~to=schema),
    `Can't decode string to unknown. The conversion is marked as never`,
  )
})

test("A never encode rejects the encode operation at creation", t => {
  let schema = S.string->S.to(S.any, ~custom={decode: Sync(value => value), encode: Never})

  t->Assert.deepEqual("Hello world!"->S.parseOrThrow(~to=schema), %raw(`"Hello world!"`))
  t->U.assertThrowsMessage(
    () => "Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Can't decode unknown to string. The conversion is marked as never`,
  )
})

test("Fails to parse when user throws error in a custom decode", t => {
  let schema =
    S.string->S.to(S.any, ~custom={decode: Sync(_ => U.fail("User error")), encode: Never})

  t->U.assertThrowsMessage(() => "Hello world!"->S.parseOrThrow(~to=schema), `User error`)
})

test("Uses the path from S.Error.throw called in the custom decode", t => {
  let schema = S.array(
    S.string->S.to(
      S.any,
      ~custom={
        decode: Sync(
          _ =>
            U.throwError(
              S.Error.make(
                InvalidInput({
                  reason: "User error",
                  path: S.Path.fromArray(["a", "b"]),
                  expected: S.unknown,
                  received: S.unknown,
                }),
              ),
            ),
        ),
        encode: Never,
      },
    ),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.parseOrThrow(~to=schema),
    `Failed at [0].a.b: User error`,
  )
})

test("Uses the path from S.Error.throw called in the custom encode", t => {
  let schema = S.array(
    S.string->S.to(
      S.any,
      ~custom={
        decode: Never,
        encode: Sync(
          _ =>
            U.throwError(
              S.Error.make(
                InvalidInput({
                  reason: "User error",
                  path: S.Path.fromArray(["a", "b"]),
                  expected: S.unknown,
                  received: S.unknown,
                }),
              ),
            ),
        ),
      },
    ),
  )

  t->U.assertThrowsMessage(
    () => ["Hello world!"]->S.decodeOrThrow(~from=schema, ~to=S.json),
    `Failed at [0].a.b: User error`,
  )
})

test("All errors thrown in operation context are caught and wrapped in SuryError", t => {
  let jsError = JsError.make("Application crashed")
  let schema = S.array(
    S.string->S.to(S.any, ~custom={decode: Sync(_ => JsError.throw(jsError)), encode: Never}),
  )

  t->U.assertThrowsMessage(
    () => {["Hello world!"]->S.parseOrThrow(~to=schema)},
    `Failed at [0]: Application crashed`,
  )
  switch ["Hello world!"]->S.parseOrThrow(~to=schema) {
  | _ => t->Assert.fail("Didn't throw")
  | exception S.Exn(error) =>
    switch error->S.Error.classify {
    | InvalidConversion({cause}) => t->Assert.is(cause->Obj.magic, jsError)
    | _ => t->Assert.fail("Thrown another exception")
    }
  }
})

test("Operation context catches ReScript exceptions as they are", t => {
  let schema = S.array(
    S.string->S.to(S.any, ~custom={decode: Sync(_ => U.throwTestException()), encode: Never}),
  )

  t->U.assertThrowsMessage(
    () => {["Hello world!"]->S.parseOrThrow(~to=schema)},
    `Failed at [0]: { RE_EXN_ID: "U.Test"; Error: Error; }`,
  )
})

test("Successfully serializes with a custom encode to the same type", t => {
  let schema = S.string->S.to(S.any, ~custom={decode: Never, encode: Sync(String.trim)})

  t->Assert.deepEqual(
    "  Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"Hello world!"`),
  )
})

test("Successfully serializes with a custom encode to another type", t => {
  let schema =
    S.float->S.to(S.any, ~custom={decode: Never, encode: Sync(value => value->Int.toFloat)})

  t->Assert.deepEqual(123->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`123`))
})

test("Fails to serialize when user throws error in a custom encode", t => {
  let schema =
    S.string->S.to(S.any, ~custom={decode: Never, encode: Sync(_ => U.fail("User error"))})

  t->U.assertThrowsMessage(
    () => "Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `User error`,
  )
})

test("Custom decodes applied in the right order when parsing", t => {
  let schema =
    S.int
    ->S.to(S.any, ~custom={decode: Sync(_ => U.fail("First transform")), encode: Never})
    ->S.to(S.any, ~custom={decode: Sync(_ => U.fail("Second transform")), encode: Never})

  t->U.assertThrowsMessage(() => 123->S.parseOrThrow(~to=schema), `First transform`)
})

test("Custom encodes applied in the right order when serializing", t => {
  let schema =
    S.int
    ->S.to(S.any, ~custom={decode: Never, encode: Sync(_ => U.fail("First transform"))})
    ->S.to(S.any, ~custom={decode: Never, encode: Sync(_ => U.fail("Second transform"))})

  t->U.assertThrowsMessage(
    () => 123->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Second transform`,
  )
})

test("Successfully parses a custom codec pair and serializes back to the initial state", t => {
  let any = %raw(`123`)

  let schema =
    S.int->S.to(
      S.float,
      ~custom={decode: Sync(Int.toFloat), encode: Sync(value => value->Int.fromFloat)},
    )

  t->Assert.deepEqual(
    any->S.parseOrThrow(~to=schema)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    any,
  )
})

test("Fails to parse async decode using parseOrThrow", t => {
  let schema =
    S.string->S.to(S.any, ~custom={decode: Async(value => Promise.resolve(value)), encode: Never})

  t->U.assertThrowsMessage(
    () => %raw(`"Hello world!"`)->S.parseOrThrow(~to=schema),
    `Invalid async during sync operation`,
  )
})

test("Successfully parses with the codec-less S.to(S.any)", t => {
  let schema = S.string->S.to(S.any)

  t->Assert.deepEqual(%raw(`"Hello world!"`)->S.parseOrThrow(~to=schema), %raw(`"Hello world!"`))
})

test("Successfully serializes with the codec-less S.to(S.any)", t => {
  let schema = S.string->S.to(S.any)

  t->Assert.deepEqual(
    "Hello world!"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"Hello world!"`),
  )
})

asyncTest("Successfully parses async decode using parseAsyncOrThrow", t => {
  let schema =
    S.string->S.to(S.any, ~custom={decode: Async(value => Promise.resolve(value)), encode: Never})

  %raw(`"Hello world!"`)
  ->S.parseAsyncOrThrow(~to=schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, %raw(`"Hello world!"`))
  })
})

asyncTest("Fails to parse async decode with user error", t => {
  let schema =
    S.string->S.to(S.any, ~custom={decode: Async(_ => U.fail("User error")), encode: Never})

  t->U.asyncAssertThrowsMessage(
    () => %raw(`"Hello world!"`)->S.parseAsyncOrThrow(~to=schema),
    `User error`,
  )
})

asyncTest("An async encode compiles through the reversed chain", async t => {
  let schema = S.string->S.to(
    S.any,
    ~custom={
      decode: Sync(value => value),
      encode: Async(value => Promise.resolve(value)),
    },
  )

  // The forward direction stays sync-parseable.
  t->Assert.deepEqual(%raw(`"abc"`)->S.parseOrThrow(~to=schema), %raw(`"abc"`))
  // Async-ness is discovered by catching the sync operation's rejection —
  // there is no S.isAsync probe.
  t->U.assertThrowsMessage(
    () => "abc"->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Invalid async during sync operation`,
  )
  t->Assert.deepEqual(await "abc"->S.decodeAsyncOrThrow(~from=schema, ~to=S.unknown), %raw(`"abc"`))
})

asyncTest("Can apply other actions after async decode", t => {
  let schema =
    S.string
    ->S.to(S.any, ~custom={decode: Async(value => Promise.resolve(value)), encode: Never})
    ->S.to(S.string)
    ->S.trim
    ->S.to(S.any, ~custom={decode: Async(value => Promise.resolve(value)), encode: Never})

  %raw(`"    Hello world!"`)
  ->S.parseAsyncOrThrow(~to=schema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(result, %raw(`"Hello world!"`))
  })
})

test("Compiled parse code snapshot", t => {
  let schema =
    S.int->S.to(
      S.float,
      ~custom={decode: Sync(Int.toFloat), encode: Sync(value => value->Int.fromFloat)},
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="number"&&i<=2147483647&&i>=-2147483648&&i%1===0||e[2](i);let v0;try{v0=e[0](i)}catch(x){e[1](x)}return v0}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema = S.int->S.to(
    S.float,
    ~custom={
      decode: Async(int => int->Int.toFloat->Promise.resolve),
      encode: Sync(value => value->Int.fromFloat),
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{typeof i==="number"&&i<=2147483647&&i>=-2147483648&&i%1===0||e[2](i);let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}return v0}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema =
    S.int->S.to(
      S.float,
      ~custom={decode: Sync(Int.toFloat), encode: Sync(value => value->Int.fromFloat)},
    )

  // The coder's result is trusted on this seam, so there's no int32
  // re-validation like the old S.transform emitted.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}return v0}`,
  )
})

test("Compiled serialize code snapshot with two custom codecs", t => {
  let schema =
    S.string
    ->S.to(
      S.int,
      ~custom={
        decode: Sync(string => string->Int.fromString->Option.getOrThrow),
        encode: Sync(int => int->Int.toString),
      },
    )
    ->S.to(
      S.float,
      ~custom={
        decode: Sync(Int.toFloat),
        encode: Sync(float => float->Float.toInt),
      },
    )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}let v1;try{v1=e[2](v0)}catch(x){e[3](x)}return v1}`,
  )
})

test("Reverse schema to the original schema", t => {
  let schema = S.int->S.to(S.float, ~custom={decode: Sync(Int.toFloat), encode: Sync(Float.toInt)})
  t->U.assertReverseReversesBack(schema)
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.int->S.to(S.float, ~custom={decode: Sync(Int.toFloat), encode: Sync(Float.toInt)})
  t->U.assertReverseParsesBack(schema, 12.)
})

test("Fails to define a custom codec for a target that already converts", t => {
  let target = S.string->S.to(S.float)

  // `codecs<'from, 'to>` types the coder against the target's output, so there
  // is no name for the chain's input to feed. Chaining says it explicitly.
  t->Assert.throws(
    () => {
      let _ = S.int->S.to(target, ~custom={decode: Sync(Int.toFloat), encode: Sync(Float.toInt)})
    },
    ~expectations={
      message: `[Sury] The target already converts. Chain S.to instead of passing a custom codec`,
    },
  )
})

test("Refines the coder's result, not what went into it", t => {
  // Regression: the trusted seam left the target's refiners on the coder's own
  // val, and those emit at the pre-transform slot, so `S.uuid`'s pattern ran
  // over the user object instead of the id the coder returned.
  let userSchema = S.schema(s => {"id": s.matches(S.string), "name": s.matches(S.string)})
  let schema = S.uuid->S.to(
    userSchema,
    ~custom={
      decode: Sync(id => {"id": id, "name": "John"}),
      encode: Sync(user => user["id"]),
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}e[2].test(v0)||e[3](v0);return v0}`,
  )
  t->Assert.deepEqual(
    {"id": "6d8d3a9a-1e0a-4f6a-9a4a-0f2d3f4a5b6c", "name": "John"}->S.decodeOrThrow(
      ~from=schema,
      ~to=S.unknown,
    ),
    %raw(`"6d8d3a9a-1e0a-4f6a-9a4a-0f2d3f4a5b6c"`),
  )
  t->U.assertThrowsMessage(() =>
    {"id": "not-a-uuid", "name": "John"}
    ->S.decodeOrThrow(~from=schema, ~to=S.unknown)
    ->ignore
  , `Expected uuid, received "not-a-uuid"`)
})

test("Picks a reading for a content link the way the ambiguity report says to", t => {
  // The report names `"pack"`/`"unpack"`, so the binding has to offer them —
  // without Pack/Unpack the remedy it points at is unwritable from ReScript.
  let packed = S.base64->S.to(S.jsonString, ~custom={decode: Pack, encode: Unpack})
  t->Assert.deepEqual("aGk="->S.parseOrThrow(~to=packed), `"aGk="`)
  t->Assert.deepEqual(`"aGk="`->S.decodeOrThrow(~from=packed, ~to=S.base64), "aGk=")

  let opened = S.base64->S.to(S.jsonString, ~custom={decode: Unpack, encode: Pack})
  t->Assert.deepEqual(`eyJhIjoxfQ==`->S.parseOrThrow(~to=opened), `{"a":1}`)

  t->U.assertThrowsMessage(
    () => "aGk="->S.parseOrThrow(~to=S.base64->S.to(S.jsonString))->ignore,
    `Ambiguous conversion from base64 to JSON string. Use S.to(from, to, "unpack" | "pack")`,
  )
})
