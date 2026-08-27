open Vitest

test("Uses default value when parsing optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`undefined`)

  let schema = S.float->S.option->S.Option.getOr(value)

  t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
})

test("Uses default value when nullable optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`null`)

  let schema = S.float->S.nullAsOption->S.Option.getOr(value)

  t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
})

test("Successfully parses with default when provided JS undefined", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), false)
})

test("Successfully parses with default when provided primitive", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), true)
})

test("Successfully serializes nested option with default value", t => {
  let schema = S.option(
    S.option(S.option(S.option(S.option(S.option(S.bool)))->S.Option.getOr(Some(Some(true))))),
  )

  // Every outer Some-level nested-none marker encodes back to the undefined
  // it was parsed from; the default arm itself is never-encode, so it yields
  // instead of failing the whole operation like the old S.transform wiring.
  t->Assert.deepEqual(
    Some(Some(Some(Some(None))))->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`undefined`),
  )
  t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
})

test("Fails to parse data with default", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertThrowsMessage(
    () => %raw(`"string"`)->S.parseOrThrow(~to=schema),
    `Expected undefined | boolean, received "string"`,
  )
})

test("Successfully parses schema with transformation", t => {
  let schema =
    S.option(S.float)
    ->S.Option.getOr(-123.)
    ->S.to(
      S.any,
      ~custom={
        decode: Sync(
          number =>
            if number > 0. {
              Some("positive")
            } else {
              None
            },
        ),
        encode: Never,
      },
    )
    ->S.to(S.option(S.string))
    ->S.Option.getOr("not positive")

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), "not positive")
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(i===void 0){i=-123;break;}if(typeof i==="number"&&i===i)break;e[0](i)}let v0;try{v0=e[1](i)}catch(x){e[2](x)}for(;;){if(v0===void 0){v0="not positive";break;}if(typeof v0==="string")break;e[3](v0)}return v0}`,
  )
})

test("Successfully serializes schema with transformation", t => {
  let schema = S.string->S.trim->S.option->S.Option.getOr("default")

  t->Assert.deepEqual(" abc"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"abc"`))
})

test("Compiled parse code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(i===void 0){i=false;break;}if(typeof i==="boolean")break;e[0](i)}return i}`,
  )
})

asyncTest("Compiled async parse code snapshot", async t => {
  let schema =
    S.option(
      S.bool->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never}),
    )->S.Option.getOr(false)

  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(~to=schema), false)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{for(;;){if(i===void 0){i=false;break;}if(typeof i==="boolean"){let v0=e[0](i);i=v0;break;}e[1](i)}return Promise.resolve(i)}`,
  )

  let schema =
    S.option(S.bool)
    ->S.Option.getOr(false)
    ->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never})

  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(~to=schema), false)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{for(;;){if(i===void 0){i=false;break;}if(typeof i==="boolean")break;e[0](i)}let v0;try{v0=e[1](i).catch(x=>e[2](x))}catch(x){e[2](x)}return v0}`,
  )
})

// https://github.com/DZakh/sury/issues/178
test("Uses default value when parsing optional union of literals", t => {
  let schema =
    S.union([S.literal("a"), S.literal("b"), S.literal("c")])->S.option->S.Option.getOr("a")

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), "a")
  t->Assert.deepEqual(%raw(`"b"`)->S.parseOrThrow(~to=schema), "b")
  t->Assert.deepEqual(%raw(`"c"`)->S.parseOrThrow(~to=schema), "c")
})

// https://github.com/DZakh/sury/issues/178
test("Fails to parse invalid value for optional union of literals with default", t => {
  let schema =
    S.union([S.literal("a"), S.literal("b"), S.literal("c")])->S.option->S.Option.getOr("a")

  t->U.assertThrowsMessage(
    () => %raw(`"d"`)->S.parseOrThrow(~to=schema),
    `Expected undefined | "a" | "b" | "c", received "d"`,
  )
})

// https://github.com/DZakh/sury/issues/178
test("Successfully serializes optional union of literals with default", t => {
  let schema =
    S.union([S.literal("a"), S.literal("b"), S.literal("c")])->S.option->S.Option.getOr("a")

  t->Assert.deepEqual("b"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"b"`))
})

test("Rejects invalid static default at schema construction", t => {
  t->Assert.throws(
    () => {
      let _ = S.bool->S.option->S.Option.getOr(%raw(`"not a bool"`))
    },
    ~expectations={
      message: `[Sury] Invalid default for undefined | boolean: Expected boolean, received "not a bool"`,
    },
  )
})

test("Uses empty array as default", t => {
  let schema = S.array(S.string)->S.option->S.Option.getOr([])

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), [])
  t->Assert.deepEqual(%raw(`["a","b"]`)->S.parseOrThrow(~to=schema), ["a", "b"])
})

test("Uses non-empty array as default", t => {
  let schema = S.array(S.string)->S.option->S.Option.getOr(["x", "y"])

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), ["x", "y"])
  t->Assert.deepEqual(%raw(`["a"]`)->S.parseOrThrow(~to=schema), ["a"])
})

test("Rejects array default whose element type doesn't match", t => {
  t->Assert.throws(
    () => {
      let _ = S.array(S.string)->S.option->S.Option.getOr(%raw(`[42]`))
    },
    ~expectations={
      message: `[Sury] Invalid default for undefined | string[]: Failed at ["0"]: Expected string, received 42`,
    },
  )
})

test("Uses object default with all required fields", t => {
  let schema =
    S.schema(s => {"a": s.matches(S.string), "b": s.matches(S.float)})
    ->S.option
    ->S.Option.getOr({"a": "hi", "b": 1.})

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), {"a": "hi", "b": 1.})
  t->Assert.deepEqual(%raw(`{"a":"x","b":2}`)->S.parseOrThrow(~to=schema), {"a": "x", "b": 2.})
})

test("Rejects object default with field of wrong type", t => {
  t->Assert.throws(
    () => {
      let _ =
        S.schema(s => {"a": s.matches(S.string)})
        ->S.option
        ->S.Option.getOr(%raw(`{"a":42}`))
    },
    ~expectations={
      message: `[Sury] Invalid default for undefined | { a: string; }: Failed at ["a"]: Expected string, received 42`,
    },
  )
})

test("Rejects object default with missing required field", t => {
  t->Assert.throws(
    () => {
      let _ = S.schema(s => {"a": s.matches(S.string)})->S.option->S.Option.getOr(%raw(`{}`))
    },
    ~expectations={
      message: `[Sury] Invalid default for undefined | { a: string; }: Failed at ["a"]: Expected string, received undefined`,
    },
  )
})

test("Uses dict default", t => {
  let schema = S.dict(S.float)->S.option->S.Option.getOr(Dict.fromArray([("x", 1.), ("y", 2.)]))

  t->Assert.deepEqual(
    %raw(`undefined`)->S.parseOrThrow(~to=schema),
    Dict.fromArray([("x", 1.), ("y", 2.)]),
  )
})

test("Rejects invalid static default that doesn't match a union member", t => {
  t->Assert.throws(
    () => {
      let _ =
        S.union([S.literal("a"), S.literal("b"), S.literal("c")])
        ->S.option
        ->S.Option.getOr(%raw(`"d"`))
    },
    ~expectations={
      message: `[Sury] Invalid default for undefined | "a" | "b" | "c": Expected "a" | "b" | "c", received "d"`,
    },
  )
})

test("Default on a primary item with S.to runs the transformation on parse and reverse", t => {
  let defaultDate = Date.fromString("2024-01-01T00:00:00.000Z")
  let otherDate = Date.fromString("2024-06-15T12:30:45.123Z")
  let schema = S.string->S.to(S.date)->S.option->S.Option.getOr(defaultDate)

  // schema.default is the input form (ISO string), not the Date — JSON Schema metadata.
  let untagged = schema->S.untag
  t->Assert.is(untagged.tag, S.AnyOf)
  t->Assert.is(untagged.anyOf->Option.getOrThrow->Array.length, 2)
  t->Assert.deepEqual(untagged.default, %raw(`"2024-01-01T00:00:00.000Z"`))
  // The default arm carries the conversion — the union itself has no `.to`.
  t->Assert.is(untagged.to, None)

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), defaultDate)
  t->Assert.deepEqual("2024-06-15T12:30:45.123Z"->S.parseOrThrow(~to=schema), otherDate)

  t->Assert.deepEqual(
    defaultDate->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"2024-01-01T00:00:00.000Z"`),
  )
  t->Assert.deepEqual(
    otherDate->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"2024-06-15T12:30:45.123Z"`),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(i===void 0){i=e[0];break;}if(typeof i==="string"){let v0=new Date(i);!Number.isNaN(v0.getTime())||e[1](v0);i=v0;break;}e[2](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i instanceof e[0]){i=i.toISOString()}else{e[1](i)}return i}`,
  )
})

// .to(jsonString) extends the .to chain rather than replacing getWithDefault's wiring.
test("Appending S.to(S.jsonString) after getOr extends the output chain", t => {
  let defaultDate = Date.fromString("2024-01-01T00:00:00.000Z")
  let schema = S.string->S.to(S.date)->S.option->S.Option.getOr(defaultDate)->S.to(S.jsonString)

  let untagged = schema->S.untag
  t->Assert.is(untagged.tag, S.AnyOf)
  t->Assert.deepEqual(untagged.default, %raw(`"2024-01-01T00:00:00.000Z"`))
  let toLevel1 = untagged.to->Option.getOrThrow->S.untag
  t->Assert.is(toLevel1.tag, S.String)

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), `"2024-01-01T00:00:00.000Z"`)
  t->Assert.deepEqual(
    "2024-06-15T12:30:45.123Z"->S.parseOrThrow(~to=schema),
    `"2024-06-15T12:30:45.123Z"`,
  )

  t->Assert.deepEqual(
    `"2024-01-01T00:00:00.000Z"`->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"2024-01-01T00:00:00.000Z"`),
  )
})

// getOr hands jsonString a bare ternary, and `+` binds tighter than `?:`, so
// splicing it between quotes unparenthesized reassociated into
// `("\""+i)===void 0?…` — which dropped the opening quote on BOTH branches and
// went unnoticed because no test paired a default with a quoted primitive.
// Spelled here rather than as a spec: `spec new` can't evaluate `$Option_getOr`.
test("getOr default reaches jsonString quoted, not reassociated", t => {
  let schema = S.string->S.to(S.bigint)->S.option->S.Option.getOr(7n)->S.to(S.jsonString)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(i===void 0){i="\\""+7n+"\\"";break;}if(typeof i==="string"){let v0;try{v0=BigInt(i)}catch(_){e[0](i)}i="\\""+v0+"\\"";break;}e[1](i)}return i}`,
  )

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), `"7"`)
  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), `"123"`)
})

// A multi-member transforming union + getOr. Each string-coercing branch
// declares its conversion var (`let v0 = +i` / `let v1 = BigInt(i)`) inside the
// try block that owns the branch's type check, so a string input dispatches
// per-branch without ever reading a var before its declaration (the previous
// codegen emitted `if(v0===v0)` above `let v0 = +i`).
test("Multi-member union with transformed members + getOr", t => {
  let schema =
    S.union([
      S.string->S.to(S.float)->S.castToUnknown,
      S.string->S.to(S.bigint)->S.castToUnknown,
      S.bool->S.castToUnknown,
    ])
    ->S.option
    ->S.Option.getOr(%raw(`true`))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), %raw(`true`))
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), %raw(`true`))
  t->Assert.deepEqual(%raw(`false`)->S.parseOrThrow(~to=schema), %raw(`false`))
  // Parsing a string used to throw ReferenceError (v0 read before declaration).
  t->Assert.deepEqual("42"->S.parseOrThrow(~to=schema), %raw(`42`))

  t->Assert.deepEqual(%raw(`42`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"42"`))
  t->Assert.deepEqual(%raw(`1n`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"1"`))
  t->Assert.deepEqual(%raw(`true`)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){let r;if(i===void 0){i=true;break;}if(typeof i==="string"){try{let v0=+i;v0===v0||e[0](i);i=v0;break}catch(x){(r||(r=[])).push(e[2](x))}try{let v1;try{v1=BigInt(i)}catch(_){e[1](i)}i=v1;break}catch(x){(r||(r=[])).push(e[2](x))}}if(typeof i==="boolean")break;e[3](i,...(r||[]))}return i}`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="number"&&i===i){i=""+i;break;}if(typeof i==="bigint"){i=""+i;break;}if(typeof i==="boolean")break;e[0](i)}return i}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  // The reversed union validates the value like any other typed decode — the
  // old noop relied on Option_getWithDefault's noopDecoder hack.
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{typeof i==="boolean"||e[0](i);return i}`)
})
