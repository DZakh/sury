open Ava

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
  t->Assert.throws(
    () => {
      let schema = S.option(
        S.option(S.option(S.option(S.option(S.option(S.bool)))->S.Option.getOr(Some(Some(true))))),
      )

      t->Assert.deepEqual(
        Some(Some(Some(Some(None))))->S.decodeOrThrow(~from=schema, ~to=S.unknown),
        Some(Some(Some(Some(None))))->Obj.magic,
      )
      // FIXME: I'm not sure this is correct
      t->Assert.deepEqual(Some(None)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
      t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
    },
    ~expectations={
      message: `Expected boolean | { BS_PRIVATE_NESTED_SOME_NONE: 0; } | { BS_PRIVATE_NESTED_SOME_NONE: 1; }, received { BS_PRIVATE_NESTED_SOME_NONE: 3; }`,
    },
  )
})

test("Fails to parse data with default", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertThrowsMessage(
    () => %raw(`"string"`)->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined, received "string"`,
  )
})

test("Successfully parses schema with transformation", t => {
  let schema =
    S.option(S.float)
    ->S.Option.getOr(-123.)
    ->S.transform(_ => {
      parser: number =>
        if number > 0. {
          Some("positive")
        } else {
          None
        },
    })
    ->S.to(S.option(S.string))
    ->S.Option.getOr("not positive")

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), "not positive")
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="number"&&!Number.isNaN(i)||i===void 0)){e[0](i)}let v0;try{v0=e[1](i===void 0?-123:i)}catch(x){e[2](x)}if(!(typeof v0==="string"||v0===void 0)){e[3](v0)}return v0===void 0?"not positive":v0}`,
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
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i===void 0?false:i}`,
  )
})

asyncTest("Compiled async parse code snapshot", async t => {
  let schema =
    S.option(S.bool->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))->S.Option.getOr(
      false,
    )

  t->Assert.deepEqual(schema->S.isAsync, true)
  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(~to=schema), false)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{let v1;if(typeof i==="boolean"){let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}i=v0}else if(!(i===void 0)){e[2](i)}v1=Promise.resolve(i);return v1.then(v1=>{return v1===void 0?false:v1})}`,
  )

  let schema =
    S.option(S.bool)
    ->S.Option.getOr(false)
    ->S.transform(_ => {asyncParser: i => Promise.resolve(i)})

  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(~to=schema), false)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}let v0;try{v0=e[1](i===void 0?false:i).catch(x=>e[2](x))}catch(x){e[2](x)}return v0}`,
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
    `Expected "a" | "b" | "c" | undefined, received "d"`,
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
      message: `[Sury] Invalid default for boolean | undefined: Expected boolean, received "not a bool"`,
    },
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
      message: `[Sury] Invalid default for "a" | "b" | "c" | undefined: Expected "a" | "b" | "c", received "d"`,
    },
  )
})

test("Default on a primary item with S.to runs the transformation on parse and reverse", t => {
  let defaultDate = Date.fromString("2024-01-01T00:00:00.000Z")
  let otherDate = Date.fromString("2024-06-15T12:30:45.123Z")
  let schema = S.string->S.to(S.date)->S.option->S.Option.getOr(defaultDate)

  // Shape: the result is a union with two members — the primary item
  // (string -> Date via S.to) and the unit (undefined) literal. The schema
  // also carries:
  //  - `default`: the input form of the user-provided Date (ISO string),
  //    obtained by running the Date through item->reverse.
  //  - `to`: a copy of the primary item's output schema (Date) with its
  //    decoder swapped for noopDecoder — getWithDefault's substitution
  //    happens via the schema's `parser`, so the `.to` chain just passes
  //    the value through and applies the serializer in reverse.
  let untagged = schema->S.untag
  t->Assert.is(untagged.tag, S.Union)
  t->Assert.is(untagged.anyOf->Option.getOrThrow->Array.length, 2)
  t->Assert.deepEqual(untagged.default, %raw(`"2024-01-01T00:00:00.000Z"`))
  t->Assert.is((untagged.to->Option.getOrThrow->S.untag).tag, S.Instance)

  // Parse: undefined falls through to the default Date; a valid ISO string is
  // transformed string -> Date via the primary item's S.to chain.
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), defaultDate)
  t->Assert.deepEqual("2024-06-15T12:30:45.123Z"->S.parseOrThrow(~to=schema), otherDate)

  // Encode: Date output gets reversed back to the ISO string input form.
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
    `i=>{if(typeof i==="string"){let v0=new Date(i);!Number.isNaN(v0.getTime())||e[0](v0);i=v0}else if(!(i===void 0)){e[1](i)}return i===void 0?e[2]:i}`,
  )
  // The output of getOr is `Date` (no longer optional), so the encoder skips
  // both the undefined branch and the Date type check — it trusts the output
  // contract and goes straight to toISOString().
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return i.toISOString()}`)
})

test("Appending S.to(S.jsonString) after getOr extends the output chain", t => {
  let defaultDate = Date.fromString("2024-01-01T00:00:00.000Z")
  let schema = S.string->S.to(S.date)->S.option->S.Option.getOr(defaultDate)->S.to(S.jsonString)

  // Shape: appending `.to(S.jsonString)` doesn't replace anything in
  // getWithDefault's wiring — it extends the `to` chain. The top-level union
  // (string | undefined) and its `default`/`parser` are unchanged; the
  // existing noopDecoder-wrapped Date schema now has its own `.to` pointing
  // at the JSON-string format.
  let untagged = schema->S.untag
  t->Assert.is(untagged.tag, S.Union)
  t->Assert.deepEqual(untagged.default, %raw(`"2024-01-01T00:00:00.000Z"`))
  let toLevel1 = untagged.to->Option.getOrThrow->S.untag
  t->Assert.is(toLevel1.tag, S.Instance) // copied Date schema
  let toLevel2 = toLevel1.to->Option.getOrThrow->S.untag
  t->Assert.is(toLevel2.tag, S.String) // jsonString is a String with json format

  // Behavior: parse undefined -> default Date -> serialised to JSON string;
  // parse a valid ISO string -> Date -> serialised to JSON string.
  t->Assert.deepEqual(
    %raw(`undefined`)->S.parseOrThrow(~to=schema),
    `"2024-01-01T00:00:00.000Z"`,
  )
  t->Assert.deepEqual(
    "2024-06-15T12:30:45.123Z"->S.parseOrThrow(~to=schema),
    `"2024-06-15T12:30:45.123Z"`,
  )

  // Encode the JSON-string output back to the raw ISO string input.
  t->Assert.deepEqual(
    `"2024-01-01T00:00:00.000Z"`->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"2024-01-01T00:00:00.000Z"`),
  )
})

// FIXME: getWithDefault's `itemOutputSchema = item->getOutputSchema`
// (Sury.res:4364) does not correctly model the output of a multi-member union
// whose members have differing `.to` chains. When the reconstituted union is
// fed back through the noopDecoder-wrapped `.to`, the per-branch transform
// codegen places its built-in input refiner (Number.isNaN, BigInt-throw, ...)
// BEFORE the corresponding `let v0 = +i` / `let v1 = BigInt(i)` declaration,
// producing a ReferenceError at parse time:
//
//   i=>{if(typeof i==="string"){
//     if(!Number.isNaN(v0)){let v0=+i; i=v0}     ← v0 used before declaration
//     ...
//   }}
test("Multi-member union with transformed members + getOr produces broken parse code", t => {
  let schema =
    S.union([
      S.string->S.to(S.float)->S.castToUnknown,
      S.string->S.to(S.bigint)->S.castToUnknown,
      S.bool->S.castToUnknown,
    ])
    ->S.option
    ->S.Option.getOr(%raw(`true`))

  // Construction succeeds and the default substitution still works because
  // the broken branch is only reached for string inputs.
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), %raw(`true`))
  t->Assert.deepEqual(%raw(`false`)->S.parseOrThrow(~to=schema), %raw(`false`))

  // The generated parse code references v0/v1 before they are declared.
  // Once fixed, the `let v0=+i` and `Number.isNaN` check should be reordered
  // so the validation reads the freshly-declared variable.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){if(!Number.isNaN(v0)){let v0=+i;i=v0}else{try{let v1;try{v1=BigInt(i)}catch(_){e[0](i)}i=v1}catch(e1){e[1](i,e1)}}}else if(!(typeof i==="boolean"||i===void 0)){e[2](i)}return i===void 0?true:i}`,
  )

  // Parsing a string input — the path that exercises the malformed code —
  // throws a JS ReferenceError instead of a Sury validation error.
  t->Assert.throws(
    () => {
      let _ = "42"->S.parseOrThrow(~to=schema)
    },
    ~expectations={message: "v0 is not defined"},
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
})
