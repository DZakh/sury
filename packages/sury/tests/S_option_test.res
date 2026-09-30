open Vitest

module Common = {
  let value = None
  let any = %raw(`undefined`)
  let invalidAny = %raw(`123.45`)
  let factory = () => S.option(S.string)

  test("Successfully parses", t => {
    let schema = factory()

    t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
  })

  test("Fails to parse", t => {
    let schema = factory()

    t->U.assertThrowsMessage(
      () => invalidAny->S.parseOrThrow(~to=schema),
      `Expected string | undefined, received 123.45`,
    )
  })

  test("Successfully serializes", t => {
    let schema = factory()

    t->Assert.deepEqual(value->S.convertOrThrow(~from=schema, ~to=S.unknown), any)
  })

  test("Compiled parse code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{try{(typeof i==="string"||i===void 0)||e[0](i);return i}catch(v0){e[1](v0)}}`,
    )
  })

  // Undefined check should be first ?
  test("Compiled async parse code snapshot", t => {
    let schema = S.option(
      S.unknown->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never}),
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{try{return Promise.resolve((async(i)=>{let v1;for(;;){try{let v0=e[0](i);i=await v0;break}catch(x){v1=[v1,e[1](x)]}if(i===void 0)break;throw e[2](i,v1)};return i})(i)).catch(e[3])}catch(v2){return e[4](v2)}}`,
    )
  })

  test("Compiled serialize code snapshot", t => {
    let schema = factory()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{try{(typeof i==="string"||i===void 0)||e[0](i);return i}catch(v0){e[1](v0)}}`,
    )
  })

  test("Reverse to self", t => {
    let schema = factory()
    t->U.assertEqualSchemas(schema->S.reverse, schema->S.castToUnknown)
  })

  test("Succesfully uses reversed schema for parsing back to initial value", t => {
    let schema = factory()
    t->U.assertReverseParsesBack(schema, Some("abc"))
    t->U.assertReverseParsesBack(schema, None)
  })
}

test("Classify schema", t => {
  let schema = S.option(S.nullAsOption(S.string))

  t->U.assertEqualSchemas(
    schema->S.castToUnknown,
    S.union([
      S.string->S.castToUnknown,
      S.unit->S.castToUnknown,
      S.nullAsUnit->S.to(S.literal({"BS_PRIVATE_NESTED_SOME_NONE": 0}))->S.castToUnknown,
    ]),
  )

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.union([
      S.string->S.castToUnknown,
      S.unit->S.castToUnknown,
      S.literal({"BS_PRIVATE_NESTED_SOME_NONE": 0})->S.to(S.nullAsUnit->S.reverse)->S.castToUnknown,
    ]),
  )
})

test("Successfully parses primitive", t => {
  let schema = S.option(S.bool)

  t->Assert.deepEqual(JSON.Encode.bool(true)->S.parseOrThrow(~to=schema), Some(true))
})

test("Fails to parse JS null", t => {
  let schema = S.option(S.bool)

  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined, received null`,
  )
})

test("Fails to parse JS undefined when schema doesn't allow optional data", t => {
  let schema = S.bool

  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(~to=schema),
    `Expected boolean, received undefined`,
  )
})

test("Serializes Some(None) to undefined for option nested in null", t => {
  let schema = S.nullAsOption(S.option(S.bool))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Some(None))
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)

  t->Assert.deepEqual(Some(None)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{for(;;){if(typeof i==="boolean")break;if(i===null){i=void 0;break}if(i===void 0){i={BS_PRIVATE_NESTED_SOME_NONE:0};break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{for(;;){if(typeof i==="boolean")break;if(i===void 0){i=null;break}if(typeof i==="object"&&i&&!Array.isArray(i)){i=void 0;break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
  )
})

test("Applies valFromOption for Some()", t => {
  let schema = S.option(S.literal())

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(Some()->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{try{i===void 0||e[0](i);return i}catch(v0){e[1](v0)}}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{for(;;){if(i===void 0)break;if(typeof i==="object"&&i&&!Array.isArray(i)){i=void 0;break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
  )
})

test("Nested option support", t => {
  let schema = S.option(S.option(S.bool))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(Some(Some(true))->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))
  t->Assert.deepEqual(Some(None)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{(typeof i==="boolean"||i===void 0)||e[0](i);return i}catch(v0){e[1](v0)}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{for(;;){if(typeof i==="boolean")break;if(i===void 0)break;if(typeof i==="object"&&i&&!Array.isArray(i)){i=void 0;break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
  )
})

test("Triple nested option support", t => {
  let schema = S.option(S.option(S.option(S.bool)))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(
    Some(Some(Some(true)))->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`true`),
  )
  t->Assert.deepEqual(
    Some(Some(None))->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`undefined`),
  )
  t->Assert.deepEqual(Some(None)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{(typeof i==="boolean"||i===void 0)||e[0](i);return i}catch(v0){e[1](v0)}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{for(;;){if(typeof i==="boolean")break;if(i===void 0)break;if(typeof i==="object"&&i&&!Array.isArray(i)){for(;;){if(i.BS_PRIVATE_NESTED_SOME_NONE===0){i=void 0;break}if(i.BS_PRIVATE_NESTED_SOME_NONE===1){i=void 0;break}throw e[0](i)}break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
  )
})

test(
  "Empty object in option: S.option(S.object(_ => ())) https://github.com/DZakh/rescript-schema/issues/110",
  t => {
    let schema = S.option(S.object(_ => ()))

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(~to=schema), Some())
    t->Assert.deepEqual(Some()->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`{}`))
    t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{try{for(;;){if(i===void 0)break;if(typeof i==="object"&&i&&!Array.isArray(i)){i={BS_PRIVATE_NESTED_SOME_NONE:0};break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{try{for(;;){if(i===void 0)break;if(typeof i==="object"&&i&&!Array.isArray(i)){i={};break}throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
    )
  },
)

test("Doesn't apply valFromOption for non-undefined literals in option", t => {
  let schema: S.t<option<Null.t<unknown>>> = S.option(S.literal(%raw(`null`)))

  // Note: It'll fail without a type annotation, but we can't do anything here
  t->Assert.deepEqual(
    Some(%raw(`null`))->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`null`),
  )
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{try{(i===null||i===void 0)||e[0](i);return i}catch(v0){e[1](v0)}}`)
})

test("Option with unknown", t => {
  let schema = S.option(S.unknown)

  t->Assert.deepEqual(
    Some(%raw(`undefined`))->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{BS_PRIVATE_NESTED_SOME_NONE: 0}`),
  )
  t->Assert.deepEqual(
    Some(%raw(`"foo"`))->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"foo"`),
  )
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Parse)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
})

test("Option with transformed unknown", t => {
  let schema = S.option(S.unknown->S.shape(v => {"field": v}))

  t->Assert.deepEqual(
    Some(%raw(`undefined`))->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`undefined`),
  )
  t->Assert.deepEqual(
    Some({"field": %raw(`"foo"`)})->S.convertOrThrow(~from=schema, ~to=S.unknown),
    %raw(`"foo"`),
  )
  t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{for(;;){i={field:i};break}return i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{for(;;){if(typeof i==="object"&&i&&!Array.isArray(i)){i=i.field;break}if(i===void 0)break;throw e[0](i)}return i}catch(v0){e[1](v0)}}`,
  )
})

module CoderToOption = {
  let blankToNone = S.string->S.to(
    S.option(S.string),
    ~custom={
      decode: Sync(s => s == "" ? None : Some(s)),
      encode: Sync(o => o->Option.getOr("")),
    },
  )

  test("Option over a coder to option parses undefined as None", t => {
    let schema = S.option(blankToNone)

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`""`)->S.parseOrThrow(~to=schema), Some(None))
    t->Assert.deepEqual(%raw(`"a"`)->S.parseOrThrow(~to=schema), Some(Some("a")))

    t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
    t->Assert.deepEqual(Some(None)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`""`))
    t->Assert.deepEqual(Some(Some("a"))->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`"a"`))
  })

  test("Option over a coder to option in an absent object field", t => {
    let schema = S.object(s => s.field("a", S.option(blankToNone)))

    t->Assert.deepEqual(%raw(`{}`)->S.parseOrThrow(~to=schema), None)
  })

  test("Option over a coder to option with getOrWith", t => {
    let schema = S.option(blankToNone)->S.Option.getOrWith(() => None)

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`"a"`)->S.parseOrThrow(~to=schema), Some("a"))
  })

  test("nullAsOption over a coder to option parses null as None", t => {
    let schema = S.nullAsOption(blankToNone)

    t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`""`)->S.parseOrThrow(~to=schema), Some(None))
    t->Assert.deepEqual(Some(None)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`""`))
    t->Assert.deepEqual(None->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  })

  test("Nested option over a coder to option", t => {
    let schema = S.option(S.option(blankToNone))

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`""`)->S.parseOrThrow(~to=schema), Some(Some(None)))
    t->Assert.deepEqual(%raw(`"a"`)->S.parseOrThrow(~to=schema), Some(Some(Some("a"))))
    t->Assert.deepEqual(Some(Some(None))->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`""`))
    t->Assert.deepEqual(Some(None)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  })

  test("Option over a coder whose input is already optional keeps undefined as None", t => {
    let schema = S.option(
      S.option(S.string)->S.to(
        S.option(S.string),
        ~custom={
          decode: Sync(o =>
            switch o {
            | Some("") => None
            | _ => o
            }
          ),
          encode: Sync(o => o),
        },
      ),
    )

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  })

  test("Option over a coder whose input takes undefined hands it to the coder", t => {
    let schema = S.option(
      S.option(S.string)->S.to(
        S.union([S.literal(1), S.literal(2)]),
        ~custom={
          decode: Sync(o => o->Option.isNone ? 1 : 2),
          encode: Sync(i => i === 1 ? None : Some("x")),
        },
      ),
    )

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Some(1))
    t->Assert.deepEqual(%raw(`"x"`)->S.parseOrThrow(~to=schema), Some(2))
  })

  test("Option over a coder to a union parses undefined as None", t => {
    let schema = S.option(
      S.string->S.to(
        S.union([S.literal(1), S.literal(2)]),
        ~custom={decode: Sync(s => s == "1" ? 1 : 2), encode: Sync(i => i->Int.toString)},
      ),
    )

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
    t->Assert.deepEqual(%raw(`"1"`)->S.parseOrThrow(~to=schema), Some(1))
    t->Assert.deepEqual(Some(2)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`"2"`))
  })
}
