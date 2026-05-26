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
      message: `[Sury] Can\'t set default for boolean | undefined | undefined | undefined`,
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

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
})
