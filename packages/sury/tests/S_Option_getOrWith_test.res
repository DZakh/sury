open Ava

test("Uses default value when parsing optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`undefined`)

  let schema = S.float->S.option->S.Option.getOrWith(() => value)

  t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
})

test("Uses default value when nullable optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`null`)

  let schema = S.float->S.nullAsOption->S.Option.getOrWith(() => value)

  t->Assert.deepEqual(any->S.parseOrThrow(~to=schema), value)
})

test("Successfully parses with default when provided JS undefined", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), false)
})

test("Successfully parses with default when provided primitive", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), true)
})

test("Successfully parses nested option with default value", t => {
  t->Assert.throws(() => {
    let schema = S.option(S.bool)->S.option->S.Option.getOrWith(() => Some(true))

    t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Some(true))
  }, ~expectations={message: "[Sury] Can\'t set default for boolean | undefined | undefined"})
})

test("Fails to parse data with default", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  t->U.assertThrowsMessage(
    () => %raw(`"string"`)->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined, received "string"`,
  )
})

test("Successfully serializes schema with transformation", t => {
  let schema = S.string->S.trim->S.option->S.Option.getOrWith(() => "default")

  t->Assert.deepEqual(" abc"->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"abc"`))
})

test("Compiled parse code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i===void 0?e[1]():i}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema =
    S.bool
    ->S.transform(_ => {asyncParser: i => Promise.resolve(i)})
    ->S.option
    ->S.Option.getOrWith(() => false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{let v1;if(typeof i==="boolean"){let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}i=v0}else if(!(i===void 0)){e[2](i)}v1=Promise.resolve(i);return v1.then(v1=>{return v1===void 0?e[3]():v1})}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Encode)
})
