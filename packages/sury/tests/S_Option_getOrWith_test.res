open Vitest

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
  let schema = S.option(S.bool)->S.option->S.Option.getOrWith(() => Some(true))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Some(true))
})

test("Fails to parse data with default", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  t->U.assertThrowsMessage(
    () => %raw(`"string"`)->S.parseOrThrow(~to=schema),
    `Expected undefined | boolean, received "string"`,
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
    `i=>{for(;;){if(i===void 0){i=e[0]();break}if(typeof i==="boolean")break;e[1](i)}return i}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema =
    S.bool
    ->S.to(S.any, ~custom={decode: Async(i => Promise.resolve(i)), encode: Never})
    ->S.option
    ->S.Option.getOrWith(() => false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{for(;;){if(i===void 0){i=e[0]();break}if(typeof i==="boolean"){let v0=e[1](i);i=v0;break}e[2](i)}return Promise.resolve(i)}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => false)

  // The reversed union validates the value like any other typed decode — the
  // old noop relied on Option_getWithDefault's noopDecoder hack.
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{typeof i==="boolean"||e[0](i);return i}`)
})

// FIXME: callback return values aren't validated, so a bad default silently
// produces a type-mismatched value.
test("Invalid dynamic default is not validated (known limitation)", t => {
  let schema = S.bool->S.option->S.Option.getOrWith(() => %raw(`"not a bool"`))

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), %raw(`"not a bool"`))
})
