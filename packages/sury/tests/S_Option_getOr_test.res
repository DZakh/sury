open Ava
open RescriptCore

test("Uses default value when parsing optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`undefined`)

  let schema = S.float->S.option->S.Option.getOr(value)

  t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
})

test("Uses default value when nullable optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`null`)

  let schema = S.float->S.null->S.Option.getOr(value)

  t->Assert.deepEqual(any->S.parseOrThrow(schema), value, ())
})

test("Successfully parses with default when provided JS undefined", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), false, ())
})

test("Successfully parses with default when provided primitive", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(schema), true, ())
})

test("Successfully serializes nested option with default value", t => {
  let schema = S.option(
    S.option(S.option(S.option(S.option(S.option(S.bool)))->S.Option.getOr(Some(Some(true))))),
  )

  t->Assert.deepEqual(
    Some(Some(Some(Some(None))))->S.reverseConvertOrThrow(schema),
    %raw(`undefined`),
    (),
  )
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
})

test("Fails to parse data with default", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertThrows(
    () => %raw(`"string"`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`"string"`)}),
      operation: Parse,
      path: S.Path.empty,
    },
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
    ->S.Option.getOr("not positive")

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), "not positive", ())
})

test("Successfully serializes schema with transformation", t => {
  let schema = S.string->S.trim->S.option->S.Option.getOr("default")

  t->Assert.deepEqual(" abc"->S.reverseConvertOrThrow(schema), %raw(`"abc"`), ())
})

test("Compiled parse code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i===void 0?e[1]:i}`,
  )
})

asyncTest("Compiled async parse code snapshot", async t => {
  let schema =
    S.bool
    ->S.transform(_ => {asyncParser: i => Promise.resolve(i)})
    ->S.option
    ->S.Option.getOr(false)

  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(schema), false, ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="boolean"){i=e[0](i)}else if(!(i===void 0)){e[1](i)}return Promise.resolve(i).then(v0=>{return v0===void 0?e[2]:v0})}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})
