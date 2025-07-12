open Ava
open RescriptCore

test("Uses default value when parsing optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`undefined`)

  let schema = S.float->S.option->S.Option.getOr(value)

  t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
})

test("Uses default value when nullable optional unknown primitive", t => {
  let value = 123.
  let any = %raw(`null`)

  let schema = S.float->S.null->S.Option.getOr(value)

  t->Assert.deepEqual(any->S.parseOrThrow(schema), value)
})

test("Successfully parses with default when provided JS undefined", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), false)
})

test("Successfully parses with default when provided primitive", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(schema), true)
})

test("Successfully serializes nested option with default value", t => {
  let schema = S.option(
    S.option(S.option(S.option(S.option(S.option(S.bool)))->S.Option.getOr(Some(Some(true))))),
  )

  t->Assert.deepEqual(
    Some(Some(Some(Some(None))))->S.reverseConvertOrThrow(schema),
    Some(Some(Some(Some(None))))->Obj.magic,
  )
  // FIXME: I'm not sure this is correct
  t->Assert.deepEqual(Some(None)->S.reverseConvertOrThrow(schema), %raw(`undefined`))
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`undefined`))
})

test("Fails to parse data with default", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertThrows(
    () => %raw(`"string"`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`"string"`)}),
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
    ->S.to(S.option(S.string))
    ->S.Option.getOr("not positive")

  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), "not positive")
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{if(!(typeof i==="number"&&!Number.isNaN(i)||i===void 0)){e[0](i)}let v0=e[1](i===void 0?-123:i);if(typeof v0!=="string"){e[2](v0)}i=v0}catch(e0){try{if(!(typeof i==="number"&&!Number.isNaN(i)||i===void 0)){e[3](i)}let v1=e[4](i===void 0?-123:i);if(v1!==void 0){e[5](v1)}i=v1}catch(e1){e[6](i,e0,e1)}}return i===void 0?"not positive":i}`,
  )
})

test("Successfully serializes schema with transformation", t => {
  let schema = S.string->S.trim->S.option->S.Option.getOr("default")

  t->Assert.deepEqual(" abc"->S.reverseConvertOrThrow(schema), %raw(`"abc"`))
})

test("Compiled parse code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i===void 0){i=false}else if(!(typeof i==="boolean")){e[0](i)}return i}`,
  )
})

asyncTest("Compiled async parse code snapshot", async t => {
  let schema =
    S.option(S.bool->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))->S.Option.getOr(
      false,
    )

  t->Assert.deepEqual(schema->S.isAsync, true)
  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(schema), false)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="boolean"){i=e[0](i)}else if(!(i===void 0)){e[1](i)}return Promise.resolve(i).then(v0=>{return v0===void 0?false:v0})}`,
  )

  let schema =
    S.option(S.bool)
    ->S.Option.getOr(false)
    ->S.transform(_ => {asyncParser: i => Promise.resolve(i)})

  t->Assert.deepEqual(await None->S.parseAsyncOrThrow(schema), false)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return e[1](i===void 0?false:i)}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.option->S.Option.getOr(false)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})
