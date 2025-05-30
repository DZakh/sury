open Ava
open RescriptCore

test("Doesn't affect valid parsing", t => {
  let schema = S.string->S.catch(_ => "fallback")

  t->Assert.deepEqual("abc"->S.parseOrThrow(schema), "abc", ())
})

test("Doesn't do anything with unknown schema", t => {
  let schema = S.unknown->S.catch(_ => %raw(`"fallback"`))

  t->Assert.deepEqual("abc"->S.parseOrThrow(schema), %raw(`"abc"`), ())
})

test("Uses fallback value when parsing failed", t => {
  let schema = S.string->S.catch(_ => "fallback")

  t->Assert.deepEqual(123->S.parseOrThrow(schema), "fallback", ())
})

test("Doesn't affect serializing in any way", t => {
  let schema = S.int->S.port->S.catch(_ => 8080)

  t->U.assertThrowsMessage(
    () => -1->S.reverseConvertOrThrow(schema),
    `Failed converting: Expected port, received -1`,
  )
})

test("Provides ctx to use in catch", t => {
  t->ExecutionContext.plan(3)
  let schema = S.string->S.catch(s => {
    t->Assert.deepEqual(
      s.error,
      U.error({
        code: InvalidType({received: %raw(`123`), expected: S.string->S.toUnknown}),
        operation: Parse,
        path: S.Path.empty,
      }),
      (),
    )
    t->Assert.deepEqual(s.input, %raw(`123`), ())
    "fallback"
  })

  t->Assert.deepEqual(123->S.parseOrThrow(schema), "fallback", ())
})

test("Can use s.fail inside of S.catch", t => {
  let schema =
    S.int
    ->S.port
    ->S.catch(s => {
      switch s.input->S.parseOrThrow(S.string) {
      | _ => 8080
      | exception S.Error(_) => s.fail("Fallback value only supported for strings.")
      }
    })
  t->Assert.deepEqual(3000->S.parseOrThrow(schema), 3000, ())
  t->Assert.deepEqual("3000"->S.parseOrThrow(schema), 8080, ())
  t->U.assertThrows(
    () => true->S.parseOrThrow(schema),
    {
      code: OperationFailed("Fallback value only supported for strings."),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  t->Assert.deepEqual(3000->S.reverseConvertOrThrow(schema), %raw(`3000`), ())
  t->U.assertThrowsMessage(
    () => -1->S.reverseConvertOrThrow(schema),
    `Failed converting: Expected port, received -1`,
  )
})

asyncTest("Uses fallback value when async schema parsing failed during the sync part", async t => {
  let schema =
    S.string
    ->S.transform(_ => {asyncParser: i => Promise.resolve(i)})
    ->S.catch(_ => "fallback")

  t->Assert.deepEqual(await 123->S.parseAsyncOrThrow(schema), "fallback", ())
})

asyncTest("Uses fallback value when async schema parsing failed during the async part", async t => {
  let schema =
    S.string->S.transform(s => {asyncParser: _ => s.fail("foo")})->S.catch(_ => "fallback")

  t->Assert.deepEqual(await "123"->S.parseAsyncOrThrow(schema), "fallback", ())
})

asyncTest(
  "Uses fallback value when async schema parsing failed during the async part in promise",
  async t => {
    let schema =
      S.string
      ->S.transform(s => {
        asyncParser: _ => Promise.resolve()->Promise.thenResolve(() => s.fail("fail")),
      })
      ->S.catch(_ => "fallback")

    t->Assert.deepEqual(await "123"->S.parseAsyncOrThrow(schema), "fallback", ())
  },
)

test("Compiled parse code snapshot", t => {
  let schema = S.bool->S.catch(_ => false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{if(typeof i!=="boolean"){e[1](i)}}catch(v0){if(v0&&v0.s===s){i=e[0](i,v0)}else{throw v0}}return i}`,
  )
})

test("Compiled async parse code snapshot", t => {
  let schema = S.bool->S.transform(_ => {asyncParser: i => Promise.resolve(i)})->S.catch(_ => false)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{let v1;try{if(typeof i!=="boolean"){e[1](i)}v1=e[2](i).catch(v0=>{if(v0&&v0.s===s){return e[0](i,v0)}else{throw v0}})}catch(v0){if(v0&&v0.s===s){v1=Promise.resolve(e[0](i,v0))}else{throw v0}}return v1}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.bool->S.catch(_ => false)

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})
