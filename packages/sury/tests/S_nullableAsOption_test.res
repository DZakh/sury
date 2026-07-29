open Vitest

test("Correctly parses", t => {
  let schema = S.nullableAsOption(S.bool)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), Some(true))
  t->U.assertThrowsMessage(
    () => %raw(`"foo"`)->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined | null, received "foo"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(typeof i==="boolean")break;if(i===void 0)break;if(i===null){i=void 0;break}e[0](i)}return i}`,
  )
})

test("Correctly parses transformed", t => {
  let schema = S.nullableAsOption(S.bool->S.to(S.string))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), Some("true"))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{for(;;){if(typeof i==="boolean"){i=""+i;break}if(i===void 0)break;if(i===null){i=void 0;break}e[0](i)}return i}`,
  )
})

test("Correctly reverse convert", t => {
  let schema = S.nullableAsOption(S.bool)

  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Some(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="boolean")break;if(i===void 0)break;if(i===void 0){i=null;break}e[0](i)}return i}`,
  )
})

test("Correctly reverse convert transformed", t => {
  let schema = S.nullableAsOption(S.bool->S.to(S.string))

  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Some("true")->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{for(;;){if(typeof i==="string"){let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0;break}if(i===void 0)break;if(i===void 0){i=null;break}e[1](i)}return i}`,
  )
})

test("Correctly parses with default", t => {
  let schema = S.nullableAsOption(S.bool)->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual(%raw(`false`)->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), true)
})
