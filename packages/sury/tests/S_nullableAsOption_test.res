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
    `i=>{if(i===null){i=void 0}else if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
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
    `i=>{if(typeof i==="boolean"){i=""+i}else if(i===null){i=void 0}else if(!(i===void 0)){e[0](i)}return i}`,
  )
})

test("Correctly reverse convert", t => {
  let schema = S.nullableAsOption(S.bool)

  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Some(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(!(typeof i==="boolean"||i===void 0)){e[0](i)}return i}`,
  )
})

test("Correctly reverse convert transformed", t => {
  let schema = S.nullableAsOption(S.bool->S.to(S.string))

  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Some("true")->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="string"){let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}else if(!(i===void 0)){e[1](i)}return i}`,
  )
})

test("Correctly parses jsonString member", t => {
  let schema = S.nullableAsOption(S.jsonString)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(`"foo"`->S.parseOrThrow(~to=schema), Some(`"foo"`))
  t->U.assertThrowsMessage(
    () => `notjson`->S.parseOrThrow(~to=schema),
    `Expected JSON string, received "notjson"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{JSON.parse(i)}catch(t){e[0](i)}}else if(i===null){i=void 0}else if(!(i===void 0)){e[1](i)}return i}`,
  )
})

test("Correctly reverse convert jsonString member", t => {
  let schema = S.nullableAsOption(S.jsonString)

  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(
    Some(`"foo"`)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`'"foo"'`),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="string"){try{JSON.parse(i)}catch(t){e[0](i)}}else if(!(i===void 0)){e[1](i)}return i}`,
  )
})

test("Correctly parses with default", t => {
  let schema = S.nullableAsOption(S.bool)->S.Option.getOr(false)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual(%raw(`false`)->S.parseOrThrow(~to=schema), false)
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), true)
})
