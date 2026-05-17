open Ava

test("Correctly parses", t => {
  let schema = S.nullable(S.bool)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), Null)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Undefined)
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), Value(true))
  t->U.assertThrowsMessage(
    () => %raw(`"foo"`)->S.parseOrThrow(~to=schema),
    `Expected boolean | undefined | null, received "foo"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0||i===null)){e[0](i)}return i}`,
  )
})

test("Correctly parses transformed", t => {
  let schema = S.nullable(S.bool->S.to(S.string))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), Null)
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(~to=schema), Undefined)
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), Value("true"))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(!(i===void 0||i===null)){e[0](i)}return i}`,
  )
})

test("Correctly reverse convert", t => {
  let schema = S.nullable(S.bool)

  t->Assert.deepEqual(Nullable.Null->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(Nullable.Undefined->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Nullable.Value(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(!(typeof i==="boolean"||i===void 0||i===null)){e[0](i)}return i}`,
  )
})

test("Correctly reverse convert transformed", t => {
  let schema = S.nullable(S.bool->S.to(S.string))

  t->Assert.deepEqual(Nullable.Null->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->Assert.deepEqual(Nullable.Undefined->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`undefined`))
  t->Assert.deepEqual(Nullable.Value("true")->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="string"){let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}else if(!(i===void 0||i===null)){e[1](i)}return i}`,
  )
})
