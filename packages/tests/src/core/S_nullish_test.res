open Ava

test("Correctly parses", t => {
  let schema = S.nullish(S.bool)

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), Null, ())
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), Undefined, ())
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(schema), Value(true), ())
  t->U.assertThrows(
    () => %raw(`"foo"`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.toUnknown, received: %raw(`"foo"`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="boolean"||i===void 0||i===null)){e[0](i)}return i}`,
  )
})

test("Correctly parses transformed", t => {
  let schema = S.nullish(S.bool->S.to(S.string))

  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), Null, ())
  t->Assert.deepEqual(%raw(`undefined`)->S.parseOrThrow(schema), Undefined, ())
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(schema), Value("true"), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="boolean"){i=""+i}else if(!(i===void 0||i===null)){e[1](i)}return i}`,
  )
})

test("Correctly reverse convert", t => {
  let schema = S.nullish(S.bool)

  t->Assert.deepEqual(Nullable.Null->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(Nullable.Undefined->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(Nullable.Value(true)->S.reverseConvertOrThrow(schema), %raw(`true`), ())

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Correctly reverse convert transformed", t => {
  let schema = S.nullish(S.bool->S.to(S.string))

  t->Assert.deepEqual(Nullable.Null->S.reverseConvertOrThrow(schema), %raw(`null`), ())
  t->Assert.deepEqual(Nullable.Undefined->S.reverseConvertOrThrow(schema), %raw(`undefined`), ())
  t->Assert.deepEqual(Nullable.Value("true")->S.reverseConvertOrThrow(schema), %raw(`true`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="string"){let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}return i}`,
  )
})
