open Ava

test("Expression of primitive schema", t => {
  t->Assert.deepEqual(S.string->S.toExpression, "string")
})

test("Expression of primitive schema with name", t => {
  t->Assert.deepEqual(S.string->S.meta({name: "Address"})->S.toExpression, "Address")
})

test("Expression of Literal schema", t => {
  t->Assert.deepEqual(S.literal(123)->S.toExpression, "123")
})

test("Expression of Literal object schema", t => {
  t->Assert.deepEqual(S.literal({"abc": 123})->S.toExpression, `{ abc: 123; }`)
})

test("Expression of Literal array schema", t => {
  t->Assert.deepEqual(S.literal((123, "abc"))->S.toExpression, `[123, "abc"]`)
})

test("Expression of Array schema", t => {
  t->Assert.deepEqual(S.array(S.string)->S.toExpression, "string[]")
})

test("Expression of Unnest schema", t => {
  t->Assert.deepEqual(
    S.unnest(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )->S.toExpression,
    "[string[], int32[]]",
  )
})

test("Expression of reversed Unnest schema", t => {
  t->Assert.deepEqual(
    S.unnest(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )
    ->S.reverse
    ->S.toExpression,
    "{ foo: string; bar: int32; }[]",
  )
})

test("Expression of Array schema with optional items", t => {
  t->Assert.deepEqual(S.array(S.option(S.string))->S.toExpression, "(string | undefined)[]")
})

test("Expression of Dict schema", t => {
  t->Assert.deepEqual(S.dict(S.string)->S.toExpression, "{ [key: string]: string; }")
})

test("Expression of Option schema", t => {
  t->Assert.deepEqual(S.option(S.string)->S.toExpression, "string | undefined")
})

test("Expression of Option schema with name", t => {
  t->Assert.deepEqual(
    S.option(S.string->S.meta({name: "Nested"}))->S.meta({name: "EnvVar"})->S.toExpression,
    "EnvVar",
  )
})

test("Expression of Null schema", t => {
  t->Assert.deepEqual(S.null(S.string)->S.toExpression, "string | null")
})

test("Expression of Union schema", t => {
  t->Assert.deepEqual(S.union([S.string, S.literal("foo")])->S.toExpression, `string | "foo"`)
})

test("Expression of Union schema with duplicated items", t => {
  t->Assert.deepEqual(
    S.union([S.literal("foo"), S.string, S.literal("foo")])->S.toExpression,
    `"foo" | string | "foo"`,
  )
})

test("Expression of Object schema", t => {
  t->Assert.deepEqual(
    S.object(s =>
      {
        "foo": s.field("foo", S.string),
        "bar": s.field("bar", S.int),
      }
    )->S.toExpression,
    `{ foo: string; bar: int32; }`,
  )
})

test("Expression of empty Object schema", t => {
  t->Assert.deepEqual(S.object(_ => ())->S.toExpression, `{}`)
})

test("Expression of Tuple schema", t => {
  t->Assert.deepEqual(
    S.tuple(s =>
      {
        "foo": s.item(0, S.string),
        "bar": s.item(1, S.int),
      }
    )->S.toExpression,
    `[string, int32]`,
  )
})

test("Expression of renamed schema", t => {
  let originalSchema = S.never
  let renamedSchema = originalSchema->S.meta({name: "Ethers.BigInt"})
  t->Assert.deepEqual(originalSchema->S.toExpression, "never")
  t->Assert.deepEqual(renamedSchema->S.toExpression, "Ethers.BigInt")
  // Uses new name when failing
  t->U.assertThrows(
    () => "smth"->S.parseOrThrow(renamedSchema),
    {
      path: S.Path.empty,
      operation: Parse,
      code: InvalidType({expected: renamedSchema->S.castToUnknown, received: "smth"->Obj.magic}),
    },
  )
  t->Assert.is(
    U.error({
      path: S.Path.empty,
      operation: Parse,
      code: InvalidType({expected: renamedSchema->S.castToUnknown, received: "smth"->Obj.magic}),
    }).message,
    `Failed parsing: Expected Ethers.BigInt, received "smth"`,
  )
  let schema = S.null(S.never)->S.meta({name: "Ethers.BigInt"})
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{try{e[0](i);}catch(e0){if(i===void 0){i=null}else{e[1](i,e0)}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{e[0](i);}catch(e0){if(i===void 0){i=null}}return i}`,
  )
  t->Assert.deepEqual(None->S.reverseConvertOrThrow(schema), %raw(`null`))
  // TODO: Can be improved. No need to duplicate Expected/received error
  t->U.assertThrowsMessage(
    () => %raw(`"smth"`)->S.parseOrThrow(schema->S.reverse),
    `Failed parsing: Expected never | undefined, received "smth"
- Expected never, received "smth"`,
  )
})
