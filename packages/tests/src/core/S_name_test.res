open Ava

test("Name of primitive schema", t => {
  t->Assert.deepEqual(S.string->S.name, "string", ())
})

test("Name of Literal schema", t => {
  t->Assert.deepEqual(S.literal(123)->S.name, "123", ())
})

test("Name of Literal object schema", t => {
  t->Assert.deepEqual(S.literal({"abc": 123})->S.name, `{ "abc": 123 }`, ())
})

test("Name of Literal array schema", t => {
  t->Assert.deepEqual(S.literal((123, "abc"))->S.name, `[123, "abc"]`, ())
})

test("Name of Array schema", t => {
  t->Assert.deepEqual(S.array(S.string)->S.name, "array<string>", ())
})

test("Name of Unnest schema", t => {
  t->Assert.deepEqual(
    S.unnest(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )->S.name,
    "[array<string>, array<int32>]",
    (),
  )
})

test("Name of reversed Unnest schema", t => {
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
    ->S.name,
    "array<{ foo: string; bar: int32; }>",
    (),
  )
})

test("Name of Array schema with optional items", t => {
  t->Assert.deepEqual(S.array(S.option(S.string))->S.name, "array<string | undefined>", ())
})

test("Name of Dict schema", t => {
  t->Assert.deepEqual(S.dict(S.string)->S.name, "dict<string>", ())
})

test("Name of Option schema", t => {
  t->Assert.deepEqual(S.option(S.string)->S.name, "string | undefined", ())
})

test("Name of Null schema", t => {
  t->Assert.deepEqual(S.null(S.string)->S.name, "string | null", ())
})

test("Name of Union schema", t => {
  t->Assert.deepEqual(S.union([S.string, S.literal("foo")])->S.name, `string | "foo"`, ())
})

test("Name of Object schema", t => {
  t->Assert.deepEqual(
    S.object(s =>
      {
        "foo": s.field("foo", S.string),
        "bar": s.field("bar", S.int),
      }
    )->S.name,
    `{ foo: string; bar: int32; }`,
    (),
  )
})

test("Name of empty Object schema", t => {
  t->Assert.deepEqual(S.object(_ => ())->S.name, `{}`, ())
})

test("Name of Tuple schema", t => {
  t->Assert.deepEqual(
    S.tuple(s =>
      {
        "foo": s.item(0, S.string),
        "bar": s.item(1, S.int),
      }
    )->S.name,
    `[string, int32]`,
    (),
  )
})

test("Name of custom schema", t => {
  t->Assert.deepEqual(S.custom("Test", s => s.fail("User error"))->S.name, "Test", ())
})

test("Name of renamed schema", t => {
  let originalSchema = S.never
  let renamedSchema = originalSchema->S.setName("Ethers.BigInt")
  t->Assert.deepEqual(originalSchema->S.name, "never", ())
  t->Assert.deepEqual(renamedSchema->S.name, "Ethers.BigInt", ())
  // Uses new name when failing
  t->U.assertRaised(
    () => "smth"->S.parseOrThrow(renamedSchema),
    {
      path: S.Path.empty,
      operation: Parse,
      code: InvalidType({expected: renamedSchema->S.toUnknown, received: "smth"->Obj.magic}),
    },
  )
  t->Assert.is(
    U.error({
      path: S.Path.empty,
      operation: Parse,
      code: InvalidType({expected: renamedSchema->S.toUnknown, received: "smth"->Obj.magic}),
    })->S.Error.message,
    `Failed parsing at root. Reason: Must be Ethers.BigInt (was "smth")`,
    (),
  )
  t->U.assertRaised(
    () => %raw(`"smth"`)->S.reverseConvertOrThrow(S.null(S.never)->S.setName("Ethers.BigInt")),
    {
      path: S.Path.empty,
      operation: ReverseConvert,
      code: InvalidType({expected: S.never->S.toUnknown, received: "smth"->Obj.magic}),
    },
  )
})
