open Vitest

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

test("Expression of compactColumns schema without S.to", t => {
  t->Assert.deepEqual(S.compactColumns(S.unknown)->S.toExpression, "unknown[][]")
  t->Assert.deepEqual(S.compactColumns(S.string)->S.toExpression, "string[][]")
  t->Assert.deepEqual(S.compactColumns(S.int)->S.toExpression, "int32[][]")
})

test("Expression of compactColumns schema", t => {
  t->Assert.deepEqual(
    S.compactColumns(S.unknown)
    ->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )
    ->S.toExpression,
    "[string[], int32[]]",
  )
})

test("Expression of reversed compactColumns schema", t => {
  t->Assert.deepEqual(
    S.compactColumns(S.unknown)
    ->S.to(
      S.array(
        S.schema(s =>
          {
            "foo": s.matches(S.string),
            "bar": s.matches(S.int),
          }
        ),
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
  t->Assert.deepEqual(S.nullAsOption(S.string)->S.toExpression, "string | null")
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
  t->U.assertThrowsMessage(
    () => "smth"->S.parseOrThrow(~to=renamedSchema),
    `Expected Ethers.BigInt, received "smth"`,
  )
  let schema = S.nullAsOption(S.never)->S.meta({name: "Ethers.BigInt"})
  // The `never` member can never match, so only the None -> null arm compiles.
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(i===void 0){i=null}else{e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(i===void 0){i=null}else{e[0](i)}return i}`,
  )
  t->Assert.deepEqual(None->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`null`))
  t->U.assertThrowsMessage(
    () => %raw(`"smth"`)->S.parseOrThrow(~to=schema->S.reverse),
    `Expected Ethers.BigInt, received "smth"`,
  )
})

test("Expression of recursive schema", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s =>
        {
          "id": s.field("Id", S.string),
          "children": s.field("Children", S.array(nodeSchema)),
        },
    )
  })

  let renamedRoot = nodeSchema->S.meta({name: `NodeRoot`})

  t->Assert.deepEqual(nodeSchema->S.toExpression, `Node`)
  t->Assert.deepEqual(renamedRoot->S.toExpression, `NodeRoot`)

  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=nodeSchema),
    `Expected { Id: string; Children: Node[]; }, received null`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=S.tuple1(nodeSchema)),
    `Expected [Node], received null`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=S.tuple1(renamedRoot)),
    `Expected [NodeRoot], received null`,
  )
  t->U.assertThrowsMessage(
    ~message=`It shouldn't rename node schema ref name`,
    () =>
      %raw(`{
      Id: "0",
      Children: [null]
    }`)->S.parseOrThrow(~to=renamedRoot),
    `Failed at ["Children"]["0"]: Expected { Id: string; Children: Node[]; }, received null`,
  )
})

test("Expression of deeply renamed recursive schema", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s =>
        {
          "id": s.field("Id", S.string),
          "children": s.field("Children", S.array(nodeSchema)),
        },
    )->S.meta({name: "MyNode"})
  })

  t->Assert.deepEqual(nodeSchema->S.toExpression, `MyNode`)
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=nodeSchema),
    `Expected MyNode, received null`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`{Id: "0"}`)->S.parseOrThrow(~to=nodeSchema),
    `Failed at ["Children"]: Expected MyNode[], received undefined`,
  )
})

test("Bounds render on the schema they constrain", t => {
  t->Assert.deepEqual(S.int->S.gt(5)->S.toExpression, `int32 > 5`)
  t->Assert.deepEqual(S.int->S.gte(5)->S.toExpression, `int32 >= 5`)
  t->Assert.deepEqual(S.float->S.lt(5.)->S.toExpression, `number < 5`)
  t->Assert.deepEqual(S.float->S.gte(1.)->S.lte(9.)->S.toExpression, `1 <= number <= 9`)
  // A format's own range is not a bound the caller wrote, so it stays implicit.
  t->Assert.deepEqual(S.int->S.toExpression, `int32`)
  t->Assert.deepEqual(S.port->S.toExpression, `port`)
})

test("An array of bounded items parenthesises the item expression", t => {
  let schema = S.array(S.int->S.gt(5))->S.maxLength(3)

  // Without the parens this reads as `int32 > (5[])`.
  t->Assert.deepEqual(schema->S.toExpression, `(int32 > 5)[].length <= 3`)
  t->U.assertThrowsMessage(
    () => %raw(`"x"`)->S.parseOrThrow(~to=schema),
    `Expected (int32 > 5)[].length <= 3, received "x"`,
  )
  // The item bound and the array bound report separately, each at its own path.
  t->U.assertThrowsMessage(
    () => %raw(`[1]`)->S.parseOrThrow(~to=schema),
    `Failed at ["0"]: Expected int32 > 5, received 1`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`[6, 7, 8, 9]`)->S.parseOrThrow(~to=schema),
    `Expected (int32 > 5)[].length <= 3, received [6, 7, 8, 9]`,
  )
})
