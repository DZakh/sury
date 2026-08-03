open Vitest

test("Expression of primitive schema", t => {
  t->Assert.deepEqual(S.string->S.inputExpression, "string")
})

test("Expression of primitive schema with name", t => {
  t->Assert.deepEqual(S.string->S.meta({name: "Address"})->S.inputExpression, "Address")
})

test("Expression of nan schema", t => {
  // No `nan` case in inputExpression: the nan schema always carries const: NaN,
  // so the `const` branch renders it, to the same string.
  t->Assert.deepEqual(S.nan->S.inputExpression, "NaN")
})

test("Expression of Literal schema", t => {
  t->Assert.deepEqual(S.literal(123)->S.inputExpression, "123")
})

test("Expression of Literal object schema", t => {
  t->Assert.deepEqual(S.literal({"abc": 123})->S.inputExpression, `{ abc: 123; }`)
})

test("Expression of Literal array schema", t => {
  t->Assert.deepEqual(S.literal((123, "abc"))->S.inputExpression, `[123, "abc"]`)
})

test("Expression of Array schema", t => {
  t->Assert.deepEqual(S.array(S.string)->S.inputExpression, "string[]")
})

test("Expression of compactColumns schema without S.to", t => {
  t->Assert.deepEqual(S.compactColumns(S.unknown)->S.inputExpression, "unknown[][]")
  t->Assert.deepEqual(S.compactColumns(S.string)->S.inputExpression, "string[][]")
  t->Assert.deepEqual(S.compactColumns(S.int)->S.inputExpression, "int32[][]")
})

test("Expression of compactColumns schema", t => {
  // The supported target, per compactColumnsDecoder's panic message: an array
  // of objects, whose item schema carries the columns.
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
    ->S.inputExpression,
    "[string[], int32[]]",
  )
})

test("Expression of compactColumns schema with an unsupported target", t => {
  // `.to(objectSchema)` is rejected by the decoder (it panics with "supports
  // only object schemas. Use ...->S.to(S.array(objectSchema))"), so there are no
  // columns to describe — it falls back to its own columnar shape rather than
  // advertising a conversion that cannot run.
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
    ->S.inputExpression,
    "unknown[][]",
  )
  // The fallback is the schema's own shape, not a fixed `unknown[][]`: an
  // unsupported target says nothing about the columns, but the item schema
  // still does.
  t->Assert.deepEqual(
    S.compactColumns(S.string)
    ->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
        }
      ),
    )
    ->S.inputExpression,
    "string[][]",
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
    ->S.inputExpression,
    "{ foo: string; bar: int32; }[]",
  )
})

test("Expression of Array schema with optional items", t => {
  t->Assert.deepEqual(S.array(S.option(S.string))->S.inputExpression, "(string | undefined)[]")
})

test("Expression of Dict schema", t => {
  t->Assert.deepEqual(S.dict(S.string)->S.inputExpression, "{ [key: string]: string; }")
})

test("Expression of Option schema", t => {
  t->Assert.deepEqual(S.option(S.string)->S.inputExpression, "string | undefined")
})

test("Expression of Option schema with name", t => {
  t->Assert.deepEqual(
    S.option(S.string->S.meta({name: "Nested"}))->S.meta({name: "EnvVar"})->S.inputExpression,
    "EnvVar",
  )
})

test("Expression of Null schema", t => {
  t->Assert.deepEqual(S.nullAsOption(S.string)->S.inputExpression, "string | null")
})

test("Expression of Union schema", t => {
  t->Assert.deepEqual(S.union([S.string, S.literal("foo")])->S.inputExpression, `string | "foo"`)
})

test("Expression of Union schema with duplicated items", t => {
  // Deduplicated on the rendered text, so the two distinct "foo" literal
  // schemas collapse into one member.
  t->Assert.deepEqual(
    S.union([S.literal("foo"), S.string, S.literal("foo")])->S.inputExpression,
    `"foo" | string`,
  )
})

test("Expression of Union schema collapses members that render alike", t => {
  // The trade: these are three different schemas, and the expression no longer
  // says so. What distinguishes them surfaces in a union error's reason list.
  t->Assert.deepEqual(
    S.union([S.string->S.min(4), S.string->S.max(1), S.string])->S.inputExpression,
    "string",
  )
})

test("Expression of Object schema", t => {
  t->Assert.deepEqual(
    S.object(s =>
      {
        "foo": s.field("foo", S.string),
        "bar": s.field("bar", S.int),
      }
    )->S.inputExpression,
    `{ foo: string; bar: int32; }`,
  )
})

test("Expression of empty Object schema", t => {
  t->Assert.deepEqual(S.object(_ => ())->S.inputExpression, `{}`)
})

test("Expression of Tuple schema", t => {
  t->Assert.deepEqual(
    S.tuple(s =>
      {
        "foo": s.item(0, S.string),
        "bar": s.item(1, S.int),
      }
    )->S.inputExpression,
    `[string, int32]`,
  )
})

test("Expression of renamed schema", t => {
  let originalSchema = S.never
  let renamedSchema = originalSchema->S.meta({name: "Ethers.BigInt"})
  t->Assert.deepEqual(originalSchema->S.inputExpression, "never")
  t->Assert.deepEqual(renamedSchema->S.inputExpression, "Ethers.BigInt")
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

  t->Assert.deepEqual(nodeSchema->S.inputExpression, `Node`)
  t->Assert.deepEqual(renamedRoot->S.inputExpression, `NodeRoot`)

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

  t->Assert.deepEqual(nodeSchema->S.inputExpression, `MyNode`)
  t->U.assertThrowsMessage(
    () => %raw(`null`)->S.parseOrThrow(~to=nodeSchema),
    `Expected MyNode, received null`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`{Id: "0"}`)->S.parseOrThrow(~to=nodeSchema),
    `Failed at ["Children"]: Expected MyNode[], received undefined`,
  )
})

test("Output expression is the input expression of the reversed schema", t => {
  let schema = S.string->S.to(S.int)
  t->Assert.deepEqual(schema->S.inputExpression, "string")
  t->Assert.deepEqual(schema->S.outputExpression, "int32")
})

test("Output expression reverses nested schemas", t => {
  let schema = S.array(S.string->S.to(S.int))
  t->Assert.deepEqual(schema->S.inputExpression, "string[]")
  t->Assert.deepEqual(schema->S.outputExpression, "int32[]")
})

test("Output expression of a schema without a transform matches the input", t => {
  t->Assert.deepEqual(S.string->S.outputExpression, "string")
})

test("toString prints both sides of a transformed schema", t => {
  let schema = S.string->S.to(S.int)
  t->Assert.deepEqual((schema->S.untag).toString(), "Schema<string, int32>")
})

test("toString collapses to one parameter when the sides match", t => {
  t->Assert.deepEqual((S.string->S.untag).toString(), "Schema<string>")
})

test("toString reverses nested schemas for the output side", t => {
  t->Assert.deepEqual((S.array(S.string->S.to(S.int))->S.untag).toString(), "Schema<string[], int32[]>")
})
