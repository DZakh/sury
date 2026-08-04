open Vitest

// https://github.com/DZakh/sury/issues/284 — stale output val schemas in
// array/dict/shaped decoders made the next `.to` segment re-decode (or
// skip-decode) already-transformed values.

type resourceInfo =
  | StorageKeys({partitionKey: string, sortKey: option<string>})
  | StreamSource({sourceUrn: string})

type resource = {name: string, resourceInfo: resourceInfo}
type wrapper = {resources: array<resource>}

let makeUnionSchema = (): S.t<resourceInfo> => {
  let sortKeySchema = S.string->S.nullAsOption
  let storageKeysSchema: S.t<resourceInfo> = S.schema(s =>
    {
      "TAG": "StorageKeys",
      "partitionKey": s.matches(S.string),
      "sortKey": s.matches(sortKeySchema),
    }
  )->Obj.magic
  let streamSourceSchema: S.t<resourceInfo> = S.schema(s =>
    {
      "TAG": "StreamSource",
      "sourceUrn": s.matches(S.string),
    }
  )->Obj.magic
  S.union([storageKeysSchema, streamSourceSchema])
}

// The bug's signature in codegen: a second item loop re-decoding the first
// loop's output. Applies to non-JSON `.to` targets too.
let countItemLoops = (fn: 'a => 'b): int => {
  let code: string = (fn->Obj.magic)["toString"]()
  code->String.split("for(let ")->Array.length - 1
}

test("Encode array(nullAsOption) to a non-JSON null-accepting target compiles a single item loop", t => {
  let fn = S.decoder(~from=S.array(S.string->S.nullAsOption), ~to=S.array(S.null(S.string)))
  t->Assert.is(fn->countItemLoops, 1)
  t->Assert.deepEqual(fn([None, Some("x")]), %raw(`[null, "x"]`))
})

test("Encode array(nullAsOption) to JSON", t => {
  // Guards a compile-time "Can't decode string | undefined to JSON"
  let fn = S.decoder(~from=S.array(S.string->S.nullAsOption), ~to=S.json)
  t->Assert.is(fn->countItemLoops, 1)
  t->Assert.deepEqual(fn([None, Some("x")]), %raw(`[null, "x"]`))
})

test("Encode dict(nullAsOption) to JSON (objectDecoder dict branch)", t => {
  // Guards a compile-time "Can't decode string | undefined to JSON"
  let fn = S.decoder(~from=S.dict(S.string->S.nullAsOption), ~to=S.json)
  t->Assert.deepEqual(
    fn(%raw(`{a: undefined, b: "x"}`)),
    %raw(`{a: null, b: "x"}`),
  )
})

test("Encode array(multi-variant union with nullAsOption field) to JSON in a single pass", t => {
  let fn = S.decoder(~from=S.array(makeUnionSchema()), ~to=S.json)
  t->Assert.is(fn->countItemLoops, 1)

  let input: array<resourceInfo> = [
    StorageKeys({partitionKey: "a", sortKey: None}),
    StreamSource({sourceUrn: "urn"}),
  ]
  t->Assert.deepEqual(
    fn(input),
    %raw(`[
      {TAG: "StorageKeys", partitionKey: "a", sortKey: null},
      {TAG: "StreamSource", sourceUrn: "urn"},
    ]`),
  )
})

test("Full issue shape (object > array > record > multi-variant union) round-trips through JSON", t => {
  let resourceSchema = S.schema(s =>
    {
      name: s.matches(S.string),
      resourceInfo: s.matches(makeUnionSchema()),
    }
  )
  let wrapperSchema = S.schema(s =>
    {
      resources: s.matches(S.array(resourceSchema)),
    }
  )

  let many = {
    resources: [{name: "r", resourceInfo: StorageKeys({partitionKey: "id", sortKey: None})}],
  }
  let encoded = many->S.decodeOrThrow(~from=wrapperSchema, ~to=S.json)
  t->Assert.deepEqual(
    encoded,
    %raw(`{resources: [{name: "r", resourceInfo: {TAG: "StorageKeys", partitionKey: "id", sortKey: null}}]}`),
  )
  t->Assert.deepEqual(encoded->S.parseOrThrow(~to=wrapperSchema), many)
})

// S.object callbacks returning variant constructors — what sury-ppx
// generates; routes encoding through the shaped serializer, unlike the
// S.schema style above
let makeShapedUnion = () =>
  S.union([
    S.object(s => {
      s.tag("TAG", "StorageKeys")
      StorageKeys({
        partitionKey: s.field("partitionKey", S.string),
        sortKey: s.field("sortKey", S.string->S.nullAsOption),
      })
    }),
    S.object(s => {
      s.tag("TAG", "StreamSource")
      StreamSource({sourceUrn: s.field("sourceUrn", S.string)})
    }),
  ])

test("S.object-style variant union (ppx shape) in an object encodes to JSON", t => {
  let schema = S.schema(s => {"u": s.matches(makeShapedUnion())})
  let value = {"u": StorageKeys({partitionKey: "id", sortKey: None})}
  let encoded = value->S.decodeOrThrow(~from=schema, ~to=S.json)
  t->Assert.deepEqual(
    encoded,
    %raw(`{u: {TAG: "StorageKeys", partitionKey: "id", sortKey: null}}`),
  )
  t->Assert.deepEqual(encoded->S.parseOrThrow(~to=schema), value)
})

test("S.object-style variant union (ppx shape) in array in object round-trips through JSON", t => {
  let wrapper = S.schema(s => {"resources": s.matches(S.array(makeShapedUnion()))})
  let value = {"resources": [StorageKeys({partitionKey: "id", sortKey: None})]}
  let encoded = value->S.decodeOrThrow(~from=wrapper, ~to=S.json)
  t->Assert.deepEqual(
    encoded,
    %raw(`{resources: [{TAG: "StorageKeys", partitionKey: "id", sortKey: null}]}`),
  )
  t->Assert.deepEqual(encoded->S.parseOrThrow(~to=wrapper), value)
})

test("Parse with .to(S.json) after array of coercing items doesn't leak non-JSON values", t => {
  // Guards the inverse failure mode: a stale item schema let the JSON segment
  // fast-path raw bigints into a value typed as JSON.t
  let schema = S.array(S.string->S.to(S.bigint))->S.to(S.json)
  let result = %raw(`["1", "2"]`)->S.parseOrThrow(~to=schema)
  t->Assert.deepEqual(result, %raw(`["1", "2"]`))
  t->Assert.is(%raw(`typeof result[0]`), "string")
})

test("Encode nested arrays of nullAsOption to JSON", t => {
  let schema = S.array(S.array(S.string->S.nullAsOption))
  t->Assert.deepEqual(
    [[None, Some("x")]]->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`[[null, "x"]]`),
  )
})

test("Encode array(dict(nullAsOption)) to JSON", t => {
  let schema = S.array(S.dict(S.string->S.nullAsOption))
  t->Assert.deepEqual(
    [dict{"a": None, "b": Some("x")}]->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`[{a: null, b: "x"}]`),
  )
})

test("Encode union of array(nullAsOption) and string to JSON", t => {
  // Guards a union dispatch failure on the array variant
  let schema = S.union([
    S.array(S.string->S.nullAsOption)->S.castToUnknown,
    S.string->S.castToUnknown,
  ])
  t->Assert.deepEqual(
    %raw(`[undefined, "x"]`)->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`[null, "x"]`),
  )
  t->Assert.deepEqual(
    %raw(`"plain"`)->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`"plain"`),
  )
})

test("Encode array(nullAsOption) to jsonString", t => {
  let schema = S.array(S.string->S.nullAsOption)
  t->Assert.deepEqual(
    [None, Some("x")]->S.decodeOrThrow(~from=schema, ~to=S.jsonString),
    `[null,"x"]`,
  )
})

test("Encode list(nullAsOption) to JSON", t => {
  let schema = S.list(S.string->S.nullAsOption)
  t->Assert.deepEqual(
    list{None, Some("x")}->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`[null, "x"]`),
  )
})

test("Encode refined array(nullAsOption) to JSON", t => {
  let schema = S.array(S.string->S.nullAsOption)->S.maxLength(3)
  t->Assert.deepEqual(
    [None, Some("x")]->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`[null, "x"]`),
  )
})

// Controls: static-items paths that were never affected

test("Encode tuple2(string, nullAsOption) to JSON (static-items control)", t => {
  let schema = S.tuple2(S.string, S.string->S.nullAsOption)
  t->Assert.deepEqual(
    ("a", None)->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`["a", null]`),
  )
})

test("Encode recursive tree with array(nullAsOption-bearing nodes) to JSON (control)", t => {
  let nodeSchema = S.recursive("Node284", node =>
    S.schema(s =>
      {
        "value": s.matches(S.string->S.nullAsOption),
        "children": s.matches(S.array(node)),
      }
    )
  )
  let tree = {
    "value": None,
    "children": [{"value": Some("x"), "children": []}],
  }
  t->Assert.deepEqual(
    tree->S.decodeOrThrow(~from=nodeSchema, ~to=S.json),
    %raw(`{value: null, children: [{value: "x", children: []}]}`),
  )
})
