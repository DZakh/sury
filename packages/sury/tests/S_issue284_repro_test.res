open Vitest

// Reproduction for https://github.com/DZakh/sury/issues/284
//
// arrayDecoder's dynamic-item branch (and objectDecoder's dict branch) used to
// set the output val's `schema` to the *pre-transform* expected schema instead
// of the item-output schema. Since `val.schema` is the type context the next
// `.to` segment decodes from, every downstream decoder (S.json's isJsonable
// gate, another array target, ...) judged the already-transformed value
// against a stale type: a second item loop was emitted that re-decoded the
// output of the first, throwing on the very `null` it just produced.

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

// The stale schema also poisoned non-JSON `.to` targets: this pair compiled
// two sequential item loops (encode, then a spurious re-decode) before the fix.
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
  // Before the fix this failed to compile at all:
  // "Can't decode string | undefined to JSON"
  let fn = S.decoder(~from=S.array(S.string->S.nullAsOption), ~to=S.json)
  t->Assert.is(fn->countItemLoops, 1)
  t->Assert.deepEqual(fn([None, Some("x")]), %raw(`[null, "x"]`))
})

test("Encode dict(nullAsOption) to JSON (objectDecoder dict branch)", t => {
  // Before the fix this failed to compile at all:
  // "Can't decode string | undefined to JSON"
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
