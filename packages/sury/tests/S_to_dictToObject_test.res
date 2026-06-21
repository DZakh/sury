open Vitest

// Tracks support for coercing a `dict<string>` into a structured object whose
// fields need their own coercion (string -> bigint) and include an optional
// field (string -> option<float>):
//
//   S.dict(S.string)->S.to(S.schema({
//     foo: S.string,
//     bar: S.bigint,
//     zoo: S.option(S.float),
//   }))
//
// Milestone 1 is implemented via "Option A": a `dict<V>` is
// `additionalProperties: V` with no required keys, so a value read by key may be
// absent. `B.Val.get` reads each additionalProperties value and models that read
// as optional (`option<V>`) when `V` is a concrete type that can't itself be
// undefined. The existing union coercion then handles a missing key uniformly:
//   - optional target field  -> absence decodes to None
//   - required `string` field -> absence errors: the `string` source variant
//     claims the `string` target (tier 1, exclusive), so the reserved `unit` arm
//     has no target and can't stringify to "undefined" (the "tier fix")
//   - required `bigint` field -> absence errors (no undefined coercion)
//
// The remaining gap (encoding back to dict<string>) is still pinned against its
// current crash with a FIXME for milestone 2-3.

let makeSchema = () =>
  S.dict(S.string)->S.to(
    S.schema(s =>
      {
        "foo": s.matches(S.string),
        "bar": s.matches(S.bigint),
        "zoo": s.matches(S.option(S.float)),
      }
    ),
  )

test("Parses input with every field present", t => {
  let schema = makeSchema()

  t->Assert.deepEqual(
    %raw(`{"foo":"a","bar":"123","zoo":"1.5"}`)->S.parseOrThrow(~to=schema),
    {"foo": "a", "bar": 123n, "zoo": Some(1.5)},
  )
})

test("[milestone 1] absent optional field decodes to None", t => {
  let schema = makeSchema()

  // A missing `zoo` key is an absent additionalProperty -> None (no longer throws).
  t->Assert.deepEqual(
    %raw(`{"foo":"a","bar":"7"}`)->S.parseOrThrow(~to=schema),
    {"foo": "a", "bar": 7n, "zoo": None},
  )
})

test("[milestone 1] absent required bigint field errors", t => {
  let schema = makeSchema()

  // Modeling the read as `option<string>` doesn't loosen required fields whose
  // target can't accept undefined: a missing `bar` (coerced to bigint) errors.
  t->U.assertThrowsMessage(
    () => %raw(`{"foo":"a","zoo":"1.5"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["bar"]: Expected string | undefined, received undefined
- At ["bar"]: Can't decode undefined to bigint. Use S.to to define a custom decoder`,
  )
})

test("[milestone 1] absent required string field errors (tier fix)", t => {
  let schema = makeSchema()

  // A missing `foo` (required string) reads as `option<string>`. The `string`
  // source variant claims the `string` target (tier 1, exclusive), so the absent
  // `unit` arm has no target left and errors instead of stringifying to
  // "undefined" — the tier fix that tightens the old None <-> "undefined" sentinel.
  t->U.assertThrowsMessage(
    () => %raw(`{"bar":"7","zoo":"1.5"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["foo"]: Expected never, received undefined`,
  )
})

test("the literal string \"undefined\" no longer decodes to None (sentinel removed)", t => {
  let schema = makeSchema()

  // `zoo` is `option<float>`; its `unit`/None arm is reserved by the absent
  // `undefined` read, so the present string "undefined" can't reach it via tier-3
  // stringified-const coercion. It is parsed as a number (`+"undefined"` = NaN)
  // and errors — the surprising string sentinel is gone.
  t->U.assertThrowsMessage(
    () => %raw(`{"foo":"a","bar":"123","zoo":"undefined"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["zoo"]: Expected number, received "undefined"`,
  )
})

test("[milestone 1] compiled parse code models each dict read as optional", t => {
  let schema = makeSchema()

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[9](i);for(let v0 in i){try{let v1=i[v0];typeof v1==="string"||e[0](v1);}catch(v2){v2.path='["'+v0+'"]'+v2.path;throw v2}}let v3=i["foo"],v5=i["bar"],v7=i["zoo"];if(v3===void 0){e[1](v3);}else if(!(typeof v3==="string")){e[2](v3)}if(typeof v5==="string"){let v4;try{v4=BigInt(v5)}catch(_){e[3](v5)}v5=v4}else if(v5===void 0){e[5](v5,e[4])}else{e[6](v5)}if(typeof v7==="string"){let v6=+v7;!Number.isNaN(v6)||e[7](v7);v7=v6}else if(!(v7===void 0)){e[8](v7)}return {"foo":v3,"bar":v5,"zoo":v7,}}`,
  )
})

test("FIXME [milestone 2-3] encoding back to dict<string> should round-trip", t => {
  let schema = makeSchema()

  // SHOULD: encode the object back into a dict of strings (None -> absent key)
  //   t->Assert.deepEqual(
  //     {"foo": "a", "bar": 123n, "zoo": Some(1.5)}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
  //     %raw(`{"foo":"a","bar":"123","zoo":"1.5"}`),
  //   )
  //   t->U.assertReverseParsesBack(schema, {"foo": "a", "bar": 7n, "zoo": None})
  // ACTUAL (broken): building the object->dict reverse crashes at *build* time.
  // A fixed-property object's `additionalItems` mode (Strip/Strict) is cast to a
  // schema in `B.dynamicScope`, then reaches `isLiteral` (`"const" in "strip"`).
  t->Assert.throws(
    () => {
      let _ = S.decoder(~from=schema, ~to=S.unknown)
    },
    ~expectations={message: "Cannot use 'in' operator to search for 'const' in"},
  )
})
