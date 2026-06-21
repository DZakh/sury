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
//   - required `string` field -> absence stringifies to "undefined" (the
//     reversibility-locked None <-> "undefined" sentinel; tightening this is a
//     deferred "tier fix")
//   - required `bigint` field -> absence errors (no undefined coercion)
//
// Milestone 2 adds the encode direction (object -> dict<string>): objectDecoder
// now recognises a fixed-property object source feeding a dict target and reuses
// the static object-literal construction, driven by the source's known keys with
// every field coerced to the dict's value schema. `completeObjectVal` drops
// absent optional fields, so a `None` source field is handled for free.
// (Encoding `None` to an absent key rather than the "undefined" sentinel is a
// deferred tier fix.)

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

test("[milestone 1] absent required string field stringifies to \"undefined\" (deferred tier fix)", t => {
  let schema = makeSchema()

  // A missing `foo` (required string) flows through the option's undefined arm,
  // which the string coercion turns into the literal "undefined" — the mirror of
  // the `string -> option` `"undefined" <-> None` sentinel, locked in by
  // reversibility. Tightening this to an error is a deferred "tier fix".
  t->Assert.deepEqual(
    %raw(`{"bar":"7","zoo":"1.5"}`)->S.parseOrThrow(~to=schema),
    {"foo": "undefined", "bar": 7n, "zoo": Some(1.5)},
  )
})

test("the literal string \"undefined\" decodes to None (string sentinel)", t => {
  let schema = makeSchema()

  // Present-value coercion routes through the option's string arm, so the literal
  // string "undefined" maps to None as well — the same sentinel as above.
  t->Assert.deepEqual(
    %raw(`{"foo":"a","bar":"123","zoo":"undefined"}`)->S.parseOrThrow(~to=schema),
    {"foo": "a", "bar": 123n, "zoo": None},
  )
})

test("[milestone 1] compiled parse code models each dict read as optional", t => {
  let schema = makeSchema()

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[9](i);for(let v0 in i){try{let v1=i[v0];typeof v1==="string"||e[0](v1);}catch(v2){v2.path='["'+v0+'"]'+v2.path;throw v2}}let v3=i["foo"],v5=i["bar"],v7=i["zoo"];if(v3===void 0){v3="undefined"}else if(!(typeof v3==="string")){e[1](v3)}if(typeof v5==="string"){let v4;try{v4=BigInt(v5)}catch(_){e[2](v5)}v5=v4}else if(v5===void 0){e[4](v5,e[3])}else{e[5](v5)}if(typeof v7==="string"){try{let v6=+v7;!Number.isNaN(v6)||e[6](v7);v7=v6}catch(e0){if(v7==="undefined"){v7=void 0}else{e[7](v7,e0)}}}else if(!(v7===void 0)){e[8](v7)}return {"foo":v3,"bar":v5,"zoo":v7,}}`,
  )
})

test("[milestone 2] encodes the object back into a dict of strings", t => {
  let schema = makeSchema()

  t->Assert.deepEqual(
    {"foo": "a", "bar": 123n, "zoo": Some(1.5)}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"foo":"a","bar":"123","zoo":"1.5"}`),
  )
})

test("[milestone 2] encode round-trips back through the schema", t => {
  let schema = makeSchema()

  t->U.assertReverseParsesBack(schema, {"foo": "a", "bar": 123n, "zoo": Some(1.5)})
  // `None` encodes to the "undefined" string sentinel (mirror of the decode
  // side), which the forward decoder maps back to `None`, so it still
  // round-trips. Encoding `None` to an absent key is a deferred tier fix.
  t->U.assertReverseParsesBack(schema, {"foo": "a", "bar": 7n, "zoo": None})
})

test("[milestone 2] compiled encode iterates the source object's fixed keys", t => {
  let schema = makeSchema()

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0=i["zoo"];if(typeof v0==="number"&&!Number.isNaN(v0)){v0=""+i["zoo"]}else if(v0===void 0){v0="undefined"}else{e[0](v0)}return {"foo":i["foo"],"bar":""+i["bar"],"zoo":v0,}}`,
  )
})
