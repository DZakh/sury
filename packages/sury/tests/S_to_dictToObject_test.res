open Vitest

test("absent required string fails (does not become the string \"undefined\")", t => {
  let schema = S.dict(S.string)->S.to(S.schema(s => {"foo": s.matches(S.string)}))
  t->U.assertThrowsMessage(
    () => %raw(`{}`)->S.parseOrThrow(~to=schema),
    `Failed at foo: Expected string, received undefined`,
  )
})

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
//   - required field (string, bigint, …) -> absence errors
//
// Milestone 2 adds the encode direction (object -> dict<string>): objectDecoder
// now recognises a fixed-property object source feeding a dict target and reuses
// the static object-literal construction, driven by the source's known keys with
// every field coerced to the dict's value schema. A field that is still optional
// after coercion is dropped when absent by `completeObjectVal`; an optional
// source field coerced to a *required* value (e.g. `option<float>` -> `string`)
// keeps its key, with `None` encoded to the "undefined" sentinel (the mirror of
// the decode side). Tightening that to an absent key is a deferred tier fix.

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
    `Failed at bar: Expected bigint, received undefined`,
  )
})

test("[milestone 1] absent required string field errors", t => {
  let schema = makeSchema()

  t->U.assertThrowsMessage(
    () => %raw(`{"bar":"7","zoo":"1.5"}`)->S.parseOrThrow(~to=schema),
    `Failed at foo: Expected string, received undefined`,
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
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[7](i);for(let v0 in i){try{let v1=i[v0];typeof v1==="string"||e[0](v1);}catch(v2){v2.path=[v0,...v2.path];throw v2}}let v3=i["foo"],v4=i["bar"],v6=i["zoo"];v3!==void 0||e[1](v3);if(v4!==void 0){let v5;try{v5=BigInt(v4)}catch(_){e[2](v4)}v4=v5;}else{e[3](v4)}if(v6!==void 0){for(;;){let r;try{let v7=+v6;v7===v7||e[4](v6);v6=v7;break}catch(x){(r||(r=[])).push(e[5](x))}if(v6==="undefined"){v6=void 0;break}e[6](v6,...(r||[]))}}return {foo:v3,bar:v4,zoo:v6}}`,
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
    `i=>{let v0=i["zoo"];for(;;){if(typeof v0==="number"&&v0===v0){v0=""+i["zoo"];break}if(v0===void 0){v0="undefined";break}e[0](v0)}return {foo:i["foo"],bar:""+i["bar"],zoo:v0}}`,
  )
})

test("[milestone 2] coerces into a dict whose value is itself a transforming object", t => {
  // The dict value (`inner`) is a composite with its own transform, so the field
  // flows through two fused `.to` stages. This previously crashed with a phantom
  // `ReferenceError: v3 is not defined` (the shared `.to`-fusion bug, now fixed
  // by the `finalized` re-read in `_notVar`).
  let inner = () => S.schema(s => {"a": s.matches(S.int->S.to(S.string))})
  // Decoding this schema fuses object{foo:inner} -> dict(inner): the object's
  // field value flows through inner's transform and then into the dict value's
  // own decode. That second stage re-reads the first's already-emitted output.
  let schema = S.schema(s => {"foo": s.matches(inner())})->S.to(S.dict(inner()))

  t->Assert.deepEqual(
    %raw(`{"foo":{"a":5}}`)->S.parseOrThrow(~to=schema),
    %raw(`{"foo":{"a":"5"}}`),
  )
})
