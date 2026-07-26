open Vitest

// https://github.com/DZakh/sury/issues/311 — encoding a nested optional
// (or nullable-as-option) field to JSON failed with "Expected undefined |
// JSON, received ..." because an object field typed as a union with an
// undefined variant was checked against generic JSON instead of having
// each variant converted recursively.
//
// How None serializes depends on whether the schema can represent it by
// absence: `option` and `nullableAsOption` accept undefined/missing, so None
// serializes as an omitted field (round-trips back to None); `nullAsOption`
// has only a `null` representation, so None serializes as JSON `null`.

type inner = {s: option<string>}
type outer = {a: option<inner>}

let makeNullableSchemas = () => {
  let innerSchema = S.schema(m => {s: m.matches(S.nullableAsOption(S.string))})
  let outerSchema = S.schema(m => {a: m.matches(S.nullableAsOption(innerSchema))})
  outerSchema
}

test("Nested nullable option encodes to JSON string (issue shape)", t => {
  let outerSchema = makeNullableSchemas()
  t->Assert.deepEqual(
    {a: Some({s: None})}->S.decodeOrThrow(~from=outerSchema, ~to=S.jsonString),
    `{"a":{}}`,
  )
  t->Assert.deepEqual(
    {a: Some({s: Some("x")})}->S.decodeOrThrow(~from=outerSchema, ~to=S.jsonString),
    `{"a":{"s":"x"}}`,
  )
  t->Assert.deepEqual({a: None}->S.decodeOrThrow(~from=outerSchema, ~to=S.jsonString), `{}`)
})

test("Nested nullable option round-trips through S.json", t => {
  let outerSchema = makeNullableSchemas()
  let value = {a: Some({s: None})}
  let encoded = value->S.decodeOrThrow(~from=outerSchema, ~to=S.json)
  t->Assert.deepEqual(encoded, %raw(`{a: {}}`))
  t->Assert.deepEqual(encoded->S.parseOrThrow(~to=outerSchema), value)
})

test("Field-level JSON encoding: option/nullableAsOption omit None, nullAsOption emits null", t => {
  // The crux of #311: a schema that can represent None by absence omits it;
  // nullAsOption can't (no undefined arm), so its None becomes JSON null.
  let optionSchema = S.schema(m => {"v": m.matches(S.option(S.string))})
  let nullableAsOptionSchema = S.schema(m => {"v": m.matches(S.nullableAsOption(S.string))})
  let nullAsOptionSchema = S.schema(m => {"v": m.matches(S.nullAsOption(S.string))})

  t->Assert.deepEqual({"v": None}->S.decodeOrThrow(~from=optionSchema, ~to=S.json), %raw(`{}`))
  t->Assert.deepEqual(
    {"v": None}->S.decodeOrThrow(~from=nullableAsOptionSchema, ~to=S.json),
    %raw(`{}`),
  )
  t->Assert.deepEqual(
    {"v": None}->S.decodeOrThrow(~from=nullAsOptionSchema, ~to=S.json),
    %raw(`{v: null}`),
  )

  // Present values serialize the same for all three.
  t->Assert.deepEqual(
    {"v": Some("x")}->S.decodeOrThrow(~from=nullableAsOptionSchema, ~to=S.json),
    %raw(`{v: "x"}`),
  )
})

test("nullAsOption nested inside an option object field emits null, not omit", t => {
  let innerSchema = S.schema(m => {"s": m.matches(S.nullAsOption(S.string))})
  let outerSchema = S.schema(m => {"a": m.matches(S.option(innerSchema))})
  t->Assert.deepEqual(
    {"a": Some({"s": None})}->S.decodeOrThrow(~from=outerSchema, ~to=S.json),
    %raw(`{a: {s: null}}`),
  )
})

test("Plain optional object field with an optional field encodes to JSON", t => {
  let innerSchema = S.schema(m => {s: m.matches(S.option(S.string))})
  let outerSchema = S.schema(m => {a: m.matches(S.option(innerSchema))})
  t->Assert.deepEqual(
    {a: Some({s: None})}->S.decodeOrThrow(~from=outerSchema, ~to=S.json),
    %raw(`{a: {}}`),
  )
  t->Assert.deepEqual({a: None}->S.decodeOrThrow(~from=outerSchema, ~to=S.json), %raw(`{}`))
})

type dictInner = {name: string, counter: option<string>}
type dictOuter = {items: option<dict<dictInner>>}

test("Optional dict of objects with optional fields encodes to JSON (issue comment repro)", t => {
  let innerSchema = S.schema(m =>
    {
      name: m.matches(S.string),
      counter: m.matches(S.option(S.string)),
    }
  )
  let outerSchema = S.schema(m => {items: m.matches(S.option(S.dict(innerSchema)))})
  let value = {items: Some(Dict.fromArray([("a", {name: "x", counter: None})]))}
  t->Assert.deepEqual(
    value->S.decodeOrThrow(~from=outerSchema, ~to=S.json),
    %raw(`{items: {a: {name: "x"}}}`),
  )
})

test("Optional array field of objects with optional fields encodes to JSON", t => {
  let innerSchema = S.schema(m => {s: m.matches(S.option(S.string))})
  let schema = S.schema(m => {"list": m.matches(S.option(S.array(innerSchema)))})
  t->Assert.deepEqual(
    {"list": Some([{s: None}, {s: Some("x")}])}->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`{list: [{}, {s: "x"}]}`),
  )
})

test("Optional non-jsonable field converts per variant instead of leaking through", t => {
  let schema = S.schema(m => {"d": m.matches(S.option(S.bigint))})
  t->Assert.deepEqual(
    {"d": Some(5n)}->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`{d: "5"}`),
  )
  t->Assert.deepEqual({"d": None}->S.decodeOrThrow(~from=schema, ~to=S.json), %raw(`{}`))
})

test("Jsonable optional field no longer runs a redundant deep JSON validation", t => {
  let innerSchema = S.schema(m => {s: m.matches(S.option(S.string))})
  let outerSchema = S.schema(m => {a: m.matches(S.option(innerSchema))})
  t->U.assertCompiledCode(
    ~schema=outerSchema,
    ~op=#EncodeToJson,
    `i=>{let v0=i["a"];if(typeof v0==="object"&&v0&&!Array.isArray(v0)){let v1=v0["s"];if(!(typeof v1==="string"||v1===void 0)){e[0](v1)}let v2={};if(v1!==void 0){v2["s"]=v1}v0=v2}else if(!(v0===void 0)){e[1](v0)}let v3={};if(v0!==void 0){v3["a"]=v0}return v3}`,
  )
})
