open Vitest

// https://github.com/DZakh/sury/issues/311 — encoding a nested optional
// (or nullable-as-option) field to JSON failed with "Expected undefined |
// JSON, received ..." because an object field typed as a union with an
// undefined variant was checked against generic JSON instead of having
// each variant converted recursively.

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
    `i=>{let v0=i["a"];if(typeof v0==="object"&&v0&&!Array.isArray(v0)){let v1=v0["s"];if(!(typeof v1==="string"||v1===void 0)){e[0](v1)}let v3={"s":v1,};try{let v2={};if(v1!==void 0){v2["s"]=v1}v3=v2}catch(e0){e[1](v3,e0)}v0=v3}else if(!(v0===void 0)){e[2](v0)}let v4={};if(v0!==void 0){v4["a"]=v0}return v4}`,
  )
})
