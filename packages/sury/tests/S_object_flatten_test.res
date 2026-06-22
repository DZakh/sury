open Vitest

test("Has correct tagged type", t => {
  let schema = S.object(s =>
    {
      "bar": s.flatten(S.object(s => s.field("bar", S.string))),
      "foo": s.field("foo", S.string),
    }
  )

  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["bar"],v1=i["foo"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"bar":v0,"foo":v1,}}`,
  )
})

test("Can flatten S.schema", t => {
  let schema = S.object(s => {
    {
      "baz": s.flatten(
        S.schema(
          s =>
            {
              "bar": s.matches(S.string),
            },
        ),
      ),
      "foo": s.field("foo", S.string),
    }
  })

  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["bar"],v1=i["foo"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"baz":{"bar":v0,},"foo":v1,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{let v0=i["baz"];return {"bar":v0["bar"],"foo":i["foo"],}}`,
  )
})

test("Can flatten & destructure S.schema", t => {
  let schema = S.object(s => {
    let flattened = s.flatten(S.schema(s => {"bar": s.matches(S.string)}))
    {
      "bar": flattened["bar"],
      "foo": s.field("foo", S.string),
    }
  })

  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["bar"],v1=i["foo"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"bar":v0,"foo":v1,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{return {"bar":i["bar"],"foo":i["foo"],}}`,
  )
})

test("Can flatten strict object", t => {
  let schema = S.object(s =>
    {
      "bar": s.flatten(S.object(s => s.field("bar", S.string))->S.strict),
      "foo": s.field("foo", S.string),
    }
  )

  t->Assert.deepEqual(
    switch schema {
    | Object({additionalItems}) => additionalItems
    | _ => assert(false)
    },
    S.Strip,
  )
  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["bar"],v1=i["foo"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"bar":v0,"foo":v1,}}`,
  )
})

test("Flatten inside of a strict object", t => {
  let schema = S.object(s =>
    {
      "bar": s.flatten(S.object(s => s.field("bar", S.string))),
      "foo": s.field("foo", S.string),
    }
  )->S.strict

  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[3](i);let v0=i["bar"],v1=i["foo"],v2;typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);for(v2 in i){if(v2!=="bar"&&v2!=="foo"){e[2](v2)}}return {"bar":v0,"foo":v1,}}`,
  )
})

// FIXME: Should work
test("Flatten schema with duplicated field of the same type (flatten first)", t => {
  t->Assert.throws(
    () => {
      S.object(
        s =>
          {
            "bar": s.flatten(S.object(s => s.field("foo", S.string))),
            "foo": s.field("foo", S.string),
          },
      )
    },
    ~expectations={
      message: `[Sury] The field "foo" defined twice with incompatible schemas`,
    },
  )
})

test("Flatten schema with duplicated field of the same type (flatten last)", t => {
  let schema = S.object(s =>
    {
      "foo": s.field("foo", S.string),
      "bar": s.flatten(S.object(s => s.field("foo", S.string))),
    }
  )

  t->U.assertReverseReversesBack(schema)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[1](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);return {"foo":v0,"bar":v0,}}`,
  )
  // FIXME: Should validate that the fields are equal and choose the right one depending on the order
  t->U.assertCompiledCode(~schema, ~op=#Encode, `i=>{return {"foo":i["bar"],}}`)
})

test("Flatten schema with duplicated field of different type", t => {
  t->Assert.throws(
    () => {
      S.object(
        s =>
          {
            "bar": s.flatten(S.object(s => s.field("foo", S.string))),
            "foo": s.field("foo", S.email),
          },
      )
    },
    ~expectations={
      message: `[Sury] The field "foo" defined twice with incompatible schemas`,
    },
  )
})

test("Can flatten renamed object schema", t => {
  let schema = S.object(s =>
    {
      "bar": s.flatten(S.object(s => s.field("bar", S.string))->S.meta({name: "My Obj"})),
      "foo": s.field("foo", S.string),
    }
  )

  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[2](i);let v0=i["bar"],v1=i["foo"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);return {"bar":v0,"foo":v1,}}`,
  )
  t->Assert.is(schema->S.toExpression, `{ bar: string; foo: string; }`)
})

test("Can flatten transformed object schema", t => {
  let schema = S.object(s =>
    {
      "bar": s.flatten(S.object(s => s.field("bar", S.string))->S.transform(_ => {parser: i => i})),
      "foo": s.field("foo", S.string),
    }
  )
  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[4](i);let v0=i["bar"],v1=i["foo"];typeof v0==="string"||e[0](v0);typeof v1==="string"||e[1](v1);let v2;try{v2=e[2](v0)}catch(x){e[3](x)}return {"bar":v2,"foo":v1,}}`,
  )
})

// https://github.com/DZakh/sury/issues/271
test("Flattened object with a transformed field decodes the field once", t => {
  let fieldSchema = S.string->S.transform(_ => {
    parser: s => s ++ "!",
    serializer: s => s->String.replaceAll("!", ""),
  })
  let inner = S.object(s => {"createdAt": s.field("createdAt", fieldSchema)})
  let schema = S.object(s => {"properties": s.flatten(inner)})

  // Before the fix the flattened field's transform ran twice ("x!!"), and a
  // string|Date transform like S.date would throw "Expected string, received
  // [object Date]" on the second run.
  t->Assert.deepEqual(
    %raw(`{"createdAt": "x"}`)->S.parseOrThrow(~to=schema),
    %raw(`{"properties": {"createdAt": "x!"}}`),
  )
  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[3](i);let v1=i["createdAt"];typeof v1==="string"||e[2](v1);let v0;try{v0=e[0](i["createdAt"])}catch(x){e[1](x)}return {"properties":{"createdAt":v0,},}}`,
  )
})

// https://github.com/DZakh/sury/issues/271
// A flattened S.schema with an identity definition has no `.to`, but a
// transformed field still must not be decoded twice.
test("Flattened schema without a reshape decodes a transformed field once", t => {
  let fieldSchema = S.string->S.transform(_ => {
    parser: s => s ++ "!",
    serializer: s => s->String.replaceAll("!", ""),
  })
  let schema = S.object(s => {"wrap": s.flatten(S.schema(s => {"name": s.matches(fieldSchema)}))})

  t->Assert.deepEqual(
    %raw(`{"name": "x"}`)->S.parseOrThrow(~to=schema),
    %raw(`{"wrap": {"name": "x!"}}`),
  )
  t->U.assertReverseReversesBack(schema)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[3](i);let v1=i["name"];typeof v1==="string"||e[2](v1);let v0;try{v0=e[0](i["name"])}catch(x){e[1](x)}return {"wrap":{"name":v0,},}}`,
  )
})

// A flattened schema without a reshape can still carry refiners; reusing the
// decoded fields must not drop them.
test("Flattened schema without a reshape keeps its refiners", t => {
  let inner =
    S.schema(s => {"n": s.matches(S.int)})->S.refine(value => value["n"] >= 0, ~error="must be non-negative")
  let schema = S.object(s => {"wrap": s.flatten(inner)})

  t->Assert.deepEqual(%raw(`{"n": 1}`)->S.parseOrThrow(~to=schema), %raw(`{"wrap": {"n": 1}}`))
  // The flattened object's fields live at the input root (there is no "wrap"
  // nesting in the input — that only exists in the output shape), so the
  // refiner reports at the root path with no prefix.
  t->U.assertThrowsMessage(
    () => %raw(`{"n": -1}`)->S.parseOrThrow(~to=schema),
    "must be non-negative",
  )
})

test("Fails to flatten non-object schema", t => {
  t->Assert.throws(
    () => {
      S.object(
        s =>
          {
            "foo": s.field("foo", S.string),
            "bar": s.flatten(S.string),
          },
      )
    },
    ~expectations={
      message: `[Sury] The 'string' schema can\'t be flattened`,
    },
  )
})

test("Successfully serializes simple object with flatten", t => {
  let schema = S.object(s =>
    {
      "foo": s.field("foo", S.string),
      "bar": s.flatten(S.object(s => s.field("bar", S.string))),
    }
  )

  t->Assert.deepEqual(
    {"foo": "foo", "bar": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"foo": "foo", "bar": "bar"}`),
  )
  t->U.assertCompiledCode(
    ~op=#Encode,
    ~schema,
    `i=>{return {"bar":i["bar"],"foo":i["foo"],}}`,
  )
})

type entityData = {
  name: string,
  age: int,
}
type entity = {
  id: string,
  ...entityData,
}

test("Can destructure flattened schema", t => {
  let entityDataSchema = S.object(s => {
    name: s.field("name", S.string),
    age: s.field("age", S.int),
  })
  let entitySchema = S.object(s => {
    let {name, age} = s.flatten(entityDataSchema)
    {
      id: s.field("id", S.string),
      name,
      age,
    }
  })

  t->U.assertCompiledCode(
    ~op=#Parse,
    ~schema=entitySchema,
    `i=>{typeof i==="object"&&i||e[3](i);let v0=i["name"],v1=i["age"],v2=i["id"];typeof v0==="string"||e[0](v0);typeof v1==="number"&&v1<=2147483647&&v1>=-2147483648&&v1%1===0||e[1](v1);typeof v2==="string"||e[2](v2);return {"id":v2,"name":v0,"age":v1,}}`,
  )

  t->Assert.deepEqual(
    {id: "1", name: "Dmitry", age: 23}->S.decodeOrThrow(~from=entitySchema, ~to=S.json),
    %raw(`{id: "1", name: "Dmitry", age: 23}`),
  )
  t->U.assertCompiledCode(
    ~op=#Encode,
    ~schema=entitySchema,
    `i=>{return {"name":i["name"],"age":i["age"],"id":i["id"],}}`,
  )
  t->U.assertCompiledCode(
    ~op=#EncodeToJson,
    ~schema=entitySchema,
    `i=>{return {"name":i["name"],"age":i["age"],"id":i["id"],}}`,
  )
})
