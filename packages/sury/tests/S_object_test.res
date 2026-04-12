open Ava

S.enableJson()

@live
type options = {fast?: bool, mode?: int}

test("Successfully parses object with inlinable string field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{field: "bar"}`)->S.parseOrThrow(~to=schema), {"field": "bar"})
})

test("Fails to parse object with inlinable string field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: 123}`)->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: Expected string, received 123`,
  )
})

test(
  "Fails to parse object with custom user error in array field (should have correct path)",
  t => {
    let schema = S.object(s =>
      {
        "field": s.field("field", S.array(S.string->S.refine(_ => false, ~error="User error"))),
      }
    )

    t->U.assertThrowsMessage(
      () => %raw(`{field: ["foo"]}`)->S.parseOrThrow(~to=schema),
      `Failed at ["field"]["0"]: User error`,
    )
  },
)

test("Successfully parses object with inlinable bool field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.bool),
    }
  )

  t->Assert.deepEqual(%raw(`{field: true}`)->S.parseOrThrow(~to=schema), {"field": true})
})

test("Fails to parse object with inlinable bool field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.bool),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: 123}`)->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: Expected boolean, received 123`,
  )
})

test("Successfully parses object with unknown field (Noop operation)", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.unknown),
    }
  )

  t->Assert.deepEqual(
    %raw(`{field: new Date("2015-12-12")}`)->S.parseOrThrow(~to=schema),
    %raw(`{field: new Date("2015-12-12")}`),
  )
})

test("Successfully serializes object with unknown field (Noop operation)", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.unknown),
    }
  )

  t->Assert.deepEqual(
    %raw(`{field: new Date("2015-12-12")}`)->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{field: new Date("2015-12-12")}`),
  )
})

test("Fails to parse object with inlinable never field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.never),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: true}`)->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: Expected never, received true`,
  )
})

test("Successfully parses object with inlinable float field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.float),
    }
  )

  t->Assert.deepEqual(%raw(`{field: 123}`)->S.parseOrThrow(~to=schema), {"field": 123.})
})

test("Fails to parse object with inlinable float field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.float),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: true}`)->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: Expected number, received true`,
  )
})

test("Successfully parses object with inlinable int field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.int),
    }
  )

  t->Assert.deepEqual(%raw(`{field: 123}`)->S.parseOrThrow(~to=schema), {"field": 123})
})

test("Fails to parse object with inlinable int field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.int),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: true}`)->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: Expected int32, received true`,
  )
})

test("Successfully parses object with not inlinable empty object field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.object(_ => ())),
    }
  )

  t->Assert.deepEqual(%raw(`{field: {}}`)->S.parseOrThrow(~to=schema), {"field": ()})
})

test("Fails to parse object with not inlinable empty object field", t => {
  let fieldSchema = S.object(_ => ())
  let schema = S.object(s =>
    {
      "field": s.field("field", fieldSchema),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: true}`)->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: Expected {}, received true`,
  )
})

test("Fails to parse object when provided invalid data", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`12`)->S.parseOrThrow(~to=schema),
    `Expected { field: string; }, received 12`,
  )
})

test("Successfully serializes object with single field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual({"field": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{field: "bar"}`))
})

test("Successfully parses object with multiple fields", t => {
  let schema = S.object(s =>
    {
      "boo": s.field("boo", S.string),
      "zoo": s.field("zoo", S.string),
    }
  )

  t->Assert.deepEqual(
    %raw(`{boo: "bar", zoo: "jee"}`)->S.parseOrThrow(~to=schema),
    {"boo": "bar", "zoo": "jee"},
  )
})

test("Successfully serializes object with multiple fields", t => {
  let schema = S.object(s =>
    {
      "boo": s.field("boo", S.string),
      "zoo": s.field("zoo", S.string),
    }
  )

  t->Assert.deepEqual(
    {"boo": "bar", "zoo": "jee"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{boo: "bar", zoo: "jee"}`),
  )
})

test("Successfully parses object with transformed field", t => {
  let schema = S.object(s =>
    {
      "string": s.field(
        "string",
        S.string->S.transform(_ => {parser: string => string ++ "field"}),
      ),
    }
  )

  t->Assert.deepEqual(%raw(`{string: "bar"}`)->S.parseOrThrow(~to=schema), {"string": "barfield"})
})

test("Fails to parse object when transformed field has throws error", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string->S.transform(s => {parser: _ => s.fail("User error")})),
    }
  )

  t->U.assertThrowsMessage(
    () => {"field": "bar"}->S.parseOrThrow(~to=schema),
    `Failed at ["field"]: User error`,
  )
})

test("Shows transformed object field name in error path when fails to parse", t => {
  let schema = S.object(s =>
    {
      "transformedFieldName": s.field(
        "originalFieldName",
        S.string->S.transform(s => {parser: _ => s.fail("User error")}),
      ),
    }
  )

  t->U.assertThrowsMessage(
    () => {"originalFieldName": "bar"}->S.parseOrThrow(~to=schema),
    `Failed at ["originalFieldName"]: User error`,
  )
})

test("Successfully serializes object with transformed field", t => {
  let schema = S.object(s =>
    {
      "string": s.field(
        "string",
        S.string->S.transform(_ => {serializer: string => string ++ "field"}),
      ),
    }
  )

  t->Assert.deepEqual(
    {"string": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"string": "barfield"}`),
  )
})

test("Fails to serializes object when transformed field has throws error", t => {
  let schema = S.object(s =>
    {
      "field": s.field(
        "field",
        S.string->S.transform(s => {serializer: _ => s.fail("User error")}),
      ),
    }
  )

  t->U.assertThrowsMessage(
    () => {"field": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Failed at ["field"]: User error`,
  )
})

test("Shows transformed object field name in error path when fails to serializes", t => {
  let schema = S.object(s =>
    {
      "transformedFieldName": s.field(
        "originalFieldName",
        S.string->S.transform(s => {serializer: _ => s.fail("User error")}),
      ),
    }
  )

  t->U.assertThrowsMessage(
    () => {"transformedFieldName": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Failed at ["transformedFieldName"]: User error`,
  )
})

test("Shows transformed to nested object field name in error path when fails to serializes", t => {
  let schema = S.object(s =>
    {
      "v1": {
        "transformedFieldName": s.field(
          "originalFieldName",
          S.string->S.transform(s => {serializer: _ => s.fail("User error")}),
        ),
      },
    }
  )

  t->U.assertThrowsMessage(() =>
    {
      "v1": {
        "transformedFieldName": "bar",
      },
    }->S.decodeOrThrow(~from=schema, ~to=S.unknown)
  , `Failed at ["v1"]["transformedFieldName"]: User error`)
})

test("Successfully parses object with optional fields", t => {
  let schema = S.object(s =>
    {
      "boo": s.field("boo", S.option(S.string)),
      "zoo": s.field("zoo", S.option(S.string)),
    }
  )

  t->Assert.deepEqual(
    %raw(`{boo: "bar"}`)->S.parseOrThrow(~to=schema),
    {"boo": Some("bar"), "zoo": None},
  )
})

test("Successfully serializes object with optional fields", t => {
  let schema = S.object(s =>
    {
      "boo": s.field("boo", S.option(S.string)),
      "zoo": s.field("zoo", S.option(S.string)),
    }
  )

  t->Assert.deepEqual(
    {"boo": Some("bar"), "zoo": None}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{boo: "bar", zoo: undefined}`),
  )
})

test("Successfully parses object with optional fields with default", t => {
  let schema = S.object(s =>
    {
      "boo": s.fieldOr("boo", S.string, "default boo"),
      "zoo": s.fieldOr("zoo", S.string, "default zoo"),
    }
  )

  t->Assert.deepEqual(
    %raw(`{boo: "bar"}`)->S.parseOrThrow(~to=schema),
    {"boo": "bar", "zoo": "default zoo"},
  )
})

test("Successfully serializes object with optional fields with default", t => {
  let schema = S.object(s =>
    {
      "boo": s.fieldOr("boo", S.string, "default boo"),
      "zoo": s.fieldOr("zoo", S.string, "default zoo"),
    }
  )

  t->Assert.deepEqual(
    {"boo": "bar", "zoo": "baz"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{boo: "bar", zoo: "baz"}`),
  )
})

test(
  "Successfully parses object with optional fields using (?). The optinal field becomes undefined instead of beeing missing",
  t => {
    let optionsSchema = S.object(s => {
      {
        fast: ?s.field("fast", S.option(S.bool)),
        mode: s.field("mode", S.int),
      }
    })

    t->Assert.deepEqual(
      %raw(`{mode: 1}`)->S.parseOrThrow(~to=optionsSchema),
      {
        fast: %raw(`undefined`),
        mode: 1,
      },
    )
  },
)

test("Successfully serializes object with optional fields using (?)", t => {
  let optionsSchema = S.object(s => {
    {
      fast: ?s.field("fast", S.option(S.bool)),
      mode: s.field("mode", S.int),
    }
  })

  t->Assert.deepEqual(
    {mode: 1}->S.decodeOrThrow(~from=optionsSchema, ~to=S.unknown),
    %raw(`{mode: 1, fast: undefined}`),
  )
})

test("Successfully parses object with mapped field names", t => {
  let schema = S.object(s =>
    {
      "name": s.field("Name", S.string),
      "email": s.field("Email", S.string),
      "age": s.field("Age", S.int),
    }
  )

  t->Assert.deepEqual(
    %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.parseOrThrow(~to=schema),
    {"name": "Dmitry", "email": "dzakh.dev@gmail.com", "age": 21},
  )
})

test("Successfully serializes object with mapped field", t => {
  let schema = S.object(s =>
    {
      "name": s.field("Name", S.string),
      "email": s.field("Email", S.string),
      "age": s.field("Age", S.int),
    }
  )

  t->Assert.deepEqual(
    {"name": "Dmitry", "email": "dzakh.dev@gmail.com", "age": 21}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`),
  )
})

test("Successfully parses object transformed to tuple", t => {
  let schema = S.object(s => (s.field("boo", S.int), s.field("zoo", S.int)))

  t->Assert.deepEqual(%raw(`{boo: 1, zoo: 2}`)->S.parseOrThrow(~to=schema), (1, 2))
})

test("Successfully serializes object transformed to tuple", t => {
  let schema = S.object(s => (s.field("boo", S.int), s.field("zoo", S.int)))

  t->Assert.deepEqual((1, 2)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{boo: 1, zoo: 2}`))
})

test("Successfully parses object transformed to nested object", t => {
  let schema = S.object(s =>
    {
      "v1": {
        "boo": s.field("boo", S.int),
        "zoo": s.field("zoo", S.int),
      },
    }
  )

  t->Assert.deepEqual(
    %raw(`{boo: 1, zoo: 2}`)->S.parseOrThrow(~to=schema),
    {"v1": {"boo": 1, "zoo": 2}},
  )
})

test("Successfully serializes object transformed to nested object", t => {
  let schema = S.object(s =>
    {
      "v1": {
        "boo": s.field("boo", S.int),
        "zoo": s.field("zoo", S.int),
      },
    }
  )

  t->Assert.deepEqual(
    {"v1": {"boo": 1, "zoo": 2}}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{boo: 1, zoo: 2}`),
  )
})

test("Successfully parses object transformed to nested tuple", t => {
  let schema = S.object(s =>
    {
      "v1": (s.field("boo", S.int), s.field("zoo", S.int)),
    }
  )

  t->Assert.deepEqual(%raw(`{boo: 1, zoo: 2}`)->S.parseOrThrow(~to=schema), {"v1": (1, 2)})
})

test("Successfully serializes object transformed to nested tuple", t => {
  let schema = S.object(s =>
    {
      "v1": (s.field("boo", S.int), s.field("zoo", S.int)),
    }
  )

  t->Assert.deepEqual({"v1": (1, 2)}->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{boo: 1, zoo: 2}`))
})

test("Successfully parses object with only one field returned from transformer", t => {
  let schema = S.object(s => s.field("field", S.bool))

  t->Assert.deepEqual(%raw(`{"field": true}`)->S.parseOrThrow(~to=schema), true)
})

test("Successfully serializes object with only one field returned from transformer", t => {
  let schema = S.object(s => s.field("field", S.bool))

  t->Assert.deepEqual(true->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"field": true}`))
})

test("Successfully parses object transformed to the one with hardcoded fields", t => {
  let schema = S.object(s =>
    {
      "hardcoded": false,
      "field": s.field("field", S.bool),
    }
  )

  t->Assert.deepEqual(
    %raw(`{"field": true}`)->S.parseOrThrow(~to=schema),
    {
      "hardcoded": false,
      "field": true,
    },
  )
})

test("Successfully serializes object transformed to the one with hardcoded fields", t => {
  let schema = S.object(s =>
    {
      "hardcoded": false,
      "field": s.field("field", S.bool),
    }
  )

  t->Assert.deepEqual(
    {
      "hardcoded": false,
      "field": true,
    }->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"field": true}`),
  )
})

test("Successfully parses object transformed to variant", t => {
  let schema = S.object(s => #VARIANT(s.field("field", S.bool)))

  t->Assert.deepEqual(%raw(`{"field": true}`)->S.parseOrThrow(~to=schema), #VARIANT(true))
})

test("Successfully serializes object transformed to variant", t => {
  let schema = S.object(s => #VARIANT(s.field("field", S.bool)))

  t->Assert.deepEqual(#VARIANT(true)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"field": true}`))
})

test("Parse reversed schema with nested objects and tuples has type validation", t => {
  let schema = S.object(s =>
    {
      "foo": 1,
      "obj": {
        "foo": 2,
        "bar": s.field("bar", S.string),
      },
      "tuple": (3, s.field("baz", S.bool)),
    }
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{typeof i==="object"&&i||e[7](i);let v0=i["foo"],v1=i["obj"],v4=i["tuple"];v0===1||e[0](v0);typeof v1==="object"&&v1||e[3](v1);let v2=v1["foo"],v3=v1["bar"];v2===2||e[1](v2);typeof v3==="string"||e[2](v3);Array.isArray(v4)&&v4.length===2||e[6](v4);let v5=v4["0"],v6=v4["1"];v5===3||e[4](v5);typeof v6==="boolean"||e[5](v6);return {"bar":v3,"baz":v6,}}`,
  )
})

module BenchmarkWithSObject = {
  let makeTestObject = () => {
    %raw(`Object.freeze({
    number: 1,
    negNumber: -1,
    maxNumber: Number.MAX_VALUE,
    string: 'string',
    longString:
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Vivendum intellegat et qui, ei denique consequuntur vix. Semper aeterno percipit ut his, sea ex utinam referrentur repudiandae. No epicuri hendrerit consetetur sit, sit dicta adipiscing ex, in facete detracto deterruisset duo. Quot populo ad qui. Sit fugit nostrum et. Ad per diam dicant interesset, lorem iusto sensibus ut sed. No dicam aperiam vis. Pri posse graeco definitiones cu, id eam populo quaestio adipiscing, usu quod malorum te. Ex nam agam veri, dicunt efficiantur ad qui, ad legere adversarium sit. Commune platonem mel id, brute adipiscing duo an. Vivendum intellegat et qui, ei denique consequuntur vix. Offendit eleifend moderatius ex vix, quem odio mazim et qui, purto expetendis cotidieque quo cu, veri persius vituperata ei nec. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.',
    boolean: true,
    deeplyNested: {
      foo: 'bar',
      num: 1,
      bool: false,
    },
  })`)
  }

  let makeSchema = () => {
    S.object(s =>
      {
        "number": s.field("number", S.float),
        "negNumber": s.field("negNumber", S.float),
        "maxNumber": s.field("maxNumber", S.float),
        "string": s.field("string", S.string),
        "longString": s.field("longString", S.string),
        "boolean": s.field("boolean", S.bool),
        "deeplyNested": s.field(
          "deeplyNested",
          S.object(s =>
            {
              "foo": s.field("foo", S.string),
              "num": s.field("num", S.float),
              "bool": s.field("bool", S.bool),
            }
          ),
        ),
      }
    )
  }

  test("Successfully parses object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(~to=schema), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[10](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"];typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6||e[9](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"];typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);return {"number":v0,"negNumber":v1,"maxNumber":v2,"string":v3,"longString":v4,"boolean":v5,"deeplyNested":{"foo":v7,"num":v8,"bool":v9,},}}`,
    )
    S.global({})
  })

  test("Successfully asserts object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(~to=schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{typeof i==="object"&&i||e[10](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"];typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6||e[9](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"];typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);return void 0}`,
    )
    S.global({})
  })

  test("Successfully parses strict object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(~to=schema), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[12](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"],v11;typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6&&!Array.isArray(v6)||e[10](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[9](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return {"number":v0,"negNumber":v1,"maxNumber":v2,"string":v3,"longString":v4,"boolean":v5,"deeplyNested":{"foo":v7,"num":v8,"bool":v9,},}}`,
    )
    S.global({})
  })

  test("Successfully asserts strict object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(~to=schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[12](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"],v11;typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6&&!Array.isArray(v6)||e[10](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[9](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return void 0}`,
    )
    S.global({})
  })

  test("Successfully serializes object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.decodeOrThrow(~from=schema, ~to=S.unknown), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v0=i["deeplyNested"];return {"number":i["number"],"negNumber":i["negNumber"],"maxNumber":i["maxNumber"],"string":i["string"],"longString":i["longString"],"boolean":i["boolean"],"deeplyNested":{"foo":v0["foo"],"num":v0["num"],"bool":v0["bool"],},}}`,
    )
    S.global({})
  })
}

module Benchmark = {
  let makeTestObject = () => {
    %raw(`Object.freeze({
    number: 1,
    negNumber: -1,
    maxNumber: Number.MAX_VALUE,
    string: 'string',
    longString:
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Vivendum intellegat et qui, ei denique consequuntur vix. Semper aeterno percipit ut his, sea ex utinam referrentur repudiandae. No epicuri hendrerit consetetur sit, sit dicta adipiscing ex, in facete detracto deterruisset duo. Quot populo ad qui. Sit fugit nostrum et. Ad per diam dicant interesset, lorem iusto sensibus ut sed. No dicam aperiam vis. Pri posse graeco definitiones cu, id eam populo quaestio adipiscing, usu quod malorum te. Ex nam agam veri, dicunt efficiantur ad qui, ad legere adversarium sit. Commune platonem mel id, brute adipiscing duo an. Vivendum intellegat et qui, ei denique consequuntur vix. Offendit eleifend moderatius ex vix, quem odio mazim et qui, purto expetendis cotidieque quo cu, veri persius vituperata ei nec. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.',
    boolean: true,
    deeplyNested: {
      foo: 'bar',
      num: 1,
      bool: false,
    },
  })`)
  }

  let makeSchema = () => {
    S.schema(s =>
      {
        "number": s.matches(S.float),
        "negNumber": s.matches(S.float),
        "maxNumber": s.matches(S.float),
        "string": s.matches(S.string),
        "longString": s.matches(S.string),
        "boolean": s.matches(S.bool),
        "deeplyNested": {
          "foo": s.matches(S.string),
          "num": s.matches(S.float),
          "bool": s.matches(S.bool),
        },
      }
    )
  }

  test("Successfully parses object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[10](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"];typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6||e[9](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"];typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);return {"number":v0,"negNumber":v1,"maxNumber":v2,"string":v3,"longString":v4,"boolean":v5,"deeplyNested":{"foo":v7,"num":v8,"bool":v9,},}}`,
    )

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(~to=schema), makeTestObject())

    S.global({})
  })

  test("Successfully asserts object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(~to=schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{typeof i==="object"&&i||e[10](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"];typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6||e[9](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"];typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);return void 0}`,
    )
    S.global({})
  })

  test("Successfully parses strict object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(~to=schema), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[12](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"],v11;typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6&&!Array.isArray(v6)||e[10](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[9](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return i}`,
    )
    S.global({})
  })

  test("Successfully asserts strict object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(~to=schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[12](i);let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v6=i["deeplyNested"],v11;typeof v0==="number"||e[0](v0);typeof v1==="number"||e[1](v1);typeof v2==="number"||e[2](v2);typeof v3==="string"||e[3](v3);typeof v4==="string"||e[4](v4);typeof v5==="boolean"||e[5](v5);typeof v6==="object"&&v6&&!Array.isArray(v6)||e[10](v6);let v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;typeof v7==="string"||e[6](v7);typeof v8==="number"||e[7](v8);typeof v9==="boolean"||e[8](v9);for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[9](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return void 0}`,
    )
    S.global({})
  })

  test("Successfully serializes object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.decodeOrThrow(~from=schema, ~to=S.unknown), makeTestObject())

    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{let v0=i["deeplyNested"];return i}`)
    S.global({})
  })
}

test("Successfully parses object and serializes it back to the initial data", t => {
  let any = %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)

  let schema = S.object(s =>
    {
      "name": s.field("Name", S.string),
      "email": s.field("Email", S.string),
      "age": s.field("Age", S.int),
    }
  )

  t->Assert.deepEqual(any->S.parseOrThrow(~to=schema)->S.decodeOrThrow(~from=schema, ~to=S.unknown), any)
})

test("Allows to create object schema with unused fields", t => {
  let schema = S.object(s => {
    ignore(s.field("unused", S.string))
    {
      "field": s.field("field", S.string),
    }
  })

  t->Assert.deepEqual(
    %raw(`{"field": "foo", "unused": "bar"}`)->S.parseOrThrow(~to=schema),
    {"field": "foo"},
  )
})

test("Fails to create object schema with single field defined multiple times", t => {
  t->Assert.throws(
    () => {
      S.object(
        s =>
          {
            "boo": s.field("field", S.string),
            "zoo": s.field("field", S.int),
          },
      )
    },
    ~expectations={
      message: `[Sury] The field "field" defined twice with incompatible schemas`,
    },
  )
})

test("Successfully parses object schema with single field registered multiple times", t => {
  let schema = S.object(s => {
    let field = s.field("field", S.string)
    {
      "field1": field,
      "field2": field,
    }
  })
  t->Assert.deepEqual(
    %raw(`{"field": "foo"}`)->S.parseOrThrow(~to=schema),
    {"field1": "foo", "field2": "foo"},
  )
})

test("Reverse convert of object schema with single field registered multiple times", t => {
  let schema = S.object(s => {
    let field = s.field("field", S.string)
    {
      "field1": field,
      "field2": field,
      "field3": field,
    }
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    // `i=>{let v0=i["field1"];if(v0!==i["field2"]){e[0]()}if(v0!==i["field3"]){e[1]()}return {"field":v0,}}`,
    `i=>{return {"field":i["field3"],}}`,
  )

  t->Assert.deepEqual(
    {"field1": "foo", "field2": "foo", "field3": "foo"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"field": "foo"}`),
  )
  // t->U.assertThrows(
  //   () => {"field1": "foo", "field2": "foo", "field3": "foz"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
  //   {
  //     code: InvalidOperation({
  //       description: `Another source has conflicting data for the field ["field"]`,
  //     }),
  //     operation: ReverseConvert,
  //     path: S.Path.fromArray(["field3"]),
  //   },
  // )
})

test("Can destructure fields of simple nested objects", t => {
  let schema = S.object(s => {
    let nested = s.field(
      "nested",
      S.object(
        s =>
          {
            "foo": s.field("foo", S.string),
            "bar": s.field("bar", S.string),
          },
      ),
    )
    {
      "baz": nested["bar"],
      "foz": nested["foo"],
    }
  })
  t->Assert.deepEqual(
    %raw(`{"nested": {"foo": "foo", "bar": "bar"}}`)->S.parseOrThrow(~to=schema),
    {"baz": "bar", "foz": "foo"},
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[3](i);let v0=i["nested"];typeof v0==="object"&&v0||e[2](v0);let v1=v0["foo"],v2=v0["bar"];typeof v1==="string"||e[0](v1);typeof v2==="string"||e[1](v2);return {"baz":v2,"foz":v1,}}`,
  )

  t->Assert.deepEqual(
    {"baz": "bar", "foz": "foo"}->S.decodeOrThrow(~from=schema, ~to=S.json),
    %raw(`{"nested": {"foo": "foo", "bar": "bar"}}`),
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"nested":{"foo":i["foz"],"bar":i["baz"],},}}`,
  )
})

test("Object schema parsing checks order", t => {
  let schema = S.object(s => {
    s.tag("tag", "value")
    {
      "key": s.field("key", S.string),
    }
  })->S.strict

  // Type check should be the first
  t->U.assertThrowsMessage(
    () => %raw(`"foo"`)->S.parseOrThrow(~to=schema),
    `Expected { tag: "value"; key: string; }, received "foo"`,
  )
  // Tag check should be the second
  t->U.assertThrowsMessage(
    () =>
      %raw(`{tag: "wrong", key: 123, unknownKey: "value", unknownKey2: "value"}`)->S.parseOrThrow(~to=
        schema,
      ),
    `Failed at ["tag"]: Expected "value", received "wrong"`,
  )
  // Field check should be the third
  t->U.assertThrowsMessage(
    () =>
      %raw(`{tag: "value", key: 123, unknownKey: "value", unknownKey2: "value"}`)->S.parseOrThrow(~to=
        schema,
      ),
    `Failed at ["key"]: Expected string, received 123`,
  )
  // Unknown keys check should be the last
  t->U.assertThrowsMessage(
    () =>
      %raw(`{tag: "value", key: "value", unknownKey: "value2", unknownKey2: "value2"}`)->S.parseOrThrow(~to=
        schema,
      ),
    `Unrecognized key "unknownKey"`,
  )
  // Parses valid
  t->Assert.deepEqual(
    %raw(`{tag: "value", key: "value"}`)->S.parseOrThrow(~to=schema),
    {
      "key": "value",
    },
  )
})

module Compiled = {
  test("Compiled code snapshot for simple object", t => {
    let schema = S.object(s =>
      {
        "foo": s.field("foo", S.string),
        "bar": s.field("bar", S.bool),
      }
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[2](i);let v0=i["foo"],v1=i["bar"];typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);return {"foo":v0,"bar":v1,}}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{return {"foo":i["foo"],"bar":i["bar"],}}`,
    )
  })

  test("Compiled code snapshot for refined nested object", t => {
    let schema = S.object(s =>
      {
        "foo": s.field("foo", S.literal(12)),
        "bar": s.field(
          "bar",
          S.object(
            s =>
              {
                "baz": s.field("baz", S.string),
              },
          )->S.refine(_ => true),
        ),
      }
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[3](i);let v0=i["foo"],v1=i["bar"];v0===12||e[0](v0);typeof v1==="object"&&v1||e[2](v1);let v2=v1["baz"];typeof v2==="string"||e[1](v2);return {"foo":v0,"bar":{"baz":v2,},}}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v0=i["bar"];e[0](v0)||e[1](v0);return i}`,
    )
  })

  test("Compiled parse code snapshot for simple object with async", t => {
    let schema = S.object(s =>
      {
        "foo": s.field("foo", S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)})),
        "bar": s.field("bar", S.bool),
      }
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{typeof i==="object"&&i||e[3](i);let v1=i["bar"];let v0;try{v0=e[0](i["foo"]).catch(x=>e[1](x))}catch(x){e[1](x)}typeof v1==="boolean"||e[2](v1);return Promise.all([v0,]).then(([v0,])=>{return {"foo":v0,"bar":v1,}})}`,
    )
  })

  test("Compiled parse code snapshot with async field registered as return", t => {
    let schema = S.object(s =>
      s.field("foo", S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ParseAsync,
      `i=>{typeof i==="object"&&i||e[2](i);let v0;try{v0=e[0](i["foo"]).catch(x=>e[1](x))}catch(x){e[1](x)}return v0}`,
    )
  })

  test("Compiled parse code snapshot for simple object with strict unknown keys", t => {
    let schema = S.object(s =>
      {
        "foo": s.field("foo", S.string),
        "bar": s.field("bar", S.bool),
      }
    )->S.strict

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[3](i);let v0=i["foo"],v1=i["bar"],v2;typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);for(v2 in i){if(v2!=="foo"&&v2!=="bar"){e[2](v2)}}return {"foo":v0,"bar":v1,}}`,
    )
  })

  test("Compiled serialize code snapshot for simple object with strict unknown keys", t => {
    let schema = S.object(s =>
      {
        "foo": s.field("foo", S.string),
        "bar": s.field("bar", S.bool),
      }
    )->S.strict

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{return {"foo":i["foo"],"bar":i["bar"],}}`,
    )
  })

  test("Compiled code snapshot for nested empty object with strict unknown keys", t => {
    let schema = S.schema(s =>
      {
        "nested": s.matches(S.object(_ => ())->S.strict),
      }
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[2](i);let v1=i["nested"];typeof v1==="object"&&v1&&!Array.isArray(v1)||e[1](v1);let v0;for(v0 in v1){if(true){e[0](v0)}}return {"nested":void 0,}}`,
    )
    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{},}}`)
  })

  test(
    "Compiled parse code snapshot for simple object with strict unknown keys, renamed fields, constants and discriminants",
    t => {
      let schema = S.object(s => {
        s.tag("tag", 0)
        {
          "foo": s.field("FOO", S.string),
          "bar": s.field("BAR", S.bool),
          "zoo": 1,
        }
      })->S.strict

      t->U.assertCompiledCode(
        ~schema,
        ~op=#Parse,
        `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[4](i);let v0=i["tag"],v1=i["FOO"],v2=i["BAR"],v3;v0===0||e[0](v0);typeof v1==="string"||e[1](v1);typeof v2==="boolean"||e[2](v2);for(v3 in i){if(v3!=="tag"&&v3!=="FOO"&&v3!=="BAR"){e[3](v3)}}return {"foo":v1,"bar":v2,"zoo":1,}}`,
      )
    },
  )

  test(
    "Compiled serialize code snapshot for simple object with strict unknown keys, renamed fields, constants and discriminants",
    t => {
      let schema = S.object(s => {
        s.tag("tag", 0)
        {
          "foo": s.field("FOO", S.string),
          "bar": s.field("BAR", S.bool),
          "zoo": 1,
        }
      })->S.strict

      t->U.assertCompiledCode(
        ~schema,
        ~op=#ReverseConvert,
        `i=>{return {"tag":0,"FOO":i["foo"],"BAR":i["bar"],}}`,
      )
    },
  )
}

test(
  "Works with object schema used multiple times as a child schema. See: https://github.com/DZakh/rescript-schema/issues/63",
  t => {
    let appVersionSpecSchema = S.object(s =>
      {
        "current": s.field("current", S.string),
        "minimum": s.field("minimum", S.string),
      }
    )

    let appVersionsSchema = S.object(s =>
      {
        "ios": s.field("ios", appVersionSpecSchema),
        "android": s.field("android", appVersionSpecSchema),
      }
    )

    let appVersions = {
      "ios": {"current": "1.1", "minimum": "1.0"},
      "android": {"current": "1.2", "minimum": "1.1"},
    }

    let value = appVersions->S.parseOrThrow(~to=appVersionsSchema)
    t->Assert.deepEqual(value, appVersions)

    let data = appVersions->S.decodeOrThrow(~from=appVersionsSchema, ~to=S.json)
    t->Assert.deepEqual(data, appVersions->Obj.magic)
  },
)

test("Reverse empty object schema to literal", t => {
  let schema = S.object(_ => ())
  t->U.assertEqualSchemas(schema->S.reverse, S.unit->S.shape(_ => Dict.make())->S.castToUnknown)
  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, ())
})

test("Compiles to async serialize operation with the sync object schema", t => {
  let schema = S.object(_ => ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertAsync,
    `i=>{i===void 0||e[0](i);return Promise.resolve({})}`,
  )
})

test("Reverse tagged object to literal without payload", t => {
  let schema = S.object(s => {
    s.tag("kind", "test")
    #Test
  })
  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.literal(#Test)
    ->S.shape(_ =>
      {
        "kind": "test",
      }
    )
    ->S.castToUnknown,
  )

  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, #Test)
})

test("Reverse tagged object to primitive schema", t => {
  let schema = S.object(s => {
    s.tag("kind", "test")
    s.field("field", S.bool)
  })

  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, true)
})

test("Reverse object with discriminant which is an object transformed to literal", t => {
  let schema = S.object(s => {
    let _ = s.field(
      "kind",
      S.object(
        s => {
          s.tag("nestedKind", "test")
          "foo"
        },
      ),
    )
    s.field("field", S.bool)
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="object"&&i||e[3](i);let v0=i["kind"],v2=i["field"];typeof v0==="object"&&v0||e[1](v0);let v1=v0["nestedKind"];v1==="test"||e[0](v1);typeof v2==="boolean"||e[2](v2);return v2}`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"kind":{"nestedKind":"test",},"field":i,}}`,
  )

  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, true)
})

test("Reverse with output of nested object/tuple schema", t => {
  let schema = S.object(s => {
    s.tag("kind", "test")
    {
      "nested": {
        "field": (s.field("raw_field", S.bool), true),
      },
    }
  })
  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, {"nested": {"field": (true, true)}})
})
