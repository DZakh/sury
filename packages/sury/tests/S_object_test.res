open Ava
open RescriptCore

@live
type options = {fast?: bool, mode?: int}

test("Successfully parses object with inlinable string field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{field: "bar"}`)->S.parseOrThrow(schema), {"field": "bar"})
})

test("Fails to parse object with inlinable string field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->U.assertThrows(
    () => %raw(`{field: 123}`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.string->S.castToUnknown, received: %raw(`123`)}),
      operation: Parse,
      path: S.Path.fromArray(["field"]),
    },
  )
})

test(
  "Fails to parse object with custom user error in array field (should have correct path)",
  t => {
    let schema = S.object(s =>
      {
        "field": s.field("field", S.array(S.string->S.refine(s => _ => s.fail("User error")))),
      }
    )

    t->U.assertThrows(
      () => %raw(`{field: ["foo"]}`)->S.parseOrThrow(schema),
      {
        code: OperationFailed("User error"),
        operation: Parse,
        path: S.Path.fromArray(["field", "0"]),
      },
    )
  },
)

test("Successfully parses object with inlinable bool field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.bool),
    }
  )

  t->Assert.deepEqual(%raw(`{field: true}`)->S.parseOrThrow(schema), {"field": true})
})

test("Fails to parse object with inlinable bool field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.bool),
    }
  )

  t->U.assertThrows(
    () => %raw(`{field: 123}`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.bool->S.castToUnknown, received: %raw(`123`)}),
      operation: Parse,
      path: S.Path.fromArray(["field"]),
    },
  )
})

test("Successfully parses object with unknown field (Noop operation)", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.unknown),
    }
  )

  t->Assert.deepEqual(
    %raw(`{field: new Date("2015-12-12")}`)->S.parseOrThrow(schema),
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
    %raw(`{field: new Date("2015-12-12")}`)->S.reverseConvertOrThrow(schema),
    %raw(`{field: new Date("2015-12-12")}`),
  )
})

test("Fails to parse object with inlinable never field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.never),
    }
  )

  t->U.assertThrows(
    () => %raw(`{field: true}`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.never->S.castToUnknown, received: %raw(`true`)}),
      operation: Parse,
      path: S.Path.fromArray(["field"]),
    },
  )
})

test("Successfully parses object with inlinable float field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.float),
    }
  )

  t->Assert.deepEqual(%raw(`{field: 123}`)->S.parseOrThrow(schema), {"field": 123.})
})

test("Fails to parse object with inlinable float field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.float),
    }
  )

  t->U.assertThrows(
    () => %raw(`{field: true}`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.float->S.castToUnknown, received: %raw(`true`)}),
      operation: Parse,
      path: S.Path.fromArray(["field"]),
    },
  )
})

test("Successfully parses object with inlinable int field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.int),
    }
  )

  t->Assert.deepEqual(%raw(`{field: 123}`)->S.parseOrThrow(schema), {"field": 123})
})

test("Fails to parse object with inlinable int field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.int),
    }
  )

  t->U.assertThrows(
    () => %raw(`{field: true}`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: S.int->S.castToUnknown, received: %raw(`true`)}),
      operation: Parse,
      path: S.Path.fromArray(["field"]),
    },
  )
})

test("Successfully parses object with not inlinable empty object field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.object(_ => ())),
    }
  )

  t->Assert.deepEqual(%raw(`{field: {}}`)->S.parseOrThrow(schema), {"field": ()})
})

test("Fails to parse object with not inlinable empty object field", t => {
  let fieldSchema = S.object(_ => ())
  let schema = S.object(s =>
    {
      "field": s.field("field", fieldSchema),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{field: true}`)->S.parseOrThrow(schema),
    `Failed parsing: Expected { field: {}; }, received { field: true; }`,
  )
})

test("Fails to parse object when provided invalid data", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->U.assertThrows(
    () => %raw(`12`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`12`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes object with single field", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual({"field": "bar"}->S.reverseConvertOrThrow(schema), %raw(`{field: "bar"}`))
})

test("Successfully parses object with multiple fields", t => {
  let schema = S.object(s =>
    {
      "boo": s.field("boo", S.string),
      "zoo": s.field("zoo", S.string),
    }
  )

  t->Assert.deepEqual(
    %raw(`{boo: "bar", zoo: "jee"}`)->S.parseOrThrow(schema),
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
    {"boo": "bar", "zoo": "jee"}->S.reverseConvertOrThrow(schema),
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

  t->Assert.deepEqual(%raw(`{string: "bar"}`)->S.parseOrThrow(schema), {"string": "barfield"})
})

test("Fails to parse object when transformed field has raises error", t => {
  let schema = S.object(s =>
    {
      "field": s.field("field", S.string->S.transform(s => {parser: _ => s.fail("User error")})),
    }
  )

  t->U.assertThrows(
    () => {"field": "bar"}->S.parseOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: Parse,
      path: S.Path.fromArray(["field"]),
    },
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

  t->U.assertThrows(
    () => {"originalFieldName": "bar"}->S.parseOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: Parse,
      path: S.Path.fromArray(["originalFieldName"]),
    },
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
    {"string": "bar"}->S.reverseConvertOrThrow(schema),
    %raw(`{"string": "barfield"}`),
  )
})

test("Fails to serializes object when transformed field has raises error", t => {
  let schema = S.object(s =>
    {
      "field": s.field(
        "field",
        S.string->S.transform(s => {serializer: _ => s.fail("User error")}),
      ),
    }
  )

  t->U.assertThrows(
    () => {"field": "bar"}->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: ReverseConvert,
      path: S.Path.fromArray(["field"]),
    },
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

  t->U.assertThrows(
    () => {"transformedFieldName": "bar"}->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: ReverseConvert,
      path: S.Path.fromArray(["transformedFieldName"]),
    },
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

  t->U.assertThrows(
    () =>
      {
        "v1": {
          "transformedFieldName": "bar",
        },
      }->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("User error"),
      operation: ReverseConvert,
      path: S.Path.fromArray(["v1", "transformedFieldName"]),
    },
  )
})

test("Successfully parses object with optional fields", t => {
  let schema = S.object(s =>
    {
      "boo": s.field("boo", S.option(S.string)),
      "zoo": s.field("zoo", S.option(S.string)),
    }
  )

  t->Assert.deepEqual(
    %raw(`{boo: "bar"}`)->S.parseOrThrow(schema),
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
    {"boo": Some("bar"), "zoo": None}->S.reverseConvertOrThrow(schema),
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
    %raw(`{boo: "bar"}`)->S.parseOrThrow(schema),
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
    {"boo": "bar", "zoo": "baz"}->S.reverseConvertOrThrow(schema),
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
      %raw(`{mode: 1}`)->S.parseOrThrow(optionsSchema),
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
    {mode: 1}->S.reverseConvertOrThrow(optionsSchema),
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
    %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`)->S.parseOrThrow(schema),
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
    {"name": "Dmitry", "email": "dzakh.dev@gmail.com", "age": 21}->S.reverseConvertOrThrow(schema),
    %raw(`{"Name":"Dmitry","Email":"dzakh.dev@gmail.com","Age":21}`),
  )
})

test("Successfully parses object transformed to tuple", t => {
  let schema = S.object(s => (s.field("boo", S.int), s.field("zoo", S.int)))

  t->Assert.deepEqual(%raw(`{boo: 1, zoo: 2}`)->S.parseOrThrow(schema), (1, 2))
})

test("Successfully serializes object transformed to tuple", t => {
  let schema = S.object(s => (s.field("boo", S.int), s.field("zoo", S.int)))

  t->Assert.deepEqual((1, 2)->S.reverseConvertOrThrow(schema), %raw(`{boo: 1, zoo: 2}`))
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
    %raw(`{boo: 1, zoo: 2}`)->S.parseOrThrow(schema),
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
    {"v1": {"boo": 1, "zoo": 2}}->S.reverseConvertOrThrow(schema),
    %raw(`{boo: 1, zoo: 2}`),
  )
})

test("Successfully parses object transformed to nested tuple", t => {
  let schema = S.object(s =>
    {
      "v1": (s.field("boo", S.int), s.field("zoo", S.int)),
    }
  )

  t->Assert.deepEqual(%raw(`{boo: 1, zoo: 2}`)->S.parseOrThrow(schema), {"v1": (1, 2)})
})

test("Successfully serializes object transformed to nested tuple", t => {
  let schema = S.object(s =>
    {
      "v1": (s.field("boo", S.int), s.field("zoo", S.int)),
    }
  )

  t->Assert.deepEqual({"v1": (1, 2)}->S.reverseConvertOrThrow(schema), %raw(`{boo: 1, zoo: 2}`))
})

test("Successfully parses object with only one field returned from transformer", t => {
  let schema = S.object(s => s.field("field", S.bool))

  t->Assert.deepEqual(%raw(`{"field": true}`)->S.parseOrThrow(schema), true)
})

test("Successfully serializes object with only one field returned from transformer", t => {
  let schema = S.object(s => s.field("field", S.bool))

  t->Assert.deepEqual(true->S.reverseConvertOrThrow(schema), %raw(`{"field": true}`))
})

test("Successfully parses object transformed to the one with hardcoded fields", t => {
  let schema = S.object(s =>
    {
      "hardcoded": false,
      "field": s.field("field", S.bool),
    }
  )

  t->Assert.deepEqual(
    %raw(`{"field": true}`)->S.parseOrThrow(schema),
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
    }->S.reverseConvertOrThrow(schema),
    %raw(`{"field": true}`),
  )
})

test("Successfully parses object transformed to variant", t => {
  let schema = S.object(s => #VARIANT(s.field("field", S.bool)))

  t->Assert.deepEqual(%raw(`{"field": true}`)->S.parseOrThrow(schema), #VARIANT(true))
})

test("Successfully serializes object transformed to variant", t => {
  let schema = S.object(s => #VARIANT(s.field("field", S.bool)))

  t->Assert.deepEqual(#VARIANT(true)->S.reverseConvertOrThrow(schema), %raw(`{"field": true}`))
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
    `i=>{if(typeof i!=="object"||!i||i["foo"]!==1||typeof i["obj"]!=="object"||!i["obj"]||i["obj"]["foo"]!==2){e[0](i)}let v0=i["obj"]["bar"],v1=i["tuple"]["1"];if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="boolean"){e[2](v1)}return {"bar":v0,"baz":v1,}}`,
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

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(schema), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"];if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"];if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}return {"number":v0,"negNumber":v1,"maxNumber":v2,"string":v3,"longString":v4,"boolean":v5,"deeplyNested":{"foo":v7,"num":v8,"bool":v9,},}}`,
    )
    S.global({})
  })

  test("Successfully asserts object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{if(typeof i!=="object"||!i||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"];if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"];if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}return void 0}`,
    )
    S.global({})
  })

  test("Successfully parses strict object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(schema), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]||Array.isArray(i["deeplyNested"])){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v11;if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[10](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return {"number":v0,"negNumber":v1,"maxNumber":v2,"string":v3,"longString":v4,"boolean":v5,"deeplyNested":{"foo":v7,"num":v8,"bool":v9,},}}`,
    )
    S.global({})
  })

  test("Successfully asserts strict object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]||Array.isArray(i["deeplyNested"])){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v11;if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[10](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return void 0}`,
    )
    S.global({})
  })

  test("Successfully serializes object from benchmark - with S.object", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.reverseConvertOrThrow(schema), makeTestObject())

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
      `i=>{if(typeof i!=="object"||!i||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"];if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"];if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}return {"number":v0,"negNumber":v1,"maxNumber":v2,"string":v3,"longString":v4,"boolean":v5,"deeplyNested":{"foo":v7,"num":v8,"bool":v9,},}}`,
    )

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(schema), makeTestObject())

    S.global({})
  })

  test("Successfully asserts object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{if(typeof i!=="object"||!i||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"];if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"];if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}return void 0}`,
    )
    S.global({})
  })

  test("Successfully parses strict object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.parseOrThrow(schema), makeTestObject())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]||Array.isArray(i["deeplyNested"])){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v11;if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[10](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return i}`,
    )
    S.global({})
  })

  test("Successfully asserts strict object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
      defaultAdditionalItems: Strict,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.assertOrThrow(schema), ())

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Assert,
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["deeplyNested"]!=="object"||!i["deeplyNested"]||Array.isArray(i["deeplyNested"])){e[0](i)}let v0=i["number"],v1=i["negNumber"],v2=i["maxNumber"],v3=i["string"],v4=i["longString"],v5=i["boolean"],v11;if(typeof v0!=="number"){e[1](v0)}if(typeof v1!=="number"){e[2](v1)}if(typeof v2!=="number"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[5](v4)}if(typeof v5!=="boolean"){e[6](v5)}let v6=i["deeplyNested"],v7=v6["foo"],v8=v6["num"],v9=v6["bool"],v10;if(typeof v7!=="string"){e[7](v7)}if(typeof v8!=="number"){e[8](v8)}if(typeof v9!=="boolean"){e[9](v9)}for(v10 in v6){if(v10!=="foo"&&v10!=="num"&&v10!=="bool"){e[10](v10)}}for(v11 in i){if(v11!=="number"&&v11!=="negNumber"&&v11!=="maxNumber"&&v11!=="string"&&v11!=="longString"&&v11!=="boolean"&&v11!=="deeplyNested"){e[11](v11)}}return void 0}`,
    )
    S.global({})
  })

  test("Successfully serializes object from benchmark", t => {
    S.global({
      disableNanNumberValidation: true,
    })
    let schema = makeSchema()

    t->Assert.deepEqual(makeTestObject()->S.reverseConvertOrThrow(schema), makeTestObject())

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

  t->Assert.deepEqual(any->S.parseOrThrow(schema)->S.reverseConvertOrThrow(schema), any)
})

test("Allows to create object schema with unused fields", t => {
  let schema = S.object(s => {
    ignore(s.field("unused", S.string))
    {
      "field": s.field("field", S.string),
    }
  })

  t->Assert.deepEqual(
    %raw(`{"field": "foo", "unused": "bar"}`)->S.parseOrThrow(schema),
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
    %raw(`{"field": "foo"}`)->S.parseOrThrow(schema),
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
    {"field1": "foo", "field2": "foo", "field3": "foo"}->S.reverseConvertOrThrow(schema),
    %raw(`{"field": "foo"}`),
  )
  // t->U.assertThrows(
  //   () => {"field1": "foo", "field2": "foo", "field3": "foz"}->S.reverseConvertOrThrow(schema),
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
    %raw(`{"nested": {"foo": "foo", "bar": "bar"}}`)->S.parseOrThrow(schema),
    {"baz": "bar", "foz": "foo"},
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[0](i)}let v0=i["nested"],v1=v0["foo"],v2=v0["bar"];if(typeof v1!=="string"){e[1](v1)}if(typeof v2!=="string"){e[2](v2)}return {"baz":v2,"foz":v1,}}`,
  )

  t->Assert.deepEqual(
    {"baz": "bar", "foz": "foo"}->S.reverseConvertToJsonOrThrow(schema),
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
  t->U.assertThrows(
    () => %raw(`"foo"`)->S.parseOrThrow(schema),
    {
      code: InvalidType({expected: schema->S.castToUnknown, received: %raw(`"foo"`)}),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  // Tag check should be the second
  t->U.assertThrows(
    () =>
      %raw(`{tag: "wrong", key: 123, unknownKey: "value", unknownKey2: "value"}`)->S.parseOrThrow(
        schema,
      ),
    {
      code: InvalidType({
        expected: schema->S.castToUnknown,
        received: %raw(`{tag: "wrong", key: 123, unknownKey: "value", unknownKey2: "value"}`),
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )
  // Field check should be the third
  t->U.assertThrows(
    () =>
      %raw(`{tag: "value", key: 123, unknownKey: "value", unknownKey2: "value"}`)->S.parseOrThrow(
        schema,
      ),
    {
      code: InvalidType({expected: S.string->S.castToUnknown, received: %raw(`123`)}),
      operation: Parse,
      path: S.Path.fromLocation("key"),
    },
  )
  // Unknown keys check should be the last
  t->U.assertThrows(
    () =>
      %raw(`{tag: "value", key: "value", unknownKey: "value2", unknownKey2: "value2"}`)->S.parseOrThrow(
        schema,
      ),
    {code: ExcessField("unknownKey"), operation: Parse, path: S.Path.empty},
  )
  // Parses valid
  t->Assert.deepEqual(
    %raw(`{tag: "value", key: "value"}`)->S.parseOrThrow(schema),
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
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"],v1=i["bar"];if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="boolean"){e[2](v1)}return {"foo":v0,"bar":v1,}}`,
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
            s => {
              {"baz": s.field("baz", S.string)}
            },
          )->S.refine(_ => _ => ()),
        ),
      }
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i||i["foo"]!==12||typeof i["bar"]!=="object"||!i["bar"]){e[0](i)}let v0=i["bar"],v1=v0["baz"];if(typeof v1!=="string"){e[1](v1)}let v2={"baz":v1,};e[2](v2);return {"foo":i["foo"],"bar":v2,}}`,
    )
    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{let v0=i["bar"];e[0](v0);return {"foo":i["foo"],"bar":{"baz":v0["baz"],},}}`,
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
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["bar"];if(typeof v0!=="boolean"){e[2](v0)}return Promise.all([e[1](i["foo"]),]).then(a=>({"foo":a[0],"bar":v0,}))}`,
    )
  })

  test("Compiled parse code snapshot with async field registered as return", t => {
    let schema = S.object(s =>
      s.field("foo", S.unknown->S.transform(_ => {asyncParser: i => Promise.resolve(i)}))
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}return e[1](i["foo"])}`,
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
      `i=>{if(typeof i!=="object"||!i||Array.isArray(i)){e[0](i)}let v0=i["foo"],v1=i["bar"],v2;if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="boolean"){e[2](v1)}for(v2 in i){if(v2!=="foo"&&v2!=="bar"){e[3](v2)}}return {"foo":v0,"bar":v1,}}`,
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
      `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]||Array.isArray(i["nested"])){e[0](i)}let v0,v1=i["nested"];for(v0 in v1){if(true){e[1](v0)}}return {"nested":e[2],}}`,
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
        `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||i["tag"]!==0){e[0](i)}let v0=i["FOO"],v1=i["BAR"],v2;if(typeof v0!=="string"){e[1](v0)}if(typeof v1!=="boolean"){e[2](v1)}for(v2 in i){if(v2!=="tag"&&v2!=="FOO"&&v2!=="BAR"){e[3](v2)}}return {"foo":v0,"bar":v1,"zoo":e[4],}}`,
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

    let value = appVersions->S.parseOrThrow(appVersionsSchema)
    t->Assert.deepEqual(value, appVersions)

    let data = appVersions->S.reverseConvertToJsonOrThrow(appVersionsSchema)
    t->Assert.deepEqual(data, appVersions->Obj.magic)
  },
)

test("Reverse empty object schema to literal", t => {
  let schema = S.object(_ => ())
  t->U.assertEqualSchemas(schema->S.reverse, S.unit->S.shape(_ => Js.Dict.empty())->S.castToUnknown)
  t->U.assertReverseReversesBack(schema)
  t->U.assertReverseParsesBack(schema, ())
})

test("Compiles to async serialize operation with the sync object schema", t => {
  let schema = S.object(_ => ())
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvertAsync,
    `i=>{if(i!==void 0){e[0](i)}return Promise.resolve({})}`,
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
  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.bool
    ->S.shape(bool =>
      {
        "kind": "test",
        "field": bool,
      }
    )
    ->S.castToUnknown,
  )

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
  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.schema(s => {
      {
        "nested": {
          "field": (s.matches(S.bool), true),
        },
      }
    })
    ->S.shape(data =>
      {
        "kind": "test",
        "raw_field": {
          let (v, _) = data["nested"]["field"]
          v
        },
      }
    )
    ->S.castToUnknown,
  )
  t->U.assertReverseParsesBack(schema, {"nested": {"field": (true, true)}})
})
