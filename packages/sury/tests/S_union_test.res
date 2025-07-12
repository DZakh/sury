open Ava

test("Throws for a Union schema factory without schemas", t => {
  t->Assert.throws(
    () => {
      S.union([])
    },
    ~expectations={
      message: "[Sury] S.union requires at least one item",
    },
  )
})

test("Successfully creates a Union schema factory with single schema and flattens it", t => {
  let schema = S.union([S.string])

  t->U.assertEqualSchemas(schema, S.string)
})

test("Successfully parses polymorphic variants", t => {
  let schema = S.union([S.literal(#apple), S.literal(#orange)])

  t->Assert.deepEqual(%raw(`"apple"`)->S.parseOrThrow(schema), #apple)
})

test("Parses when both schemas misses parser and have the same type", t => {
  let schema = S.union([
    S.string->S.transform(_ => {serializer: _ => "apple"}),
    S.string->S.transform(_ => {serializer: _ => "apple"}),
  ])

  try {
    let _ = %raw(`null`)->S.parseOrThrow(schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Error(error) =>
    t->Assert.is(error.message, `Failed parsing: Expected string | string, received null`)
  }

  try {
    let _ = %raw(`"foo"`)->S.parseOrThrow(schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Error(error) =>
    t->Assert.is(
      error.message,
      `Failed parsing: Expected string | string, received "foo"
- The S.transform parser is missing`,
    )
  }

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{throw e[0]}catch(e0){try{throw e[1]}catch(e1){e[2](i,e0,e1)}}}else{e[3](i)}return i}`,
  )
})

test("Parses when both schemas misses parser and have different types", t => {
  let schema = S.union([
    S.literal(#apple)->S.transform(_ => {serializer: _ => #apple}),
    S.string->S.transform(_ => {serializer: _ => "apple"}),
  ])

  try {
    let _ = %raw(`null`)->S.parseOrThrow(schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Error(error) =>
    t->Assert.is(error.message, `Failed parsing: Expected "apple" | string, received null`)
  }

  try {
    let _ = %raw(`"abc"`)->S.parseOrThrow(schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Error(error) =>
    t->Assert.is(
      error.message,
      `Failed parsing: Expected "apple" | string, received "abc"
- The S.transform parser is missing`,
    )
  }

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){if(i==="apple"){throw e[0]}else{try{throw e[1]}catch(e1){e[2](i,e1)}}}else{e[3](i)}return i}`,
  )
})

test("Serializes when both schemas misses serializer", t => {
  let schema = S.union([
    S.literal(#apple)->S.transform(_ => {parser: _ => #apple}),
    S.string->S.transform(_ => {parser: _ => #apple}),
  ])

  try {
    let _ = %raw(`null`)->S.reverseConvertOrThrow(schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Error(error) =>
    t->Assert.is(
      error.message,
      `Failed converting: Expected unknown | unknown, received null
- The S.transform serializer is missing`,
    )
  }

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{throw e[0]}catch(e0){try{throw e[1]}catch(e1){e[2](i,e0,e1)}}return i}`,
  )
})

test("When union of json and string schemas, should parse the first one", t => {
  let schema = S.union([S.json->S.shape(_ => #json), S.string->S.shape(_ => #str)])

  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(schema), #json)
  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(schema),
    `Failed parsing: Expected JSON | string, received undefined
- Expected JSON, received undefined`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{let v0=e[0](i);i=e[1]}catch(e0){if(typeof i==="string"){i=e[2]}else{e[3](i,e0)}}return i}`,
  )
})

test("Ensures parsing order with unknown schema", t => {
  let schema = S.union([
    S.string->S.length(2),
    S.bool->Obj.magic, // Should be checked before unknown
    S.unknown->S.transform(_ => {parser: _ => "pass"}),
    // TODO: Should disabled deopt at this point
    S.float->Obj.magic,
    S.bigint->Obj.magic,
  ])

  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(schema), "pass")
  t->Assert.deepEqual(%raw(`"to"`)->S.parseOrThrow(schema), "to")
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(schema), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{if(typeof i!=="string"){e[0](i)}if(i.length!==e[1]){e[2]()}}catch(e0){try{if(typeof i!=="boolean"){e[3](i)}}catch(e1){try{i=e[4](i)}catch(e2){if(!(typeof i==="number"&&!Number.isNaN(i)||typeof i==="bigint")){e[5](i,e0,e1,e2)}}}}return i}`,
  )
})

test("Parses when second schema misses parser", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {serializer: _ => "apple"})])

  t->Assert.deepEqual("apple"->S.parseOrThrow(schema), #apple)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){if(!(i==="apple")){try{throw e[0]}catch(e1){e[1](i,e1)}}}else{e[2](i)}return i}`,
  )
})

test("Parses with string catch all", t => {
  let schema = S.union([S.literal("apple"), S.string, S.literal("banana")])

  t->Assert.deepEqual("apple"->S.parseOrThrow(schema), "apple")
  t->Assert.deepEqual("foo"->S.parseOrThrow(schema), "foo")

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(typeof i==="string")){e[0](i)}return i}`)
})

test("Serializes when second struct misses serializer", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {parser: _ => #apple})])

  t->Assert.deepEqual(#apple->S.reverseConvertOrThrow(schema), %raw(`"apple"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{if(i!=="apple"){e[0](i)}}catch(e0){try{throw e[1]}catch(e1){e[2](i,e0,e1)}}return i}`,
  )
})

module Advanced = {
  // TypeScript type for reference (https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html#discriminated-unions)
  // type Shape =
  // | { kind: "circle"; radius: number }
  // | { kind: "square"; x: number }
  // | { kind: "triangle"; x: number; y: number };

  type shape = Circle({radius: float}) | Square({x: float}) | Triangle({x: float, y: float})

  let circleSchema = S.object(s => {
    s.tag("kind", "circle")
    Circle({
      radius: s.field("radius", S.float),
    })
  })
  let squareSchema = S.object(s => {
    s.tag("kind", "square")
    Square({
      x: s.field("x", S.float),
    })
  })
  let triangleSchema = S.object(s => {
    s.tag("kind", "triangle")
    Triangle({
      x: s.field("x", S.float),
      y: s.field("y", S.float),
    })
  })

  let shapeSchema = S.union([circleSchema, squareSchema, triangleSchema])

  test("Successfully parses Circle shape", t => {
    t->Assert.deepEqual(
      %raw(`{
      "kind": "circle",
      "radius": 1,
    }`)->S.parseOrThrow(shapeSchema),
      Circle({radius: 1.}),
    )
  })

  test("Successfully parses Square shape", t => {
    t->Assert.deepEqual(
      %raw(`{
      "kind": "square",
      "x": 2,
    }`)->S.parseOrThrow(shapeSchema),
      Square({x: 2.}),
    )
  })

  test("Successfully parses Triangle shape", t => {
    t->Assert.deepEqual(
      %raw(`{
      "kind": "triangle",
      "x": 2,
      "y": 3,
    }`)->S.parseOrThrow(shapeSchema),
      Triangle({x: 2., y: 3.}),
    )
  })

  test("Fails to parse with unknown kind", t => {
    let shape = %raw(`{
      "kind": "oval",
      "x": 2,
      "y": 3,
    }`)

    let error: U.errorPayload = {
      code: InvalidType({
        expected: shapeSchema->S.castToUnknown,
        received: shape->Obj.magic,
      }),
      operation: Parse,
      path: S.Path.empty,
    }

    t->U.assertThrows(() => shape->S.parseOrThrow(shapeSchema), error)
  })

  test("Fails to parse with unknown kind when the union is an object field", t => {
    let schema = S.object(s => s.field("field", shapeSchema))

    let shape = {
      "kind": "oval",
      "x": 2,
      "y": 3,
    }
    let data = {
      "field": shape,
    }

    let error: U.errorPayload = {
      code: InvalidType({
        expected: shapeSchema->S.castToUnknown,
        received: shape->Obj.magic,
      }),
      operation: Parse,
      path: S.Path.fromLocation("field"),
    }

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["field"];if(typeof v0==="object"&&v0){if(v0["kind"]==="circle"){let v1=v0["radius"];if(typeof v1!=="number"||Number.isNaN(v1)){e[1](v1)}v0={"TAG":e[2],"radius":v1,}}else if(v0["kind"]==="square"){let v2=v0["x"];if(typeof v2!=="number"||Number.isNaN(v2)){e[3](v2)}v0={"TAG":e[4],"x":v2,}}else if(v0["kind"]==="triangle"){let v3=v0["x"],v4=v0["y"];if(typeof v3!=="number"||Number.isNaN(v3)){e[5](v3)}if(typeof v4!=="number"||Number.isNaN(v4)){e[6](v4)}v0={"TAG":e[7],"x":v3,"y":v4,}}else{e[8](v0)}}else{e[9](v0)}return v0}`,
    )

    t->U.assertThrows(() => data->S.parseOrThrow(schema), error)
    t->Assert.is(
      (error->U.error).message,
      `Failed parsing at ["field"]: Expected { kind: "circle"; radius: number; } | { kind: "square"; x: number; } | { kind: "triangle"; x: number; y: number; }, received { kind: "oval"; x: 2; y: 3; }`,
    )
  })

  test("Fails to parse with invalid data type", t => {
    t->U.assertThrows(
      () => %raw(`"Hello world!"`)->S.parseOrThrow(shapeSchema),
      {
        code: InvalidType({
          expected: shapeSchema->S.castToUnknown,
          received: %raw(`"Hello world!"`),
        }),
        operation: Parse,
        path: S.Path.empty,
      },
    )
  })

  test("Passes through not defined item on converting without type validation", t => {
    let incompleteSchema = S.union([
      S.object(s => {
        s.tag("kind", "circle")
        Circle({
          radius: s.field("radius", S.float),
        })
      }),
      S.object(s => {
        s.tag("kind", "square")
        Square({
          x: s.field("x", S.float),
        })
      }),
    ])

    let v = Triangle({x: 2., y: 3.})

    // This is not valid but expected behavior. Use parse to ensure type validation
    t->Assert.is(v->S.reverseConvertOrThrow(incompleteSchema), v->Obj.magic)
  })

  test("Successfully serializes Circle shape", t => {
    t->Assert.deepEqual(
      Circle({radius: 1.})->S.reverseConvertOrThrow(shapeSchema),
      %raw(`{
          "kind": "circle",
          "radius": 1,
        }`),
    )
  })

  test("Successfully serializes Square shape", t => {
    t->Assert.deepEqual(
      Square({x: 2.})->S.reverseConvertOrThrow(shapeSchema),
      %raw(`{
        "kind": "square",
        "x": 2,
      }`),
    )
  })

  test("Successfully serializes Triangle shape", t => {
    t->Assert.deepEqual(
      Triangle({x: 2., y: 3.})->S.reverseConvertOrThrow(shapeSchema),
      %raw(`{
        "kind": "triangle",
        "x": 2,
        "y": 3,
      }`),
    )
  })

  test("Compiled parse code snapshot of shape schema", t => {
    t->U.assertCompiledCode(
      ~schema=shapeSchema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i){if(i["kind"]==="circle"){let v0=i["radius"];if(typeof v0!=="number"||Number.isNaN(v0)){e[0](v0)}i={"TAG":e[1],"radius":v0,}}else if(i["kind"]==="square"){let v1=i["x"];if(typeof v1!=="number"||Number.isNaN(v1)){e[2](v1)}i={"TAG":e[3],"x":v1,}}else if(i["kind"]==="triangle"){let v2=i["x"],v3=i["y"];if(typeof v2!=="number"||Number.isNaN(v2)){e[4](v2)}if(typeof v3!=="number"||Number.isNaN(v3)){e[5](v3)}i={"TAG":e[6],"x":v2,"y":v3,}}else{e[7](i)}}else{e[8](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of shape schema", t => {
    t->U.assertCompiledCode(
      ~schema=shapeSchema,
      ~op=#ReverseConvert,
      `i=>{if(typeof i==="object"&&i){if(i["TAG"]==="Circle"){i={"kind":"circle","radius":i["radius"],}}else if(i["TAG"]==="Square"){i={"kind":"square","x":i["x"],}}else if(i["TAG"]==="Triangle"){i={"kind":"triangle","x":i["x"],"y":i["y"],}}}return i}`,
    )
  })
}

test("NaN should be checked before number even if it's later item in the union", t => {
  // This is needed because NaN check might be disabled for number
  // and NaN case won't be reached as expected

  S.global({disableNanNumberValidation: true})

  let schema = S.union([
    S.float->S.min(0)->S.shape(v => Some(v)),
    S.literal(%raw(`NaN`))->S.shape(_ => None),
  ])

  t->Assert.deepEqual(%raw(`NaN`)->S.parseOrThrow(schema), None)
  t->Assert.deepEqual(1.->S.parseOrThrow(schema), Some(1.))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(Number.isNaN(i)){i=e[0]}else if(typeof i==="number"){if(i<e[1]){e[2]()}}else{e[3](i)}return i}`,
  )

  S.global({})
})

test("Array should be checked before object even if it's later item in the union", t => {
  // This is needed because S.object in strip mode doesn't perform
  // array check so actual array case won't be reached as expected

  let schema = S.union([S.object(s => [s.field("foo", S.string)]), S.array(S.string)])

  t->Assert.deepEqual(%raw(`["baz"]`)->S.parseOrThrow(schema), ["baz"])
  t->Assert.deepEqual(%raw(`{"foo": "bar"}`)->S.parseOrThrow(schema), ["bar"])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(Array.isArray(i)){for(let v0=0;v0<i.length;++v0){try{let v2=i[v0];if(typeof v2!=="string"){e[0](v2)}}catch(v1){if(v1&&v1.s===s){v1.path=""+\'["\'+v0+\'"]\'+v1.path}throw v1}}}else if(typeof i==="object"&&i){let v3=i["foo"];if(typeof v3!=="string"){e[1](v3)}i=[v3,]}else{e[2](i)}return i}`,
  )
})

test("Instance schema should be checked before object even if it's later item in the union", t => {
  // This is needed because S.object in strip mode doesn't perform
  // array check so actual array case won't be reached as expected

  let schema = S.union([
    S.object(s => [s.field("foo", S.string)]),
    S.instance(%raw(`Set`))->Obj.magic,
  ])

  t->Assert.deepEqual(%raw(`new Set(["baz"])`)->S.parseOrThrow(schema), %raw(`new Set(["baz"])`))
  t->Assert.deepEqual(%raw(`{"foo": "bar"}`)->S.parseOrThrow(schema), ["bar"])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i instanceof e[0]){}else if(typeof i==="object"&&i){let v0=i["foo"];if(typeof v0!=="string"){e[1](v0)}i=[v0,]}else{e[2](i)}return i}`,
  )
})

test("Two different instance schemas in union", t => {
  let schema = S.union([S.instance(%raw(`Set`))->Obj.magic, S.instance(%raw(`Map`))->Obj.magic])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(i instanceof e[0]||i instanceof e[1])){e[2](i)}return i}`,
  )
})

@unboxed
type uboxedVariant = String(string) | Int(int)
test("Successfully serializes unboxed variant", t => {
  let toInt =
    S.string
    ->S.transform(_ => {
      parser: string => string->Int.fromString->Option.getOrThrow,
      serializer: Int.toString(_),
    })
    ->S.shape(i => Int(i))
  let toString = S.string->S.shape(s => String(s))
  let schema = S.union([toInt, toString])

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), Int(123))
  t->Assert.deepEqual(String("abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`))
  t->Assert.deepEqual(Int(123)->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{i=e[0](i)}catch(e0){e[1](i,e0)}}else{e[2](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{let v0=e[0](i);if(typeof v0!=="string"){e[1](v0)}i=v0}catch(e0){}return i}`,
  )

  // The same, but toString schema is the first
  // toInt is skipped during parsing in this case
  // since it's the second
  let schema = S.union([toString, toInt])

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), String("123"))
  t->Assert.deepEqual(String("abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`))
  t->Assert.deepEqual(Int(123)->S.reverseConvertOrThrow(schema), %raw(`"123"`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(typeof i==="string")){e[0](i)}return i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{if(typeof i!=="string"){e[0](i)}}catch(e0){try{let v0=e[1](i);if(typeof v0!=="string"){e[2](v0)}i=v0}catch(e1){e[3](i,e0,e1)}}return i}`,
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.union([S.literal(0), S.literal(1)])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="number"&&(i===0||i===1))){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(!(typeof i==="number"&&(i===0||i===1))){e[0](i)}return i}`,
  )
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

asyncTest("Compiled async parse code snapshot", async t => {
  let schema = S.union([
    S.literal(0)->S.transform(_ => {asyncParser: i => Promise.resolve(i)}),
    S.literal(1),
  ])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="number"){if(i===0){i=e[0](i)}else if(!(i===1)){e[1](i)}}else{e[2](i)}return Promise.resolve(i)}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ConvertAsync,
    `i=>{if(typeof i==="number"){if(i===0){i=e[0](i)}}return Promise.resolve(i)}`,
  )

  t->Assert.deepEqual(await 1->S.parseAsyncOrThrow(schema), 1)
  t->Assert.throws(
    () => 2->S.parseAsyncOrThrow(schema),
    ~expectations={
      message: "Failed async parsing: Expected 0 | 1, received 2",
    },
  )
})

test("Union with nested variant", t => {
  let schema = S.union([
    S.schema(s =>
      {
        "foo": {
          "tag": #Null(s.matches(S.null(S.string))),
        },
      }
    ),
    S.schema(s =>
      {
        "foo": {
          "tag": #Option(s.matches(S.option(S.string))),
        },
      }
    ),
  ])

  t->Assert.deepEqual(
    {
      "foo": {
        "tag": #Null(None),
      },
    }->S.reverseConvertOrThrow(schema),
    %raw(`{"foo":{"tag":{"NAME":"Null","VAL":null}}}`),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    // TODO: Can make it work without the second case since it doesn't do anything besides i=i
    `i=>{if(typeof i==="object"&&i){if(typeof i["foo"]==="object"&&i["foo"]&&typeof i["foo"]["tag"]==="object"&&i["foo"]["tag"]&&i["foo"]["tag"]["NAME"]==="Null"){let v0=i["foo"];let v1=v0["tag"];let v2=v1["VAL"];if(v2===void 0){v2=null}i={"foo":{"tag":{"NAME":v1["NAME"],"VAL":v2,},},}}else if(typeof i["foo"]==="object"&&i["foo"]&&typeof i["foo"]["tag"]==="object"&&i["foo"]["tag"]&&i["foo"]["tag"]["NAME"]==="Option"){let v3=i["foo"];let v4=v3["tag"];let v5=v4["VAL"];i={"foo":{"tag":{"NAME":v4["NAME"],"VAL":v5,},},}}}return i}`,
  )
})

test("Nested union doesn't mutate the input", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.union([S.string, S.bool->S.to(S.string)])),
    }
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["foo"];if(typeof v0==="boolean"){v0=""+v0}else if(!(typeof v0==="string")){e[1](v0)}return {"foo":v0,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=i["foo"];if(typeof v0==="boolean"){v0=""+v0}return {"foo":v0,}}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.union([S.literal(0), S.literal(1)])

  t->U.assertCompiledCodeIsNoop(~schema, ~op=#Convert)
  t->U.assertCompiledCodeIsNoop(~schema, ~op=#ReverseConvert)
})

test("Compiled serialize code snapshot of objects returning literal fields", t => {
  let schema = S.union([
    S.object(s => s.field("foo", S.literal(0))),
    S.object(s => s.field("bar", S.literal(1))),
  ])

  t->Assert.deepEqual(1->S.reverseConvertOrThrow(schema), %raw(`{"bar":1}`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="number"){if(i===0){i={"foo":i,}}else if(i===1){i={"bar":i,}}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{if(typeof i==="object"&&i){if(i["foo"]===0){i=i["foo"]}else if(i["bar"]===1){i=i["bar"]}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i){if(i["foo"]===0){i=i["foo"]}else if(i["bar"]===1){i=i["bar"]}else{e[0](i)}}else{e[1](i)}return i}`,
  )
})

test("Enum is a shorthand for union", t => {
  t->U.assertEqualSchemas(S.enum([0, 1]), S.union([S.literal(0), S.literal(1)]))
})

test("Reverse schema with items", t => {
  let schema = S.union([S.literal(%raw(`0`)), S.null(S.bool)])

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.union([S.literal(%raw(`0`)), S.bool->S.castToUnknown, S.nullAsUnit->S.reverse]),
  )
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.union([S.literal(%raw(`0`)), S.null(S.bool)])
  t->U.assertReverseParsesBack(schema, None)
})

// Reported in https://gist.github.com/cknitt/4ac6813a3f3bc907187105e01a4324ca
module CrazyUnion = {
  type rec test =
    | A(array<test>)
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z(array<test>)

  let schema = S.recursive("Crazy", schema =>
    S.union([
      S.object(s => {
        s.tag("type", "A")
        A(s.field("nested", S.array(schema)))
      }),
      S.literal(B),
      S.literal(C),
      S.literal(D),
      S.literal(E),
      S.literal(F),
      S.literal(G),
      S.literal(H),
      S.literal(I),
      S.literal(J),
      S.literal(K),
      S.literal(L),
      S.literal(M),
      S.literal(N),
      S.literal(O),
      S.literal(P),
      S.literal(Q),
      S.literal(R),
      S.literal(S),
      S.literal(T),
      S.literal(U),
      S.literal(V),
      S.literal(W),
      S.literal(X),
      S.literal(Y),
      S.object(s => {
        s.tag("type", "Z")
        Z(s.field("nested", S.array(schema)))
      }),
    ])
  )

  test("Compiled parse code snapshot of crazy union", t => {
    S.global({})
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{let v0=e[0](i);return v0}
Crazy: i=>{if(typeof i==="object"&&i){if(i["type"]==="A"){let v0=i["nested"];if(!Array.isArray(v0)){e[0](v0)}let v5=new Array(v0.length);for(let v1=0;v1<v0.length;++v1){let v4;try{let v3=e[1][1](v0[v1]);v4=v3}catch(v2){if(v2&&v2.s===s){v2.path="[\\"nested\\"]"+'["'+v1+'"]'+v2.path}throw v2}v5[v1]=v4}i={"TAG":e[2],"_0":v5,}}else if(i["type"]==="Z"){let v6=i["nested"];if(!Array.isArray(v6)){e[3](v6)}let v11=new Array(v6.length);for(let v7=0;v7<v6.length;++v7){let v10;try{let v9=e[4][1](v6[v7]);v10=v9}catch(v8){if(v8&&v8.s===s){v8.path="[\\"nested\\"]"+'["'+v7+'"]'+v8.path}throw v8}v11[v7]=v10}i={"TAG":e[5],"_0":v11,}}else{e[6](i)}}else if(!(typeof i==="string"&&(i==="B"||i==="C"||i==="D"||i==="E"||i==="F"||i==="G"||i==="H"||i==="I"||i==="J"||i==="K"||i==="L"||i==="M"||i==="N"||i==="O"||i==="P"||i==="Q"||i==="R"||i==="S"||i==="T"||i==="U"||i==="V"||i==="W"||i==="X"||i==="Y"))){e[7](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of crazy union", t => {
    S.global({})
    let reversed = schema->S.reverse
    let code = `i=>{let v0=e[0](i);return v0}
Crazy: i=>{if(typeof i==="object"&&i){if(i["TAG"]==="A"){let v0=i["_0"],v5=new Array(v0.length);for(let v1=0;v1<v0.length;++v1){let v4;try{let v3=e[0][0](v0[v1]);v4=v3}catch(v2){if(v2&&v2.s===s){v2.path="[\\"_0\\"]"+'["'+v1+'"]'+v2.path}throw v2}v5[v1]=v4}i={"type":"A","nested":v5,}}else if(i["TAG"]==="Z"){let v6=i["_0"],v11=new Array(v6.length);for(let v7=0;v7<v6.length;++v7){let v10;try{let v9=e[1][0](v6[v7]);v10=v9}catch(v8){if(v8&&v8.s===s){v8.path="[\\"_0\\"]"+'["'+v7+'"]'+v8.path}throw v8}v11[v7]=v10}i={"type":"Z","nested":v11,}}}return i}`
    t->U.assertCompiledCode(~schema=reversed, ~op=#Convert, code)
    // There was an issue with reverse when it doesn't return the same code on second run
    t->U.assertCompiledCode(~schema=reversed, ~op=#Convert, code)
  })
}

test("json-rpc response", t => {
  let jsonRpcSchema = (okSchema, errorSchema) =>
    S.union([
      S.object(s => Ok(s.field("result", okSchema))),
      S.object(s => Error(s.field("error", errorSchema))),
    ])

  let getLogsResponseSchema = jsonRpcSchema(
    S.array(S.string),
    S.union([
      S.object(s => {
        s.tag("message", "NotFound")
        #LogsNotFound
      }),
      S.object(s => {
        s.tag("message", "Invalid")
        #InvalidData(s.field("data", S.string))
      }),
    ]),
  )

  t->Assert.deepEqual(
    %raw(`{
        "jsonrpc": "2.0",
        "id": 1,
        "result": ["foo", "bar"]
      }`)->S.parseOrThrow(getLogsResponseSchema),
    Ok(["foo", "bar"]),
  )

  t->Assert.deepEqual(
    %raw(`{
        "jsonrpc": "2.0",
        "id": 1,
        "error": {
          "message": "NotFound"
        }
      }`)->S.parseOrThrow(getLogsResponseSchema),
    Error(#LogsNotFound),
  )

  t->Assert.deepEqual(
    %raw(`{
        "jsonrpc": "2.0",
        "id": 1,
        "error": {
          "message": "Invalid",
          "data": "foo"
        }
      }`)->S.parseOrThrow(getLogsResponseSchema),
    Error(#InvalidData("foo")),
  )
})

test("Issue https://github.com/DZakh/rescript-schema/issues/101", t => {
  let syncRequestSchema = S.schema(s =>
    #request({
      "collectionName": s.matches(S.string),
    })
  )
  let syncResponseSchema = S.schema(s =>
    #response({
      "collectionName": s.matches(S.string),
      "response": s.matches(S.enum(["accepted", "rejected"])),
    })
  )
  let schema = S.union([syncRequestSchema, syncResponseSchema])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i){if(i["NAME"]==="request"&&typeof i["VAL"]==="object"&&i["VAL"]){let v0=i["VAL"];i=i}else if(i["NAME"]==="response"&&typeof i["VAL"]==="object"&&i["VAL"]){let v1=i["VAL"];i=i}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i){if(i["NAME"]==="request"&&typeof i["VAL"]==="object"&&i["VAL"]){let v0=i["VAL"];let v1=v0["collectionName"];if(typeof v1!=="string"){e[0](v1)}i={"NAME":i["NAME"],"VAL":{"collectionName":v1,},}}else if(i["NAME"]==="response"&&typeof i["VAL"]==="object"&&i["VAL"]){let v2=i["VAL"];let v3=v2["collectionName"],v4=v2["response"];if(typeof v3!=="string"){e[1](v3)}if(!(typeof v4==="string"&&(v4==="accepted"||v4==="rejected"))){e[2](v4)}i={"NAME":i["NAME"],"VAL":{"collectionName":v3,"response":v4,},}}else{e[3](i)}}else{e[4](i)}return i}`,
  )

  t->Assert.deepEqual(
    #response({
      "collectionName": "foo",
      "response": "accepted",
    })->S.reverseConvertOrThrow(schema),
    #response({
      "collectionName": "foo",
      "response": "accepted",
    })->Obj.magic,
  )
})

test("Regression https://github.com/DZakh/sury/issues/121", t => {
  let schema = S.union([S.literal(%raw(`null`))->S.castToUnknown, S.unknown])

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{try{if(i!==null){e[0](i)}}catch(e0){}return i}`)

  let data = %raw(`{a: 'hey'}`)
  t->Assert.deepEqual(data->S.parseOrThrow(schema), data)
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(schema), %raw(`null`))
})

test("Union of strings with different refinements", t => {
  let schema = S.union([S.string->S.email, S.string->S.url])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{if(!e[0].test(i)){e[1]()}}catch(e0){try{try{new URL(i)}catch(_){e[2]()}}catch(e1){e[3](i,e0,e1)}}}else{e[4](i)}return i}`,
  )
})

test("Objects with the same discriminant", t => {
  let schema = S.union([
    S.object(s => {
      s.tag("type", "A")
      Ok(s.field("value", S.enum(["foo", "bar"])))
    }),
    S.object(s => {
      s.tag("type", "A")
      Error(s.field("value", S.string))
    }),
  ])

  t->Assert.deepEqual(%raw(`{"type":"A","value":"foo"}`)->S.parseOrThrow(schema), Ok("foo"))
  t->Assert.deepEqual(%raw(`{"type":"A","value":"baz"}`)->S.parseOrThrow(schema), Error("baz"))
  t->U.assertThrowsMessage(
    () => %raw(`{"type":"A","value":1}`)->S.parseOrThrow(schema),
    `Failed parsing: Expected { type: "A"; value: "foo" | "bar"; } | { type: "A"; value: string; }, received { type: "A"; value: 1; }
- At ["value"]: Expected "foo" | "bar", received 1
- At ["value"]: Expected string, received 1`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i){if(i["type"]==="A"){try{let v0=i["value"];if(!(typeof v0==="string"&&(v0==="foo"||v0==="bar"))){e[0](v0)}i={"TAG":e[1],"_0":v0,}}catch(e0){try{let v1=i["value"];if(typeof v1!=="string"){e[2](v1)}i={"TAG":e[3],"_0":v1,}}catch(e1){e[4](i,e0,e1)}}}else{e[5](i)}}else{e[6](i)}return i}`,
  )
})
