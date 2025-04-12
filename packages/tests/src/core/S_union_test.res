open Ava
open RescriptCore

test("Throws for a Union schema factory without schemas", t => {
  t->Assert.throws(
    () => {
      S.union([])
    },
    ~expectations={
      message: "[rescript-schema] S.union requires at least one item",
    },
    (),
  )
})

test("Successfully creates a Union schema factory with single schema and flattens it", t => {
  let schema = S.union([S.string])

  t->U.assertEqualSchemas(schema, S.string)
})

test("Successfully parses polymorphic variants", t => {
  let schema = S.union([S.literal(#apple), S.literal(#orange)])

  t->Assert.deepEqual(%raw(`"apple"`)->S.parseOrThrow(schema), #apple, ())
})

Failing.test("Parses when both schemas misses parser and have the same type", t => {
  let schema = S.union([
    S.string->S.transform(_ => {serializer: _ => "apple"}),
    S.string->S.transform(_ => {serializer: _ => "apple"}),
  ])

  t->U.assertRaised(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: schema->S.toUnknown,
        received: %raw(`null`),
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )

  t->U.assertRaised(
    () => %raw(`"foo"`)->S.parseOrThrow(schema),
    {
      code: InvalidUnion([
        U.error({
          code: InvalidOperation({description: "The S.transform parser is missing"}),
          operation: Parse,
          path: S.Path.empty,
        }),
        U.error({
          code: InvalidOperation({description: "The S.transform parser is missing"}),
          operation: Parse,
          path: S.Path.empty,
        }),
      ]),
      operation: Parse,
      path: S.Path.empty,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="string"){e[3](i)}else{try{throw e[0]}catch(e0){try{throw e[1]}catch(e1){e[2]([e0,e1,])}}}return i}`,
  )
})

Failing.test("Parses when both schemas misses parser and have different types", t => {
  let schema = S.union([
    S.literal(#apple)->S.transform(_ => {serializer: _ => #apple}),
    S.string->S.transform(_ => {serializer: _ => "apple"}),
  ])

  t->U.assertRaised(
    () => %raw(`null`)->S.parseOrThrow(schema),
    {
      code: InvalidType({
        expected: schema->S.toUnknown,
        received: %raw(`null`),
      }),
      operation: Parse,
      path: S.Path.empty,
    },
  )

  t->U.assertRaised(
    () => %raw(`"abc"`)->S.parseOrThrow(schema),
    {
      code: InvalidOperation({description: "The S.transform parser is missing"}),
      operation: Parse,
      path: S.Path.empty,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(i!=="apple")){throw e[0]}else if(!(typeof i!=="string")){throw e[1]}else{e[2](i)}return i}`,
  )
})

Failing.test("Serializes when both schemas misses serializer", t => {
  let schema = S.union([
    S.literal(#apple)->S.transform(_ => {parser: _ => #apple}),
    S.string->S.transform(_ => {parser: _ => #apple}),
  ])

  t->U.assertRaised(
    () => %raw(`null`)->S.reverseConvertToJsonOrThrow(schema),
    {
      code: InvalidUnion([
        U.error({
          code: InvalidOperation({description: "The S.transform serializer is missing"}),
          operation: ReverseConvertToJson,
          path: S.Path.empty,
        }),
        U.error({
          code: InvalidOperation({description: "The S.transform serializer is missing"}),
          operation: ReverseConvertToJson,
          path: S.Path.empty,
        }),
      ]),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    },
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{throw e[0]}catch(e0){try{throw e[1]}catch(e1){e[2]([e0,e1,])}}return i}`,
  )
})

test("When union of json and string schemas, should parse the first one", t => {
  let schema = S.union([S.json(~validate=false)->S.shape(_ => #json), S.string->S.shape(_ => #str)])

  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(schema), #json, ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{i=e[0]}catch(e0){if(typeof i==="string"){i=e[1]}else{e[2](i)}}return i}`,
  )
})

test("Ensures parsing order with unknown schema", t => {
  let schema = S.union([
    S.string->S.stringLength(2),
    S.bool->Obj.magic, // Should be checked before unknown
    S.custom("unknown string", _ => {parser: _ => "pass"}),
    // TODO: Should disabled deopt at this point
    S.float->Obj.magic,
    S.bigint->Obj.magic,
  ])

  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(schema), "pass", ())
  t->Assert.deepEqual(%raw(`"to"`)->S.parseOrThrow(schema), "to", ())
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(schema), %raw(`true`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{if(typeof i!=="string"){e[0](i)}if(i.length!==e[1]){e[2]()}}catch(e0){try{if(typeof i!=="boolean"){e[3](i)}}catch(e1){try{i=e[4](i)}catch(e2){if(!(typeof i==="number"&&!Number.isNaN(i)||typeof i==="bigint")){e[5](i)}}}}return i}`,
  )
})

test("Parses when second schema misses parser", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {serializer: _ => "apple"})])

  t->Assert.deepEqual("apple"->S.parseOrThrow(schema), #apple, ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){if(!(i==="apple")){try{throw e[0]}catch(e1){e[1](i)}}}else{e[2](i)}return i}`,
  )
})

test("Serializes when second struct misses serializer", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {parser: _ => #apple})])

  t->Assert.deepEqual(#apple->S.reverseConvertOrThrow(schema), %raw(`"apple"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{if(i!=="apple"){e[0](i)}}catch(e0){try{throw e[1]}catch(e1){}}return i}`,
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
      (),
    )
  })

  test("Successfully parses Square shape", t => {
    t->Assert.deepEqual(
      %raw(`{
      "kind": "square",
      "x": 2,
    }`)->S.parseOrThrow(shapeSchema),
      Square({x: 2.}),
      (),
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
      (),
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
        expected: shapeSchema->S.toUnknown,
        received: shape->Obj.magic,
      }),
      operation: Parse,
      path: S.Path.empty,
    }

    t->U.assertRaised(() => shape->S.parseOrThrow(shapeSchema), error)
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
        expected: shapeSchema->S.toUnknown,
        received: shape->Obj.magic,
      }),
      operation: Parse,
      path: S.Path.fromLocation("field"),
    }

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[9](i)}let v0=i["field"];if(typeof v0==="object"&&v0){if(v0["kind"]==="circle"){let v1=v0["radius"];if(typeof v1!=="number"||Number.isNaN(v1)){e[0](v1)}v0={"TAG":e[1],"radius":v1,}}else if(v0["kind"]==="square"){let v2=v0["x"];if(typeof v2!=="number"||Number.isNaN(v2)){e[2](v2)}v0={"TAG":e[3],"x":v2,}}else if(v0["kind"]==="triangle"){let v3=v0["x"],v4=v0["y"];if(typeof v3!=="number"||Number.isNaN(v3)){e[4](v3)}if(typeof v4!=="number"||Number.isNaN(v4)){e[5](v4)}v0={"TAG":e[6],"x":v3,"y":v4,}}else{e[7](v0)}}else{e[8](v0)}return v0}`,
    )

    t->U.assertRaised(() => data->S.parseOrThrow(schema), error)
    t->Assert.is(
      error->U.error->S.Error.message,
      `Failed parsing at ["field"]: Expected { kind: "circle"; radius: number; } | { kind: "square"; x: number; } | { kind: "triangle"; x: number; y: number; }, received {"kind": "oval", "x": 2, "y": 3}`,
      (),
    )
  })

  test("Fails to parse with invalid data type", t => {
    t->U.assertRaised(
      () => %raw(`"Hello world!"`)->S.parseOrThrow(shapeSchema),
      {
        code: InvalidType({
          expected: shapeSchema->S.toUnknown,
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
    t->Assert.is(v->S.reverseConvertOrThrow(incompleteSchema), v->Obj.magic, ())
  })

  test("Successfully serializes Circle shape", t => {
    t->Assert.deepEqual(
      Circle({radius: 1.})->S.reverseConvertOrThrow(shapeSchema),
      %raw(`{
          "kind": "circle",
          "radius": 1,
        }`),
      (),
    )
  })

  test("Successfully serializes Square shape", t => {
    t->Assert.deepEqual(
      Square({x: 2.})->S.reverseConvertOrThrow(shapeSchema),
      %raw(`{
        "kind": "square",
        "x": 2,
      }`),
      (),
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
      (),
    )
  })

  test("Compiled parse code snapshot of shape schema", t => {
    t->U.assertCompiledCode(
      ~schema=shapeSchema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i){if(i["kind"]==="circle"){let v0=i["radius"];if(typeof v0!=="number"||Number.isNaN(v0)){e[0](v0)}i={"TAG":e[1],"radius":v0,}}else if(i["kind"]==="square"){let v1=i["x"];if(typeof v1!=="number"||Number.isNaN(v1)){e[2](v1)}i={"TAG":e[3],"x":v1,}}else if(i["kind"]==="triangle"){let v2=i["x"],v3=i["y"];if(typeof v2!=="number"||Number.isNaN(v2)){e[4](v2)}if(typeof v3!=="number"||Number.isNaN(v3)){e[5](v3)}i={"TAG":e[6],"x":v2,"y":v3,}}}else{e[7](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of shape schema", t => {
    t->U.assertCompiledCode(
      ~schema=shapeSchema,
      ~op=#ReverseConvert,
      `i=>{if(typeof i==="object"&&i){if(i["TAG"]==="Circle"){let v0=i["TAG"];if(v0!=="Circle"){e[0](v0)}i={"kind":e[1],"radius":i["radius"],}}else if(i["TAG"]==="Square"){let v1=i["TAG"];if(v1!=="Square"){e[2](v1)}i={"kind":e[3],"x":i["x"],}}else if(i["TAG"]==="Triangle"){let v2=i["TAG"];if(v2!=="Triangle"){e[4](v2)}i={"kind":e[5],"x":i["x"],"y":i["y"],}}}return i}`,
    )
  })
}

@unboxed
type uboxedVariant = String(string) | Int(int)
test("Successfully serializes unboxed variant", t => {
  let toInt =
    S.string
    ->S.transform(_ => {
      parser: string => string->Int.fromString->Option.getExn,
      serializer: Int.toString(_),
    })
    ->S.shape(i => Int(i))
  let toString = S.string->S.shape(s => String(s))
  let schema = S.union([toInt, toString])

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), Int(123), ())
  t->Assert.deepEqual(String("abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`), ())
  t->Assert.deepEqual(Int(123)->S.reverseConvertOrThrow(schema), %raw(`"123"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{i=e[0](i)}catch(e0){e[1](i)}}else{e[2](i)}return i}`,
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

  t->Assert.deepEqual("123"->S.parseOrThrow(schema), String("123"), ())
  t->Assert.deepEqual(String("abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`), ())
  t->Assert.deepEqual(Int(123)->S.reverseConvertOrThrow(schema), %raw(`"123"`), ())

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(typeof i==="string")){e[0](i)}return i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{try{if(typeof i!=="string"){e[0](i)}}catch(e0){try{let v0=e[1](i);if(typeof v0!=="string"){e[2](v0)}i=v0}catch(e1){}}return i}`,
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

  t->Assert.deepEqual(await 1->S.parseAsyncOrThrow(schema), 1, ())
  t->Assert.throws(
    () => 2->S.parseAsyncOrThrow(schema),
    ~expectations={
      message: "Failed async parsing: Expected 0 | 1, received 2",
    },
    (),
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
    (),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i){try{let v0=i["foo"];let v1=v0["tag"];let v2=v1["NAME"],v3=v1["VAL"];if(v2!=="Null"){e[0](v2)}if(v3===void 0){v3=null}i={"foo":{"tag":{"NAME":v2,"VAL":v3,},},}}catch(e0){try{let v4=i["foo"];let v5=v4["tag"];let v6=v5["NAME"],v7=v5["VAL"];if(v6!=="Option"){e[1](v6)}i=i}catch(e1){}}}return i}`,
  )
})

test("Nested union doesn't mutate the input", t => {
  let schema = S.schema(s =>
    {
      "foo": s.matches(S.union([S.string, S.bool->S.coerce(S.string)])),
    }
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[2](i)}let v0=i["foo"];if(typeof v0==="boolean"){v0=""+v0}else if(!(typeof v0==="string")){e[1](v0)}return {"foo":v0,}}`,
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

  t->Assert.deepEqual(1->S.reverseConvertOrThrow(schema), %raw(`{"bar":1}`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="number"){if(i===0){i={"foo":i,}}else if(i===1){i={"bar":i,}}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    // FIXME: Remove duplicate literal check
    `i=>{if(typeof i==="object"&&i){if(i["foo"]===0){let v0=i["foo"];if(v0!==0){e[0](v0)}i=v0}else if(i["bar"]===1){let v1=i["bar"];if(v1!==1){e[1](v1)}i=v1}}return i}`,
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
    S.union([S.literal(%raw(`0`)), S.option(S.bool)])->S.toUnknown,
  )
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.union([S.literal(%raw(`0`)), S.null(S.bool)])
  t->U.assertReverseParsesBack(schema, None)
})

module CknittelBugReport = {
  module A = {
    @schema
    type payload = {a?: string}

    @schema
    type t = {payload: payload}
  }

  module B = {
    @schema
    type payload = {b?: int}

    @schema
    type t = {payload: payload}
  }

  type value = A(A.t) | B(B.t)

  test("Union serializing of objects with optional fields", t => {
    let schema = S.union([A.schema->S.shape(m => A(m)), B.schema->S.shape(m => B(m))])

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(typeof i==="object"&&i){if(i["TAG"]==="A"){let v0=i["TAG"],v1=i["_0"];if(v0!=="A"){e[0](v0)}let v2=v1["payload"];i=v1}else if(i["TAG"]==="B"){let v4=i["TAG"],v5=i["_0"];if(v4!=="B"){e[1](v4)}let v6=v5["payload"];i=v5}}return i}`,
    )

    let x = {
      B.payload: {
        b: 42,
      },
    }
    t->Assert.deepEqual(B(x)->S.reverseConvertOrThrow(schema), %raw(`{"payload":{"b":42}}`), ())
  })
}

module CknittelBugReport2 = {
  @schema
  type a = {x: int}

  @schema
  type b = {y: string}

  type test = A(a) | B(b)

  let testSchema = S.union([
    S.object(s => {
      s.tag("type", "a")
      A(s.flatten(aSchema))
    }),
    S.object(s => {
      s.tag("type", "b")
      B(s.flatten(bSchema))
    }),
  ])

  @schema
  type t = {test: option<test>}

  test("Successfully parses nested optional union", t => {
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      // FIXME: Improve nested union handling
      `i=>{if(typeof i!=="object"||!i){e[7](i)}let v0=i["test"];try{if(typeof v0==="object"&&v0){if(v0["type"]==="a"){let v1=v0["x"];if(typeof v1!=="number"||v1>2147483647||v1<-2147483648||v1%1!==0){e[0](v1)}v0={"TAG":e[1],"_0":{"x":v1,},}}else if(v0["type"]==="b"){let v2=v0["y"];if(typeof v2!=="string"){e[2](v2)}v0={"TAG":e[3],"_0":{"y":v2,},}}else{e[4](v0)}}else{e[5](v0)}v0=v0}catch(e0){if(!(v0===void 0)){e[6](v0)}}return {"test":v0,}}`,
    )

    t->Assert.deepEqual(S.parseJsonStringOrThrow("{}", schema), {test: None}, ())
  })

  type responseError = {serviceCode: string, text: string}

  test("Nested literal field with catch", t => {
    let schema = S.union([
      S.object(s => {
        let _ = s.nested("statusCode").field("kind", S.literal("ok"))
        let _ = s.nested("statusCode").field("text", S.literal("")->S.catch(_ => ""))
        Ok()
      }),
      S.object(s => {
        let _ = s.nested("statusCode").field("kind", S.literal("serviceError"))
        Error({
          serviceCode: s.nested("statusCode").field("serviceCode", S.string),
          text: s.nested("statusCode").field("text", S.string),
        })
      }),
    ])

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i){try{let v0=i["statusCode"];if(typeof v0!=="object"||!v0||v0["kind"]!=="ok"){e[0](v0)}let v1=v0["text"];try{if(v1!==""){e[2](v1)}}catch(v2){if(v2&&v2.s===s){v1=e[1](v1,v2)}else{throw v2}}i={"TAG":e[3],"_0":e[4],}}catch(e0){try{let v3=i["statusCode"];if(typeof v3!=="object"||!v3||v3["kind"]!=="serviceError"){e[5](v3)}let v4=v3["serviceCode"],v5=v3["text"];if(typeof v4!=="string"){e[6](v4)}if(typeof v5!=="string"){e[7](v5)}i={"TAG":e[8],"_0":{"serviceCode":v4,"text":v5,},}}catch(e1){e[9](i)}}}else{e[10](i)}return i}`,
    )

    t->Assert.deepEqual(
      S.parseJsonStringOrThrow(`{"statusCode": {"kind": "ok"}}`, schema),
      Ok(),
      (),
    )
  })
}

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

  let schema = S.recursive(schema =>
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
    S.setGlobalConfig({})
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{let r0=i=>{if(typeof i==="object"&&i){if(i["type"]==="A"){let v0=i["nested"],v4=new Array(v0.length);if(!Array.isArray(v0)){e[0](v0)}for(let v1=0;v1<v0.length;++v1){let v3;try{v3=r0(v0[v1])}catch(v2){if(v2&&v2.s===s){v2.path="[\\"nested\\"]"+\'["\'+v1+\'"]\'+v2.path}throw v2}v4[v1]=v3}i={"TAG":e[1],"_0":v4,}}else if(i["type"]==="Z"){let v5=i["nested"],v9=new Array(v5.length);if(!Array.isArray(v5)){e[2](v5)}for(let v6=0;v6<v5.length;++v6){let v8;try{v8=r0(v5[v6])}catch(v7){if(v7&&v7.s===s){v7.path="[\\"nested\\"]"+\'["\'+v6+\'"]\'+v7.path}throw v7}v9[v6]=v8}i={"TAG":e[3],"_0":v9,}}else{e[4](i)}}else if(!(typeof i==="string"&&(i==="B"||i==="C"||i==="D"||i==="E"||i==="F"||i==="G"||i==="H"||i==="I"||i==="J"||i==="K"||i==="L"||i==="M"||i==="N"||i==="O"||i==="P"||i==="Q"||i==="R"||i==="S"||i==="T"||i==="U"||i==="V"||i==="W"||i==="X"||i==="Y"))){e[5](i)}return i};return r0(i)}`,
    )
  })

  test("Compiled serialize code snapshot of crazy union", t => {
    S.setGlobalConfig({})
    let code = `i=>{let r0=i=>{if(typeof i==="object"&&i){if(i["TAG"]==="A"){let v0=i["TAG"],v1=i["_0"],v5=new Array(v1.length);if(v0!=="A"){e[0](v0)}for(let v2=0;v2<v1.length;++v2){let v4;try{v4=r0(v1[v2])}catch(v3){if(v3&&v3.s===s){v3.path="[\\"_0\\"]"+\'["\'+v2+\'"]\'+v3.path}throw v3}v5[v2]=v4}i={"type":e[1],"nested":v5,}}else if(i["TAG"]==="Z"){let v6=i["TAG"],v7=i["_0"],v11=new Array(v7.length);if(v6!=="Z"){e[2](v6)}for(let v8=0;v8<v7.length;++v8){let v10;try{v10=r0(v7[v8])}catch(v9){if(v9&&v9.s===s){v9.path="[\\"_0\\"]"+\'["\'+v8+\'"]\'+v9.path}throw v9}v11[v8]=v10}i={"type":e[3],"nested":v11,}}}return i};return r0(i)}`
    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, code)
    // There was an issue with reverse when it doesn't return the same code on second run
    t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, code)
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
    (),
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
    (),
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
    (),
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

  // FIXME: Don't need to repeat the literal check after it's done in the typeFilter
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(typeof i==="object"&&i){if(i["NAME"]==="request"){let v0=i["NAME"],v1=i["VAL"];if(v0!=="request"){e[0](v0)}i=i}else if(i["NAME"]==="response"){let v2=i["NAME"],v3=i["VAL"];if(v2!=="response"){e[1](v2)}i=i}}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i){if(i["NAME"]==="request"){let v0=i["VAL"];if(typeof v0!=="object"||!v0){e[0](v0)}let v1=v0["collectionName"];if(typeof v1!=="string"){e[1](v1)}i={"NAME":i["NAME"],"VAL":{"collectionName":v1,},}}else if(i["NAME"]==="response"){let v2=i["VAL"];if(typeof v2!=="object"||!v2){e[2](v2)}let v3=v2["collectionName"],v4=v2["response"];if(typeof v3!=="string"){e[3](v3)}if(!(typeof v4==="string"&&(v4==="accepted"||v4==="rejected"))){e[4](v4)}i={"NAME":i["NAME"],"VAL":{"collectionName":v3,"response":v4,},}}else{e[5](i)}}else{e[6](i)}return i}`,
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
    (),
  )
})
