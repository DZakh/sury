open Ava

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

test("Parses when both schemas misses parser and have the same type", t => {
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

test("Parses when both schemas misses parser and have different types", t => {
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
    `i=>{if(i!=="apple"){if(typeof i!=="string"){e[2](i)}else{throw e[1]}}else{throw e[0]}return i}`,
  )
})

test("Serializes when both schemas misses serializer", t => {
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

  // FIXME: This is not working. Should be #json instead
  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(schema), #str, ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{let v0=i;if(typeof i!=="string"){v0=e[0]}else{v0=e[1]}return v0}`,
  )
})

test("Parses when second struct misses parser", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {serializer: _ => "apple"})])

  t->Assert.deepEqual("apple"->S.parseOrThrow(schema), #apple, ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i!=="apple"){if(typeof i!=="string"){e[1](i)}else{throw e[0]}}return i}`,
  )
})

test("Serializes when second struct misses serializer", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {parser: _ => #apple})])

  t->Assert.deepEqual(#apple->S.reverseConvertOrThrow(schema), %raw(`"apple"`), ())

  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{if(i!=="apple"){throw e[0]}return i}`)
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

    t->U.assertRaised(() => data->S.parseOrThrow(schema), error)
    t->Assert.is(
      error->U.error->S.Error.message,
      `Failed parsing at ["field"]. Reason: Expected { kind: "circle"; radius: number; } | { kind: "square"; x: number; } | { kind: "triangle"; x: number; y: number; }, received { "kind": "oval", "x": 2, "y": 3 }`,
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

  test("Fails to serialize incomplete schema", t => {
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

    let error: U.errorPayload = {
      code: InvalidType({
        expected: incompleteSchema->S.reverse,
        received: Triangle({x: 2., y: 3.})->Obj.magic,
      }),
      operation: ReverseConvert,
      path: S.Path.empty,
    }

    t->U.assertRaised(
      () => Triangle({x: 2., y: 3.})->S.reverseConvertOrThrow(incompleteSchema),
      error,
    )

    t->Assert.is(
      error->U.error->S.Error.message,
      `Failed converting at root. Reason: Expected { TAG: "Circle"; radius: number; } | { TAG: "Square"; x: number; }, received { "TAG": "Triangle", "x": 2, "y": 3 }`,
      (),
    )
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
      `i=>{let v1=i;if(typeof i!=="object"||!i||i["kind"]!=="circle"){if(typeof i!=="object"||!i||i["kind"]!=="square"){if(typeof i!=="object"||!i||i["kind"]!=="triangle"){e[7](i)}else{let v3=i["x"],v4=i["y"];if(typeof v3!=="number"||Number.isNaN(v3)){e[4](v3)}if(typeof v4!=="number"||Number.isNaN(v4)){e[5](v4)}v1={"TAG":e[6],"x":v3,"y":v4,}}}else{let v2=i["x"];if(typeof v2!=="number"||Number.isNaN(v2)){e[2](v2)}v1={"TAG":e[3],"x":v2,}}}else{let v0=i["radius"];if(typeof v0!=="number"||Number.isNaN(v0)){e[0](v0)}v1={"TAG":e[1],"radius":v0,}}return v1}`,
    )
  })

  test("Compiled serialize code snapshot of shape schema", t => {
    t->U.assertCompiledCode(
      ~schema=shapeSchema,
      ~op=#ReverseConvert,
      `i=>{let v1=i;if(typeof i!=="object"||!i||i["TAG"]!=="Circle"){if(typeof i!=="object"||!i||i["TAG"]!=="Square"){if(typeof i!=="object"||!i||i["TAG"]!=="Triangle"){e[6](i)}else{let v3=i["TAG"];if(v3!=="Triangle"){e[4](v3)}v1={"kind":e[5],"x":i["x"],"y":i["y"],}}}else{let v2=i["TAG"];if(v2!=="Square"){e[2](v2)}v1={"kind":e[3],"x":i["x"],}}}else{let v0=i["TAG"];if(v0!=="Circle"){e[0](v0)}v1={"kind":e[1],"radius":i["radius"],}}return v1}`,
    )
  })
}

@unboxed
type uboxedVariant = String(string) | Int(int)
test("Successfully serializes unboxed variant", t => {
  let schema = S.union([
    S.string->S.shape(s => String(s)),
    S.string
    ->S.transform(_ => {
      parser: string => string->Int.fromString->Option.getExn,
      serializer: Int.toString(_),
    })
    ->S.shape(i => Int(i)),
  ])

  t->Assert.deepEqual(String("abc")->S.reverseConvertOrThrow(schema), %raw(`"abc"`), ())
  t->Assert.deepEqual(Int(123)->S.reverseConvertOrThrow(schema), %raw(`"123"`), ())

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=i;if(typeof i!=="string"){v0=e[0](i)}return v0}`,
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.union([S.literal(0), S.literal(1)])

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(i!==0){if(i!==1){e[0](i)}}return i}`)
})

test("Compiled async parse code snapshot", t => {
  let schema = S.union([
    S.literal(0)->S.transform(_ => {asyncParser: i => Promise.resolve(i)}),
    S.literal(1),
  ])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{let v0=i;if(i!==0){if(i!==1){e[1](i)}}else{v0=e[0](i)}return Promise.resolve(v0)}`,
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
    `i=>{let v5=i;if(typeof i!=="object"||!i){e[3](i)}else{try{let v0=i["foo"];let v1=v0["tag"];let v2=v1["NAME"],v3=v1["VAL"],v4;if(v2!=="Null"){e[0](v2)}if(v3!==void 0){v4=v3}else{v4=null}v5={"foo":{"tag":{"NAME":v2,"VAL":v4,},},}}catch(e0){try{let v6=i["foo"];let v7=v6["tag"];let v8=v7["NAME"];if(v8!=="Option"){e[1](v8)}v5=i}catch(e1){e[2]([e0,e1,])}}}return v5}`,
  )
})

test("Compiled serialize code snapshot", t => {
  let schema = S.union([S.literal(0), S.literal(1)])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i!==0){if(i!==1){e[0](i)}}return i}`,
  )
})

test("Compiled serialize code snapshot of objects returning literal fields", t => {
  let schema = S.union([
    S.object(s => s.field("foo", S.literal(0))),
    S.object(s => s.field("bar", S.literal(1))),
  ])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=i;if(i!==0){if(i!==1){e[0](i)}else{v0={"bar":i,}}}else{v0={"foo":i,}}return v0}`,
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
      `i=>{let v3=i;if(typeof i!=="object"||!i||i["TAG"]!=="A"){if(typeof i!=="object"||!i||i["TAG"]!=="B"){e[2](i)}else{let v4=i["TAG"],v5=i["_0"];if(v4!=="B"){e[1](v4)}let v6=v5["payload"];v3=v5}}else{let v0=i["TAG"],v1=i["_0"];if(v0!=="A"){e[0](v0)}let v2=v1["payload"];v3=v1}return v3}`,
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
      `i=>{if(typeof i!=="object"||!i){e[5](i)}let v0=i["test"],v4;if(v0!==void 0){let v2=v0;if(typeof v0!=="object"||!v0||v0["type"]!=="a"){if(typeof v0!=="object"||!v0||v0["type"]!=="b"){e[4](v0)}else{let v3=v0["y"];if(typeof v3!=="string"){e[2](v3)}v2={"TAG":e[3],"_0":{"y":v3,},}}}else{let v1=v0["x"];if(typeof v1!=="number"||v1>2147483647||v1<-2147483648||v1%1!==0){e[0](v1)}v2={"TAG":e[1],"_0":{"x":v1,},}}v4=v2}return {"test":v4,}}`,
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
      `i=>{let v3=i;if(typeof i!=="object"||!i){e[10](i)}else{try{let v0=i["statusCode"];if(typeof v0!=="object"||!v0||v0["kind"]!=="ok"||false){e[0](v0)}let v1=v0["text"];try{if(v1!==""){e[2](v1)}}catch(v2){if(v2&&v2.s===s){v1=e[1](v1,v2)}else{throw v2}}v3={"TAG":e[3],"_0":e[4],}}catch(e0){try{let v4=i["statusCode"];if(typeof v4!=="object"||!v4||v4["kind"]!=="serviceError"){e[5](v4)}let v5=v4["serviceCode"],v6=v4["text"];if(typeof v5!=="string"){e[6](v5)}if(typeof v6!=="string"){e[7](v6)}v3={"TAG":e[8],"_0":{"serviceCode":v5,"text":v6,},}}catch(e1){e[9]([e0,e1,])}}}return v3}`,
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
      `i=>{let r0=i=>{let v5=i;if(typeof i!=="object"||!i||i["type"]!=="A"){if(i!=="B"){if(i!=="C"){if(i!=="D"){if(i!=="E"){if(i!=="F"){if(i!=="G"){if(i!=="H"){if(i!=="I"){if(i!=="J"){if(i!=="K"){if(i!=="L"){if(i!=="M"){if(i!=="N"){if(i!=="O"){if(i!=="P"){if(i!=="Q"){if(i!=="R"){if(i!=="S"){if(i!=="T"){if(i!=="U"){if(i!=="V"){if(i!=="W"){if(i!=="X"){if(i!=="Y"){if(typeof i!=="object"||!i||i["type"]!=="Z"){e[4](i)}else{let v6=i["nested"],v10=new Array(v6.length);if(!Array.isArray(v6)){e[2](v6)}for(let v7=0;v7<v6.length;++v7){let v9;try{v9=r0(v6[v7])}catch(v8){if(v8&&v8.s===s){v8.path="[\\"nested\\"]"+\'["\'+v7+\'"]\'+v8.path}throw v8}v10[v7]=v9}v5={"TAG":e[3],"_0":v10,}}}}}}}}}}}}}}}}}}}}}}}}}}}else{let v0=i["nested"],v4=new Array(v0.length);if(!Array.isArray(v0)){e[0](v0)}for(let v1=0;v1<v0.length;++v1){let v3;try{v3=r0(v0[v1])}catch(v2){if(v2&&v2.s===s){v2.path="[\\"nested\\"]"+\'["\'+v1+\'"]\'+v2.path}throw v2}v4[v1]=v3}v5={"TAG":e[1],"_0":v4,}}return v5};return r0(i)}`,
    )
  })

  test("Compiled serialize code snapshot of crazy union", t => {
    S.setGlobalConfig({})
    let code = `i=>{let r0=i=>{let v6=i;if(typeof i!=="object"||!i||i["TAG"]!=="A"){if(i!=="B"){if(i!=="C"){if(i!=="D"){if(i!=="E"){if(i!=="F"){if(i!=="G"){if(i!=="H"){if(i!=="I"){if(i!=="J"){if(i!=="K"){if(i!=="L"){if(i!=="M"){if(i!=="N"){if(i!=="O"){if(i!=="P"){if(i!=="Q"){if(i!=="R"){if(i!=="S"){if(i!=="T"){if(i!=="U"){if(i!=="V"){if(i!=="W"){if(i!=="X"){if(i!=="Y"){if(typeof i!=="object"||!i||i["TAG"]!=="Z"){e[4](i)}else{let v7=i["TAG"],v8=i["_0"],v12=new Array(v8.length);if(v7!=="Z"){e[2](v7)}for(let v9=0;v9<v8.length;++v9){let v11;try{v11=r0(v8[v9])}catch(v10){if(v10&&v10.s===s){v10.path="[\\"_0\\"]"+\'["\'+v9+\'"]\'+v10.path}throw v10}v12[v9]=v11}v6={"type":e[3],"nested":v12,}}}}}}}}}}}}}}}}}}}}}}}}}}}else{let v0=i["TAG"],v1=i["_0"],v5=new Array(v1.length);if(v0!=="A"){e[0](v0)}for(let v2=0;v2<v1.length;++v2){let v4;try{v4=r0(v1[v2])}catch(v3){if(v3&&v3.s===s){v3.path="[\\"_0\\"]"+\'["\'+v2+\'"]\'+v3.path}throw v3}v5[v2]=v4}v6={"type":e[1],"nested":v5,}}return v6};return r0(i)}`
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
    `i=>{let v2=i;if(typeof i!=="object"||!i||i["NAME"]!=="request"){if(typeof i!=="object"||!i||i["NAME"]!=="response"){e[3](i)}else{let v3=i["NAME"],v4=i["VAL"];if(v3!=="response"){e[1](v3)}let v5=v4["response"];if(v5!=="accepted"){if(v5!=="rejected"){e[2](v5)}}v2={"NAME":v3,"VAL":{"collectionName":v4["collectionName"],"response":v5,},}}}else{let v0=i["NAME"],v1=i["VAL"];if(v0!=="request"){e[0](v0)}v2=i}return v2}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{let v2=i;if(typeof i!=="object"||!i||i["NAME"]!=="request"){if(typeof i!=="object"||!i||i["NAME"]!=="response"){e[5](i)}else{let v3=i["VAL"];if(typeof v3!=="object"||!v3){e[2](v3)}let v4=v3["collectionName"],v5=v3["response"];if(typeof v4!=="string"){e[3](v4)}if(v5!=="accepted"){if(v5!=="rejected"){e[4](v5)}}v2={"NAME":i["NAME"],"VAL":{"collectionName":v4,"response":v5,},}}}else{let v0=i["VAL"];if(typeof v0!=="object"||!v0){e[0](v0)}let v1=v0["collectionName"];if(typeof v1!=="string"){e[1](v1)}v2={"NAME":i["NAME"],"VAL":{"collectionName":v1,},}}return v2}`,
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
