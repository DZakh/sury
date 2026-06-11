open Vitest

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

  t->Assert.deepEqual(%raw(`"apple"`)->S.parseOrThrow(~to=schema), #apple)
})

test("Parses when both schemas misses parser and have the same type", t => {
  let schema = S.union([
    S.string->S.transform(_ => {serializer: _ => "apple"}),
    S.string->S.transform(_ => {serializer: _ => "apple"}),
  ])

  try {
    let _ = %raw(`null`)->S.parseOrThrow(~to=schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Exn(error) => t->Assert.is(error.message, `Expected string | string, received null`)
  }

  try {
    let _ = %raw(`"foo"`)->S.parseOrThrow(~to=schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Exn(error) =>
    t->Assert.is(
      error.message,
      `Expected string | string, received "foo"
- The S.transform parser is missing`,
    )
  }

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){e[2](i,e[0],e[1])}else{e[3](i)}return i}`,
  )
})

test("Parses when both schemas misses parser and have different types", t => {
  let schema = S.union([
    S.literal(#apple)->S.transform(_ => {serializer: _ => #apple}),
    S.string->S.transform(_ => {serializer: _ => "apple"}),
  ])

  try {
    let _ = %raw(`null`)->S.parseOrThrow(~to=schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Exn(error) => t->Assert.is(error.message, `Expected "apple" | string, received null`)
  }

  try {
    let _ = %raw(`"abc"`)->S.parseOrThrow(~to=schema)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Exn(error) =>
    t->Assert.is(
      error.message,
      `Expected "apple" | string, received "abc"
- The S.transform parser is missing`,
    )
  }

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){e[2](i,e[0],e[1])}else{e[3](i)}return i}`,
  )
})

test("Serializes when both schemas misses serializer", t => {
  let schema = S.union([
    S.literal(#apple)->S.transform(_ => {parser: _ => #apple}),
    S.string->S.transform(_ => {parser: _ => #apple}),
  ])

  try {
    let _ = %raw(`null`)->S.decodeOrThrow(~from=schema, ~to=S.unknown)
    t->Assert.fail("Expected to throw")
  } catch {
  | S.Exn(error) =>
    t->Assert.is(
      error.message,
      `Expected unknown | unknown, received null
- The S.transform serializer is missing`,
    )
  }

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{e[2](i,e[0],e[1]);return i}`,
  )
})

test("When union of json and string schemas, should parse the first one", t => {
  let schema = S.union([S.json->S.shape(_ => #json), S.string->S.shape(_ => #str)])

  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(~to=schema), #json)
  t->U.assertThrowsMessage(
    () => %raw(`undefined`)->S.parseOrThrow(~to=schema),
    `Expected JSON | string, received undefined
- Expected JSON, received undefined`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{e[0](i);i="json"}catch(e0){if(typeof i==="string"){i="str"}else{e[1](i,e0)}}return i}`,
  )
})

test("Ensures parsing order with unknown schema", t => {
  let schema = S.union([
    S.string->S.length(2),
    S.bool->Obj.magic, // Should be checked before unknown
    S.unknown->S.transform(_ => {parser: _ => "pass"}),
    S.float->Obj.magic,
    S.bigint->Obj.magic,
  ])

  t->Assert.deepEqual(%raw(`"string"`)->S.parseOrThrow(~to=schema), "pass")
  t->Assert.deepEqual(%raw(`"to"`)->S.parseOrThrow(~to=schema), "to")
  t->Assert.deepEqual(%raw(`true`)->S.parseOrThrow(~to=schema), %raw(`true`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{try{typeof i==="string"||e[1](i);i.length===2||e[0](i);}catch(e2){try{typeof i==="boolean"||e[2](i);}catch(e3){try{let v0;try{v0=e[3](i)}catch(x){e[4](x)}i=v0}catch(e4){if(!(typeof i==="number"&&!Number.isNaN(i)||typeof i==="bigint")){e[5](i,e2,e3,e4)}}}}return i}`,
  )
})

test("Parses when second schema misses parser", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {serializer: _ => "apple"})])

  t->Assert.deepEqual("apple"->S.parseOrThrow(~to=schema), #apple)
  t->U.assertThrowsMessage(
    () => "foo"->S.parseOrThrow(~to=schema),
    `Expected "apple" | string, received "foo"
- The S.transform parser is missing`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){if(!(i==="apple")){e[1](i,e[0])}}else{e[2](i)}return i}`,
  )
})

test("Parses with string catch all", t => {
  let schema = S.union([S.literal("apple"), S.string, S.literal("banana")])

  t->Assert.deepEqual("apple"->S.parseOrThrow(~to=schema), "apple")
  t->Assert.deepEqual("foo"->S.parseOrThrow(~to=schema), "foo")

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(typeof i==="string")){e[0](i)}return i}`)
})

// https://github.com/DZakh/sury/issues/115
test("Reports the named union schema when a string-shape fallback rejects a non-string input", t => {
  let schema =
    S.union([
      S.literal(#hyper),
      S.literal(#development),
      S.literal(#small),
      S.literal(#medium),
      S.literal(#large),
      S.literal(#dedicated),
      S.string->S.shape(_ => #unknown),
    ])->S.meta({name: "indexer plan"})

  t->Assert.deepEqual(%raw(`"hyper"`)->S.parseOrThrow(~to=schema), #hyper)
  t->Assert.deepEqual(%raw(`"anything-else"`)->S.parseOrThrow(~to=schema), #unknown)
  t->U.assertThrowsMessage(
    () => %raw(`42`)->S.parseOrThrow(~to=schema),
    `Expected indexer plan, received 42`,
  )

  t->Assert.deepEqual(#hyper->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"hyper"`))
  t->U.assertThrowsMessage(
    () => #unknown->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected indexer plan, received "unknown"
- Missing input for string`,
  )
})

test("Serializes when second struct misses serializer", t => {
  let schema = S.union([S.literal(#apple), S.string->S.transform(_ => {parser: _ => #apple})])

  t->Assert.deepEqual(#apple->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"apple"`))
  // FIXME: literal case should report `Expected "apple"`, but the union codegen
  // folds the literal's discriminant onto `typeValidationOutput` via `pushCheck`
  // (Sury.res:3885), which snaps its error to `typeValidationOutput.expected`
  // (= plain `string()`). Recover the literal's `expected` so this becomes
  // `Expected "apple", received "orange"`.
  t->U.assertThrowsMessage(
    () => #orange->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Expected "apple" | unknown, received "orange"
- Expected string, received "orange"
- The S.transform serializer is missing`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{typeof i==="string"&&(i==="apple")||e[0](i);}catch(e1){e[2](i,e1,e[1])}return i}`,
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
    }`)->S.parseOrThrow(~to=shapeSchema),
      Circle({radius: 1.}),
    )
  })

  test("Successfully parses Square shape", t => {
    t->Assert.deepEqual(
      %raw(`{
      "kind": "square",
      "x": 2,
    }`)->S.parseOrThrow(~to=shapeSchema),
      Square({x: 2.}),
    )
  })

  test("Successfully parses Triangle shape", t => {
    t->Assert.deepEqual(
      %raw(`{
      "kind": "triangle",
      "x": 2,
      "y": 3,
    }`)->S.parseOrThrow(~to=shapeSchema),
      Triangle({x: 2., y: 3.}),
    )
  })

  test("Fails to parse with unknown kind", t => {
    let shape = %raw(`{
      "kind": "oval",
      "x": 2,
      "y": 3,
    }`)

    t->U.assertThrowsMessage(
      () => shape->S.parseOrThrow(~to=shapeSchema),
      `Expected { kind: "circle"; radius: number; } | { kind: "square"; x: number; } | { kind: "triangle"; x: number; y: number; }, received { kind: "oval"; x: 2; y: 3; }`,
    )
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

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{typeof i==="object"&&i||e[6](i);let v0=i["field"];if(typeof v0==="object"&&v0&&!Array.isArray(v0)){if(v0["kind"]==="circle"){let v1=v0["radius"];typeof v1==="number"&&!Number.isNaN(v1)||e[0](v1);v0={"TAG":"Circle","radius":v1,}}else if(v0["kind"]==="square"){let v2=v0["x"];typeof v2==="number"&&!Number.isNaN(v2)||e[1](v2);v0={"TAG":"Square","x":v2,}}else if(v0["kind"]==="triangle"){let v3=v0["x"],v4=v0["y"];typeof v3==="number"&&!Number.isNaN(v3)||e[2](v3);typeof v4==="number"&&!Number.isNaN(v4)||e[3](v4);v0={"TAG":"Triangle","x":v3,"y":v4,}}else{e[4](v0)}}else{e[5](v0)}return v0}`,
    )

    t->U.assertThrowsMessage(
      () => data->S.parseOrThrow(~to=schema),
      `Failed at ["field"]: Expected { kind: "circle"; radius: number; } | { kind: "square"; x: number; } | { kind: "triangle"; x: number; y: number; }, received { kind: "oval"; x: 2; y: 3; }`,
    )
  })

  test("Fails to parse with invalid data type", t => {
    t->U.assertThrowsMessage(
      () => %raw(`"Hello world!"`)->S.parseOrThrow(~to=shapeSchema),
      `Expected { kind: "circle"; radius: number; } | { kind: "square"; x: number; } | { kind: "triangle"; x: number; y: number; }, received "Hello world!"`,
    )
  })

  test("Performs exhaustiveness check on converting without type validation", t => {
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

    t->U.assertThrowsMessage(
      () => v->S.decodeOrThrow(~from=incompleteSchema, ~to=S.unknown),
      `Expected { TAG: "Circle"; radius: number; } | { TAG: "Square"; x: number; }, received { TAG: "Triangle"; x: 2; y: 3; }`,
    )
  })

  test("Successfully serializes Circle shape", t => {
    t->Assert.deepEqual(
      Circle({radius: 1.})->S.decodeOrThrow(~from=shapeSchema, ~to=S.unknown),
      %raw(`{
          "kind": "circle",
          "radius": 1,
        }`),
    )
  })

  test("Successfully serializes Square shape", t => {
    t->Assert.deepEqual(
      Square({x: 2.})->S.decodeOrThrow(~from=shapeSchema, ~to=S.unknown),
      %raw(`{
        "kind": "square",
        "x": 2,
      }`),
    )
  })

  test("Successfully serializes Triangle shape", t => {
    t->Assert.deepEqual(
      Triangle({x: 2., y: 3.})->S.decodeOrThrow(~from=shapeSchema, ~to=S.unknown),
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
      `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["kind"]==="circle"){let v0=i["radius"];typeof v0==="number"&&!Number.isNaN(v0)||e[0](v0);i={"TAG":"Circle","radius":v0,}}else if(i["kind"]==="square"){let v1=i["x"];typeof v1==="number"&&!Number.isNaN(v1)||e[1](v1);i={"TAG":"Square","x":v1,}}else if(i["kind"]==="triangle"){let v2=i["x"],v3=i["y"];typeof v2==="number"&&!Number.isNaN(v2)||e[2](v2);typeof v3==="number"&&!Number.isNaN(v3)||e[3](v3);i={"TAG":"Triangle","x":v2,"y":v3,}}else{e[4](i)}}else{e[5](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of shape schema", t => {
    t->U.assertCompiledCode(
      ~schema=shapeSchema,
      ~op=#Encode,
      // TODO: Can be optimized
      `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["TAG"]==="Circle"){let v0=i["radius"];typeof v0==="number"&&!Number.isNaN(v0)||e[0](v0);i={"kind":"circle","radius":v0,}}else if(i["TAG"]==="Square"){let v1=i["x"];typeof v1==="number"&&!Number.isNaN(v1)||e[1](v1);i={"kind":"square","x":v1,}}else if(i["TAG"]==="Triangle"){let v2=i["x"],v3=i["y"];typeof v2==="number"&&!Number.isNaN(v2)||e[2](v2);typeof v3==="number"&&!Number.isNaN(v3)||e[3](v3);i={"kind":"triangle","x":v2,"y":v3,}}else{e[4](i)}}else{e[5](i)}return i}`,
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

  t->Assert.deepEqual(%raw(`NaN`)->S.parseOrThrow(~to=schema), None)
  t->Assert.deepEqual(1.->S.parseOrThrow(~to=schema), Some(1.))

  // A number that satisfies the type but fails the constraint must surface
  // the constraint-specific message, not the generic union mismatch. This
  // is the regression that the type-narrow / refine partition in B.merge
  // restores — keep this assertion even if the compiled-code snapshot is
  // refreshed, otherwise a refactor that recollapses checks into the
  // routing predicate stays green while breaking error specificity.
  t->U.assertThrowsMessage(
    () => %raw(`-1`)->S.parseOrThrow(~to=schema),
    `Number must be greater than or equal to 0`,
  )
  t->U.assertThrowsMessage(
    () => %raw(`"abc"`)->S.parseOrThrow(~to=schema),
    `Expected number | NaN, received "abc"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(Number.isNaN(i)){i=void 0}else if(typeof i==="number"){i>=e[0]||e[1](i);}else{e[2](i)}return i}`,
  )

  S.global({})
})

test("Array should be checked before object even if it's later item in the union", t => {
  // This is needed because S.object in strip mode doesn't perform
  // array check so actual array case won't be reached as expected

  let schema = S.union([S.object(s => [s.field("foo", S.string)]), S.array(S.string)])

  t->Assert.deepEqual(%raw(`["baz"]`)->S.parseOrThrow(~to=schema), ["baz"])
  t->Assert.deepEqual(%raw(`{"foo": "bar"}`)->S.parseOrThrow(~to=schema), ["bar"])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(Array.isArray(i)){for(let v0=0;v0<i.length;++v0){try{let v1=i[v0];typeof v1==="string"||e[0](v1);}catch(v2){v2.path=\'["\'+v0+\'"]\'+v2.path;throw v2}}}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v3=i["foo"];typeof v3==="string"||e[1](v3);i=[v3,]}else{e[2](i)}return i}`,
  )
})

test("Instance schema should be checked before object even if it's later item in the union", t => {
  // This is needed because S.object in strip mode doesn't perform
  // array check so actual array case won't be reached as expected

  let schema = S.union([
    S.object(s => [s.field("foo", S.string)]),
    S.instance(%raw(`Set`))->Obj.magic,
  ])

  t->Assert.deepEqual(%raw(`new Set(["baz"])`)->S.parseOrThrow(~to=schema), %raw(`new Set(["baz"])`))
  t->Assert.deepEqual(%raw(`{"foo": "bar"}`)->S.parseOrThrow(~to=schema), ["bar"])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(i instanceof e[0]){}else if(typeof i==="object"&&i&&!Array.isArray(i)){let v0=i["foo"];typeof v0==="string"||e[1](v0);i=[v0,]}else{e[2](i)}return i}`,
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

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), Int(123))
  t->Assert.deepEqual("abc"->S.parseOrThrow(~to=schema), String("abc"))
  t->Assert.deepEqual(String("abc")->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"abc"`))
  t->Assert.deepEqual(Int(123)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{let v0;try{v0=e[0](i)}catch(x){e[1](x)}i=v0}catch(e0){}}else{e[2](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="string"||e[2](v0);i=v0}catch(e0){if(!(typeof i==="string")){e[3](i,e0)}}return i}`,
  )

  // The same, but toString schema is the first
  // toInt is skipped during parsing in this case
  // since it's the second
  let schema = S.union([toString, toInt])

  t->Assert.deepEqual("123"->S.parseOrThrow(~to=schema), String("123"))
  t->Assert.deepEqual(String("abc")->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"abc"`))
  t->Assert.deepEqual(Int(123)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`"123"`))

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{if(!(typeof i==="string")){e[0](i)}return i}`)
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{try{typeof i==="string"||e[0](i);}catch(e1){try{let v0;try{v0=e[1](i)}catch(x){e[2](x)}typeof v0==="string"||e[3](v0);i=v0}catch(e2){e[4](i,e1,e2)}}return i}`,
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.union([S.literal(0), S.literal(1)])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="number"&&!Number.isNaN(i)&&(i===0||i===1))){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseParse,
    `i=>{if(!(typeof i==="number"&&!Number.isNaN(i)&&(i===0||i===1))){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{if(!(typeof i==="number"&&!Number.isNaN(i)&&(i===0||i===1))){e[0](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(!(typeof i==="number"&&!Number.isNaN(i)&&(i===0||i===1))){e[0](i)}return i}`,
  )
})

asyncTest("Compiled async parse code snapshot", async t => {
  let schema = S.union([
    S.literal(0)->S.transform(_ => {asyncParser: i => Promise.resolve(i)}),
    S.literal(1),
  ])

  t->U.assertCompiledCode(
    ~schema,
    ~op=#ParseAsync,
    `i=>{if(typeof i==="number"&&!Number.isNaN(i)){if(i===0){let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}i=v0}else if(!(i===1)){e[2](i)}}else{e[3](i)}return Promise.resolve(i)}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ConvertAsync,
    `i=>{if(typeof i==="number"&&!Number.isNaN(i)){if(i===0){let v0;try{v0=e[0](i).catch(x=>e[1](x))}catch(x){e[1](x)}i=v0}else if(!(i===1)){e[2](i)}}else{e[3](i)}return Promise.resolve(i)}`,
  )

  t->Assert.deepEqual(await 1->S.parseAsyncOrThrow(~to=schema), 1)
  t->Assert.throws(
    () => 2->S.parseAsyncOrThrow(~to=schema),
    ~expectations={
      message: "Expected 0 | 1, received 2",
    },
  )
})

test("Union with nested variant", t => {
  let schema = S.union([
    S.schema(s =>
      {
        "foo": {
          "tag": #Null(s.matches(S.nullAsOption(S.string))),
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
    }->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"foo":{"tag":{"NAME":"Null","VAL":null}}}`),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    // TODO: Can optimize it
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){try{let v0=i["foo"];typeof v0==="object"&&v0||e[2](v0);let v1=v0["tag"];typeof v1==="object"&&v1&&v1["NAME"]==="Null"||e[1](v1);let v2=v1["VAL"];if(v2===void 0){v2=null}else if(!(typeof v2==="string")){e[0](v2)}i={"foo":{"tag":{"NAME":v1["NAME"],"VAL":v2,},},}}catch(e0){try{let v3=i["foo"];typeof v3==="object"&&v3||e[5](v3);let v4=v3["tag"];typeof v4==="object"&&v4&&v4["NAME"]==="Option"||e[4](v4);let v5=v4["VAL"];if(!(typeof v5==="string"||v5===void 0)){e[3](v5)}i={"foo":{"tag":{"NAME":v4["NAME"],"VAL":v5,},},}}catch(e1){e[6](i,e0,e1)}}}else{e[7](i)}return i}`,
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
    // FIXME: i["foo"] shouldn't be duplicated
    `i=>{typeof i==="object"&&i||e[1](i);let v0=i["foo"];if(typeof v0==="boolean"){v0=""+i["foo"]}else if(!(typeof v0==="string")){e[0](v0)}return {"foo":v0,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{let v0=i["foo"];if(typeof v0==="boolean"){v0=""+i["foo"]}else if(!(typeof v0==="string")){e[0](v0)}return {"foo":v0,}}`,
  )
})

test("Compiled serialize code snapshot of objects returning literal fields", t => {
  let schema = S.union([
    S.object(s => s.field("foo", S.literal(0))),
    S.object(s => s.field("bar", S.literal(1))),
  ])

  t->Assert.deepEqual(1->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"bar":1}`))

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Encode,
    `i=>{if(typeof i==="number"&&!Number.isNaN(i)){if(i===0){i={"foo":i,}}else if(i===1){i={"bar":i,}}else{e[0](i)}}else{e[1](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Convert,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["foo"]===0){i=i["foo"]}else if(i["bar"]===1){i=i["bar"]}else{e[0](i)}}else{e[1](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["foo"]===0){i=i["foo"]}else if(i["bar"]===1){i=i["bar"]}else{e[0](i)}}else{e[1](i)}return i}`,
  )
})

test("Enum is a shorthand for union", t => {
  t->U.assertEqualSchemas(S.enum([0, 1]), S.union([S.literal(0), S.literal(1)]))
})

test("Reverse schema with items", t => {
  let schema = S.union([S.literal(%raw(`0`)), S.nullAsOption(S.bool)])

  t->U.assertEqualSchemas(
    schema->S.reverse,
    S.union([S.literal(%raw(`0`)), S.bool->S.castToUnknown, S.nullAsUnit->S.reverse]),
  )
})

test("Succesfully uses reversed schema for parsing back to initial value", t => {
  let schema = S.union([S.literal(%raw(`0`)), S.nullAsOption(S.bool)])
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
      `i=>{let v0;v0=e[0](i);return v0}
Crazy: i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["type"]==="A"){let v0=i["nested"];Array.isArray(v0)||e[1](v0);let v4=new Array(v0.length);for(let v1=0;v1<v0.length;++v1){try{let v2;v2=e[0]["unknown->Crazy--0"](v0[v1]);v4[v1]=v2}catch(v3){v3.path="[\\"nested\\"]"+'["'+v1+'"]'+v3.path;throw v3}}i={"TAG":"A","_0":v4,}}else if(i["type"]==="Z"){let v5=i["nested"];Array.isArray(v5)||e[3](v5);let v9=new Array(v5.length);for(let v6=0;v6<v5.length;++v6){try{let v7;v7=e[2]["unknown->Crazy--0"](v5[v6]);v9[v6]=v7}catch(v8){v8.path="[\\"nested\\"]"+'["'+v6+'"]'+v8.path;throw v8}}i={"TAG":"Z","_0":v9,}}else{e[4](i)}}else if(!(typeof i==="string"&&(i==="B"||i==="C"||i==="D"||i==="E"||i==="F"||i==="G"||i==="H"||i==="I"||i==="J"||i==="K"||i==="L"||i==="M"||i==="N"||i==="O"||i==="P"||i==="Q"||i==="R"||i==="S"||i==="T"||i==="U"||i==="V"||i==="W"||i==="X"||i==="Y"))){e[5](i)}return i}`,
    )
  })

  test("Compiled serialize code snapshot of crazy union", t => {
    S.global({})
    let reversed = schema->S.reverse
    let code = `i=>{let v0;v0=e[0](i);return v0}
Crazy: i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["TAG"]==="A"){let v0=i["_0"];Array.isArray(v0)||e[1](v0);let v4=new Array(v0.length);for(let v1=0;v1<v0.length;++v1){try{let v2;v2=e[0](v0[v1]);v4[v1]=v2}catch(v3){v3.path="[\\"_0\\"]"+'["'+v1+'"]'+v3.path;throw v3}}i={"type":"A","nested":v4,}}else if(i["TAG"]==="Z"){let v5=i["_0"];Array.isArray(v5)||e[3](v5);let v9=new Array(v5.length);for(let v6=0;v6<v5.length;++v6){try{let v7;v7=e[2](v5[v6]);v9[v6]=v7}catch(v8){v8.path="[\\"_0\\"]"+'["'+v6+'"]'+v8.path;throw v8}}i={"type":"Z","nested":v9,}}else{e[4](i)}}else if(!(typeof i==="string"&&(i==="B"||i==="C"||i==="D"||i==="E"||i==="F"||i==="G"||i==="H"||i==="I"||i==="J"||i==="K"||i==="L"||i==="M"||i==="N"||i==="O"||i==="P"||i==="Q"||i==="R"||i==="S"||i==="T"||i==="U"||i==="V"||i==="W"||i==="X"||i==="Y"))){e[5](i)}return i}`
    t->U.assertCompiledCode(~schema=reversed, ~op=#Convert, code, ~embedded=[("Crazy", 0)])
    // There was an issue with reverse when it doesn't return the same code on second run
    t->U.assertCompiledCode(~schema=reversed, ~op=#Convert, code, ~embedded=[("Crazy", 0)])
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
      }`)->S.parseOrThrow(~to=getLogsResponseSchema),
    Ok(["foo", "bar"]),
  )

  t->Assert.deepEqual(
    %raw(`{
        "jsonrpc": "2.0",
        "id": 1,
        "error": {
          "message": "NotFound"
        }
      }`)->S.parseOrThrow(~to=getLogsResponseSchema),
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
      }`)->S.parseOrThrow(~to=getLogsResponseSchema),
    Error(#InvalidData("foo")),
  )

  t->U.assertCompiledCode(
    ~schema=getLogsResponseSchema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){try{let v0=i["result"];Array.isArray(v0)||e[1](v0);for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];typeof v2==="string"||e[0](v2);}catch(v3){v3.path="[\\"result\\"]"+'["'+v1+'"]'+v3.path;throw v3}}i={"TAG":"Ok","_0":v0,}}catch(e0){try{let v4=i["error"];if(typeof v4==="object"&&v4&&!Array.isArray(v4)){if(v4["message"]==="NotFound"){v4="LogsNotFound"}else if(v4["message"]==="Invalid"){let v5=v4["data"];typeof v5==="string"||e[2](v5);v4={"NAME":"InvalidData","VAL":v5,}}else{e[3](v4)}}else{e[4](v4)}i={"TAG":"Error","_0":v4,}}catch(e1){e[5](i,e0,e1)}}}else{e[6](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema=getLogsResponseSchema,
    ~op=#Encode,
    // FIXME: Exhaustive check doesn't work
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["TAG"]==="Ok"){let v0=i["_0"];Array.isArray(v0)||e[1](v0);for(let v1=0;v1<v0.length;++v1){try{let v2=v0[v1];typeof v2==="string"||e[0](v2);}catch(v3){v3.path="[\\"_0\\"]"+\'["\'+v1+\'"]\'+v3.path;throw v3}}i={"result":v0,}}else if(i["TAG"]==="Error"){let v4=i["_0"];if(typeof v4==="string"){if(v4==="LogsNotFound"){v4={"message":"NotFound",}}}else if(typeof v4==="object"&&v4&&!Array.isArray(v4)){if(v4["NAME"]==="InvalidData"){let v5=v4["VAL"];typeof v5==="string"||e[2](v5);v4={"message":"Invalid","data":v5,}}}else{e[3](v4)}i={"error":v4,}}else{e[4](i)}}else{e[5](i)}return i}`,
  )

  // FIXME: pin the current (buggy) Encode behaviour for the
  // exhaustive-check gap noted above. The inner per-discriminant arms
  // (LogsNotFound / InvalidData) lack their own `else { fail }`, so a
  // value of the right outer type but a bogus inner variant silently
  // round-trips instead of throwing. When the codegen gap is closed
  // these decodeOrThrow calls will throw — switch them to
  // assertThrowsMessage and remove the FIXME on the snapshot above.
  t->Assert.unsafeDeepEqual(
    %raw(`{TAG:"Error",_0:"BogusVariant"}`)->S.decodeOrThrow(
      ~from=getLogsResponseSchema,
      ~to=S.unknown,
    ),
    %raw(`{"error":"BogusVariant"}`),
  )
  t->Assert.unsafeDeepEqual(
    %raw(`{TAG:"Error",_0:{NAME:"BogusObj"}}`)->S.decodeOrThrow(
      ~from=getLogsResponseSchema,
      ~to=S.unknown,
    ),
    %raw(`{"error":{"NAME":"BogusObj"}}`),
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
    ~op=#Encode,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["NAME"]==="request"){let v0=i["VAL"];typeof v0==="object"&&v0||e[1](v0);let v1=v0["collectionName"];typeof v1==="string"||e[0](v1);i={"NAME":i["NAME"],"VAL":{"collectionName":v1,},}}else if(i["NAME"]==="response"){let v2=i["VAL"];typeof v2==="object"&&v2||e[4](v2);let v3=v2["collectionName"],v4=v2["response"];typeof v3==="string"||e[2](v3);if(!(typeof v4==="string"&&(v4==="accepted"||v4==="rejected"))){e[3](v4)}i={"NAME":i["NAME"],"VAL":{"collectionName":v3,"response":v4,},}}else{e[5](i)}}else{e[6](i)}return i}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["NAME"]==="request"){let v0=i["VAL"];typeof v0==="object"&&v0||e[1](v0);let v1=v0["collectionName"];typeof v1==="string"||e[0](v1);i={"NAME":i["NAME"],"VAL":{"collectionName":v1,},}}else if(i["NAME"]==="response"){let v2=i["VAL"];typeof v2==="object"&&v2||e[4](v2);let v3=v2["collectionName"],v4=v2["response"];typeof v3==="string"||e[2](v3);if(!(typeof v4==="string"&&(v4==="accepted"||v4==="rejected"))){e[3](v4)}i={"NAME":i["NAME"],"VAL":{"collectionName":v3,"response":v4,},}}else{e[5](i)}}else{e[6](i)}return i}`,
  )

  t->Assert.deepEqual(
    #response({
      "collectionName": "foo",
      "response": "accepted",
    })->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    #response({
      "collectionName": "foo",
      "response": "accepted",
    })->Obj.magic,
  )
})

test("Regression https://github.com/DZakh/sury/issues/121", t => {
  let schema = S.union([S.literal(%raw(`null`))->S.castToUnknown, S.unknown])

  t->U.assertCompiledCode(~schema, ~op=#Parse, `i=>{try{i===null||e[0](i);}catch(e1){}return i}`)

  let data = %raw(`{a: 'hey'}`)
  t->Assert.deepEqual(data->S.parseOrThrow(~to=schema), data)
  t->Assert.deepEqual(%raw(`null`)->S.parseOrThrow(~to=schema), %raw(`null`))
})

test("Union of strings with different refinements", t => {
  let schema = S.union([S.email, S.url])

  t->U.assertThrowsMessage(
    () => %raw(`"123"`)->S.parseOrThrow(~to=schema),
    `Expected email | url, received "123"
- Expected email, received "123"
- Expected url, received "123"`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="string"){try{e[0].test(i)||e[1](i);}catch(e0){try{e[2](i)||e[3](i);}catch(e1){e[4](i,e0,e1)}}}else{e[5](i)}return i}`,
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

  t->Assert.deepEqual(%raw(`{"type":"A","value":"foo"}`)->S.parseOrThrow(~to=schema), Ok("foo"))
  t->Assert.deepEqual(%raw(`{"type":"A","value":"baz"}`)->S.parseOrThrow(~to=schema), Error("baz"))
  t->U.assertThrowsMessage(
    () => %raw(`{"type":"A","value":1}`)->S.parseOrThrow(~to=schema),
    `Expected { type: "A"; value: "foo" | "bar"; } | { type: "A"; value: string; }, received { type: "A"; value: 1; }
- At ["value"]: Expected "foo" | "bar", received 1
- At ["value"]: Expected string, received 1`,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["type"]==="A"){try{let v0=i["value"];if(!(typeof v0==="string"&&(v0==="foo"||v0==="bar"))){e[0](v0)}i={"TAG":"Ok","_0":v0,}}catch(e0){try{let v1=i["value"];typeof v1==="string"||e[1](v1);i={"TAG":"Error","_0":v1,}}catch(e1){e[2](i,e0,e1)}}}else{e[3](i)}}else{e[4](i)}return i}`,
  )
})

module CknittelBugReport = {
  module A = {
    type payload = {a?: string}

    let payloadSchema = S.schema(s => {
      a: ?s.matches(S.option(S.string)),
    })

    type t = {payload: payload}

    let schema = S.schema(s => {
      payload: s.matches(payloadSchema),
    })
  }

  module B = {
    type payload = {b?: int}

    let payloadSchema = S.schema(s => {
      b: ?s.matches(S.option(S.int)),
    })

    type t = {payload: payload}

    let schema = S.schema(s => {
      payload: s.matches(payloadSchema),
    })
  }

  type value = A(A.t) | B(B.t)

  test("Union serializing of objects with optional fields", t => {
    let schema = S.union([A.schema->S.shape(m => A(m)), B.schema->S.shape(m => B(m))])

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){try{let v0=i["payload"];typeof v0==="object"&&v0||e[1](v0);let v1=v0["a"];if(!(typeof v1==="string"||v1===void 0)){e[0](v1)}i={"TAG":"A","_0":{"payload":{"a":v1,},},}}catch(e0){try{let v2=i["payload"];typeof v2==="object"&&v2||e[3](v2);let v3=v2["b"];if(!(typeof v3==="number"&&!Number.isNaN(v3)&&(v3<=2147483647&&v3>=-2147483648&&v3%1===0)||v3===void 0)){e[2](v3)}i={"TAG":"B","_0":{"payload":{"b":v3,},},}}catch(e1){e[4](i,e0,e1)}}}else{e[5](i)}return i}`,
    )

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Encode,
      `i=>{if(typeof i==="object"&&i&&!Array.isArray(i)){if(i["TAG"]==="A"){let v0=i["_0"];typeof v0==="object"&&v0||e[2](v0);let v1=v0["payload"];typeof v1==="object"&&v1||e[1](v1);let v2=v1["a"];if(!(typeof v2==="string"||v2===void 0)){e[0](v2)}i={"payload":{"a":v2,},}}else if(i["TAG"]==="B"){let v3=i["_0"];typeof v3==="object"&&v3||e[5](v3);let v4=v3["payload"];typeof v4==="object"&&v4||e[4](v4);let v5=v4["b"];if(!(typeof v5==="number"&&!Number.isNaN(v5)&&(v5<=2147483647&&v5>=-2147483648&&v5%1===0)||v5===void 0)){e[3](v5)}i={"payload":{"b":v5,},}}else{e[6](i)}}else{e[7](i)}return i}`,
    )

    let x = {
      B.payload: {
        b: 42,
      },
    }
    t->Assert.deepEqual(B(x)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"payload":{"b":42}}`))
    let x = {
      A.payload: {
        a: "foo",
      },
    }
    t->Assert.deepEqual(A(x)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"payload":{"a":"foo"}}`))
  })
}

test("Optional of int32 should keep a format validation", t => {
  let schema = S.option(S.int)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(!(typeof i==="number"&&!Number.isNaN(i)&&(i<=2147483647&&i>=-2147483648&&i%1===0)||i===void 0)){e[0](i)}return i}`,
  )
})

// Tagged tuple union — dispatches on i["0"] === "a" / "b", which is what the
// `B.hoistChildChecks` helper lifts from each tuple's literal first field
// into the parent's validation list as union discriminants.
test("Tagged tuple union dispatches via literal first-field discriminant", t => {
  let schema = S.union([
    S.tuple(s => (s.item(0, S.literal("a")), s.item(1, S.string))),
    S.tuple(s => (s.item(0, S.literal("b")), s.item(1, S.string))),
  ])

  t->Assert.deepEqual(("a", "hello")->S.parseOrThrow(~to=schema), ("a", "hello"))
  t->Assert.deepEqual(("b", "world")->S.parseOrThrow(~to=schema), ("b", "world"))
})
