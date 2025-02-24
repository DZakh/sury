module Suite = {
  module BenchmarkResult = {
    type t

    @send
    external toString: t => string = "toString"
  }

  type t
  type event = {target: BenchmarkResult.t}

  @module("benchmark") @scope("default") @new
  external make: unit => t = "Suite"

  @send
  external add: (t, string, unit => 'a) => t = "add"

  let addWithPrepare = (suite, name, fn) => {
    suite->add(name, fn())
  }

  @send
  external _onCycle: (t, @as(json`"cycle"`) _, event => unit) => t = "on"

  @send
  external _run: t => unit = "run"

  let run = suite => {
    suite
    ->_onCycle(event => {
      Console.log(event.target->BenchmarkResult.toString)
    })
    ->_run
  }
}

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

let makeAdvancedObjectSchema = () => {
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

let makeObjectSchema = () => {
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

S.setGlobalConfig({
  disableNanNumberValidation: true,
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

  let testData1 = Z(Array.make(~length=25, Z(Array.make(~length=25, Z(Array.make(~length=25, Y))))))

  let testData2 = A(Array.make(~length=25, A(Array.make(~length=25, A(Array.make(~length=25, B))))))

  let test = () => {
    Console.time("testData1 serialize")
    let json = S.reverseConvertOrThrow(testData1, schema)
    Console.timeEnd("testData1 serialize")

    Console.time("testData1 parse")
    let _ = S.parseOrThrow(json, schema)
    Console.timeEnd("testData1 parse")

    Console.time("testData2 serialize")
    let json = S.reverseConvertOrThrow(testData2, schema)
    Console.timeEnd("testData2 serialize")

    Console.time("testData2 parse")
    let _ = S.parseOrThrow(json, schema)
    Console.timeEnd("testData2 parse")
  }
}

CrazyUnion.test()

let data = makeTestObject()
Console.time("makeObjectSchema")
let schema = makeObjectSchema()
Console.timeEnd("makeObjectSchema")

Console.time("parseOrThrow: 1")
data->S.parseOrThrow(schema)->ignore
Console.timeEnd("parseOrThrow: 1")
Console.time("parseOrThrow: 2")
data->S.parseOrThrow(schema)->ignore
Console.timeEnd("parseOrThrow: 2")
Console.time("parseOrThrow: 3")
data->S.parseOrThrow(schema)->ignore
Console.timeEnd("parseOrThrow: 3")

Console.time("serializeWith: 1")
data->S.reverseConvertOrThrow(schema)->ignore
Console.timeEnd("serializeWith: 1")
Console.time("serializeWith: 2")
data->S.reverseConvertOrThrow(schema)->ignore
Console.timeEnd("serializeWith: 2")
Console.time("serializeWith: 3")
data->S.reverseConvertOrThrow(schema)->ignore
Console.timeEnd("serializeWith: 3")

Console.time("S.Error.make")
let _ = S.Error.make(
  ~code=OperationFailed("Should be positive"),
  ~flag=S.Flag.typeValidation,
  ~path=S.Path.empty,
)
Console.timeEnd("S.Error.make")

Suite.make()
->Suite.add("S.schema - make", () => makeObjectSchema())
->Suite.addWithPrepare("S.schema - make + parse", () => {
  let data = makeTestObject()
  () => {
    let schema = makeObjectSchema()
    data->S.parseOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.schema - parse", () => {
  let schema = makeObjectSchema()
  let data = makeTestObject()
  () => {
    data->S.parseOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.schema - parse strict", () => {
  S.setGlobalConfig({
    disableNanNumberValidation: true,
    defaultUnknownKeys: Strict,
  })
  let schema = makeObjectSchema()
  S.setGlobalConfig({
    disableNanNumberValidation: true,
  })
  let data = makeTestObject()
  () => {
    data->S.parseOrThrow(schema)
  }
})
->Suite.add("S.schema - make + reverse", () => makeObjectSchema()->S.reverse)
->Suite.addWithPrepare("S.schema - make + reverse convert", () => {
  let data = makeTestObject()
  () => {
    let schema = makeObjectSchema()
    data->S.reverseConvertOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.schema - reverse convert", () => {
  let schema = makeObjectSchema()
  let data = makeTestObject()
  () => {
    data->S.reverseConvertOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.schema - reverse convert (compiled)", () => {
  let schema = makeObjectSchema()
  let data = makeTestObject()
  let fn = schema->S.compile(~input=Value, ~output=Unknown, ~mode=Sync, ~typeValidation=false)
  () => {
    fn(data)
  }
})
->Suite.addWithPrepare("S.schema - assert", () => {
  let schema = makeObjectSchema()
  let data = makeTestObject()
  () => {
    data->S.assertOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.schema - assert (compiled)", () => {
  let schema = makeObjectSchema()
  let data = makeTestObject()
  let assertFn = schema->S.compile(~input=Any, ~output=Assert, ~mode=Sync, ~typeValidation=true)
  () => {
    assertFn(data)
  }
})
->Suite.addWithPrepare("S.schema - assert strict", () => {
  S.setGlobalConfig({
    disableNanNumberValidation: true,
    defaultUnknownKeys: Strict,
  })
  let schema = makeObjectSchema()
  S.setGlobalConfig({
    disableNanNumberValidation: true,
  })
  let data = makeTestObject()
  () => {
    data->S.assertOrThrow(schema)
  }
})
->Suite.add("S.object - make", () => makeAdvancedObjectSchema())
->Suite.addWithPrepare("S.object - make + parse", () => {
  let data = makeTestObject()
  () => {
    let schema = makeAdvancedObjectSchema()
    data->S.parseOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.object - parse", () => {
  let schema = makeAdvancedObjectSchema()
  let data = makeTestObject()
  () => {
    data->S.parseOrThrow(schema)
  }
})
->Suite.add("S.object - make + reverse", () => makeAdvancedObjectSchema()->S.reverse)
->Suite.addWithPrepare("S.object - make + reverse convert", () => {
  let data = makeTestObject()
  () => {
    let schema = makeAdvancedObjectSchema()
    data->S.reverseConvertOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.object - reverse convert", () => {
  let schema = makeAdvancedObjectSchema()
  let data = makeTestObject()
  () => {
    data->S.reverseConvertOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.string - parse", () => {
  let schema = S.string
  let data = "Hello world!"
  () => {
    data->S.parseOrThrow(schema)
  }
})
->Suite.addWithPrepare("S.string - reverse convert", () => {
  let schema = S.string
  let data = "Hello world!"
  () => {
    data->S.reverseConvertOrThrow(schema)
  }
})
->Suite.run

/*
V7.0.1
makeObjectSchema: 0.174ms
parseOrThrow: 1: 0.465ms
parseOrThrow: 2: 0.006ms
parseOrThrow: 3: 0.004ms
serializeWith: 1: 0.208ms
serializeWith: 2: 0.004ms
serializeWith: 3: 0.003ms
S.Error.make: 0.029ms
Parse string x 607,790,506 ops/sec ±0.21% (100 runs sampled)
Reverse convert string x 607,895,909 ops/sec ±0.23% (99 runs sampled)
Advanced object schema factory x 789,559 ops/sec ±0.28% (99 runs sampled)
Parse advanced object x 70,550,720 ops/sec ±0.51% (98 runs sampled)
Create and parse advanced object x 54,592 ops/sec ±0.49% (93 runs sampled)
Parse advanced strict object x 26,614,621 ops/sec ±0.30% (93 runs sampled)
Reverse convert advanced object x 598,233,913 ops/sec ±0.19% (95 runs sampled)
 */

/*
PR remove-definer (before s.nested)

testData1 serialize: 4.949ms
testData1 parse: 3.77ms
testData2 serialize: 0.514ms
testData2 parse: 0.646ms
makeObjectSchema: 0.142ms
parseOrThrow: 1: 0.169ms
parseOrThrow: 2: 0.004ms
parseOrThrow: 3: 0.004ms
serializeWith: 1: 0.095ms
serializeWith: 2: 0.003ms
serializeWith: 3: 0.002ms
S.Error.make: 0.031ms
S.schema - make x 1,123,935 ops/sec ±0.32% (92 runs sampled)
S.schema - make + parse x 187,355 ops/sec ±1.15% (97 runs sampled)
S.schema - parse x 94,323,009 ops/sec ±2.90% (86 runs sampled)
S.schema - parse strict x 29,137,739 ops/sec ±0.78% (95 runs sampled)
S.schema - make + reverse x 1,087,350 ops/sec ±1.33% (96 runs sampled)
S.schema - make + reverse convert x 391,049 ops/sec ±0.94% (98 runs sampled)
S.schema - reverse convert x 108,649,950 ops/sec ±0.95% (99 runs sampled)
S.schema - reverse convert (compiled) x 180,119,516 ops/sec ±6.45% (77 runs sampled)
S.schema - assert x 95,220,749 ops/sec ±3.50% (87 runs sampled)
S.schema - assert (compiled) x 106,000,705 ops/sec ±1.96% (89 runs sampled)
S.schema - assert strict x 29,048,110 ops/sec ±0.85% (95 runs sampled)
S.object - make x 1,061,761 ops/sec ±0.35% (98 runs sampled)
S.object - make + parse x 147,676 ops/sec ±0.25% (97 runs sampled)
S.object - parse x 50,794,005 ops/sec ±1.43% (95 runs sampled)
S.object - make + reverse x 254,673 ops/sec ±0.58% (96 runs sampled)
S.object - make + reverse convert x 135,036 ops/sec ±0.84% (91 runs sampled)
S.object - reverse convert x 58,534,347 ops/sec ±1.90% (85 runs sampled)
S.string - parse x 92,787,449 ops/sec ±2.85% (91 runs sampled)
S.string - reverse convert x 102,776,056 ops/sec ±2.27% (89 runs sampled)

 */

/*
V9 final touches

testData1 serialize: 3.269ms
testData1 parse: 2.357ms
testData2 serialize: 1.077ms
testData2 parse: 0.552ms
makeObjectSchema: 0.156ms
parseOrThrow: 1: 0.328ms
parseOrThrow: 2: 0.012ms
parseOrThrow: 3: 0.005ms
serializeWith: 1: 0.127ms
serializeWith: 2: 0.003ms
serializeWith: 3: 0.002ms
S.Error.make: 0.047ms
S.schema - make x 1,950,402 ops/sec ±1.29% (98 runs sampled)
S.schema - make + parse x 191,489 ops/sec ±2.24% (95 runs sampled)
S.schema - parse x 75,990,331 ops/sec ±2.68% (89 runs sampled)
S.schema - parse strict x 26,483,566 ops/sec ±1.32% (92 runs sampled)
S.schema - make + reverse x 1,354,272 ops/sec ±0.48% (98 runs sampled)
S.schema - make + reverse convert x 404,155 ops/sec ±0.72% (97 runs sampled)
S.schema - reverse convert x 103,090,924 ops/sec ±1.81% (93 runs sampled)
S.schema - reverse convert (compiled) x 181,516,443 ops/sec ±5.88% (75 runs sampled)
S.schema - assert x 97,051,911 ops/sec ±3.07% (87 runs sampled)
S.schema - assert (compiled) x 106,033,893 ops/sec ±2.03% (94 runs sampled)
S.schema - assert strict x 28,592,103 ops/sec ±1.53% (94 runs sampled)
S.object - make x 1,541,313 ops/sec ±2.32% (94 runs sampled)
S.object - make + parse x 150,339 ops/sec ±1.60% (94 runs sampled)
S.object - parse x 53,976,976 ops/sec ±1.99% (91 runs sampled)
S.object - make + reverse x 256,418 ops/sec ±0.81% (93 runs sampled)
S.object - make + reverse convert x 151,959 ops/sec ±1.52% (91 runs sampled)
S.object - reverse convert x 57,477,511 ops/sec ±2.25% (87 runs sampled)
S.string - parse x 87,622,705 ops/sec ±2.25% (91 runs sampled)
S.string - reverse convert x 95,897,043 ops/sec ±1.64% (92 runs sampled)
 */
