open Vitest

test("Successfully parses object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\"\'\`", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{"\"\'\`": "bar"}`)->S.parseOrThrow(~to=schema), {"field": "bar"})
})

test("Successfully parses object with new line in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\n", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{"\n": "bar"}`)->S.parseOrThrow(~to=schema), {"field": "bar"})
})

test("Successfully serializing object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\"\'\`", S.string),
    }
  )

  t->Assert.deepEqual(
    {"field": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"\"\'\`": "bar"}`),
  )
})

test("Successfully parses object transformed to object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "\"\'\`": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{"field": "bar"}`)->S.parseOrThrow(~to=schema), {"\"\'\`": "bar"})
})

test("Successfully serializes object transformed to object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "\"\'\`": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual(
    {"\"\'\`": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{"field": "bar"}`),
  )
})

test("Successfully parses object with discriminant which has quotes as the field name", t => {
  let schema = S.object(s => {
    ignore(s.field("\"\'\`", S.literal(Null.null)))
    {
      "field": s.field("field", S.string),
    }
  })

  t->Assert.deepEqual(
    %raw(`{
      "\"\'\`": null,
      "field": "bar",
    }`)->S.parseOrThrow(~to=schema),
    {"field": "bar"},
  )
})

test("Successfully serializes object with discriminant which has quotes as the field name", t => {
  let schema = S.object(s => {
    ignore(s.field("\"\'\`", S.literal(Null.null)))
    {
      "field": s.field("field", S.string),
    }
  })

  t->Assert.deepEqual(
    {"field": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{
        "\"\'\`": null,
        "field": "bar",
      }`),
  )
})

test("Successfully parses object with discriminant which has quotes as the literal value", t => {
  let schema = S.object(s => {
    ignore(s.field("kind", S.literal("\"\'\`")))
    {
      "field": s.field("field", S.string),
    }
  })

  t->Assert.deepEqual(
    %raw(`{
      "kind": "\"\'\`",
      "field": "bar",
    }`)->S.parseOrThrow(~to=schema),
    {"field": "bar"},
  )
})

test(
  "Successfully serializes object with discriminant which has quotes as the literal value",
  t => {
    let schema = S.object(s => {
      ignore(s.field("kind", S.literal("\"\'\`")))
      {
        "field": s.field("field", S.string),
      }
    })

    t->Assert.deepEqual(
      {"field": "bar"}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      %raw(`{
          "kind": "\"\'\`",
          "field": "bar",
        }`),
    )
  },
)

test(
  "Successfully parses object transformed to object with quotes in name of hardcoded field",
  t => {
    let schema = S.object(s =>
      {
        "\"\'\`": "hardcoded",
        "field": s.field("field", S.string),
      }
    )

    t->Assert.deepEqual(
      %raw(`{"field": "bar"}`)->S.parseOrThrow(~to=schema),
      {
        "\"\'\`": "hardcoded",
        "field": "bar",
      },
    )
  },
)

test(
  "Successfully serializes object transformed to object with quotes in name of hardcoded field",
  t => {
    let schema = S.object(s =>
      {
        "\"\'\`": "hardcoded",
        "field": s.field("field", S.string),
      }
    )

    t->Assert.deepEqual(
      {
        "\"\'\`": "hardcoded",
        "field": "bar",
      }->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      %raw(`{"field": "bar"}`),
    )
  },
)

test(
  "Successfully parses object transformed to object with quotes in value of hardcoded field",
  t => {
    let schema = S.object(s =>
      {
        "hardcoded": "\"\'\`",
        "field": s.field("field", S.string),
      }
    )

    t->Assert.deepEqual(
      %raw(`{"field": "bar"}`)->S.parseOrThrow(~to=schema),
      {
        "hardcoded": "\"\'\`",
        "field": "bar",
      },
    )
  },
)

test(
  "Successfully serializes object transformed to object with quotes in value of hardcoded field",
  t => {
    let schema = S.object(s =>
      {
        "hardcoded": "\"\'\`",
        "field": s.field("field", S.string),
      }
    )

    t->Assert.deepEqual(
      {
        "hardcoded": "\"\'\`",
        "field": "bar",
      }->S.decodeOrThrow(~from=schema, ~to=S.unknown),
      %raw(`{"field": "bar"}`),
    )
  },
)

test("Has proper error path when fails to parse object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\"\'\`", S.string->S.refine(_ => false, ~error="User error")),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{"\"\'": "bar"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["\\"\'\`"]: Expected string, received undefined`,
  )
})

test("Has proper error path when fails to serialize object with quotes in a field name", t => {
  let schema = S.object(s =>
    Dict.fromArray([
      ("\"\'\`", s.field("field", S.string->S.refine(_ => false, ~error="User error"))),
    ])
  )

  t->U.assertThrowsMessage(
    () => Dict.fromArray([("\"'", "bar")])->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Failed at ["\\"'\`"]: User error`,
  )
})

test("Field name in a format of a path is handled properly", t => {
  let schema = S.object(s =>
    {
      "field": s.field(`["abc"]["cde"]`, S.string),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{"bar": "foo"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["[\\"abc\\"][\\"cde\\"]"]: Expected string, received undefined`,
  )
})

test("Successfully parses object with field names that shadow Object.prototype members", t => {
  let schema = S.object(s =>
    {
      "constructor": s.field("constructor", S.string),
      "hasOwnProperty": s.field("hasOwnProperty", S.float),
      "toString": s.field("toString", S.bool),
      "valueOf": s.field("valueOf", S.string),
    }
  )

  t->Assert.deepEqual(
    %raw(`{constructor: "a", hasOwnProperty: 1, toString: true, valueOf: "b"}`)->S.parseOrThrow(
      ~to=schema,
    ),
    {
      "constructor": "a",
      "hasOwnProperty": 1.,
      "toString": true,
      "valueOf": "b",
    },
  )
})

test("Successfully serializes object with field names that shadow Object.prototype members", t => {
  let schema = S.object(s =>
    {
      "constructor": s.field("constructor", S.string),
      "hasOwnProperty": s.field("hasOwnProperty", S.float),
    }
  )

  t->Assert.deepEqual(
    {
      "constructor": "a",
      "hasOwnProperty": 1.,
    }->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    %raw(`{constructor: "a", hasOwnProperty: 1}`),
  )
})

test("Fails with a proper error path for a field named after an Object.prototype member", t => {
  let schema = S.object(s =>
    {
      "field": s.field("constructor", S.string),
    }
  )

  t->U.assertThrowsMessage(
    () => %raw(`{"constructor": 1}`)->S.parseOrThrow(~to=schema),
    `Failed at ["constructor"]: Expected string, received 1`,
  )
})

test("Successfully parses S.schema with a field named \"constructor\"", t => {
  let schema = S.schema(s =>
    {
      "constructor": s.matches(S.string),
      "nested": {"constructor": s.matches(S.float)},
    }
  )

  t->Assert.deepEqual(
    %raw(`{constructor: "a", nested: {constructor: 1}}`)->S.parseOrThrow(~to=schema),
    {
      "constructor": "a",
      "nested": {"constructor": 1.},
    },
  )
})
