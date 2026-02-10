open Ava

test("Successfully parses object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\"\'\`", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{"\"\'\`": "bar"}`)->S.parseOrThrow(schema), {"field": "bar"})
})

test("Successfully parses object with new line in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\n", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{"\n": "bar"}`)->S.parseOrThrow(schema), {"field": "bar"})
})

test("Successfully serializing object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "field": s.field("\"\'\`", S.string),
    }
  )

  t->Assert.deepEqual({"field": "bar"}->S.reverseConvertOrThrow(schema), %raw(`{"\"\'\`": "bar"}`))
})

test("Successfully parses object transformed to object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "\"\'\`": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual(%raw(`{"field": "bar"}`)->S.parseOrThrow(schema), {"\"\'\`": "bar"})
})

test("Successfully serializes object transformed to object with quotes in a field name", t => {
  let schema = S.object(s =>
    {
      "\"\'\`": s.field("field", S.string),
    }
  )

  t->Assert.deepEqual({"\"\'\`": "bar"}->S.reverseConvertOrThrow(schema), %raw(`{"field": "bar"}`))
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
    }`)->S.parseOrThrow(schema),
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
    {"field": "bar"}->S.reverseConvertOrThrow(schema),
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
    }`)->S.parseOrThrow(schema),
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
      {"field": "bar"}->S.reverseConvertOrThrow(schema),
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
      %raw(`{"field": "bar"}`)->S.parseOrThrow(schema),
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
      }->S.reverseConvertOrThrow(schema),
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
      %raw(`{"field": "bar"}`)->S.parseOrThrow(schema),
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
      }->S.reverseConvertOrThrow(schema),
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
    () => %raw(`{"\"\'": "bar"}`)->S.parseOrThrow(schema),
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
    () => Dict.fromArray([("\"'", "bar")])->S.reverseConvertOrThrow(schema),
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
    () => %raw(`{"bar": "foo"}`)->S.parseOrThrow(schema),
    `Failed at ["[\\"abc\\"][\\"cde\\"]"]: Expected string, received undefined`,
  )
})
