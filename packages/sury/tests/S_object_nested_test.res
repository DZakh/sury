open Ava
open RescriptCore

test("Object with a single nested field", t => {
  let schema = S.object(s => s.nested("nested").field("foo", S.string))

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s => s.field("nested", S.object(s => s.field("foo", S.string)))),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[1](i)}let v0=i["nested"];let v1=v0["foo"];if(typeof v1!=="string"){e[0](v1)}return v1}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{"foo":i,},}}`)
})

test("Object with a single nested field with S.null", t => {
  let schema = S.object(s => s.nested("nested").field("foo", S.null(S.string)))

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s => s.field("nested", S.object(s => s.field("foo", S.null(S.string))))),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[1](i)}let v0=i["nested"];let v1=v0["foo"];if(v1===null){v1=void 0}else if(!(typeof v1==="string")){e[0](v1)}return v1}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{if(i===void 0){i=null}return {"nested":{"foo":i,},}}`,
  )
  t->Assert.deepEqual(
    Some("bar")->S.reverseConvertOrThrow(schema),
    %raw(`{"nested":{"foo":"bar"}}`),
    (),
  )
})

test("Object with a single nested field with S.transform", t => {
  let schema = S.object(s =>
    s.nested("nested").field(
      "foo",
      S.float->S.transform(
        s => {
          parser: f => f->Float.toString,
          serializer: string => {
            // There used to be a case of double application of the serializer.
            // Check that it doesn't happen again.
            if string->typeof !== #string {
              s.fail("Unexpected type")
            }
            switch string->Float.fromString {
            | Some(float) => float
            | None => s.fail("Invalid float")
            }
          },
        },
      ),
    )
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[2](i)}let v0=i["nested"];let v1=v0["foo"];if(typeof v1!=="number"||Number.isNaN(v1)){e[0](v1)}return e[1](v1)}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{"foo":e[0](i),},}}`)
  t->Assert.deepEqual(
    "123.4"->S.reverseConvertOrThrow(schema),
    %raw(`{"nested":{"foo":123.4}}`),
    (),
  )
})

test("Object with a nested tag and optional field", t => {
  let schema = S.object(s => {
    s.nested("nested").tag("tag", "value")
    {
      "foo": s.nested("nested").fieldOr("foo", S.string, ""),
      "bar": s.field("bar", S.string),
    }
  })

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s => {
      let _ = s.field(
        "nested",
        S.object(
          s => {
            s.tag("tag", "value")
            let _ = s.fieldOr("foo", S.string, "")
          },
        ),
      )
      let _ = s.field("bar", S.string)
    }),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]||i["nested"]["tag"]!=="value"){e[3](i)}let v0=i["nested"],v2=i["bar"];let v1=v0["foo"];if(!(typeof v1==="string"||v1===void 0)){e[0](v1)}if(typeof v2!=="string"){e[2](v2)}return {"foo":v1===void 0?e[1]:v1,"bar":v2,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"nested":{"tag":e[0],"foo":i["foo"],},"bar":i["bar"],}}`,
  )
})

test("Object with a two nested field using the same ctx", t => {
  let schema = S.object(s => {
    let nested = s.nested("nested")
    {
      "foo": nested.field("foo", S.string),
      "bar": nested.field("bar", S.string),
    }
  })

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.object(
          s =>
            {
              "foo": s.field("foo", S.string),
              "bar": s.field("bar", S.string),
            },
        ),
      )
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[2](i)}let v0=i["nested"];let v1=v0["foo"],v2=v0["bar"];if(typeof v1!=="string"){e[0](v1)}if(typeof v2!=="string"){e[1](v2)}return {"foo":v1,"bar":v2,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"nested":{"foo":i["foo"],"bar":i["bar"],},}}`,
  )
})

test("Object with a single nested nested field", t => {
  let schema = S.object(s => s.nested("nested").nested("deeply").field("foo", S.string))

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field("nested", S.object(s => s.field("deeply", S.object(s => s.field("foo", S.string)))))
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]||typeof i["nested"]["deeply"]!=="object"||!i["nested"]["deeply"]){e[1](i)}let v0=i["nested"];let v1=v0["deeply"];let v2=v1["foo"];if(typeof v2!=="string"){e[0](v2)}return v2}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"nested":{"deeply":{"foo":i,},},}}`,
  )
})

test("Object with a two nested field calling s.nested twice", t => {
  let schema = S.object(s => {
    {
      "foo": s.nested("nested").field("foo", S.string),
      "bar": s.nested("nested").field("bar", S.string),
    }
  })

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.object(
          s =>
            {
              "foo": s.field("foo", S.string),
              "bar": s.field("bar", S.string),
            },
        ),
      )
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[2](i)}let v0=i["nested"];let v1=v0["foo"],v2=v0["bar"];if(typeof v1!=="string"){e[0](v1)}if(typeof v2!=="string"){e[1](v2)}return {"foo":v1,"bar":v2,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"nested":{"foo":i["foo"],"bar":i["bar"],},}}`,
  )
})

test("Object with a flattened nested field", t => {
  let schema = S.object(s =>
    s.nested("nested").flatten(
      S.schema(
        s =>
          {
            "foo": s.matches(S.string),
          },
      ),
    )
  )

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
            },
        ),
      )
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[1](i)}let v0=i["nested"];let v1=v0["foo"];if(typeof v1!=="string"){e[0](v1)}return {"foo":v1,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{"foo":i["foo"],},}}`)
})

test("Object with a strict flattened nested field", t => {
  let schema = S.object(s =>
    s.nested("nested").flatten(
      S.schema(
        s =>
          {
            "foo": s.matches(S.string),
          },
      )->S.strict,
    )
  )

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
            },
        ),
      )
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[1](i)}let v0=i["nested"];let v1=v0["foo"];if(typeof v1!=="string"){e[0](v1)}return {"foo":v1,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{"foo":i["foo"],},}}`)
})

test("S.schema object with a deep strict applied to the nested field parent", t => {
  let schema = S.schema(s =>
    {
      "nested": {
        "foo": s.matches(S.string),
      },
    }
  )->S.deepStrict

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
            },
        )->S.strict,
      )
    )->S.strict,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["nested"]!=="object"||!i["nested"]||Array.isArray(i["nested"])){e[3](i)}let v0=i["nested"],v3;let v1=v0["foo"],v2;if(typeof v1!=="string"){e[0](v1)}for(v2 in v0){if(v2!=="foo"){e[1](v2)}}for(v3 in i){if(v3!=="nested"){e[2](v3)}}return i}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{let v0=i["nested"];return i}`)
})

test("Nested tags on reverse convert", t => {
  let schema = S.object(s => {
    s.nested("nested").tag("tag", "value")
  })

  t->Assert.deepEqual(()->S.reverseConvertOrThrow(schema), %raw(`{"nested":{"tag":"value"}}`), ())
})

test("Nested preprocessed tags on reverse convert", t => {
  let prefixedWithUnderscore =
    S.string
    ->S.transform(s => {
      parser: v => {
        if v->String.startsWith("_") {
          v->String.sliceToEnd(~start=1)
        } else {
          s.fail("String should start with an underscore")
        }
      },
      serializer: v => "_" ++ v,
    })
    ->S.to(S.string)

  let schema = S.object(s => {
    let _ = s.nested("nested").field("tag", prefixedWithUnderscore->S.to(S.literal("value")))
    let _ = s.nested("nested").field("intTag", prefixedWithUnderscore->S.to(S.literal(1)))
  })

  t->U.assertCompiledCode(
    ~op=#ReverseConvert,
    ~schema,
    `i=>{if(i!==void 0){e[5](i)}return {"nested":{"tag":e[2](e[0]),"intTag":e[4]("1"),},}}`,
  )

  t->U.assertCompiledCode(
    ~op=#Parse,
    ~schema,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[9](i)}let v0=i["nested"];let v1=v0["tag"],v2=e[1](v1),v3=v0["intTag"],v4=e[5](v3);if(typeof v1!=="string"){e[0](v1)}if(typeof v2!=="string"){e[2](v2)}if(v2!=="value"){e[3](v2)}if(typeof v3!=="string"){e[4](v3)}if(typeof v4!=="string"){e[6](v4)}v4==="1"||e[7](v4);return e[8]}`,
  )

  t->Assert.deepEqual(
    ()->S.reverseConvertOrThrow(schema),
    %raw(`{"nested":{"tag":"_value", "intTag":"_1"}}`),
    (),
  )

  t->Assert.deepEqual(
    %raw(`{"nested":{"tag":"_value", "intTag":"_1"}}`)->S.parseOrThrow(schema),
    (),
    (),
  )
})

test("S.schema object with a deep strict applied to the nested field parent + reverse", t => {
  let schema =
    S.schema(s =>
      {
        "nested": {
          "foo": s.matches(S.null(S.string)),
        },
      }
    )
    ->S.reverse
    ->S.deepStrict

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.schema(
          s =>
            {
              "foo": s.matches(S.option(S.string)),
            },
        )->S.strict,
      )
    )->S.strict,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["nested"]!=="object"||!i["nested"]||Array.isArray(i["nested"])){e[3](i)}let v0=i["nested"],v3;let v1=v0["foo"],v2;if(v1===void 0){v1=null}else if(!(typeof v1==="string")){e[0](v1)}for(v2 in v0){if(v2!=="foo"){e[1](v2)}}for(v3 in i){if(v3!=="nested"){e[2](v3)}}return {"nested":{"foo":v1,},}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["nested"];let v1=v0["foo"];if(v1===null){v1=void 0}return {"nested":{"foo":v1,},}}`,
  )
})

test("Object with a deep strict applied to the nested field parent", t => {
  let schema = S.object(s => s.nested("nested").field("foo", S.string))->S.deepStrict

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
            },
        )->S.strict,
      )
    )->S.strict,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||Array.isArray(i)||typeof i["nested"]!=="object"||!i["nested"]||Array.isArray(i["nested"])){e[3](i)}let v0=i["nested"],v3;let v1=v0["foo"],v2;if(typeof v1!=="string"){e[0](v1)}for(v2 in v0){if(v2!=="foo"){e[1](v2)}}for(v3 in i){if(v3!=="nested"){e[2](v3)}}return v1}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{"foo":i,},}}`)
})

test("Object with a deep strict applied to the nested field parent + reverse", t => {
  let schema =
    S.object(s => {"foo": s.nested("nested").field("foo", S.string)})
    ->S.reverse
    ->S.deepStrict

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.schema(s =>
      {
        "foo": s.matches(S.string),
      }
    )->S.strict,
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    // FIXME: Missing type validation for nested field
    // FIXME: Test for deepStrict applying to flattened nested fields
    // Test deepStrict for reversed schema
    // Test strict & deepStrict for S.shape
    `i=>{if(typeof i!=="object"||!i||Array.isArray(i)){e[1](i)}let v0;for(v0 in i){if(v0!=="foo"){e[0](v0)}}return {"nested":{"foo":i["foo"],},}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{let v0=i["nested"];return {"foo":v0["foo"],}}`,
  )
})

test("Object with nested field together with flatten", t => {
  let schema = S.object(s =>
    {
      "flattened": s.nested("nested").flatten(
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
            },
        ),
      ),
      "field": s.nested("nested").field("bar", S.string),
    }
  )

  t->U.unsafeAssertEqualSchemas(
    schema,
    S.object(s =>
      s.field(
        "nested",
        S.schema(
          s =>
            {
              "foo": s.matches(S.string),
              "bar": s.matches(S.string),
            },
        ),
      )
    ),
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[2](i)}let v0=i["nested"];let v1=v0["foo"],v2=v0["bar"];if(typeof v1!=="string"){e[0](v1)}if(typeof v2!=="string"){e[1](v2)}return {"flattened":{"foo":v1,},"field":v2,}}`,
  )
  t->U.assertCompiledCode(
    ~schema,
    ~op=#ReverseConvert,
    `i=>{return {"nested":{"foo":i["flattened"]["foo"],"bar":i["field"],},}}`,
  )
})

test("s.nested conflicts with s.field", t => {
  t->Assert.throws(
    () => {
      S.object(
        s => {
          let _ = s.nested("nested").field("foo", S.string)
          let _ = s.field("nested", S.object(s => s.field("foo", S.string)))
        },
      )
    },
    ~expectations={
      message: `[Sury] The field "nested" defined twice with incompatible schemas`,
    },
    (),
  )
})

test("s.nested.flattened doesn't work with S.object", t => {
  t->Assert.throws(
    () => {
      S.object(
        s => {
          let _ = s.nested("nested").flatten(S.object(s => s.field("foo", S.string)))
        },
      )
    },
    ~expectations={
      message: `[Sury] Unsupported nested flatten for advanced object schema '{ foo: string; }'`,
    },
    (),
  )
})

test("s.nested.flattened doesn't work with transformed S.schema", t => {
  t->Assert.throws(
    () => {
      S.object(
        s => {
          let _ = s.nested("nested").flatten(
            S.schema(
              s =>
                {
                  "foo": s.matches(S.string),
                },
            )->S.transform(_ => {parser: i => i}),
          )
        },
      )
    },
    ~expectations={
      message: `[Sury] Unsupported nested flatten for transformed schema \'{ foo: string; }\'`,
    },
    (),
  )
})

test("s.nested.flattened doesn't work with S.schema->S.shape", t => {
  t->Assert.throws(
    () => {
      S.object(
        s => {
          let _ = s.nested("nested").flatten(
            S.schema(
              s =>
                {
                  "foo": s.matches(S.string),
                },
            )->S.shape(v => {"foo": v["foo"]}),
          )
        },
      )
    },
    ~expectations={
      message: `[Sury] Unsupported nested flatten for transformed schema \'{ foo: string; }\'`,
    },
    (),
  )
})

test("s.nested.flattened doesn't work with S.string", t => {
  t->Assert.throws(
    () => {
      S.object(
        s => {
          let _ = s.nested("nested").flatten(S.string)
        },
      )
    },
    ~expectations={
      message: `[Sury] The 'string' schema can\'t be flattened`,
    },
    (),
  )
})

test("s.nested.flattened works with S.schema->S.shape to self", t => {
  let schema = S.object(s => {
    s.nested("nested").flatten(
      S.schema(
        s =>
          {
            "foo": s.matches(S.string),
          },
      )->S.shape(v => v),
    )
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i||typeof i["nested"]!=="object"||!i["nested"]){e[1](i)}let v0=i["nested"];let v1=v0["foo"];if(typeof v1!=="string"){e[0](v1)}return {"foo":v1,}}`,
  )
  t->U.assertCompiledCode(~schema, ~op=#ReverseConvert, `i=>{return {"nested":{"foo":i["foo"],},}}`)
})

test("s.nested.flatten conflicts with s.nested.field", t => {
  t->Assert.throws(
    () => {
      S.object(
        s => {
          let _ = s.nested("nested").flatten(
            S.schema(
              s =>
                {
                  "foo": s.matches(S.string),
                },
            ),
          )
          let _ = s.nested("nested").field("foo", S.string)
        },
      )
    },
    ~expectations={
      message: `[Sury] The field "foo" defined twice`,
    },
    (),
  )
})
