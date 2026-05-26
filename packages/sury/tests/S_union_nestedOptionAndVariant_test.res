open Ava

// Reproduction for https://github.com/DZakh/sury/issues/240
//
// Building a reverse decoder for an outer object that contains BOTH:
//   1. a nested object field whose schema has at least one S.option field, AND
//   2. a sibling field that is a S.union of record-payload variants (sury-ppx
//      shape, i.e. each variant is `S.schema(s => { TAG: "X", ... })`),
// crashes during decoder compilation with:
//   TypeError: val.p.a is not a function
test("Reverse-encode of {nested:{opt?}, variant:union} compiles", t => {
  let nestedSchema = S.schema(s =>
    {
      "opt": s.matches(S.option(S.string)),
    }
  )

  let variantSchema = S.union([
    S.schema(s =>
      {
        "TAG": "A",
        "x": s.matches(S.string),
      }
    ),
    S.schema(s =>
      {
        "TAG": "B",
        "x": s.matches(S.string),
      }
    ),
  ])

  let outerSchema = S.schema(s =>
    {
      "nested": s.matches(nestedSchema),
      "variant": s.matches(variantSchema),
    }
  )

  // Forward parse works in isolation
  t->Assert.deepEqual(
    %raw(`{"nested":{"opt":"v"},"variant":{"TAG":"A","x":"hello"}}`)->S.parseOrThrow(
      ~to=outerSchema,
    ),
    %raw(`{"nested":{"opt":"v"},"variant":{"TAG":"A","x":"hello"}}`),
  )

  // Reverse (encode to JSON) is the failing case from #240.
  // Just compiling the decoder is enough to reproduce the crash.
  let value = %raw(`{"nested":{"opt":"v"},"variant":{"TAG":"A","x":"hello"}}`)
  t->Assert.deepEqual(
    value->S.decodeOrThrow(~from=outerSchema->S.reverse, ~to=S.json),
    %raw(`{"nested":{"opt":"v"},"variant":{"TAG":"A","x":"hello"}}`),
  )
})
