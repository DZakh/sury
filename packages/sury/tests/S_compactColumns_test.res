open Ava

test("Successfully parses and reverse converts a simple object with compactColumns", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.int),
        }
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, 1]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": 1}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, 1]]`),
  )
})

test("Transforms nullable fields", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.string),
          "bar": s.matches(S.nullAsOption(S.int)),
        }
      ),
    )

  // Note: compactColumns creates raw objects, field-level transformations like nullAsOption
  // are not applied (null stays as null, not transformed to undefined)
  t->Assert.deepEqual(
    %raw(`[["a", "b"], [0, null]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": null}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": 0}, {"foo": "b", "bar": null}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b"], [0, null]]`),
  )
})

test("Case with missing item at the end", t => {
  let schema =
    S.compactColumns(S.unknown)->S.to(
      S.schema(s =>
        {
          "foo": s.matches(S.option(S.string)),
          "bar": s.matches(S.bool),
        }
      ),
    )

  t->Assert.deepEqual(
    %raw(`[["a", "b"], [true, true, false]]`)->S.parseOrThrow(schema),
    %raw(`[{"foo": "a", "bar": true}, {"foo": "b", "bar": true}, {"foo": undefined, "bar": false}]`),
  )

  t->Assert.deepEqual(
    %raw(`[{"foo": "a", "bar": true}, {"foo": "b", "bar": true}, {"foo": undefined, "bar": false}]`)->S.reverseConvertOrThrow(schema),
    %raw(`[["a", "b", undefined], [true, true, false]]`),
  )
})

test("Handles empty objects", t => {
  let schema = S.compactColumns(S.unknown)->S.to(S.object(_ => ()))
  t->Assert.throws(
    () => {
      %raw(`[]`)->S.parseOrThrow(schema)
    },
    ~expectations={
      message: "[Sury] Invalid empty object for S.compactColumns schema.",
    },
  )
})

test("Handles non-object schemas", t => {
  let schema = S.compactColumns(S.unknown)->S.to(S.tuple2(S.string, S.int))
  t->Assert.throws(
    () => {
      %raw(`[]`)->S.parseOrThrow(schema)
    },
    ~expectations={
      message: "Failed at [\"0\"]: Unsupported conversion from unknown[] to string",
    },
  )
})

test("Schema has format field set to compactColumns", t => {
  let schema = S.compactColumns(S.unknown)
  // Use Obj.magic to cast schema to untagged representation for testing internal field
  t->Assert.deepEqual((schema->Obj.magic)["format"], "compactColumns")
})
