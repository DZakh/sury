open Vitest

// Tracks support for coercing a `dict<string>` into a structured object whose
// fields need their own coercion (string -> bigint) and include an optional
// field (string -> option<float>):
//
//   S.dict(S.string)->S.to(S.schema({
//     foo: S.string,
//     bar: S.bigint,
//     zoo: S.option(S.float),
//   }))
//
// Parsing fully-populated input already works (thanks to the #265 union-hoist
// fix). The remaining gaps are captured below. Each test currently asserts the
// *actual (broken)* behavior so the suite stays green — every one carries a
// FIXME with the intended `SHOULD:` behavior to switch to as its milestone
// lands. Agreed semantics: `None` <-> an absent dict key.

let makeSchema = () =>
  S.dict(S.string)->S.to(
    S.schema(s =>
      {
        "foo": s.matches(S.string),
        "bar": s.matches(S.bigint),
        "zoo": s.matches(S.option(S.float)),
      }
    ),
  )

test("Parses input with every field present", t => {
  let schema = makeSchema()

  t->Assert.deepEqual(
    %raw(`{"foo":"a","bar":"123","zoo":"1.5"}`)->S.parseOrThrow(~to=schema),
    {"foo": "a", "bar": 123n, "zoo": Some(1.5)},
  )
})

test("FIXME [milestone 1] absent optional field should decode to None", t => {
  let schema = makeSchema()

  // SHOULD: an absent `zoo` key decodes to None
  //   t->Assert.deepEqual(
  //     %raw(`{"foo":"a","bar":"7"}`)->S.parseOrThrow(~to=schema),
  //     {"foo": "a", "bar": 7n, "zoo": None},
  //   )
  // ACTUAL (broken): a genuinely-missing key throws. The option's None branch is
  // pinned to the *literal string* "undefined" (because the source is
  // dict<string> and the whole option is coerced from a string), so real absence
  // is not recognized as None.
  t->U.assertThrowsMessage(
    () => %raw(`{"foo":"a","bar":"7"}`)->S.parseOrThrow(~to=schema),
    `Failed at ["zoo"]: Expected number | undefined, received undefined
- At ["zoo"]: Expected number, received undefined`,
  )
})

test("FIXME [milestone 1] None currently comes from the string \"undefined\", not an absent key", t => {
  let schema = makeSchema()

  // ACTUAL (broken): the literal string "undefined" is what decodes to None today.
  // SHOULD: once `None` <-> absent key, this string should instead fail coercion
  // (it is not a valid number), and absence should be the only None source.
  t->Assert.deepEqual(
    %raw(`{"foo":"a","bar":"123","zoo":"undefined"}`)->S.parseOrThrow(~to=schema),
    {"foo": "a", "bar": 123n, "zoo": None},
  )
})

test("FIXME [milestone 2-3] encoding back to dict<string> should round-trip", t => {
  let schema = makeSchema()

  // SHOULD: encode the object back into a dict of strings (None -> absent key)
  //   t->Assert.deepEqual(
  //     {"foo": "a", "bar": 123n, "zoo": Some(1.5)}->S.decodeOrThrow(~from=schema, ~to=S.unknown),
  //     %raw(`{"foo":"a","bar":"123","zoo":"1.5"}`),
  //   )
  //   t->U.assertReverseParsesBack(schema, {"foo": "a", "bar": 7n, "zoo": None})
  // ACTUAL (broken): building the object->dict reverse crashes at *build* time.
  // A fixed-property object's `additionalItems` mode (Strip/Strict) is cast to a
  // schema in `B.dynamicScope`, then reaches `isLiteral` (`"const" in "strip"`).
  t->Assert.throws(
    () => {
      let _ = S.decoder(~from=schema, ~to=S.unknown)
    },
    ~expectations={message: "Cannot use 'in' operator to search for 'const' in"},
  )
})
