open Ava

test("OperationFailed error", t => {
  t->Assert.is(
    U.error({
      code: OperationFailed("Should be positive"),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    "Failed parsing: Should be positive",
  )
})

test("Error with Serializing operation", t => {
  t->Assert.is(
    U.error({
      code: OperationFailed("Should be positive"),
      operation: ReverseConvert,
      path: S.Path.empty,
    }).message,
    "Failed converting: Should be positive",
  )
})

test("Error with path", t => {
  t->Assert.is(
    U.error({
      code: OperationFailed("Should be positive"),
      operation: Parse,
      path: S.Path.fromArray(["0", "foo"]),
    }).message,
    `Failed parsing at ["0"]["foo"]: Should be positive`,
  )
})

test("InvalidOperation error", t => {
  t->Assert.is(
    U.error({
      code: InvalidOperation({description: "The S.transform serializer is missing"}),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    "Failed parsing: The S.transform serializer is missing",
  )
})

test("InvalidType error", t => {
  t->Assert.is(
    U.error({
      code: InvalidType({expected: S.string->S.castToUnknown, received: Obj.magic(true)}),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    "Failed parsing: Expected string, received true",
  )
})

test("UnexpectedAsync error", t => {
  t->Assert.is(
    U.error({
      code: UnexpectedAsync,
      operation: Parse,
      path: S.Path.empty,
    }).message,
    "Failed parsing: Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead",
  )
})

test("InvalidType with literal error", t => {
  t->Assert.is(
    U.error({
      code: InvalidType({expected: S.literal(false)->S.castToUnknown, received: true->Obj.magic}),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    "Failed parsing: Expected false, received true",
  )
})

test("ExcessField error", t => {
  t->Assert.is(
    U.error({
      code: ExcessField("unknownKey"),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    `Failed parsing: Unrecognized key "unknownKey"`,
  )
})

test("InvalidType error (replacement for InvalidTupleSize)", t => {
  t->Assert.is(
    U.error({
      code: InvalidType({
        expected: S.tuple2(S.bool, S.int)->S.castToUnknown,
        received: (1, 2, "foo")->Obj.magic,
      }),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    `Failed parsing: Expected [boolean, int32], received [1, 2, "foo"]`,
  )
})

test("InvalidType error with union errors", t => {
  t->Assert.is(
    U.error({
      code: InvalidType({
        expected: S.unknown,
        received: "foo"->Obj.magic,
        unionErrors: [
          U.error({
            code: InvalidType({
              expected: S.literal("circle")->S.castToUnknown,
              received: "oval"->Obj.magic,
            }),
            operation: Parse,
            path: S.Path.fromArray(["kind"]),
          }),
          U.error({
            code: InvalidType({
              expected: S.literal("square")->S.castToUnknown,
              received: "oval"->Obj.magic,
            }),
            operation: Parse,
            path: S.Path.fromArray(["kind"]),
          }),
          U.error({
            code: InvalidType({
              expected: S.literal("triangle")->S.castToUnknown,
              received: "oval"->Obj.magic,
            }),
            operation: Parse,
            path: S.Path.fromArray(["kind"]),
          }),
        ],
      }),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    `Failed parsing: Expected unknown, received "foo"
- At ["kind"]: Expected "circle", received "oval"
- At ["kind"]: Expected "square", received "oval"
- At ["kind"]: Expected "triangle", received "oval"`,
  )
})

test("InvalidUnion filters similar reasons", t => {
  t->Assert.is(
    U.error({
      code: InvalidType({
        expected: S.unknown,
        received: "foo"->Obj.magic,
        unionErrors: [
          U.error({
            code: InvalidType({
              expected: S.bool->S.castToUnknown,
              received: %raw(`"Hello world!"`),
            }),
            operation: Parse,
            path: S.Path.empty,
          }),
          U.error({
            code: InvalidType({
              expected: S.bool->S.castToUnknown,
              received: %raw(`"Hello world!"`),
            }),
            operation: Parse,
            path: S.Path.empty,
          }),
          U.error({
            code: InvalidType({
              expected: S.bool->S.castToUnknown,
              received: %raw(`"Hello world!"`),
            }),
            operation: Parse,
            path: S.Path.empty,
          }),
        ],
      }),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    `Failed parsing: Expected unknown, received "foo"
- Expected boolean, received "Hello world!"`,
  )
})

test("Nested InvalidUnion error", t => {
  t->Assert.is(
    U.error({
      code: InvalidType({
        expected: S.unknown,
        received: "foo"->Obj.magic,
        unionErrors: [
          U.error({
            code: InvalidType({
              expected: S.bool->S.castToUnknown,
              received: "foo"->Obj.magic,
              unionErrors: [
                U.error({
                  code: InvalidType({
                    expected: S.bool->S.castToUnknown,
                    received: %raw(`"Hello world!"`),
                  }),
                  operation: Parse,
                  path: S.Path.empty,
                }),
                U.error({
                  code: InvalidType({
                    expected: S.bool->S.castToUnknown,
                    received: %raw(`"Hello world!"`),
                  }),
                  operation: Parse,
                  path: S.Path.empty,
                }),
                U.error({
                  code: InvalidType({
                    expected: S.bool->S.castToUnknown,
                    received: %raw(`"Hello world!"`),
                  }),
                  operation: Parse,
                  path: S.Path.empty,
                }),
              ],
            }),
            operation: Parse,
            path: S.Path.empty,
          }),
        ],
      }),
      operation: Parse,
      path: S.Path.empty,
    }).message,
    `Failed parsing: Expected unknown, received "foo"
- Expected boolean, received "foo"
  - Expected boolean, received "Hello world!"`,
  )
})

test("InvalidJsonSchema error", t => {
  t->Assert.is(
    U.error({
      code: InvalidJsonSchema(S.option(S.literal(true))->S.castToUnknown),
      operation: ReverseConvertToJson,
      path: S.Path.empty,
    }).message,
    `Failed converting to JSON: true | undefined is not valid JSON`,
  )
})
