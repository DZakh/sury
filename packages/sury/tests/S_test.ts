import { test, expectTypeOf, assertType } from "vitest";
import { format, inspect } from "node:util";

import * as S from "../index.mjs";

// FIXME: S.lte should be applied to output
// From https://x.com/dzakh_dev/status/1963982551208309222
// const PixelSchema = S.pattern(/^\d{1,3}px$/)
//   .with(S.to, S.number, parseInt)
//   .with(S.lte, 100)
//   .with(S.meta, {
//     description: "A pixel value between 0 and 100",
//   });

// FIXME: Move the test to e2e
// import { stringSchema } from "../genType/GenType.gen.js";

// FIXME: This is fails
// S.parser(
//   S.union([
//     "bar",
//     "bas",
//     S.string.with(S.to, S.schema("unknown").with(S.noValidation, true)),
//   ])
// )

// Exact (bidirectional) type equality. expect-type's `toEqualTypeOf` can't be
// wrapped in a generic helper and still fire at call sites, so the dual
// Input+Output check is enforced via a required-argument constraint instead.
type Equal<TLeft, TRight> =
  (<T>() => T extends TLeft ? 1 : 2) extends <T>() => T extends TRight ? 1 : 2
    ? true
    : false;

const expectSchemaType = <TSchema extends S.Schema<unknown, unknown>>(
  _schema: TSchema,
) => ({
  toBe: <TInput, TOutput = TInput>(
    ..._mismatch: Equal<S.Input<TSchema>, TInput> extends true
      ? Equal<S.Output<TSchema>, TOutput> extends true
        ? []
        : [output: S.Output<TSchema>]
      : [input: S.Input<TSchema>]
  ) => {},
});

// Can use genType schema
// expectSchemaType(stringSchema).toBe<unknown, string>();

test("S.to returns the schema itself when the target is the same instance", (t) => {
  const make = () => S.string.with(S.to, S.number, (string) => string.length);
  const schema = make();

  t.expect(S.to(schema, schema)).toBe(schema);
  t.expect(schema.with(S.to, schema)).toBe(schema);
  t.expect(S.parser(schema.with(S.to, schema))("hello")).toBe(5);

  // Without the shortcut this appends a second copy of the chain, so the
  // decoder runs twice over its own output — silently wrong, not an error.
  t.expect(S.parser(S.to(schema, make()))("hello")).toBe(1);

  // Custom coders still mean a real conversion step, same instance or not.
  const doubled = S.to(schema, schema, (n) => String(n * 2));
  t.expect(doubled).not.toBe(schema);
  t.expect(S.parser(doubled)("hello")).toBe(2);

  expectSchemaType(schema).toBe<string, number>();
  expectSchemaType(S.to(schema, schema)).toBe<string, number>();
});

test("Function literal schema", (t) => {
  const fn = function () {};

  const schema = S.schema(fn);

  expectSchemaType(schema).toBe<() => void, () => void>();
  if (schema.type !== "function") {
    t.expect.fail("Schema should be a function");
    return;
  }
  t.expect(schema.const).toBe(fn);

  const value = S.parser(schema)(fn);

  t.expect(value).toEqual(fn);
  t.expect(value).not.toEqual(function () {});
});

test("Successfully parses float when NaN is provided and NaN check disabled in global config", (t) => {
  S.global({
    disableNanNumberValidation: true,
  });
  const schema = S.number;
  const value = S.parser(schema)(NaN);
  S.global({});

  t.expect(value).toEqual(NaN);

  expectSchemaType(schema).toBe<number, number>();
  expectTypeOf(value).toEqualTypeOf<number>();
});

test("Can get a reason from an error", (t) => {
  const schema = S.never;

  const result = S.safe(() => S.parser(schema)(true));

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.reason).toBe("Expected never, received true");
});

test("Parse JSON string to object with bigint and back", (t) => {
  const messageSchema = S.schema({
    type: "info",
    value: S.bigint,
  });

  const decode = S.decoder(S.jsonString, messageSchema);
  const encode = S.decoder(
    messageSchema,
    // Cast to string to disable json string encoder
    S.jsonString.with(S.to, S.string, (string) => string),
    S.uint8Array,
  );

  t.expect(decode(`{"type": "info", "value": "123"}`)).toEqual({
    type: "info",
    value: 123n,
  });
  t.expect(encode({ type: "info", value: 123n })).toEqual(
    new Uint8Array([
      123, 34, 116, 121, 112, 101, 34, 58, 34, 105, 110, 102, 111, 34, 44, 34,
      118, 97, 108, 117, 101, 34, 58, 34, 49, 50, 51, 34, 125,
    ]),
  );
});

test("Optional enum", (t) => {
  const statuses = S.union(["Win", "Draw", "Loss"]);
  const schema = S.optional(statuses);

  t.expect(S.parser(schema)("Win")).toEqual("Win");
  t.expect(S.parser(schema)(undefined)).toEqual(undefined);

  expectTypeOf(schema).toEqualTypeOf<
    S.Schema<
      "Win" | "Draw" | "Loss" | undefined,
      "Win" | "Draw" | "Loss" | undefined
    >
  >();

  const inlineOptional = S.optional(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parser(inlineOptional)("Win")).toEqual("Win");
  t.expect(S.encoder(inlineOptional)("Win")).toEqual("Win");
  expectTypeOf(inlineOptional).toEqualTypeOf<
    S.Schema<
      "Win" | "Draw" | "Loss" | undefined,
      "Win" | "Draw" | "Loss" | undefined
    >
  >();

  const inlineNullable = S.nullable(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parser(inlineNullable)("Win")).toEqual("Win");
  t.expect(S.encoder(inlineNullable)("Win")).toEqual("Win");
  expectTypeOf(inlineNullable).toEqualTypeOf<
    S.Schema<"Win" | "Draw" | "Loss" | null, "Win" | "Draw" | "Loss" | null>
  >();

  const inlineNullish = S.nullish(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parser(inlineNullish)("Win")).toEqual("Win");
  t.expect(S.encoder(inlineNullish)("Win")).toEqual("Win");
  expectTypeOf(inlineNullish).toEqualTypeOf<
    S.Schema<
      "Win" | "Draw" | "Loss" | null | undefined,
      "Win" | "Draw" | "Loss" | null | undefined
    >
  >();

  const inlineArray = S.array(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parser(inlineArray)(["Win", "Loss"])).toEqual(["Win", "Loss"]);
  t.expect(S.encoder(inlineArray)(["Win", "Loss"])).toEqual(["Win", "Loss"]);
  expectTypeOf(inlineArray).toEqualTypeOf<
    S.Schema<("Win" | "Draw" | "Loss")[], ("Win" | "Draw" | "Loss")[]>
  >();

  const inlineRecord = S.record(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parser(inlineRecord)({ a: "Win" })).toEqual({ a: "Win" });
  t.expect(S.encoder(inlineRecord)({ a: "Win" })).toEqual({ a: "Win" });
  expectTypeOf(inlineRecord).toEqualTypeOf<
    S.Schema<
      Record<string, "Win" | "Draw" | "Loss">,
      Record<string, "Win" | "Draw" | "Loss">
    >
  >();

  const inlineObject = S.schema({
    status: S.union(["Win", "Draw", "Loss"]),
  });
  t.expect(S.parser(inlineObject)({ status: "Win" })).toEqual({
    status: "Win",
  });
  t.expect(S.encoder(inlineObject)({ status: "Win" })).toEqual({
    status: "Win",
  });
  expectTypeOf(inlineObject).toEqualTypeOf<
    S.Schema<
      { status: "Win" | "Draw" | "Loss" },
      { status: "Win" | "Draw" | "Loss" }
    >
  >();

  const inlineTuple = S.schema([S.union(["Win", "Draw", "Loss"]), S.number]);
  t.expect(S.parser(inlineTuple)(["Win", 1])).toEqual(["Win", 1]);
  t.expect(S.encoder(inlineTuple)(["Win", 1])).toEqual(["Win", 1]);
  expectTypeOf(inlineTuple).toEqualTypeOf<
    S.Schema<
      ["Win" | "Draw" | "Loss", number],
      ["Win" | "Draw" | "Loss", number]
    >
  >();

  const nestedDeep = S.optional(
    S.array(S.nullable(S.union(["Win", "Draw", "Loss"]))),
  );
  t.expect(S.parser(nestedDeep)(["Win", null])).toEqual(["Win", null]);
  t.expect(S.encoder(nestedDeep)(["Win", null])).toEqual(["Win", null]);
  expectTypeOf(nestedDeep).toEqualTypeOf<
    S.Schema<
      ("Win" | "Draw" | "Loss" | null)[] | undefined,
      ("Win" | "Draw" | "Loss" | null)[] | undefined
    >
  >();
});

test("Successfully parses nullable string with dynamic default", (t) => {
  const schema = S.nullable(S.string, () => "bar");
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual("bar");

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string | null, string>>();
  expectTypeOf(value1).toEqualTypeOf<string>();
});

test("Pattern match on schema", (t) => {
  const schema = S.int32;

  if (schema.type === "number") {
    t.expect(schema.format).toBe("int32");
  } else {
    t.expect.fail("Not a schema");
  }
});

test("Test extended JSON Schema", (t) => {
  const schema = S.int32
    .with(S.extendJSONSchema, {
      $ref: "Foo",
    })
    .with(S.extendJSONSchema, {
      readOnly: true,
    });

  t.expect(S.toJSONSchema(schema)).toEqual({
    $ref: "Foo",
    readOnly: true,
    type: "integer",
    minimum: -2147483648,
    maximum: 2147483647,
  });
});

test("toJSONSchema omits default additionalProperties schemas", (t) => {
  const expected = {
    type: "object",
    properties: { value: { type: "string" } },
  };
  const input = { value: "ok", extra: { nested: true } };

  for (const additionalProperties of [true, {}] as const) {
    const schema = S.fromJSONSchema({
      type: "object",
      properties: { value: { type: "string" } },
      additionalProperties,
    });
    t.expect(S.parser(schema)(input)).toBe(input);
    t.expect(S.toJSONSchema(schema)).toEqual(expected);
  }

  const referencedAny = S.fromJSONSchema({
    type: "object",
    additionalProperties: { $ref: "#/$defs/any" },
    $defs: { any: {} },
  });
  t.expect(S.parser(referencedAny)(input)).toBe(input);
  t.expect(S.toJSONSchema(referencedAny)).toEqual({ type: "object" });
  t.expect(S.toJSONSchema(S.record(S.json))).toEqual({ type: "object" });
});

test("S.asyncEncoder runs an async encode codec", async (t) => {
  const schema = S.string.with(S.to, S.number, {
    decode: (string) => string.length,
    encode: { async: (number) => Promise.resolve("x".repeat(number)) },
  });

  // The forward direction stays sync-parseable; async-ness is discovered by
  // catching the sync operation's rejection, not via a dedicated probe.
  t.expect(S.parser(schema)("abc")).toBe(3);
  t.expect(() => S.encoder(schema)).toThrow(
    "The conversion is async. Use the Async version of the operation",
  );
  await t.expect(S.asyncEncoder(schema)(3)).resolves.toBe("xxx");
});

test("All-auto codecs behave exactly like the coder-less spelling", (t) => {
  const schema = S.string.with(S.to, S.number, (string) => string.length);

  // Same-instance target: the self-chain shortcut must apply to the
  // all-"auto" object too, not only to the omitted argument.
  t.expect(S.to(schema, schema, { decode: "auto", encode: "auto" })).toBe(schema);
});

test("Rejects unknown codec slot values at schema creation", (t) => {
  t.expect(() =>
    S.string.with(S.to, S.number, { decode: 1 as any, encode: "auto" }),
  ).toThrow('[Sury] Invalid conversion 1. Expected a function, "auto", "never" or {async: fn}');
  // {async} is strict: extra keys are a misuse, not something to guess about.
  t.expect(() =>
    S.string.with(S.to, S.number, {
      decode: { async: async (value: string) => value.length, sync: 1 } as any,
      encode: "auto",
    }),
  ).toThrow("[Sury] Invalid conversion");
  // A missing (or nulled) direction reads as the incomplete pair it is.
  t.expect(() =>
    S.string.with(S.to, S.number, { decode: null as any, encode: "auto" }),
  ).toThrow(
    '[Sury] Custom codecs must define both decode and encode. Use "auto" for the built-in conversion',
  );
});

test("JS refine produces invalid_input error with expected/received populated", (t) => {
  const schema = S.string.with(S.refine, () => false, { error: "nope" });
  const result = S.safe(() => S.parser(schema)("123"));
  if (result.success) {
    t.expect.fail("Should have thrown");
    return;
  }
  t.expect(result.error.code).toBe("invalid_input");
  t.expect(result.error.reason).toBe("nope");
  if (result.error.code === "invalid_input") {
    t.expect(result.error.expected.type).toBe("string");
    t.expect(result.error.received.type).toBe("string");
  }
});

test("Successfully parses async schema", async (t) => {
  const schema = S.string.with(S.asyncDecoderAssert, async (string) => {
    expectTypeOf(string).toEqualTypeOf<string>();
  });
  const value = await S.safeAsync(() => S.asyncParser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  expectTypeOf(value).toEqualTypeOf<S.Result<string>>();
});

test("Fails to parses async schema", async (t) => {
  const schema = S.string.with(S.asyncDecoderAssert, async () => {
    throw new Error("User error");
  });

  const result = await S.safeAsync(() => S.asyncParser(schema)("123"));

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe("User error");
  t.expect(result.error instanceof S.Error).toBe(true);

  expectTypeOf(result.error.code).toEqualTypeOf<
    | "invalid_input"
    | "invalid_operation"
    | "unsupported_decode"
    | "invalid_conversion"
    | "unrecognized_keys"
  >();

  t.expect(result.error.code).toBe("invalid_conversion");
});

test("Fails to parse strict object with exccess fields which created using global config override", (t) => {
  S.global({
    defaultAdditionalItems: "strict",
  });
  const schema = S.schema({
    foo: S.string,
  });
  // Reset global config back
  S.global({});

  t.expect(() => {
    const value = S.parser(schema)({
      foo: "bar",
      bar: true,
    });
    expectTypeOf(schema).toEqualTypeOf<
      S.Schema<{ foo: string }, { foo: string }>
    >();
    expectTypeOf(value).toEqualTypeOf<{ foo: string }>();
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Unrecognized key "bar"`,
    }),
  );
});

test("Fails to parse intersected objects with transform", (t) => {
  t.expect(() => {
    const schema = S.merge(
      S.schema({
        foo: S.string,
        bar: S.boolean,
      }).with(S.shape, (obj) => ({
        abc: obj.foo,
      })),
      S.schema({
        baz: S.string,
      }),
    );
  }).toThrow(
    t.expect.objectContaining({
      name: "Error",
      message: `[Sury] Can't merge transformed { foo: string; bar: boolean; }`,
    }),
  );

  // expectSchemaType(schema).toBe<
  //   Record<string, unknown>,
  //   {
  //     abc: string;
  //     baz: string;
  //   }
  // >();

  // const result = S.safe(() =>
  //   S.parser(
  //     {
  //       foo: "bar",
  //       bar: true,
  //     },
  //     schema
  //   )
  // );
  // if (result.success) {
  //   t.fail("Should fail");
  //   return;
  // }
  // t.is(
  //   result.error.message,
  //   `Failed at ["baz"]: Expected string, received undefined`
  // );

  // const value = S.parser(
  //   {
  //     foo: "bar",
  //     baz: "baz",
  //     bar: true,
  //   },
  //   schema
  // );
  // t.deepEqual(value, {
  //   abc: "bar",
  //   baz: "baz",
  // });
});

test("Object with an S.never field is inferred as a required never property", (t) => {
  const schema = S.schema({
    key: S.string,
    oldKey: S.never,
  });

  // The field can never hold a value, so it stays a required `never` property:
  // the object is uninhabited, which is what you would write by hand.
  expectSchemaType(schema).toBe<{ key: string; oldKey: never }>();

  // ...and parsing always fails on the never field (see S_never_test.res).
  t.expect(() => S.parser(schema)({ key: "value" })).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at ["oldKey"]: Expected never, received undefined`,
    }),
  );
});

test("Object with an S.optional(S.never) field is inferred as optional undefined", (t) => {
  const schema = S.schema({
    key: S.string,
    oldKey: S.optional(S.never),
  });

  // The realistic deprecated-field pattern: optional collapses to `undefined`,
  // so the field is optional and the object stays inhabited.
  expectSchemaType(schema).toBe<{ key: string; oldKey?: undefined }>();

  const value = S.parser(schema)({ key: "value" });
  t.expect(value).toEqual({ key: "value", oldKey: undefined });
});

test("S.name", (t) => {
  t.expect(S.inputExpression(S.unknown.with(S.meta, { name: "BlaBla" }))).toBe(
    `BlaBla`,
  );
});

test("Successfully parses and returns result", (t) => {
  const schema = S.string;
  const value = S.safe(() => S.parser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  expectTypeOf(value).toEqualTypeOf<S.Result<string>>();
  if (value.success) {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: true;
      readonly value: string;
      readonly error?: undefined;
    }>();
  } else {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: false;
      readonly error: S.Error;
    }>();
  }
});

test("Successfully reverse converts and returns result", (t) => {
  const schema = S.string;
  const value = S.safe(() => S.encoder(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  if (value.success) {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: true;
      readonly value: string;
      readonly error?: undefined;
    }>();
  } else {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: false;
      readonly error: S.Error;
    }>();
  }
});

test("Successfully parses undefined using the default value for transformed schema", (t) => {
  // FIXME: Test that it works correctly:
  // const schema = S.boolean.with(S.optional, false).with(S.to, S.string);
  const schema = S.boolean.with(S.to, S.string).with(S.optional, "false");

  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual("false");
  t.expect(schema.default).toEqual(false);

  expectTypeOf(schema.default).toEqualTypeOf<boolean | undefined>();
  expectSchemaType(schema).toBe<boolean | undefined, string>();
});

test("Successfully parses undefined using the default value from callback", (t) => {
  const schema = S.string.with(S.optional, () => "foo");

  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual("foo");
  t.expect(schema.default).toEqual(undefined);

  //FIXME: This is broken
  // @ts-expect-error
  expectSchemaType(schema).toBe<string | undefined, string>();
});

test("Creates schema with description and title", (t) => {
  const undocumentedStringSchema = S.string;

  expectTypeOf(undocumentedStringSchema).toEqualTypeOf<
    S.Schema<string, string>
  >();

  const documentedStringSchema = undocumentedStringSchema.with(S.meta, {
    title: "My schema",
    description: "A useful bit of text, if you know what to do with it.",
  });

  expectTypeOf(documentedStringSchema).toEqualTypeOf<
    S.Schema<string, string>
  >();

  expectTypeOf(documentedStringSchema.title).toEqualTypeOf<
    string | undefined
  >();
  expectTypeOf(documentedStringSchema.description).toEqualTypeOf<
    string | undefined
  >();

  t.expect(undocumentedStringSchema.description).toEqual(undefined);
  t.expect(documentedStringSchema.description).toEqual(
    "A useful bit of text, if you know what to do with it.",
  );
  t.expect(undocumentedStringSchema.title).toEqual(undefined);
  t.expect(documentedStringSchema.title).toEqual("My schema");
});

test("Creates schema with deprecation", (t) => {
  const schema = S.string;

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string, string>>();

  const deprecatedStringSchema = schema.with(S.meta, {
    deprecated: true,
    description: "Use number instead.",
  });

  expectTypeOf(deprecatedStringSchema).toEqualTypeOf<
    S.Schema<string, string>
  >();

  expectTypeOf(deprecatedStringSchema.deprecated).toEqualTypeOf<
    boolean | undefined
  >();
  expectTypeOf(deprecatedStringSchema.description).toEqualTypeOf<
    string | undefined
  >();

  t.expect(schema.deprecated).toEqual(undefined);
  t.expect(deprecatedStringSchema.deprecated).toEqual(true);
  t.expect(deprecatedStringSchema.description).toEqual("Use number instead.");
});

test("Tuple types", (t) => {
  const emptyTuple = S.schema([]);
  expectSchemaType(emptyTuple).toBe<[]>();

  const tuple1WithLiteral = S.schema(["foo"]);
  expectSchemaType(tuple1WithLiteral).toBe<["foo"]>();

  const tuple1WithSchema = S.schema([S.string]);
  expectSchemaType(tuple1WithSchema).toBe<[string]>();

  const tuple1WithObject = S.schema([{ foo: S.string }]);
  expectSchemaType(tuple1WithObject).toBe<[{ foo: string }]>();

  const tuple2WithLiterals = S.schema(["foo", 123]);
  expectSchemaType(tuple2WithLiterals).toBe<["foo", 123]>();

  const tuple2WithSchemas = S.schema([S.string, S.boolean]);
  expectSchemaType(tuple2WithSchemas).toBe<[string, boolean]>();

  const tuple2LiteralAndSchema = S.schema(["foo", S.boolean]);
  expectSchemaType(tuple2LiteralAndSchema).toBe<["foo", boolean]>();

  const tuple2LiteralAsCosntAndSchema = S.schema(["foo" as const, S.boolean]);
  expectSchemaType(tuple2LiteralAsCosntAndSchema).toBe<["foo", boolean]>();

  const tuple2LiteralSchemaAndSchema = S.schema([S.schema("foo"), S.boolean]);
  expectSchemaType(tuple2LiteralSchemaAndSchema).toBe<["foo", boolean]>();
});

test("Standard schema", (t) => {
  const schema = S.nullable(S.string);

  t.expect(schema["~standard"]["vendor"]).toEqual("sury");
  t.expect(schema["~standard"]["version"]).toEqual(1);
  t.expect(schema["~standard"]["validate"](undefined)).toEqual({
    issues: [
      {
        message: "Expected string | null, received undefined",
        path: undefined,
      },
    ],
  });
  t.expect(schema["~standard"]["validate"]("foo")).toEqual({
    value: "foo",
  });
  t.expect(schema["~standard"]["validate"](null)).toEqual({
    value: null,
  });

  expectTypeOf<S.StandardSchemaV1.InferInput<typeof schema>>().toEqualTypeOf<
    string | null
  >();
  expectTypeOf<S.StandardSchemaV1.InferOutput<typeof schema>>().toEqualTypeOf<
    string | null
  >();
});

// getDecoder answers a repeated call from a per-operation node cache on the
// schema (see OpNode in parse.ts), and
// `~standard.validate` holds its compiled decoder in a closure. Both are keyed
// on the arguments and the global flag, so anything that picks a different
// compiled operation must still get it.
test("Compiled operations stay per-operation and per-global-config", (t) => {
  const schema = S.schema({
    a: S.string.with(S.to, S.number, { decode: Number, encode: String }),
  });

  // Alternating operations on one schema must not answer each other.
  for (let i = 0; i < 3; i++) {
    t.expect(S.parser(schema)({ a: "1" })).toEqual({ a: 1 });
    t.expect(S.encoder(schema)({ a: 1 })).toEqual({ a: "1" });
    t.expect(S.decoder(schema)({ a: "3" })).toEqual({ a: 3 });
    t.expect(S.is(schema, { a: "1" })).toBe(true);
    t.expect(S.is(schema, { a: 1 })).toBe(false);
    t.expect(schema["~standard"].validate({ a: "2" })).toEqual({
      value: { a: 2 },
    });
  }

  // A derived schema must compile its own operation, not inherit the original's.
  t.expect(S.is(S.number, NaN)).toBe(false);
  const derived = S.number.with(S.meta, { title: "t" });
  t.expect(S.parser(derived)(1)).toBe(1);
  t.expect(S.is(derived, NaN)).toBe(false);

  const standard = S.number["~standard"];
  const nanRejected = {
    issues: [{ message: "Expected number, received NaN", path: undefined }],
  };
  t.expect(standard.validate(NaN)).toEqual(nanRejected);
  try {
    S.global({ disableNanNumberValidation: true });
    t.expect(S.is(S.number, NaN)).toBe(true);
    t.expect(standard.validate(NaN)).toEqual({ value: NaN });
  } finally {
    S.global({});
  }
  t.expect(S.is(S.number, NaN)).toBe(false);
  t.expect(standard.validate(NaN)).toEqual(nanRejected);
});

// A conversion rejected at operation creation fails for every input, so it
// isn't a fact about the value being validated — it's a bug in the schema, and
// `issues` is the channel a consumer renders to the person filling in the
// form. It throws to the developer instead, and keeps throwing: `validate`
// holds its decoder across calls, and a compile that never produced one must
// not leave the cache claiming to be current.
test("A conversion rejected at operation creation throws from validate, on every call", (t) => {
  const standard = S.boolean.with(S.to, S.number)["~standard"];
  const message = "Can't decode boolean to number. Use S.to to define a custom decoder";
  for (let i = 0; i < 3; i++) {
    t.expect(() => standard.validate(true)).toThrow(message);
  }

  // Only the compile is promoted: an input that fails validation is still a
  // result, not an exception.
  const schema = S.schema({ id: S.string });
  t.expect(schema["~standard"].validate({ id: "a" })).toEqual({ value: { id: "a" } });
  t.expect(schema["~standard"].validate({ id: 1 })).toEqual({
    issues: [{ message: "Expected string, received 1", path: ["id"] }],
  });
});

// `S.is` makes the same split, for the same reason: `false` is an answer about
// the value, and a schema with no compilable operation has no answer to give.
test("A conversion rejected at operation creation throws from S.is, rather than reading as false", (t) => {
  const rejected = S.boolean.with(S.to, S.number);
  const message = "Can't decode boolean to number. Use S.to to define a custom decoder";
  t.expect(() => S.is(rejected, true)).toThrow(message);
  t.expect(() => S.is(rejected, true)).toThrow(message);
  t.expect(() => S.is(true, rejected)).toThrow(message);

  const schema = S.schema({ id: S.string });
  t.expect(S.is(schema, { id: "a" })).toBe(true);
  t.expect(S.is(schema, { id: 1 })).toBe(false);
  // Both arg orders, and the falsy-data guard that keeps `null`/`undefined`
  // out of the schema slot.
  t.expect(S.is({ id: "a" }, schema)).toBe(true);
  t.expect(S.is(schema, null)).toBe(false);
  t.expect(S.is(null, schema)).toBe(false);
  t.expect(S.is(schema, undefined)).toBe(false);

  // Only a Sury validation failure becomes `false` — a user refinement that
  // throws something else still propagates.
  const boom = S.string.with(S.refine, () => {
    throw new RangeError("boom");
  });
  t.expect(() => S.is(boom, "x")).toThrow("boom");
});

// A recursive def marks itself in-progress in the operation cache before
// compiling (OpNode `v === 0`, parse.ts). A compile that throws must unlink
// that node: left behind, a retry reads it as a live circular reference and
// builds an operation that calls 0 at runtime.
test("A failed recursive compile reports the same error on retry, not a poisoned cache node", (t) => {
  // Nested rather than top-level: a top-level call derives a fresh input
  // schema per compile, so only the nested shape keeps the def-to-def cache
  // triple stable enough for a retry to find the leftover node.
  const schema = S.schema({
    node: S.recursive<{ bad: boolean }, { bad: number }>("BrokenRec", (_) =>
      S.schema({ bad: S.boolean.with(S.to, S.number) }),
    ),
  });
  const message = "Can't decode boolean to number. Use S.to to define a custom decoder";
  t.expect(() => S.parser(schema)).toThrow(message);
  t.expect(() => S.parser(schema)({ node: { bad: true } })).toThrow(message);
});

test("Standard JSON Schema interface support", (t) => {
  const schema = S.schema({ foo: S.to(S.string, S.number) });
  const standard = schema["~standard"];

  // Throws until enableStandardJSONSchema is called.
  t.expect(() => standard.jsonSchema.input({ target: "draft-07" })).toThrow(
    "~standard.jsonSchema requires S.enableStandardJSONSchema() to be called first"
  );

  S.enableStandardJSONSchema();

  // The `~standard` property now also exposes the Standard JSON Schema
  // `jsonSchema` converter. https://standardschema.dev/json-schema
  const jsonSchema: S.StandardJSONSchemaV1.Converter = standard.jsonSchema;

  const inputJsonSchema: Record<string, unknown> = jsonSchema.input({
    target: "draft-07",
  });
  const outputJsonSchema: Record<string, unknown> = jsonSchema.output({
    target: "draft-07",
  });

  // `input` returns the JSON Schema of the input type, with the `$schema` URI
  // for the requested target stamped on top of `S.toJSONSchema(schema)`.
  t.expect(inputJsonSchema).toEqual({
    $schema: "http://json-schema.org/draft-07/schema#",
    ...(S.toJSONSchema(schema) as Record<string, unknown>),
  });
  // `output` returns the JSON Schema of the output type, which differs.
  t.expect(inputJsonSchema).not.toEqual(outputJsonSchema);

  // The `draft-2020-12` target stamps a different `$schema` URI.
  t.expect(jsonSchema.input({ target: "draft-2020-12" }).$schema).toBe(
    "https://json-schema.org/draft/2020-12/schema"
  );
  // The `openapi-3.0` target omits `$schema`.
  t.expect(jsonSchema.input({ target: "openapi-3.0" }).$schema).toBe(undefined);
  // An unsupported target throws.
  t.expect(() =>
    jsonSchema.input({ target: "unsupported-target" })
  ).toThrow("Unsupported JSON Schema target: unsupported-target");
});

test("Full Set schema", (t) => {
  const mySet = <T>(itemSchema: S.Schema<unknown, T>): S.Schema<unknown, Set<T>> =>
    S.instance(Set<unknown>)
      .with(S.to, S.instance(Set<T>), (input) => {
        const output = new Set<T>();
        input.forEach((item, index) => {
          try {
            output.add(S.parser(itemSchema)(item));
          } catch (e) {
            if (e instanceof S.Error) {
              throw new Error(`At item ${index} - ${e.reason}`);
            }
            throw e;
          }
        });
        return output;
      })
      .with(S.meta, {
        name: `Set<${S.inputExpression(itemSchema)}>`,
      });

  const numberSetSchema = mySet(S.number);

  expectSchemaType(numberSetSchema).toBe<unknown, Set<number>>();

  t.expect(S.parser(numberSetSchema)(new Set([1, 2, 3]))).toEqual(
    new Set([1, 2, 3]),
  );

  t.expect(() => S.parser(numberSetSchema)([1, 2, "3"])).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected Set<number>, received [1, 2, "3"]`,
    }),
  );
  t.expect(() => S.parser(numberSetSchema)(new Set([1, 2, "3"]))).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `At item 3 - Expected number, received "3"`,
    }),
  );
});

test("Assert throws with invalid data", (t) => {
  const schema: S.Schema<string> = S.string;

  t.expect(() => {
    S.assert(schema, 123);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected string, received 123",
    }),
  );
});

test("Assert passes with valid data", (t) => {
  const schema = S.string;

  const data: unknown = "abc";
  expectTypeOf(data).toEqualTypeOf<unknown>();
  S.assert(schema, data);
  expectTypeOf(data).toEqualTypeOf<string>();
});

test("Assert supports both (schema, data) and (data, schema) arg orders", (t) => {
  const schema = S.string;

  // (schema, data)
  const a: unknown = "abc";
  S.assert(schema, a);
  expectTypeOf(a).toEqualTypeOf<string>();

  // (data, schema)
  const b: unknown = "abc";
  S.assert(b, schema);
  expectTypeOf(b).toEqualTypeOf<string>();

  // Both orders throw on invalid data
  t.expect(() => S.assert(schema, 123)).toThrow();
  t.expect(() => S.assert(123, schema)).toThrow();
});

test("Is returns a boolean and narrows the type", (t) => {
  const schema = S.string;

  const data: unknown = "abc";
  t.expect(S.is(schema, data)).toBe(true);
  t.expect(S.is(schema, 123)).toBe(false);

  if (S.is(schema, data)) {
    expectTypeOf(data).toEqualTypeOf<string>();
  }
});

test("Is supports both (schema, data) and (data, schema) arg orders", (t) => {
  const schema = S.string;

  // (schema, data)
  t.expect(S.is(schema, "abc")).toBe(true);
  t.expect(S.is(schema, 123)).toBe(false);

  // (data, schema)
  t.expect(S.is("abc", schema)).toBe(true);
  t.expect(S.is(123, schema)).toBe(false);

  // Narrowing works in (data, schema) order too
  const data: unknown = "abc";
  if (S.is(data, schema)) {
    expectTypeOf(data).toEqualTypeOf<string>();
  }
});

test("Is works with advanced schemas", (t) => {
  const schema = S.schema({ foo: S.string });

  t.expect(S.is(schema, { foo: "bar" })).toBe(true);
  t.expect(S.is(schema, { foo: 123 })).toBe(false);
  t.expect(S.is(schema, null)).toBe(false);
});

test("Is returns false for null/undefined data in both arg orders", (t) => {
  const schema = S.string;

  // (schema, data)
  t.expect(S.is(schema, null)).toBe(false);
  t.expect(S.is(schema, undefined)).toBe(false);

  // (data, schema) — nullish data must not throw on schema detection
  t.expect(S.is(null, schema)).toBe(false);
  t.expect(S.is(undefined, schema)).toBe(false);
});

test("Assert throws a Sury error for null/undefined data in both arg orders", (t) => {
  const schema = S.string;

  // (schema, data)
  t.expect(() => S.assert(schema, null)).toThrow(S.Error);
  t.expect(() => S.assert(schema, undefined)).toThrow(S.Error);

  // (data, schema) — nullish data must throw a Sury error, not a TypeError
  t.expect(() => S.assert(null, schema)).toThrow(S.Error);
  t.expect(() => S.assert(undefined, schema)).toThrow(S.Error);
});

test("Schema of object with empty prototype", (t) => {
  const obj = Object.create(null) as { foo: S.Schema<string> };
  obj.foo = S.string;
  const schema = S.schema(obj);

  const data = {
    foo: "bar",
  };
  t.expect(S.parser(schema)(data)).toEqual(data);
  t.expect(S.encoder(schema)(data)).toEqual(data);
});

test("Successfully parses recursive object", (t) => {
  type Node = {
    id: string;
    children: Node[];
  };

  // The one-arg form relies on `TOutput = TInput` — keep it compiling for
  // identity recursion even if the signature changes.
  let nodeSchema = S.recursive<Node>("Node", (nodeSchema) =>
    S.schema({
      id: S.string,
      children: S.array(nodeSchema),
    }),
  );

  expectSchemaType(nodeSchema).toBe<Node, Node>();

  t.expect(
    S.parser(nodeSchema)({
      id: "1",
      children: [
        { id: "2", children: [] },
        { id: "3", children: [{ id: "4", children: [] }] },
      ],
    }),
  ).toEqual({
    id: "1",
    children: [
      { id: "2", children: [] },
      { id: "3", children: [{ id: "4", children: [] }] },
    ],
  });
});

test("Mutually recursive objects", (t) => {
  type User = {
    email: string;
    posts: Post[];
  };
  type Post = {
    title: string;
    author: User;
  };

  const makeUserSchema = (postSchema: S.Schema<unknown, Post>) =>
    S.schema({
      email: S.string,
      posts: S.array(postSchema),
    });
  const makePostSchema = (userSchema: S.Schema<unknown, User>) =>
    S.schema({
      Title: S.string,
      Author: userSchema,
    }).with(S.shape, (post) => ({
      title: post.Title,
      author: post.Author,
    }));

  const userSchema = S.recursive<unknown, User>("User", (userSchema) =>
    makeUserSchema(
      S.recursive<unknown, Post>("Post", (_) => makePostSchema(userSchema)),
    ),
  );
  const postSchema = S.recursive<unknown, Post>("Post", (postSchema) =>
    makePostSchema(
      S.recursive<unknown, User>("User", (_) => makeUserSchema(postSchema)),
    ),
  );

  expectSchemaType(userSchema).toBe<unknown, User>();
  expectSchemaType(postSchema).toBe<unknown, Post>();

  t.expect(
    S.parser(userSchema)({
      email: "test@test.com",
      posts: [
        { Title: "Hello", Author: { email: "test@test.com", posts: [] } },
      ],
    }),
  ).toEqual({
    email: "test@test.com",
    posts: [{ title: "Hello", author: { email: "test@test.com", posts: [] } }],
  });

  t.expect(
    S.parser(postSchema)({
      Title: "Hello",
      Author: { email: "test@test.com", posts: [] },
    }),
  ).toEqual({ title: "Hello", author: { email: "test@test.com", posts: [] } });
});

test("Recursive object with S.shape", (t) => {
  type Node = {
    id: string;
    children: Node[];
  };

  let nodeSchema = S.recursive<unknown, Node>("Node", (nodeSchema) =>
    S.schema({
      ID: S.string,
      CHILDREN: S.array(nodeSchema),
    }).with(S.shape, (input) => ({
      id: input.ID,
      children: input.CHILDREN,
    })),
  );

  expectSchemaType(nodeSchema).toBe<unknown, Node>();

  t.expect(
    S.parser(nodeSchema)({
      ID: "1",
      CHILDREN: [
        { ID: "2", CHILDREN: [] },
        { ID: "3", CHILDREN: [{ ID: "4", CHILDREN: [] }] },
      ],
    }),
  ).toEqual({
    id: "1",
    children: [
      { id: "2", children: [] },
      { id: "3", children: [{ id: "4", children: [] }] },
    ],
  });
});

test("Recursive with self as transform target", (t) => {
  type Node = Node[];

  t.expect(() => {
    let nodeSchema = S.recursive<string, Node>("Node", (self) =>
      S.string.with(S.to, S.array(self)),
    );
    expectSchemaType(nodeSchema).toBe<string, Node>();

    t.expect(S.parser(nodeSchema)(`["[]","[]"]`)).toEqual([[], []]);
  }).toThrow(
    t.expect.objectContaining({
      message:
        "Can't decode string to Node[]. Use S.to to define a custom decoder",
    }),
  );
});

test("Parse to literal with no validation to emulate assert", async (t) => {
  const fn = S.parser(
    S.schema({ foo: S.string }),
    S.schema(true).with(S.noValidation, true),
  );

  expectTypeOf(fn).toEqualTypeOf<(data: unknown) => true>();
  t.expect(fn({ foo: "bar" })).toEqual(true);
  t.expect(fn.toString()).toEqual(
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);return true}`,
  );
});

test("ArkType pattern matching", async (t) => {
  const schema = S.recursive("DbJSON", (self) =>
    S.union([
      S.to(S.bigint, S.string),
      S.string,
      S.number,
      S.boolean,
      null,
      S.record(self),
    ]),
  );

  t.expect(S.parser(schema)(`foo`)).toEqual("foo");
  t.expect(S.parser(schema)(5n)).toEqual("5");
  t.expect(S.parser(schema)({ nested: 5n })).toEqual({ nested: "5" });
  t.expect(S.encoder(schema)("5")).toEqual(5n);
  t.expect(S.encoder(schema)("foo")).toEqual("foo");
});

test("Example of transformed schema", (t) => {
  // 1. Create a schema
  //    S.to - for easy & fast coercion
  //    S.shape - for easy & fast transformation
  //    S.meta - with examples in transformed format
  const userSchema = S.schema({
    USER_ID: S.string.with(S.to, S.bigint),
    USER_NAME: S.string,
  })
    .with(S.shape, (input) => ({
      id: input.USER_ID,
      name: input.USER_NAME,
    }))
    .with(S.meta, {
      description: "User entity in our system",
      examples: [
        {
          id: 0n,
          name: "Dmitry",
        },
      ],
    });
  // On hover: S.Schema<{
  //     id: bigint;
  //     name: string;
  // }, {
  //     USER_ID: string;
  //     USER_NAME: string;
  // }>

  // 2. Infer User type
  type User = S.Output<typeof userSchema>;
  // type User = {
  //   id: bigint;
  //   name: string;
  // }

  // 3. Use examples directly
  //    See how they are in the Input format 🔥
  t.expect(userSchema.examples).toEqual([
    {
      USER_ID: "0",
      USER_NAME: "Dmitry",
    },
  ]);

  // 4. Or via JSON Schema
  t.expect(S.toJSONSchema(userSchema)).toEqual({
    type: "object",
    properties: {
      USER_ID: {
        type: "string",
      },
      USER_NAME: {
        type: "string",
      },
    },
    required: ["USER_ID", "USER_NAME"],
    description: "User entity in our system",
    examples: [
      {
        USER_ID: "0",
        USER_NAME: "Dmitry",
      },
    ],
  });

  const fromJsonSchema = S.fromJSONSchema(S.toJSONSchema(userSchema));
  const jsonInput = { USER_ID: "0", USER_NAME: "Dmitry" };
  t.expect(S.parser(fromJsonSchema)(jsonInput)).toEqual(jsonInput);
});

test("Brand", (t) => {
  const schema = S.string.with(S.brand, "Foo");
  type Foo = S.Infer<typeof schema>;
  expectSchemaType(schema).toBe<string, S.Brand<string, "Foo">>();
  const result = S.parser(schema)("hello");
  assertType<S.Brand<string, "Foo">>(result);
  t.expect(result).toEqual("hello");
  t.expect(schema.name).toEqual("Foo");

  // @ts-expect-error - Branded string is not assignable to string
  const a: Foo = "bar";
});

test("fromJSONSchema", (t) => {
  const emailSchema = S.fromJSONSchema({
    type: "string",
    format: "email",
  });
  expectSchemaType(emailSchema).toBe<string, string>();
  const result = S.safe(() => S.assert(emailSchema, "example.com"));

  t.expect(result.error?.message).toBe(
    `Expected email, received "example.com"`,
  );
});

test("fromJSONSchema: takes untyped input, `satisfies` checks an inline one", (t) => {
  // A schema loaded from a file or an API is untyped, and must not need a cast.
  const loaded: unknown = JSON.parse(`{"type":"string"}`);
  expectSchemaType(S.fromJSONSchema(loaded)).toBe<S.JSON, S.JSON>();
  t.expect(S.parser(S.fromJSONSchema(loaded))("hello")).toBe("hello");

  const asJson: S.JSON = { type: "boolean" };
  expectSchemaType(S.fromJSONSchema(asJson)).toBe<S.JSON, S.JSON>();
  t.expect(S.parser(S.fromJSONSchema(asJson))(true)).toBe(true);

  const authored = {
    type: "object",
    properties: { id: { type: "string" } },
    required: ["id"],
    "x-internal": true,
  } satisfies S.JSONSchema;
  t.expect(S.parser(S.fromJSONSchema(authored))({ id: "1" })).toEqual({
    id: "1",
  });

  const typo = {
    type: "object",
    // @ts-expect-error - an unknown keyword is still caught; only `x-` is open
    requird: ["id"],
  } satisfies S.JSONSchema;
  t.expect(typo.type).toBe("object");
});

test("fromJSONSchema: an inline schema infers the type it describes", (t) => {
  const userSchema = S.fromJSONSchema({
    type: "object",
    properties: {
      id: { type: "string" },
      role: { enum: ["admin", "user"] },
      tags: { type: "array", items: { type: "string" } },
      point: { type: "array", prefixItems: [{ type: "number" }, { type: "number" }] },
      score: { type: "number", nullable: true },
    },
    required: ["id", "role"],
  });
  expectSchemaType(userSchema).toBe<{
    id: string;
    role: "admin" | "user";
    tags?: string[] | undefined;
    point?: [number?, number?, ...S.JSON[]] | undefined;
    score?: number | null | undefined;
  }>();
  t.expect(S.parser(userSchema)({ id: "1", role: "admin" })).toEqual({
    id: "1",
    role: "admin",
  });

  // Local $ref pointers resolve, including recursive ones. The runtime still
  // parses a $ref as plain JSON — the static type leads it here.
  const treeSchema = S.fromJSONSchema({
    $ref: "#/$defs/node",
    $defs: {
      node: {
        type: "object",
        properties: {
          value: { type: "string" },
          children: { type: "array", items: { $ref: "#/$defs/node" } },
        },
        required: ["value"],
      },
    },
  });
  type Tree = S.Output<typeof treeSchema>;
  const tree: Tree = { value: "root", children: [{ value: "leaf" }] };
  assertType<string | undefined>(tree.children?.[0]?.value);

  // A dialect interface isn't a literal, so it falls back to Schema<JSON, JSON>.
  expectSchemaType(
    S.fromJSONSchema(S.toJSONSchema(S.schema({ a: S.string }))),
  ).toBe<S.JSON, S.JSON>();
});

test("fromJSONSchema: assertion-only schemas preserve valid JSON", (t) => {
  const anySchema = S.fromJSONSchema(true);
  const noSchema = S.fromJSONSchema(false);
  const emptyEnum = S.fromJSONSchema({ enum: [] });
  expectSchemaType(anySchema).toBe<S.JSON>();
  expectSchemaType(noSchema).toBe<never>();
  expectSchemaType(emptyEnum).toBe<never>();
  t.expect(S.toJSONSchema(emptyEnum)).toEqual({ not: {} });
  t.expect(S.parser(anySchema)({ nested: [1, true] })).toEqual({ nested: [1, true] });
  t.expect(() => S.parser(noSchema)(null)).toThrow("Expected never");
  t.expect(() => S.parser(emptyEnum)("anything")).toThrow("Expected never");

  const composed = S.fromJSONSchema({
    type: "string",
    minLength: 2,
    anyOf: [{ pattern: "^a" }, { pattern: "z$" }],
  });
  expectSchemaType(composed).toBe<string>();
  t.expect(S.parser(composed)("ab")).toBe("ab");
  t.expect(S.parser(composed)("zz")).toBe("zz");
  t.expect(() => S.parser(composed)("bb")).toThrow(
    "Should pass at least one schema according to the anyOf property."
  );

  const tupleSchema = S.fromJSONSchema({
    type: "array",
    items: [{ type: "string" }, { type: "number" }],
    additionalItems: { type: "boolean" },
  });
  expectSchemaType(tupleSchema).toBe<[
    (string | undefined)?,
    (number | undefined)?,
    ...boolean[],
  ]>();
  for (const value of [[], ["a"], ["a", 1], ["a", 1, true, false]]) {
    t.expect(S.parser(tupleSchema)(value)).toBe(value);
  }
  t.expect(() => S.parser(tupleSchema)(["a", 1, 2])).toThrow(
    "Should pass the positional and additional item schemas."
  );

  const optionalTransformedTuple = S.fromJSONSchema({
    type: "array",
    prefixItems: [
      {
        type: "object",
        properties: { value: { type: "string", default: "fallback" } },
      },
    ],
    items: false,
  });
  expectSchemaType(optionalTransformedTuple).toBe<
    [({ value?: string | undefined } | undefined)?, ...never[]]
  >();
  const absentPrefix: [] = [];
  t.expect(S.parser(optionalTransformedTuple)(absentPrefix)).toBe(absentPrefix);
  t.expect(absentPrefix).toHaveLength(0);
  const presentPrefix: [{ value?: string }] = [{}];
  t.expect(S.parser(optionalTransformedTuple)(presentPrefix)).toBe(presentPrefix);
  t.expect(presentPrefix).toEqual([{}]);

  const closedTupleSchema = S.fromJSONSchema({
    type: "array",
    minItems: 2,
    maxItems: 2,
    items: [{ type: "string" }, { type: "number" }],
  });
  expectSchemaType(closedTupleSchema).toBe<[string, number]>();
  t.expect(S.parser(closedTupleSchema)(["a", 1])).toEqual(["a", 1]);
  // The tuple's own arity is the length check, so `minItems`/`maxItems` add
  // nothing and the error reads like a hand-written `S.tuple`.
  t.expect(() => S.parser(closedTupleSchema)(["a"])).toThrow(
    "Expected [string, number], received"
  );
  t.expect(() => S.parser(closedTupleSchema)(["a", 1, true])).toThrow(
    "Expected [string, number], received"
  );

  // Bounds that cross describe an array no value can have, rather than a tuple
  // carrying two contradictory length checks.
  const emptyTupleRange = S.fromJSONSchema({
    type: "array",
    prefixItems: [{ type: "string" }],
    minItems: 3,
    items: false,
  });
  expectSchemaType(emptyTupleRange).toBe<never>();
  t.expect(S.toJSONSchema(emptyTupleRange)).toEqual({ not: {} });

  const objectSchema = S.fromJSONSchema({
    type: "object",
    properties: { value: { type: "string", default: "fallback" } },
    required: ["constructor"],
    additionalProperties: { type: "integer" },
  });
  expectSchemaType(objectSchema).toBe<
    { value?: string | undefined; constructor: number },
    { value?: string | undefined; constructor: number }
  >();
  const objectInput = { constructor: 1, value: "set", extra: 2 };
  t.expect(S.parser(objectSchema)(objectInput)).toBe(objectInput);
  t.expect(S.parser(objectSchema)({ constructor: 1 })).toEqual({ constructor: 1 });
  t.expect(() => S.parser(objectSchema)({})).toThrow(
    "Should contain every required property."
  );
  t.expect(() => S.parser(objectSchema)({ constructor: 1, extra: "no" })).toThrow(
    "Should pass the additionalProperties schema."
  );

  const nativeDefaultObject = S.fromJSONSchema({
    type: "object",
    properties: { value: { type: "string", default: "fallback" } },
    additionalProperties: false,
  });
  expectSchemaType(nativeDefaultObject).toBe<
    { value?: string | undefined },
    { value: string }
  >();
  t.expect(S.parser(nativeDefaultObject)({})).toEqual({ value: "fallback" });

  const defaultRecord = S.fromJSONSchema({
    type: "object",
    additionalProperties: {
      type: "object",
      properties: { value: { type: "string", default: "fallback" } },
      additionalProperties: false,
    },
  });
  expectSchemaType(defaultRecord).toBe<
    { [key: string]: { value?: string | undefined } },
    { [key: string]: { value: string } }
  >();
  t.expect(S.parser(defaultRecord)({ first: {} })).toEqual({
    first: { value: "fallback" },
  });

  const openObjectSchema = S.fromJSONSchema({
    type: "object",
    properties: { value: { type: "string" } },
    additionalProperties: true,
  });
  const ownProto = JSON.parse(
    '{"value":"ok","__proto__":{"polluted":true}}'
  ) as S.Input<typeof openObjectSchema>;
  const parsedOwnProto = S.parser(openObjectSchema)(ownProto);
  t.expect(parsedOwnProto).toBe(ownProto);
  t.expect(Object.getPrototypeOf(parsedOwnProto)).toBe(Object.prototype);
  t.expect(Object.hasOwn(parsedOwnProto, "__proto__")).toBe(true);
  t.expect((parsedOwnProto as { polluted?: boolean }).polluted).toBeUndefined();

  const unicode = S.fromJSONSchema({
    type: "string",
    minLength: 2,
    maxLength: 2,
  });
  t.expect(S.parser(unicode)("\u{10400}\u{10401}")).toBe("\u{10400}\u{10401}");
  t.expect(() => S.parser(unicode)("😀")).toThrow(
    "Should have a code-point length within the JSON Schema bounds."
  );

  const legacyPattern = S.fromJSONSchema({
    type: "string",
    pattern: "^\\d{3}\\-\\d{4}$",
  });
  t.expect(S.parser(legacyPattern)("123-4567")).toBe("123-4567");
  t.expect(() => S.parser(legacyPattern)("1234567")).toThrow("Invalid pattern");
  t.expect(() => S.fromJSONSchema({ type: "string", pattern: "[" })).toThrow(
    'Invalid JSON Schema pattern: "["'
  );
});

test("fromJSONSchema: $ref siblings follow the declared dialect", (t) => {
  const target = { type: "string" } as const;
  const modern = S.fromJSONSchema({
    $schema: "https://json-schema.org/draft/2020-12/schema",
    $ref: "#/$defs/id",
    minLength: 3,
    anyOf: [{ pattern: "c$" }],
    $defs: { id: target },
  });
  const legacy = S.fromJSONSchema({
    $schema: "http://json-schema.org/draft-07/schema#",
    $ref: "#/$defs/id",
    minLength: 3,
    anyOf: [{ pattern: "never$" }],
    $defs: { id: target },
  });
  expectSchemaType(modern).toBe<string>();
  expectSchemaType(legacy).toBe<string>();
  t.expect(S.parser(modern)("abc")).toBe("abc");
  t.expect(() => S.parser(modern)("ab")).toThrow(
    "Should pass the keywords adjacent to the $ref."
  );
  t.expect(() => S.parser(modern)("abd")).toThrow(
    "Should pass at least one schema according to the anyOf property."
  );
  t.expect(() => S.parser(legacy)("ab")).toThrow(
    "Should pass at least one schema according to the anyOf property."
  );
  // The `$ref` resolved to a finite shape and inlined, so it left no `$defs`
  // entry behind — and the rendering doesn't depend on whether options were
  // passed, only on the dialect they name.
  const legacyRendering = {
    type: "string",
    anyOf: [{ pattern: "never$" }],
  };
  t.expect(S.toJSONSchema(legacy)).toEqual(legacyRendering);
  t.expect(S.toJSONSchema(legacy, { target: "openapi-3.0" })).toEqual(legacyRendering);
  t.expect(S.toJSONSchema(legacy, { target: "draft-07" })).toEqual({
    ...legacyRendering,
    $schema: "http://json-schema.org/draft-07/schema#",
  });

  const modernAlias = S.fromJSONSchema({
    $schema: "http://json-schema.org/draft/2020-12/schema#",
    $ref: "#/$defs/id",
    type: "number",
    $defs: { id: target },
  });
  const legacyAlias = S.fromJSONSchema({
    $schema: "https://json-schema.org/draft-07/schema",
    $ref: "#/$defs/id",
    type: "number",
    $defs: { id: target },
  });
  expectSchemaType(modernAlias).toBe<never>();
  expectSchemaType(legacyAlias).toBe<string>();
  t.expect(() => S.parser(modernAlias)("abc")).toThrow(
    "Should pass the keywords adjacent to the $ref."
  );
  t.expect(S.parser(legacyAlias)("abc")).toBe("abc");

  const customDialect = {
    $schema: "https://json-schema.org/draft/2020-12/custom",
    $ref: "#/$defs/id",
    type: "number",
    $defs: { id: target },
  } as const;
  const customDialectSchema = S.fromJSONSchema(customDialect);
  expectSchemaType(customDialectSchema).toBe<string>();
  t.expect(S.parser(customDialectSchema)("abc")).toBe("abc");

  const nestedModern = S.fromJSONSchema({
    $schema: "https://json-schema.org/draft/2020-12/schema",
    type: "object",
    properties: { id: { $ref: "#/$defs/id" } },
    $defs: { id: target },
  });
  expectSchemaType(nestedModern).toBe<{ id?: string | undefined }>();
  t.expect(S.toJSONSchema(nestedModern)).toEqual({
    type: "object",
    properties: { id: { type: "string" } },
  });
});

test("toJSONSchema: the target picks the dialect of the result", (t) => {
  const tupleSchema = S.schema([S.string, S.number]);

  const draft07 = S.toJSONSchema(tupleSchema);
  expectTypeOf(draft07).toEqualTypeOf<S.JSONSchema7>();
  t.expect(draft07.items).toEqual([{ type: "string" }, { type: "number" }]);

  const draft2020 = S.toJSONSchema(tupleSchema, { target: "draft-2020-12" });
  expectTypeOf(draft2020).toEqualTypeOf<S.JSONSchema2020>();
  t.expect(draft2020.prefixItems).toEqual([
    { type: "string" },
    { type: "number" },
  ]);

  const openapi = S.toJSONSchema(S.nullable(S.string), {
    target: "openapi-3.0",
  });
  expectTypeOf(openapi).toEqualTypeOf<S.OpenAPISchema30>();
  t.expect(openapi.nullable).toBe(true);

  const imported2020 = S.fromJSONSchema({
    $schema: "https://json-schema.org/draft/2020-12/schema",
    type: "array",
    items: [{ type: "string" }, { type: "number" }],
    additionalItems: false,
    minItems: 2,
    maxItems: 2,
  });
  t.expect(S.toJSONSchema(imported2020, { target: "draft-07" })).toEqual({
    $schema: "http://json-schema.org/draft-07/schema#",
    type: "array",
    minItems: 2,
    maxItems: 2,
    items: [{ type: "string" }, { type: "number" }],
  });
  t.expect(S.toJSONSchema(imported2020, { target: "openapi-3.0" })).not.toHaveProperty(
    "$schema",
  );

  // A target held in a variable can't select a dialect, so the result widens.
  const target: S.StandardJSONSchemaV1.Target = "draft-07";
  expectTypeOf(S.toJSONSchema(tupleSchema, { target })).toEqualTypeOf<
    S.JSONSchema
  >();

  // @ts-expect-error - draft-07 spells tuples with `items`, not `prefixItems`
  draft07.prefixItems;
  // @ts-expect-error - OpenAPI 3.0 has no `const`; it uses a one-value `enum`
  openapi.const;

  // Every dialect's result feeds back in without a cast.
  t.expect(S.parser(S.fromJSONSchema(draft2020))(["a", 1])).toEqual(["a", 1]);
  t.expect(S.parser(S.fromJSONSchema(openapi))(null)).toBe(null);

  // Every dialect stays assignable to the wide type — the invariant that keeps
  // `extendJSONSchema(schema, toJSONSchema(other, { target }))` compiling. This
  // breaks when a shared keyword is typed incompatibly across the two (extra
  // dialect-only keywords slip through structurally — parity there is on the
  // comment in src/types/jsonschema.d.ts).
  expectTypeOf<S.JSONSchema7>().toExtend<S.JSONSchema>();
  expectTypeOf<S.JSONSchema2020>().toExtend<S.JSONSchema>();
  expectTypeOf<S.OpenAPISchema30>().toExtend<S.JSONSchema>();
});

test("fromJSONSchema: assertion keywords bind without an explicit `type`", (t) => {
  const parse = (js: object) => S.parser(S.fromJSONSchema(js)) as (d: unknown) => unknown;

  const obj = parse({ properties: { bar: { type: "integer" } }, required: ["bar"] });
  t.expect(obj({ bar: 2 })).toEqual({ bar: 2 });
  t.expect(S.safe(() => obj({ bar: "x" })).error).toBeDefined();
  t.expect(S.safe(() => obj({})).error).toBeDefined();

  const min = parse({ minimum: 5 });
  t.expect(min(7)).toBe(7);
  t.expect(S.safe(() => min(3)).error).toBeDefined();
  // Vacuous off-type: `minimum` says nothing about a string.
  t.expect(min("abc")).toBe("abc");

  const minLength = parse({ minLength: 3 });
  t.expect(minLength("abcd")).toBe("abcd");
  t.expect(S.safe(() => minLength("a")).error).toBeDefined();
  t.expect(minLength(1)).toBe(1);

  t.expect(parse({ additionalItems: false })(["anything"])).toEqual(["anything"]);
});

test("fromJSONSchema: annotations stay on a synthesized type union's root", (t) => {
  const schema = S.fromJSONSchema({
    type: ["string", "number"],
    title: "Value",
  });
  t.expect(S.toJSONSchema(schema)).toEqual({
    anyOf: [{ type: "string" }, { type: "number" }],
    title: "Value",
  });
});

test("fromJSONSchema: composition keywords constrain in addition to the base shape", (t) => {
  const emptyAllOf = S.fromJSONSchema({ allOf: [] });
  expectSchemaType(emptyAllOf).toBe<S.JSON>();
  t.expect(S.parser(emptyAllOf)({ anything: true })).toEqual({ anything: true });

  const schema = S.fromJSONSchema({
    type: "object",
    properties: { bar: { type: "integer" } },
    required: ["bar"],
    allOf: [{ properties: { foo: { type: "string" } }, required: ["foo"] }],
  });
  const parse = S.parser(schema) as (d: unknown) => unknown;

  const input = { bar: 2, foo: "x" };
  t.expect(parse(input)).toEqual({ bar: 2 });
  // Fails the base shape.
  t.expect(S.safe(() => parse({ bar: "no", foo: "x" })).error).toBeDefined();
  // Fails only the allOf branch — the base shape alone used to win.
  t.expect(S.safe(() => parse({ bar: 2 })).error).toBeDefined();
});

test("fromJSONSchema: oneOf counts matches, `not` and if/then/else layer on", (t) => {
  const one = S.parser(
    S.fromJSONSchema({ oneOf: [{ type: "number" }, { type: "string" }] }),
  ) as (d: unknown) => unknown;
  t.expect(one(1)).toBe(1);
  t.expect(S.safe(() => one(true)).error).toBeDefined();

  const not = S.parser(S.fromJSONSchema({ not: { type: "string" } })) as (
    d: unknown,
  ) => unknown;
  t.expect(not(1)).toBe(1);
  t.expect(S.safe(() => not("x")).error).toBeDefined();

  // `then`/`else` are each optional and default to "always passes".
  const ite = S.parser(
    S.fromJSONSchema({ if: { type: "number" }, then: { minimum: 5 } }),
  ) as (d: unknown) => unknown;
  t.expect(ite(7)).toBe(7);
  t.expect(ite("anything")).toBe("anything");
  t.expect(S.safe(() => ite(3)).error).toBeDefined();
});

test("fromJSONSchema: an unmodelled assertion keyword fails at creation", (t) => {
  // Ignoring it would widen the schema — the validator would accept data the
  // author wrote the keyword to reject — so this must not silently succeed.
  const result = S.safe(() => S.fromJSONSchema({ type: "object", patternProperties: {} }));
  t.expect(result.error?.message).toContain("Unsupported JSON Schema keyword: patternProperties");

  t.expect(
    S.safe(() => S.fromJSONSchema({ type: "array", uniqueItems: true })).error?.message,
  ).toContain("uniqueItems");

  t.expect(
    S.safe(() => S.fromJSONSchema({ $dynamicRef: "#items" })).error?.message,
  ).toContain("$dynamicRef");

  t.expect(
    S.safe(() => S.fromJSONSchema({ $recursiveRef: "#" })).error?.message,
  ).toContain("$recursiveRef");

  t.expect(
    S.parser(
      S.fromJSONSchema({
        $schema: "https://example.com/custom-meta-schema",
        type: "number",
      }),
    )(1),
  ).toBe(1);
});

test("fromJSONSchema: exclusiveMaximum bounds the maximum, not the minimum", (t) => {
  const parse = S.parser(
    S.fromJSONSchema({ type: "integer", exclusiveMaximum: 5 }),
  ) as (d: unknown) => unknown;
  t.expect(parse(4)).toBe(4);
  t.expect(S.safe(() => parse(5)).error).toBeDefined();
  t.expect(S.safe(() => parse(9)).error).toBeDefined();
});

test("Compile types", async (t) => {
  const schema = S.union([
    S.string,
    S.schema(null).with(S.to, S.schema(undefined)),
  ]);

  const fn1 = S.decoder(schema);
  expectTypeOf(fn1).toEqualTypeOf<
    (input: string | null) => string | undefined
  >();
  t.expect(fn1("hello")).toEqual("hello");
  t.expect(fn1(null)).toEqual(undefined);

  const fn2 = S.encoder(schema);
  expectTypeOf(fn2).toEqualTypeOf<
    (input: string | undefined) => string | null
  >();
  t.expect(fn2("hello")).toEqual("hello");
  t.expect(fn2(undefined)).toEqual(null);

  const fn3 = S.parser(schema);
  expectTypeOf(fn3).toEqualTypeOf<(input: unknown) => string | undefined>();
  t.expect(fn3("hello")).toEqual("hello");
  t.expect(fn3(null)).toEqual(undefined);

  const fn4 = S.decoder(S.json, schema);
  expectTypeOf(fn4).toEqualTypeOf<(input: S.JSON) => string | undefined>();
  t.expect(fn4("hello")).toEqual("hello");
  t.expect(fn4(null)).toEqual(undefined);

  const fn5 = S.decoder(S.jsonString, schema);
  expectTypeOf(fn5).toEqualTypeOf<(input: string) => string | undefined>();
  t.expect(fn5(`"hello"`)).toEqual("hello");
  t.expect(fn5("null")).toEqual(undefined);

  const fn6 = S.encoder(schema, S.json);
  expectTypeOf(fn6).toEqualTypeOf<(input: string | undefined) => S.JSON>();
  t.expect(fn6("hello")).toEqual("hello");
  t.expect(fn6(undefined)).toEqual(null);

  const fn7 = S.encoder(schema, S.jsonString);
  expectTypeOf(fn7).toEqualTypeOf<(input: string | undefined) => string>();
  t.expect(fn7("hello")).toEqual(`"hello"`);
  t.expect(fn7(undefined)).toEqual("null");

  // FIXME:
  // const fn8 = S.compile(schema, "Output", "Assert", "Sync", true);
  // expectTypeOf(fn8).toEqualTypeOf<(input: string | undefined) => void>();
  // t.deepEqual(fn8("hello"), undefined);
  // t.deepEqual(fn8(undefined), undefined);

  // const fn9 = S.compile(schema, "Output", "JsonString", "Async");
  // expectTypeOf(fn9).toEqualTypeOf<(input: string | undefined) => Promise<string>>();
  // t.deepEqual(await fn9("hello"), `"hello"`);
  // t.deepEqual(await fn9(undefined), "null");
});

test("Preprocess nested fields", (t) => {
  const stripPrefix = <TInput>(
    schema: S.Schema<TInput, string>,
    prefix: string,
  ): S.Schema<TInput, string> =>
    S.to(schema, S.string, {
      decode: (v) => {
        if (v.startsWith(prefix)) {
          return v.slice(1);
        } else {
          throw new Error(`String must start with ${prefix}`);
        }
      },
      encode: (v) => prefix + v,
    });

  const schema = S.schema({
    nested: {
      tag: S.string.with(stripPrefix, "_").with(S.to, S.schema("foo")),
      numberTag: S.string.with(stripPrefix, "~").with(S.to, S.schema(1)),
    },
  }).with(S.shape, (_) => undefined);

  const fn = S.encoder(schema);

  t.expect(fn.toString()).toEqual(
    // The junction seam validates each coder's result against its target.
    `i=>{i===void 0||e[6](i);let v0;try{v0=e[0]("foo")}catch(x){e[1](x)}typeof v0==="string"||e[2](v0);let v1;try{v1=e[3]("1")}catch(x){e[4](x)}typeof v1==="string"||e[5](v1);return {"nested":{"tag":v0,"numberTag":v1,},}}`,
  );

  const value = fn(undefined);
  t.expect(value).toEqual({
    nested: {
      numberTag: "~1",
      tag: "_foo",
    },
  });
});

test("Union of object keys", (t) => {
  // https://github.com/DZakh/sury/issues/128
  const allCurrencies = {
    USD: 1,
    BGP: 2,
    EUR: 3,
  };

  const schema = S.union(Object.keys(allCurrencies));
  expectSchemaType(schema).toBe<string, string>();
  t.expect(S.parser(schema)("USD")).toEqual("USD");
  t.expect(() => S.parser(schema)("GBP")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "USD" | "BGP" | "EUR", received "GBP"`,
    }),
  );

  const schema2 = S.union(
    Object.keys(allCurrencies) as (keyof typeof allCurrencies)[],
  );
  expectSchemaType(schema2).toBe<
    "USD" | "BGP" | "EUR",
    "USD" | "BGP" | "EUR"
  >();
  t.expect(S.parser(schema)("USD")).toEqual("USD");
  t.expect(() => S.parser(schema)("GBP")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "USD" | "BGP" | "EUR", received "GBP"`,
    }),
  );

  const schema3 = S.union(
    (Object.keys(allCurrencies) as (keyof typeof allCurrencies)[]).map(
      (literal) => S.schema(literal),
    ),
  );
  expectSchemaType(schema3).toBe<
    "USD" | "BGP" | "EUR",
    "USD" | "BGP" | "EUR"
  >();
  t.expect(S.parser(schema)("USD")).toEqual("USD");
  t.expect(() => S.parser(schema)("GBP")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "USD" | "BGP" | "EUR", received "GBP"`,
    }),
  );
});

test("Union of dynamic enum as const", (t) => {
  // https://github.com/DZakh/sury/issues/137
  const test = ["a", "b", "c"] as const;
  const schema = S.union(test);

  expectSchemaType(schema).toBe<"a" | "b" | "c", "a" | "b" | "c">();
  t.expect(S.parser(schema)("a")).toEqual("a");
  t.expect(() => S.parser(schema)("d")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "a" | "b" | "c", received "d"`,
    }),
  );
});

test("Overwrite error message", (t) => {
  const schema = S.string.with(S.minLength, 3, "Invalid string");

  const fieldSchema = <TInput, TOutput>(
    schema: S.Schema<TInput, TOutput>,
  ): S.Schema<TInput, TOutput> => {
    return S.any.with(S.to, schema, (v) => {
      try {
        S.assert(schema, v);
        return v;
      } catch (e) {
        if (e instanceof S.Error) {
          throw new Error(e.reason);
        }
        throw e;
      }
    });
  };

  // Doesn't work starting from 11.0.0-alpha.4
  // The error is always wrapped in SuryError
  t.expect(() =>
    S.parser(S.schema({ foo: fieldSchema(schema) }))({ foo: "hi" }),
  ).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at ["foo"]: Invalid string`,
    }),
  );
});

test("Uint8Array", (t) => {
  let data = new Uint8Array([1, 2, 3]);

  t.expect(S.parser(S.uint8Array)(data)).toEqual(data);
  t.expect(S.parser(S.uint8Array).toString()).toEqual(
    `i=>{i instanceof e[0]||e[1](i);return i}`,
  );

  t.expect(S.decoder(S.string, S.uint8Array, S.jsonString)("data")).toEqual(
    `"data"`,
  );
  t.expect(S.decoder(S.string, S.uint8Array, S.jsonString).toString()).toEqual(
    `i=>{return e[2](e[1].decode(e[0].encode(i)))}`,
  );
  t.expect(S.decoder(S.unknown, S.uint8Array, S.jsonString).toString()).toEqual(
    `i=>{i instanceof e[2]||e[3](i);return e[1](e[0].decode(i))}`,
  );

  // As an object field: jsonString's fallback asks the field's own schema for
  // the string conversion, since uint8Array only performs it when it is
  // itself the target. Not a spec — the encode direction's output holds a
  // Uint8Array, which the spec harness can't write as source (see
  // CONTRIBUTING.md's Spec Harness Suggestions).
  const withField = S.schema({ payload: S.uint8Array });
  t.expect(
    S.encoder(withField, S.jsonString)({ payload: new TextEncoder().encode("hi") }),
  ).toEqual(`{"payload":"hi"}`);
  t.expect(S.decoder(S.jsonString, withField)(`{"payload":"hi"}`)).toEqual({
    payload: new TextEncoder().encode("hi"),
  });
});

test("Throwing one retained error instance twice doesn't accumulate the path", (t) => {
  // The path a throw is reached through is prepended to the error, so doing it
  // on the caught instance leaves the second parse reporting `["a"]["a"]`.
  // Nothing stops user code from holding one error and throwing it again.
  const retained = S.safe(() => S.parser(S.string)(1)).error!;
  const schema = S.schema({
    a: S.string.with(S.to, S.number, () => {
      throw retained;
    }),
  });
  const parse = S.parser(schema);

  for (const _ of [1, 2, 3]) {
    const result = S.safe(() => parse({ a: "x" }));
    t.expect(result.error?.message).toBe(
      `Failed at ["a"]: Expected string, received 1`,
    );
    t.expect(result.error?.path).toBe(`["a"]`);
  }
  // The instance user code holds is left as it was caught.
  t.expect(retained.path).toBe("");

  // Top level: nothing to prepend, so the error is passed through rather than
  // copied. Still must not pick up a path or mutate what was thrown.
  const flat = S.parser(
    S.string.with(S.to, S.number, () => {
      throw retained;
    }),
  );
  for (const _ of [1, 2, 3]) {
    t.expect(S.safe(() => flat("x")).error?.path).toBe("");
  }
  t.expect(retained.path).toBe("");
});

test("A contradictory bound pair is rejected where it's written", (t) => {
  // The schema would compile and then reject every possible value, which only
  // surfaces in production — so it fails at construction instead. Both sides
  // render through inputExpression, so the message is in the same syntax the
  // schema is, not the constructor names the caller happened to use.
  t.expect(() => S.number.with(S.gte, 5).with(S.lte, 1)).toThrow(
    `[Sury] number <= 1 contradicts number >= 5`,
  );
  t.expect(() => S.number.with(S.lte, 1).with(S.gte, 5)).toThrow(
    `[Sury] number >= 5 contradicts number <= 1`,
  );
  // Exclusive bounds make the touching cases empty too.
  t.expect(() => S.number.with(S.gt, 5).with(S.lte, 5)).toThrow(
    `[Sury] number <= 5 contradicts number > 5`,
  );
  t.expect(() => S.number.with(S.gte, 5).with(S.lt, 5)).toThrow(
    `[Sury] number < 5 contradicts number >= 5`,
  );
  t.expect(() => S.string.with(S.minLength, 5).with(S.maxLength, 1)).toThrow(
    `[Sury] string.length <= 1 contradicts string.length >= 5`,
  );
  t.expect(() => S.array(S.string).with(S.minLength, 5).with(S.maxLength, 1)).toThrow(
    `[Sury] string[].length <= 1 contradicts string[].length >= 5`,
  );
  // `nonEmpty` desugars to a length bound, and reports as one rather than
  // naming a constructor the caller didn't write.
  t.expect(() => S.string.with(S.minLength, 2).with(S.length, 0)).toThrow(
    `[Sury] string.length <= 0 contradicts string.length >= 2`,
  );
  // A format's range is a bound like any other, so a value outside it conflicts.
  t.expect(() => S.int32.with(S.gte, 3000000000)).toThrow(
    `[Sury] int32 >= 3000000000 contradicts int32 <= 2147483647`,
  );
  t.expect(() => S.port.with(S.lte, -1)).toThrow(
    `[Sury] port <= -1 contradicts port >= 0`,
  );
  // Combining divisors stores their LCM; an LCM past 2^53 rounds and would
  // validate the wrong set, and fractional divisors have no float LCM — both
  // refuse rather than silently drift.
  t.expect(() =>
    S.integer.with(S.multipleOf, 67108859).with(S.multipleOf, 134217689).with(S.multipleOf, 2097143)
  ).toThrow(`[Sury] multipleOf 2097143 cannot be combined with multipleOf 9007195966406851`);
  t.expect(() => S.number.with(S.multipleOf, 0.3).with(S.multipleOf, 0.2)).toThrow(
    `[Sury] multipleOf 0.2 cannot be combined with multipleOf 0.3`,
  );
  // A divisor excluded by the range is NOT a construction error, unlike a
  // pair of bounds: detecting it needs multiples-in-range arithmetic (see the
  // updateBounds comment). The schema builds and rejects everything, with the
  // divisor and the range both in the message.
  t.expect(
    S.safe(() =>
      S.assert(S.number.with(S.gt, 0).with(S.lt, 5).with(S.multipleOf, 10), 3)
    ).error?.message
  ).toBe("Expected 0 < (number % 10) < 5, received 3");

  // A single point is satisfiable, so these stay legal.
  t.expect(S.toJSONSchema(S.number.with(S.gte, 5).with(S.lte, 5))).toEqual({
    type: "number",
    minimum: 5,
    maximum: 5,
  });
  t.expect(S.toJSONSchema(S.number.with(S.gt, 5).with(S.lt, 6))).toEqual({
    type: "number",
    exclusiveMinimum: 5,
    exclusiveMaximum: 6,
  });
  // A divisor larger than the range still admits 0, and a single point is a
  // point like any other.
  t.expect(S.toJSONSchema(S.int32.with(S.multipleOf, 3000000000))).toEqual({
    type: "integer",
    minimum: -2147483648,
    maximum: 2147483647,
    multipleOf: 3000000000,
  });
});

test("A superseded bound takes its message with it", (t) => {
  // The surviving check is the one the caller's message has to reach, so a
  // message written on a bound that doesn't narrow carries onto it...
  t.expect(
    S.safe(() => S.assert(S.number.with(S.gte, 5).with(S.gte, 1, "MY MESSAGE"), 3)).error?.message
  ).toBe("MY MESSAGE");
  // ...and a narrowing replacement without one clears the stale text rather
  // than reporting a bound the schema no longer advertises.
  t.expect(
    S.safe(() => S.assert(S.number.with(S.gte, 5, "A").with(S.gte, 10), 7)).error?.message
  ).toBe("Expected number >= 10, received 7");
  // Switching form replaces the field, so the message keyed to the old form
  // goes with it instead of lingering where nothing reads it.
  const flipped = S.number.with(S.gte, 5, "A").with(S.gt, 10);
  t.expect(flipped.errorMessage?.minimum).toBe(undefined);
  t.expect(
    S.safe(() => S.assert(flipped, 7)).error?.message
  ).toBe("Expected number > 10, received 7");
});

test("An unsatisfiable JSON Schema document loads as never", (t) => {
  // Legal JSON Schema — it just describes a type nothing inhabits — so it has
  // to load rather than fail the way the hand-written equivalent does.
  for (const definition of [
    { type: "number", minimum: 5, maximum: 1 },
    { type: "integer", minimum: 5, maximum: 1 },
    { type: "number", exclusiveMinimum: 5, maximum: 5 },
    { type: "string", minLength: 5, maxLength: 1 },
    { type: "string", minLength: -1 },
    { type: "array", minItems: 5, maxItems: 1 },
    { type: "array", minItems: -1 },
    { type: "array", maxItems: -1 },
    { type: [] },
  ] as const) {
    const schema = S.fromJSONSchema(definition);
    t.expect(S.inputExpression(schema)).toEqual("never");
  }
});

test("fromJSONSchema: literal bounds narrow the inferred type", () => {
  expectSchemaType(
    S.fromJSONSchema({
      type: "array",
      items: { type: "string" },
      minItems: 2,
      maxItems: 2,
    }),
  ).toBe<[string, string]>();
  expectSchemaType(
    S.fromJSONSchema({ type: "array", items: { type: "string" }, maxItems: 0 }),
  ).toBe<[]>();
  expectSchemaType(
    S.fromJSONSchema({ type: "string", minLength: 0, maxLength: 0 }),
  ).toBe<"">();
  expectSchemaType(
    S.fromJSONSchema({ type: "number", minimum: 5, maximum: 1 }),
  ).toBe<never>();
  expectSchemaType(
    S.fromJSONSchema({ type: "number", exclusiveMinimum: 5, maximum: 5 }),
  ).toBe<never>();
  expectSchemaType(
    S.fromJSONSchema({ type: "string", minLength: 5, maxLength: 1 }),
  ).toBe<never>();
  expectSchemaType(
    S.fromJSONSchema({ type: "array", minItems: 5, maxItems: 1 }),
  ).toBe<never>();
  expectSchemaType(S.fromJSONSchema({ type: [] })).toBe<never>();
});

test("Schema toString prints Schema<input, output>", (t) => {
  t.expect(S.string.toString()).toBe("Schema<string>");
  t.expect(S.to(S.string, S.number).toString()).toBe("Schema<string, number>");
  t.expect(`${S.schema({ a: S.string })}`).toBe("Schema<{ a: string; }>");
  t.expect(`${S.union([S.string, S.number])}`).toBe("Schema<string | number>");

  // Nested transforms only reverse correctly through S.reverse, not a .to walk.
  t.expect(`${S.schema({ a: S.to(S.string, S.number) })}`).toBe(
    "Schema<{ a: string; }, { a: number; }>",
  );

  // The apparent type supplies toString without S.d.ts declaring it.
  expectTypeOf(S.string.toString()).toEqualTypeOf<string>();
});

// The schema prototype is Object.create(null), so before there was a toString
// there was nothing to coerce through and every one of these threw
// "Cannot convert object to primitive value" rather than merely reading badly.
test("Schema survives string coercion", (t) => {
  t.expect(String(S.string)).toBe("Schema<string>");
  t.expect(S.string + "").toBe("Schema<string>");
  t.expect([S.string, S.number].join(", ")).toBe("Schema<string>, Schema<number>");
});

// util.inspect ignores toString, and no inspect hook is registered on purpose,
// so console.log still reveals the internal shape for debugging. Asserted so
// that adding a hook is a deliberate change rather than a silent one.
test("console.log shows the internal schema shape, not the expression", (t) => {
  const dump = inspect(S.string);
  t.expect(dump).not.toBe("Schema<string>");
  t.expect(dump).toContain("type: 'string'");

  // %s formats via toString, which is the opt-in path.
  t.expect(format("%s", S.to(S.string, S.number))).toBe("Schema<string, number>");
});

test("Error messages render through inputExpression, not toString", (t) => {
  const schema = S.schema({ id: S.string });
  let error: { message: string; reason: string; expected: unknown } | undefined;
  try {
    S.parser(schema)({ id: 1 });
  } catch (exn) {
    error = exn as typeof error;
  }

  // No "Schema<…>" wrapper: the message names the type, it does not print the
  // schema object.
  t.expect(error!.message).toBe('Failed at ["id"]: Expected string, received 1');
  t.expect(error!.reason).toBe("Expected string, received 1");
  t.expect(`${error}`).toBe(
    'SuryError: Failed at ["id"]: Expected string, received 1',
  );

  // The schema hanging off the error is where toString does help.
  t.expect(`${error!.expected}`).toBe("Schema<string>");
});

// Rendering the received value used to walk objects and arrays without a
// limit, so a cyclic input overflowed the stack inside the error formatter — a
// validation failure surfaced as a RangeError instead of a SuryError. One level
// of expansion keeps that fixed: the cycle is reached at depth 1 and named.
test("A cyclic input is reported, not a stack overflow", (t) => {
  const cyclic: Record<string, unknown> = { a: 1 };
  cyclic["self"] = cyclic;

  t.expect(() => S.parser(S.string)(cyclic)).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected string, received { a: 1; self: object; }",
    }),
  );
});

test("A received value is expanded one level", (t) => {
  const reasonFor = (value: unknown): string => {
    try {
      S.parser(S.string)(value);
      return "(accepted)";
    } catch (exn) {
      return (exn as { reason: string }).reason;
    }
  };
  const received = (value: unknown) => reasonFor(value).replace("Expected string, received ", "");

  // Primitives keep their value — `received 42` beats `received number` — and
  // bigint keeps its suffix so it stays distinguishable from a number.
  t.expect(received(42)).toBe("42");
  t.expect(received(10n)).toBe("10n");
  t.expect(received(NaN)).toBe("NaN");

  // Plain objects and arrays expand; anything else names its constructor.
  t.expect(received({ a: 1, b: "x" })).toBe('{ a: 1; b: "x"; }');
  t.expect(received([1, "a"])).toBe('[1, "a"]');
  t.expect(received({})).toBe("{}");
  t.expect(received([])).toBe("[]");
  t.expect(received(Object.create(null))).toBe("{}");
  t.expect(received(new Date(0))).toBe("Date");
  t.expect(received(new Map())).toBe("Map");
  t.expect(received(new (class Foo {})())).toBe("Foo");

  // One level only: a nested value names its type instead of recursing, and an
  // array keeps its length because against a tuple that is the diagnostic.
  t.expect(received({ a: 1, meta: { z: 9 } })).toBe("{ a: 1; meta: object; }");
  t.expect(received({ a: 1, tags: [1, 2, 3] })).toBe("{ a: 1; tags: Array(3); }");
  t.expect(received([[1, 2], { a: 1 }])).toBe("[Array(2), object]");

  // Anything without a useful constructor name is lowercase `object`, the same
  // way a primitive is named by its type — a plain object, a null prototype and
  // an anonymous class all read alike, and none of them read as `Object`.
  t.expect(received({ a: Object.create(null) })).toBe("{ a: object; }");
  t.expect(received({ a: new (class {})() })).toBe("{ a: object; }");

  // Width is capped too, or one wide input still produces a huge message.
  t.expect(received(Object.fromEntries(Array.from({ length: 40 }, (_, i) => [i, i])))).toBe(
    "{ 0: 0; 1: 1; 2: 2; 3: 3; 4: 4; ... }",
  );
  t.expect(received([1, 2, 3, 4, 5, 6, 7, 8])).toBe("[1, 2, 3, 4, 5, ...]");
});

// There is no `nan` case in inputExpression: the sole nan schema always carries
// `const: NaN`, so the `const` branch renders it — via stringify, to the same
// string. Pinned here because removing that branch is only safe while this holds.
test("A nan schema renders as NaN without a dedicated branch", (t) => {
  t.expect(S.inputExpression(S.schema(NaN))).toBe("NaN");
  t.expect(`${S.schema(NaN)}`).toBe("Schema<NaN>");
});

// A literal length on an array pins arity in the type (specs/array-length,
// specs/array-empty, specs/string-empty pin the direct cases). Pinned here are
// the fallbacks a spec can't express: a non-literal bound narrows nothing, and
// past 64 the tuple spelling bails to the unbounded type instead of hitting
// TS's recursion ceiling — both must stay `string[]`, not become errors.
test("Array length type pinning falls back to the unbounded type", () => {
  expectSchemaType(S.array(S.string).with(S.length, 2)).toBe<[string, string]>();
  expectSchemaType(S.length(S.array(S.boolean), 3)).toBe<[boolean, boolean, boolean]>();
  expectSchemaType(S.array(S.number).with(S.length, 0)).toBe<[]>();
  expectSchemaType(S.string.with(S.length, 0)).toBe<"">();
  // length picks up an earlier bound's subsumption unchanged
  expectSchemaType(S.array(S.string).with(S.minLength, 1).with(S.length, 2)).toBe<
    [string, string]
  >();
  // On an already-pinned arity the bound is redundant and must change nothing:
  // rebuilding a tuple from the union of its elements would widen
  // `["bar", number]` into `[number | "bar", number | "bar"]`.
  expectSchemaType(S.tuple(["bar", S.number]).with(S.length, 2)).toBe<["bar", number]>();
  expectSchemaType(S.array(S.string).with(S.length, 2).with(S.length, 2)).toBe<
    [string, string]
  >();

  const n: number = 2;
  expectSchemaType(S.array(S.string).with(S.length, n)).toBe<string[]>();
  expectSchemaType(S.array(S.string).with(S.length, 100)).toBe<string[]>();
  expectSchemaType(S.length(S.array(S.string), 1e6)).toBe<string[]>();

  // Never called, and the bound has to arrive as a parameter: `S.length` raises
  // on a bound no value can satisfy, and a `const` initialized to a literal is
  // narrowed to it, so a local would test the literal case over again.
  const _typeOnly = (union: 0 | 2) => {
    // A bound that isn't one literal resolves per member rather than letting
    // the smallest match stand for all of them.
    expectSchemaType(S.array(S.string).with(S.length, union)).toBe<[string, string] | []>();
    // A tuple built by recursing until it matched would report an unsatisfiable
    // bound as a compile error on the recursion limit, not as the runtime error
    // it already is.
    expectSchemaType(S.length(S.array(S.string), -1)).toBe<string[]>();
    expectSchemaType(S.length(S.array(S.string), 2.5)).toBe<string[]>();
  };
});

// A lower bound fixes a head and leaves the tail open. specs/array-nonEmpty and
// specs/array-minLength pin the direct cases; what needs pinning here is that it
// only has something to say while the tail *is* open — every no-op the runtime
// makes of a redundant bound the type has to make too, or the two disagree about
// a schema that compiled fine.
test("A lower bound only widens an array whose length is still open", () => {
  expectSchemaType(S.array(S.string).with(S.nonEmpty)).toBe<[string, ...string[]]>();
  expectSchemaType(S.minLength(S.array(S.number), 2)).toBe<[number, number, ...number[]]>();
  // Stacking lower bounds keeps the strictest, as the runtime does.
  expectSchemaType(S.array(S.string).with(S.minLength, 1).with(S.minLength, 3)).toBe<
    [string, string, string, ...string[]]
  >();
  // Already pinned to an arity: `narrowsSize` drops the bound outright, so the
  // type must not widen back to `[string, ...string[]]` either.
  expectSchemaType(S.array(S.string).with(S.length, 2).with(S.minLength, 1)).toBe<
    [string, string]
  >();
  // A zero lower bound is no bound at all.
  expectSchemaType(S.array(S.string).with(S.minLength, 0)).toBe<string[]>();
  expectSchemaType(S.array(S.string).with(S.minLength, 100)).toBe<string[]>();

  // TypeScript counts tuple elements, not characters, so a string keeps its type
  // under every lower bound — only the exact bound reaches `""`, the one
  // length with a literal to name it.
  expectSchemaType(S.string.with(S.nonEmpty)).toBe<string>();
  expectSchemaType(S.string.with(S.minLength, 3)).toBe<string>();
});

// The bound binds the array, and a codec's input is a different value reachable
// from it — pinning the array's arity says nothing about the string it decodes
// from, which is why the input side is rewritten only when it is the same type.
test("A length bound leaves the other side of a codec alone", () => {
  const csv = S.string.with(S.to, S.array(S.string), {
    decode: (s) => s.split(","),
    encode: (a) => a.join(","),
  });
  expectSchemaType(csv.with(S.length, 0)).toBe<string, []>();
  expectSchemaType(csv.with(S.length, 2)).toBe<string, [string, string]>();
  expectSchemaType(csv.with(S.nonEmpty)).toBe<string, [string, ...string[]]>();
  expectSchemaType(csv.with(S.minLength, 2)).toBe<string, [string, string, ...string[]]>();
});

// The runtime tags a spec cannot record: specs snapshot codegen and types,
// not the introspectable schema object itself.
test("Schema introspection tags survive on coerced and instance schemas", (t) => {
  t.expect(S.jsonString.type === "string" && S.jsonString.format === "json").toBe(true);

  const coerced = S.to(S.string, S.number);
  t.expect(coerced.to).toBe(S.number);

  const portFromString = S.string.with(S.to, S.port);
  t.expect(portFromString.type === "string" && portFromString.format === undefined).toBe(true);
  const reversedPort = S.reverse(portFromString);
  t.expect(reversedPort.type === "number" && (reversedPort as { format?: string }).format === "port").toBe(true);

  const setSchema = S.instance(Set);
  t.expect(setSchema.type === "instance" && setSchema.class === Set).toBe(true);
});
