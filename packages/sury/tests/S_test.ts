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

test("JSON string demo", (t) => {
  t.expect(S.parser(S.jsonString)("123")).toEqual("123");
  // i=>{if(typeof i!=="string"){e[1](i)}try{JSON.parse(i)}catch(t){e[0](i)}return i}

  const schemaWithTo = S.jsonString.with(S.to, S.number);
  t.expect(S.parser(schemaWithTo)("123")).toEqual(123);
  // i=>{if(typeof i!=="string"){e[2](i)}let v0;try{v0=JSON.parse(i)}catch(t){e[0](i)}if(typeof v0!=="number"||Number.isNaN(v0)){e[1](v0)}return v0}

  const schemaWithTo2 = S.number.with(S.to, S.jsonString);
  t.expect(S.decoder(schemaWithTo2)(123)).toEqual("123");
  // i=>{return ""+i}
});

test("Successfully parses string with built-in refinement", (t) => {
  const schema = S.string.with(S.length, 5);
  const result = S.safe(() => S.parser(schema)("123"));

  expectTypeOf(result).toEqualTypeOf<S.Result<string>>();

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe(
    'Expected string.length == 5, received "123"',
  );

  expectSchemaType(schema).toBe<string, string>();
  expectTypeOf(result).toEqualTypeOf<{
    readonly success: false;
    readonly error: S.Error;
  }>();
});

test("Successfully parses string with built-in refinement and custom message", (t) => {
  const schema = S.string.with(S.length, 5, "Postcode must have 5 symbols");
  const result = S.safe(() => S.parser(schema)("123"));

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe("Postcode must have 5 symbols");

  expectSchemaType(schema).toBe<string, string>();
});

test("S.pattern preserves the Input type through a transform (#282)", (t) => {
  // Regression test for https://github.com/DZakh/sury/pull/282 — pattern's
  // .d.ts declaration was missing the `schema` param, which hard-coded
  // Input to `string` and broke any schema whose Input differs from Output.
  const schema = S.number.with(S.to, S.jsonString).with(S.pattern, /^\d+$/);

  t.expect(S.decoder(schema)(123)).toEqual("123");

  expectSchemaType(schema).toBe<number, string>();
});

test("Successfully parses string with built-in transform", (t) => {
  const schema = S.trim(S.string);
  const value = S.parser(schema)("  123");

  t.expect(value).toEqual("123");

  expectSchemaType(schema).toBe<string, string>();
  expectTypeOf(value).toEqualTypeOf<string>();
});

test("Successfully parses string to Date via S.to(S.date)", (t) => {
  const schema = S.to(S.string, S.date);
  const value = S.parser(schema)("2020-01-01T00:00:00Z");

  t.expect(value).toEqual(new Date("2020-01-01T00:00:00Z"));

  expectSchemaType(schema).toBe<string, Date>();
  expectTypeOf(value).toEqualTypeOf<Date>();
});

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

test("Successfully parses string to Date with S.to", (t) => {
  const schema = S.string.with(S.to, S.date);
  const value = S.parser(schema)("2024-01-01T00:00:00.000Z");

  t.expect(value).toEqual(new Date("2024-01-01T00:00:00.000Z"));

  expectSchemaType(schema).toBe<string, Date>();
  expectTypeOf(value).toEqualTypeOf<Date>();
});

test("Successfully converts Date to string with S.to", (t) => {
  const schema = S.date.with(S.to, S.string);
  const value = S.decoder(schema)(new Date("2024-01-01T00:00:00.000Z"));

  t.expect(value).toBe("2024-01-01T00:00:00.000Z");

  expectSchemaType(schema).toBe<Date, string>();
  expectTypeOf(value).toEqualTypeOf<string>();
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

test("Successfully parses json", (t) => {
  const schema = S.json;
  const value = S.parser(schema)(true);

  t.expect(value).toEqual(true);

  expectSchemaType(schema).toBe<S.JSON, S.JSON>();
  expectTypeOf(value).toEqualTypeOf<S.JSON>();
});

test("Successfully parses invalid json without validation", (t) => {
  const schema = S.json.with(S.noValidation, true);

  let fn = S.parser(schema);

  const value = fn(undefined);
  t.expect(value).toEqual(undefined);

  t.expect(fn.name).toEqual(`noopOperation`);

  t.expect(fn([undefined])).toEqual([undefined]);

  expectSchemaType(schema).toBe<S.JSON, S.JSON>();
  expectTypeOf(value).toEqualTypeOf<S.JSON>();
});

test("Successfully parses undefined", (t) => {
  const schema = S.schema(undefined);
  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual(undefined);

  expectSchemaType(schema).toBe<undefined, undefined>();
  expectTypeOf(value).toEqualTypeOf<undefined>();
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

test("Successfully parses array", (t) => {
  const schema = S.array(S.string);
  const value = S.parser(schema)(["foo"]);

  t.expect(value).toEqual(["foo"]);

  expectSchemaType(schema).toBe<string[], string[]>();
  expectTypeOf(value).toEqualTypeOf<string[]>();
});

test("Transforms array of bigint to array of string", (t) => {
  const fn = S.decoder(S.array(S.bigint), S.array(S.string));

  t.expect(fn.toString()).toEqual(
    `i=>{let v2=new Array(i.length);for(let v1=0;v1<i.length;++v1){v2[v1]=""+i[v1]}return v2}`,
  );
  t.expect(fn([123n])).toEqual(["123"]);
});

test("Successfully parses array with min and max refinements", (t) => {
  const schema = S.array(S.string).with(S.minLength, 1).with(S.maxLength, 2);
  const value = S.parser(schema)(["foo"]);
  t.expect(value).toEqual(["foo"]);

  const result = S.safe(() => S.parser(schema)([]));
  t.expect(result.error?.message).toEqual("Expected 1 <= string[].length <= 2, received []");

  expectSchemaType(schema).toBe<string[], string[]>();
  expectTypeOf(value).toEqualTypeOf<string[]>();
});

test("Successfully parses record", (t) => {
  const schema = S.record(S.string);
  const value = S.parser(schema)({ foo: "bar" });

  t.expect(value).toEqual({ foo: "bar" });

  expectSchemaType(schema).toBe<Record<string, string>>();
  expectTypeOf(value).toEqualTypeOf<Record<string, string>>();
});

test("Successfully parses JSON string", (t) => {
  const schema = S.jsonString.with(S.to, S.boolean);
  const value = S.parser(schema)(`true`);

  t.expect(value).toEqual(true);
  t.expect(schema.type === "string" && schema.format === "json").toEqual(true);

  expectSchemaType(schema).toBe<string, boolean>();
  expectTypeOf(value).toEqualTypeOf<boolean>();
});

test("Parse JSON string, extract a field, and serialize it back to JSON string", (t) => {
  const schema = S.jsonString
    .with(
      S.to,
      S.schema({
        type: "info",
        value: S.number,
      }).with(S.shape, (msg) => msg.value),
    )
    .with(S.to, S.jsonString);

  t.expect(S.parser(schema)(`{"type": "info", "value": 123}`)).toEqual("123");
  t.expect(() => S.parser(schema)(`{"type": "info", "value": "123"}`)).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at ["value"]: Expected number, received "123"`,
    }),
  );

  t.expect(S.encoder(schema)("123")).toEqual(`{"type":"info","value":123}`);

  expectSchemaType(schema).toBe<string, string>();
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

test("Successfully serialized JSON object", (t) => {
  const objectSchema = S.schema({ foo: [1, S.number] });
  const schema = S.jsonString.with(S.to, objectSchema);
  const schemaWithSpace = S.jsonStringWithSpace(2).with(S.to, objectSchema);

  const value = S.encoder(schema)({ foo: [1, 2] });
  t.expect(value).toEqual('{"foo":[1,2]}');

  const valueWithSpace = S.encoder(schemaWithSpace)({ foo: [1, 2] });
  t.expect(valueWithSpace).toEqual('{\n  "foo": [\n    1,\n    2\n  ]\n}');

  expectSchemaType(schema).toBe<string, { foo: [1, number] }>();
  expectSchemaType(schema).toBe<
    S.Input<typeof schemaWithSpace>,
    S.Output<typeof schemaWithSpace>
  >();
  expectTypeOf(value).toEqualTypeOf<string>();
});

test("Successfully parses optional string", (t) => {
  const schema = S.optional(S.string);
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(undefined);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);

  expectTypeOf(schema).toEqualTypeOf<
    S.Schema<string | undefined, string | undefined>
  >();
  expectTypeOf(value1).toEqualTypeOf<string | undefined>();
  expectTypeOf(value2).toEqualTypeOf<string | undefined>();
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

test("Successfully parses schema wrapped in optional multiple times", (t) => {
  const schema = S.optional(S.optional(S.optional(S.string)));
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(undefined);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);

  expectTypeOf(schema).toEqualTypeOf<
    S.Schema<string | undefined, string | undefined>
  >();
  expectTypeOf(value1).toEqualTypeOf<string | undefined>();
  expectTypeOf(value2).toEqualTypeOf<string | undefined>();
});

test("Successfully parses nullable string", (t) => {
  const schema = S.nullable(S.string);
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(null);

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string | null, string | null>>();
  expectTypeOf(value1).toEqualTypeOf<string | null>();
});

test("Successfully parses nullable of array with default", (t) => {
  const schema = S.nullable(S.array(S.string), []);
  const value1 = S.parser(schema)(["foo"]);
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual(["foo"]);
  t.expect(value2).toEqual([]);

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string[] | null, string[]>>();
  expectTypeOf(value1).toEqualTypeOf<string[]>();
});

test("Successfully parses nullable string with default", (t) => {
  const schema = S.nullable(S.string, "bar");

  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual("bar");

  t.expect(() => S.parser(schema)(undefined)).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected string | null, received undefined",
    }),
  );

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string | null, string>>();
  expectTypeOf(value1).toEqualTypeOf<string>();
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

test("Successfully parses nullish string", (t) => {
  const schema = S.nullish(S.string);
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(undefined);
  const value3 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);
  t.expect(value3).toEqual(null);

  expectTypeOf(schema).toEqualTypeOf<
    S.Schema<string | undefined | null, string | undefined | null>
  >();
  expectTypeOf(value1).toEqualTypeOf<string | undefined | null>();
});

test("Successfully parses schema wrapped in nullable multiple times", (t) => {
  const nullable = S.nullable(S.string);
  const schema = S.nullable(S.nullable(nullable));
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  // TODO: Test that it should flatten nested nullable schemas

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(null);

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string | null, string | null>>();
  expectTypeOf(value1).toEqualTypeOf<string | null>();
  expectTypeOf(value2).toEqualTypeOf<string | null>();
});

test("Fails to parse with invalid data", (t) => {
  const schema = S.string;

  t.expect(() => {
    S.parser(schema)(123);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected string, received 123",
    }),
  );
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

test("Successfully reverse converts with valid value", (t) => {
  const schema = S.string;
  const result = S.encoder(schema)("123");

  t.expect(result).toEqual("123");

  expectTypeOf(result).toEqualTypeOf<string>();
});

test("Successfully reverse converts to Json with valid value", (t) => {
  const schema = S.string;
  const result = S.encoder(schema, S.json)("123");

  t.expect(result).toEqual("123");

  expectTypeOf(result).toEqualTypeOf<S.JSON>();
});

test("Successfully reverse converts to Json string with valid value", (t) => {
  const result = S.encoder(S.int32, S.jsonString)(123);

  t.expect(result).toEqual(`123`);

  expectTypeOf(result).toEqualTypeOf<string>();
});

test("Fails to serialize never", (t) => {
  const schema = S.never;

  t.expect(() => {
    S.encoder(schema)("123" as never);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected never, received "123"`,
    }),
  );
});

test("Successfully parses with transform to another type", (t) => {
  const schema = S.string.with(S.to, S.number, (string) => Number(string));
  const value = S.parser(schema)("123");

  t.expect(value).toEqual(123);

  expectTypeOf(value).toEqualTypeOf<number>();
});

test("Handles errors during custom encoding", (t) => {
  const schema = S.string.with(S.to, S.number, undefined, (number) => {
    if (number < 100) {
      throw new Error("Number is too small");
    }
    return number.toString();
  });

  const output = S.parser(schema)("80");
  t.expect(output).toEqual(80);

  t.expect(() => {
    S.encoder(schema)(output);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Number is too small",
    }),
  );
});

test("Fails to parse with transform with user error", (t) => {
  const schema = S.string.with(S.to, S.number, (string) => {
    const number = Number(string);
    if (Number.isNaN(number)) {
      throw new Error("Invalid number");
    }
    return number;
  });
  const value = S.parser(schema)("123");
  t.expect(value).toEqual(123);
  expectTypeOf(value).toEqualTypeOf<number>();

  t.expect(() => {
    S.parser(schema)("asdf");
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Invalid number",
    }),
  );
});

test("Successfully converts reversed schema with transform to another type", (t) => {
  const schema = S.string.with(S.to, S.number, undefined, (number) => {
    expectTypeOf(number).toEqualTypeOf<number>();
    return number.toString();
  });
  const result = S.encoder(schema)(123);

  t.expect(result).toEqual("123");

  expectTypeOf(result).toEqualTypeOf<string>();
});

test("Successfully parses with refine", (t) => {
  const schema = S.string.with(S.refine, (string) => {
    expectTypeOf(string).toEqualTypeOf<string>();
    return true;
  });
  const value = S.parser(schema)("123");

  t.expect(value).toEqual("123");

  expectTypeOf(value).toEqualTypeOf<string>();
});

test("Successfully reverse converts with refine", (t) => {
  const schema = S.string.with(S.refine, (string) => {
    expectTypeOf(string).toEqualTypeOf<string>();
    return true;
  });
  const result = S.encoder(schema)("123");

  t.expect(result).toEqual("123");

  expectTypeOf(result).toEqualTypeOf<string>();
});

test("Fails to parses with refine raising an error", (t) => {
  const schema = S.string.with(S.refine, () => false, {
    error: "User error",
  });

  t.expect(() => {
    S.parser(schema)("123");
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "User error",
    }),
  );
});

test("Fails to parse with refine with path option", (t) => {
  const schema = S.string.with(S.refine, () => false, {
    error: "User error",
    path: ["data", "field"],
  });

  t.expect(() => {
    S.parser(schema)("123");
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at ["data"]["field"]: User error`,
    }),
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

test("Successfully parses object by provided shape", (t) => {
  const schema = S.schema({
    foo: S.string,
    bar: S.boolean,
  });
  const value = S.parser(schema)({
    foo: "bar",
    bar: true,
  });

  t.expect(value).toEqual({
    foo: "bar",
    bar: true,
  });

  expectSchemaType(schema).toBe<
    { foo: string; bar: boolean },
    { foo: string; bar: boolean }
  >();
  expectTypeOf(value).toEqualTypeOf<{ foo: string; bar: boolean }>();
});

test("Successfully parses object with quoted keys", (t) => {
  const schema = S.schema({
    [`"`]: S.string,
    [`'`]: S.string,
    ["`"]: S.string,
  });
  const value = S.parser(schema)({
    '"': '"',
    "'": "'",
    "`": "`",
  });

  t.expect(value).toEqual({
    '"': '"',
    "'": "'",
    "`": "`",
  });

  expectSchemaType(schema).toBe<{ '"': string; "'": string; "`": string }>();
});

test("Successfully parses tagged object", (t) => {
  const schema = S.schema({
    tag: "block" as const,
    bar: S.boolean,
  });
  const value = S.parser(schema)({
    tag: "block",
    bar: true,
  });

  t.expect(value).toEqual({
    tag: "block",
    bar: true,
  });

  expectSchemaType(schema).toBe<
    { tag: "block"; bar: boolean },
    { tag: "block"; bar: boolean }
  >();
  expectTypeOf(value).toEqualTypeOf<{ tag: "block"; bar: boolean }>();
});

test("Successfully parses and reverse convert object with optional field", (t) => {
  const schema = S.schema({
    bar: S.optional(S.boolean),
    baz: S.boolean,
  });
  const value = S.parser(schema)({ baz: true });
  t.expect(value).toEqual({ bar: undefined, baz: true });

  const reversed = S.encoder(schema)({ baz: true });
  t.expect(reversed).toEqual({ baz: true });

  expectSchemaType(schema).toBe<
    { bar?: boolean | undefined; baz: boolean },
    { bar?: boolean | undefined; baz: boolean }
  >();
});

test("Successfully parses object with field names transform", (t) => {
  const schema = S.object((s) => ({
    foo: s.field("Foo", S.string),
    bar: s.field("Bar", S.boolean),
  }));
  const value = S.parser(schema)({
    Foo: "bar",
    Bar: true,
  });

  t.expect(value).toEqual({
    foo: "bar",
    bar: true,
  });

  expectSchemaType(schema).toBe<
    Record<string, unknown>,
    { foo: string; bar: boolean }
  >();
  expectTypeOf(value).toEqualTypeOf<{ foo: string; bar: boolean }>();
});

test("Successfully parses advanced object with all features", (t) => {
  const schema = S.object((s) => {
    s.tag("type", 0);
    return {
      nested: s.nested("nested").field("field", S.number),
      flattened: s.flatten(S.schema({ id: S.string })),
      foo: s.field("Foo", S.string),
      bar: s.fieldOr("Bar", S.boolean, true),
    };
  });

  const value = S.parser(schema)({
    nested: {
      field: 123,
    },
    type: 0,
    id: "id",
    Foo: "bar",
  });

  t.expect(value).toEqual({
    nested: 123,
    flattened: { id: "id" },
    foo: "bar",
    bar: true,
  });

  expectSchemaType(schema).toBe<
    Record<string, unknown>,
    { nested: number; flattened: { id: string }; foo: string; bar: boolean }
  >();
});

test("Successfully parses object with transformed field", (t) => {
  const schema = S.schema({
    foo: S.string.with(S.to, S.number, (string) => Number(string)),
    bar: S.boolean,
  });
  const value = S.parser(schema)({
    foo: "123",
    bar: true,
  });

  t.expect(value).toEqual({
    foo: 123,
    bar: true,
  });

  expectSchemaType(schema).toBe<
    { foo: string; bar: boolean },
    { foo: number; bar: boolean }
  >();
  expectTypeOf(value).toEqualTypeOf<{ foo: number; bar: boolean }>();
});

test("Fails to parse strict object with exccess fields", (t) => {
  const schema = S.schema({
    foo: S.string,
  }).with(S.strict);

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

test("Fails to parse deep strict object with exccess fields", (t) => {
  const schema = S.schema({
    foo: {
      a: S.string,
    },
  }).with(S.deepStrict);

  t.expect(() => {
    const value = S.parser(schema)({
      foo: {
        a: "bar",
        b: true,
      },
    });
    expectSchemaType(schema).toBe<{ foo: { a: string } }>();
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at ["foo"]: Unrecognized key "b"`,
    }),
  );
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

test("Resets object strict mode with strip method", (t) => {
  const schema = S.strip(
    S.strict(
      S.schema({
        foo: S.string,
      }),
    ),
  );

  const value = S.parser(schema)({
    foo: "bar",
    bar: true,
  });

  t.expect(value).toEqual({ foo: "bar" });

  expectSchemaType(schema).toBe<{ foo: string }, { foo: string }>();
  expectTypeOf(value).toEqualTypeOf<{ foo: string }>();
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
      // TODO: Can theoretically support this case
      message: `[Sury] The merge supports only structured object schemas without transformations`,
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

test("Merge overwrites the left fields by schema from the right", (t) => {
  const baseSchema = S.schema({
    type: S.union(["foo", "bar"]),
    name: S.string,
  });

  const fooSchema = S.merge(
    baseSchema,
    S.schema({
      type: "foo" as const,
      fooCount: S.number,
    }),
  );

  const value = S.parser(fooSchema)({
    type: "foo",
    name: "foo",
    fooCount: 123,
  });

  expectSchemaType(fooSchema).toBe<
    { type: "foo"; name: string; fooCount: number },
    { type: "foo"; name: string; fooCount: number }
  >();

  t.expect(value).toEqual({
    type: "foo",
    name: "foo",
    fooCount: 123,
  });

  t.expect(() =>
    S.parser(fooSchema)({
      type: "bar",
      name: "foo",
      fooCount: 123,
    }),
  ).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at ["type"]: Expected "foo", received "bar"`,
    }),
  );
});

test("Name of merge schema", (t) => {
  const schema = S.merge(
    S.schema({
      foo: S.string,
      bar: S.boolean,
    }),
    S.schema({
      baz: S.string,
    }),
  );

  t.expect(S.inputExpression(schema)).toBe(
    `{ foo: string; bar: boolean; baz: string; }`,
  );
});

test("Successfully parses object using S.schema", (t) => {
  const schema = S.schema({
    foo: S.string,
    bar: S.boolean,
  });
  const value = S.parser(schema)({
    foo: "bar",
    bar: true,
  });

  t.expect(value).toEqual({
    foo: "bar",
    bar: true,
  });

  expectSchemaType(schema).toBe<{ foo: string; bar: boolean }>();
  expectTypeOf(value).toEqualTypeOf<{ foo: string; bar: boolean }>();
});

test("Successfully parses tuple using S.schema", (t) => {
  const schema = S.schema([S.string, S.boolean] as const);
  const value = S.parser(schema)(["bar", true]);

  t.expect(value).toEqual(["bar", true]);

  expectSchemaType(schema).toBe<[string, boolean]>();
  expectTypeOf(value).toEqualTypeOf<[string, boolean]>();
});

test("Successfully parses primitive schema passed to S.schema", (t) => {
  const schema = S.schema(S.string);
  const value = S.parser(schema)("bar");

  t.expect(value).toEqual("bar");

  expectSchemaType(schema).toBe<string, string>();
  expectTypeOf(value).toEqualTypeOf<string>();
});

test("Successfully parses literal using S.schema with as cost", (t) => {
  const schema = S.schema("foo" as const);

  const value = S.parser(schema)("foo");

  t.expect(value).toEqual("foo");

  expectSchemaType(schema).toBe<"foo">();
  expectTypeOf(value).toEqualTypeOf<"foo">();
});

test("Successfully parses nested object using S.schema", (t) => {
  const schema = S.schema({
    foo: {
      bar: S.number,
    },
  });
  const value = S.parser(schema)({
    foo: { bar: 123 },
  });

  t.expect(value).toEqual({
    foo: { bar: 123 },
  });

  expectSchemaType(schema).toBe<{ foo: { bar: number } }>();
  expectTypeOf(value).toEqualTypeOf<{ foo: { bar: number } }>();
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

test("S.schema example", (t) => {
  type Shape =
    | { kind: "circle"; radius: number }
    | { kind: "square"; x: number };

  let circleSchema: S.Schema<Shape, Shape> = S.schema({
    kind: "circle",
    radius: S.number,
  });

  const value = S.parser(circleSchema)({
    kind: "circle",
    radius: 123,
  });

  t.expect(value).toEqual({
    kind: "circle",
    radius: 123,
  });

  expectTypeOf(circleSchema).toEqualTypeOf<S.Schema<Shape, Shape>>();
  expectTypeOf(value).toEqualTypeOf<Shape>();
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

test("Successfully parses union", (t) => {
  const schema = S.union([S.string, S.number]);
  const value = S.safe(() => S.parser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  expectSchemaType(schema).toBe<string | number>();
});

test("Successfully parses union of literals", (t) => {
  const schema = S.union(["foo", 123, true]);
  const value = S.safe(() => S.parser(schema)("foo"));

  t.expect(value).toEqual({ success: true, value: "foo" });

  expectSchemaType(schema).toBe<"foo" | 123 | true>();
});

test("Shape union", (t) => {
  const shapeSchema = S.union([
    {
      kind: "circle" as const,
      radius: S.number,
    },
    {
      kind: "square" as const,
      x: S.number,
    },
    {
      kind: "triangle" as const,
      x: S.number,
      y: S.number,
    },
  ]);
  const value = S.parser(shapeSchema)({
    kind: "circle",
    radius: 123,
  });

  t.expect(value).toEqual({
    kind: "circle",
    radius: 123,
  });

  expectTypeOf(shapeSchema).toEqualTypeOf<
    S.Schema<
      | { kind: "circle"; radius: number }
      | { kind: "square"; x: number }
      | { kind: "triangle"; x: number; y: number },
      | { kind: "circle"; radius: number }
      | { kind: "square"; x: number }
      | { kind: "triangle"; x: number; y: number }
    >
  >();
});

test("Successfully parses union with transformed items", (t) => {
  const schema = S.union([
    S.string.with(S.to, S.number, (string) => Number(string)),
    S.number,
  ]);
  const value = S.safe(() => S.parser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: 123 });

  expectSchemaType(schema).toBe<string | number, number>();
});

test("Correctly infers type", (t) => {
  const schema = S.string.with(S.to, S.number, Number);
  expectSchemaType(schema).toBe<string, number>();
  expectTypeOf<S.Input<typeof schema>>().toEqualTypeOf<string>();
  expectTypeOf<S.Output<typeof schema>>().toEqualTypeOf<number>();
});

test("Successfully parses undefined using the default value", (t) => {
  const schema = S.string.with(S.optional, "foo");

  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual("foo");
  t.expect(schema.default).toEqual("foo");

  expectTypeOf(schema.default).toEqualTypeOf<string | undefined>();
  expectSchemaType(schema).toBe<string | undefined, string>();
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

test("Tuple with single element", (t) => {
  const schema = S.schema([S.string.with(S.to, S.number, (s) => Number(s))]);

  t.expect(S.parser(schema)(["123"])).toEqual([123]);

  expectSchemaType(schema).toBe<[string], [number]>();
});

test("Tuple with multiple elements", (t) => {
  const schema = S.schema([S.string, S.number, true]);

  t.expect(S.parser(schema)(["123", 123, true])).toEqual(["123", 123, true]);

  expectSchemaType(schema).toBe<[string, number, true]>();
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
  const schema = S.schema({ a: S.string.with(S.to, S.number, Number, String) });

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

test("Env schema: Reggression version", (t) => {
  const env = <T>(schema: S.Schema<unknown, T>): S.Schema<string, T> => {
    if (schema.type === "boolean") {
      return S.union([
        S.schema("t").with(S.to, S.schema(true)).with(S.to, schema),
        S.schema("1").with(S.to, S.schema(true)).with(S.to, schema),
        S.schema("f").with(S.to, S.schema(false)).with(S.to, schema),
        S.schema("0").with(S.to, S.schema(false)).with(S.to, schema),
        S.string.with(S.to, schema),
      ]);
    } else if (
      schema.type === "number" ||
      schema.type === "bigint" ||
      schema.type === "string"
    ) {
      return S.string.with(S.to, schema);
    } else {
      return S.jsonString.with(S.to, schema);
    }
  };

  t.expect(S.parser(env(S.boolean)).toString()).toEqual(
    `i=>{for(;;){if(typeof i==="string"&&i==="t"){i=true;break}if(typeof i==="string"&&i==="1"){i=true;break}if(typeof i==="string"&&i==="f"){i=false;break}if(typeof i==="string"&&i==="0"){i=false;break}if(typeof i==="string"){let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0;break}e[1](i)}return i}`,
  );

  t.expect(S.parser(env(S.boolean))("t")).toEqual(true);
  t.expect(S.parser(env(S.boolean))("true")).toEqual(true);
});

test("CompactColumns schema", (t) => {
  const schema = S.to(
    S.compactColumns(S.unknown),
    S.array(
      S.schema({
        id: S.string,
        name: S.nullable(S.string),
        deleted: S.boolean,
      }),
    ),
  );

  // Test parsing columnar data to row objects
  const parse = S.parser(schema);
  const parsed = parse([
    ["0", "1"],
    ["Hello", null],
    [false, true],
  ] as unknown[][]);
  t.expect(parsed).toEqual([
    { id: "0", name: "Hello", deleted: false },
    { id: "1", name: null, deleted: true },
  ]);

  // Test encoding row objects back to columnar data
  const encode = S.encoder(schema);
  const encoded = encode([
    { id: "0", name: "Hello", deleted: false },
    { id: "1", name: null, deleted: true },
  ]);
  t.expect(encoded).toEqual([
    ["0", "1"],
    ["Hello", null],
    [false, true],
  ]);
});

test("CompactColumns with json and bigint", (t) => {
  const schema = S.to(
    S.compactColumns(S.json),
    S.array(
      S.schema({
        id: S.string,
        amount: S.bigint,
      }),
    ),
  );

  // Test parsing - json strings are converted to bigint via BigInt()
  const parse = S.parser(schema);
  const parsed = parse([
    ["0", "1"],
    ["12345678901234567890", "98765432109876543210"],
  ]);
  t.expect(parsed).toEqual([
    { id: "0", amount: 12345678901234567890n },
    { id: "1", amount: 98765432109876543210n },
  ]);

  // Test encoding - bigint values are converted back to strings for json
  const encode = S.encoder(schema);
  const encoded = encode([
    { id: "0", amount: 12345678901234567890n },
    { id: "1", amount: 98765432109876543210n },
  ]);
  t.expect(encoded).toEqual([
    ["0", "1"],
    ["12345678901234567890", "98765432109876543210"],
  ]);
});

test("Set schema", (t) => {
  const schema = S.instance(Set);

  expectSchemaType(schema).toBe<Set<unknown>, Set<unknown>>();
  if (schema.type === "instance") {
    expectTypeOf(schema.class).toEqualTypeOf<S.Class<Set<unknown>>>();
    t.expect(schema.class).toBe(Set);
  }

  const parser = S.parser(schema);
  expectTypeOf(parser).toEqualTypeOf<(input: unknown) => Set<unknown>>();

  t.expect(parser.toString()).toBe("i=>{i instanceof e[0]||e[1](i);return i}");

  const data = new Set(["foo", "bar"]);
  t.expect(parser(data)).toBe(data);

  t.expect(() => parser(123)).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected Set, received 123",
    }),
  );
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

test("Coerce string to number", (t) => {
  const schema = S.to(S.string, S.number);

  t.expect(schema.to).toBe(S.number);

  expectSchemaType(schema).toBe<string, number>();
  expectTypeOf(schema.to).toEqualTypeOf<S.Schema<unknown> | undefined>();

  t.expect(S.parser(schema)("123")).toEqual(123);
  t.expect(S.parser(schema)("123.4")).toEqual(123.4);
  t.expect(S.encoder(schema)(123)).toEqual("123");
});

test("Shape string to object", (t) => {
  const schema = S.shape(S.string, (string) => ({ foo: string }));

  t.expect(S.parser(schema)("bar")).toEqual({ foo: "bar" });
  t.expect(S.encoder(schema)({ foo: "bar" })).toEqual("bar");
});

test("Tuple with transform to object", (t) => {
  let pointSchema = S.tuple((s) => {
    s.tag(0, "point");
    return {
      x: s.item(1, S.int32),
      y: s.item(2, S.int32),
    };
  });

  t.expect(S.parser(pointSchema)(["point", 1, -4])).toEqual({ x: 1, y: -4 });

  expectSchemaType(pointSchema).toBe<unknown[], { x: number; y: number }>();
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

test("Port schema", (t) => {
  const portSchema = S.port;
  if (portSchema.type === "number") {
    t.expect(portSchema.format).toEqual("port");
  } else {
    t.expect.fail("portSchema should be a number");
  }

  expectSchemaType(portSchema).toBe<number, number>();

  t.expect(() => {
    S.parser(portSchema)(10.2);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected port, received 10.2",
    }),
  );

  const portCoercedFromString = S.string.with(S.to, S.port);
  expectSchemaType(portCoercedFromString).toBe<string, number>();

  if (portCoercedFromString.type === "string") {
    t.expect(portCoercedFromString.format).toEqual(undefined);
  } else {
    t.expect.fail("portCoercedFromString should be a string");
  }

  if (S.reverse(portCoercedFromString).type === "number") {
    t.expect(S.parser(portCoercedFromString)("10")).toEqual(10);
    t.expect(() => {
      S.parser(portCoercedFromString)(10.2);
    }).toThrow(
      t.expect.objectContaining({
        name: "SuryError",
        message: "Expected string, received 10.2",
      }),
    );
    t.expect(() => {
      S.parser(portCoercedFromString)("10.2");
    }).toThrow(
      t.expect.objectContaining({
        name: "SuryError",
        message: "Expected port, received 10.2",
      }),
    );
    t.expect(S.encoder(portCoercedFromString)(10)).toEqual("10");
  } else {
    t.expect.fail("portCoercedFromString should be a number");
  }
});

test("Example", (t) => {
  // Create login schema with email and password
  const loginSchema = S.schema({
    email: S.email,
    password: S.string.with(S.minLength, 8),
  });

  // Infer output TypeScript type of login schema
  type LoginData = S.Output<typeof loginSchema>; // { email: string; password: string }

  t.expect(() => {
    // Throws the S.Error(`Failed at ["email"]: Expected email, received ""`)
    S.parser(loginSchema)({ email: "", password: "" });
  }).toThrow(
    t.expect.objectContaining({
      message: `Failed at ["email"]: Expected email, received ""`,
    }),
  );

  // Returns data as { email: string; password: string }
  const result = S.parser(loginSchema)({
    email: "jane@example.com",
    password: "12345678",
  });

  t.expect(result).toEqual({
    email: "jane@example.com",
    password: "12345678",
  });

  expectSchemaType(loginSchema).toBe<
    { email: string; password: string },
    { email: string; password: string }
  >();
  expectTypeOf<LoginData>().toEqualTypeOf<{
    email: string;
    password: string;
  }>();
});

test("Decode from json", async (t) => {
  t.expect(S.decoder(S.json, S.array(S.bigint))(["123"])).toEqual([123n]);
  t.expect(S.decoder(S.array(S.bigint), S.json)([123n])).toEqual(["123"]);

  const schema = S.string.with(S.nullable);

  t.expect(S.decoder(S.json, schema)("hello")).toEqual("hello");
  t.expect(S.decoder(S.json, schema)(null)).toEqual(null);

  // Date fields should be encoded to ISO string when decoding to JSON
  const dateSchema = S.schema({ field: S.date });
  const dateToJson = S.decoder(dateSchema, S.json);
  t.expect(dateToJson({ field: new Date("2024-01-01T00:00:00.000Z") })).toEqual(
    {
      field: "2024-01-01T00:00:00.000Z",
    },
  );
  t.expect(dateToJson.toString()).toEqual(
    `i=>{return {"field":i["field"].toISOString(),}}`,
  );

  // Date fields should work through the full jsonString pipeline
  const dateToJsonString = S.decoder(dateSchema, S.jsonString);
  t.expect(
    dateToJsonString({ field: new Date("2024-01-01T00:00:00.000Z") }),
  ).toEqual(`{"field":"2024-01-01T00:00:00.000Z"}`);

  // JSON to Date: decode ISO string from JSON back to Date
  const jsonToDate = S.decoder(S.json, dateSchema);
  t.expect(jsonToDate({ field: "2024-01-01T00:00:00.000Z" })).toEqual({
    field: new Date("2024-01-01T00:00:00.000Z"),
  });
  t.expect(jsonToDate.toString()).toEqual(
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[2](i);let v1=i["field"];typeof v1==="string"||e[1](v1);let v0=new Date(i["field"]);!Number.isNaN(v0.getTime())||e[0](v0);return {"field":v0,}}`,
  );

  // JSON string to Date: full round-trip through jsonString
  const jsonStringToDate = S.decoder(S.jsonString, dateSchema);
  t.expect(jsonStringToDate(`{"field":"2024-01-01T00:00:00.000Z"}`)).toEqual({
    field: new Date("2024-01-01T00:00:00.000Z"),
  });
});

test("Decode from json string", async (t) => {
  const schema = S.nullable(S.string);

  t.expect(S.decoder(S.jsonString, schema)(`"hello"`)).toEqual("hello");
  t.expect(S.decoder(S.jsonString, schema)("null")).toEqual(null);
});

test("Decode from json string, convert to number", async (t) => {
  const fn = S.decoder(S.jsonString, S.string, S.number);

  expectTypeOf(fn).toEqualTypeOf<(data: string) => number>();

  t.expect(fn(`"123"`)).toEqual(123);
});

test("Decode from json string to array of bigints", async (t) => {
  const fn = S.decoder(S.jsonString, S.array(S.bigint));

  expectTypeOf(fn).toEqualTypeOf<(data: string) => bigint[]>();

  t.expect(fn(`["123"]`)).toEqual([123n]);
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
  t.expect(
    S.parser(fromJsonSchema)({ USER_ID: "0", USER_NAME: "Dmitry" }),
  ).toEqual({
    USER_ID: "0",
    USER_NAME: "Dmitry",
  });
  if (fromJsonSchema.type === "object") {
    t.expect(fromJsonSchema.additionalItems).toBe("strip");
    t.expect(Object.keys(fromJsonSchema.properties)).toEqual([
      "USER_ID",
      "USER_NAME",
    ]);
  } else {
    t.expect.fail("fromJsonSchema should be an object");
  }
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
  const emailSchema = S.fromJSONSchema<string>({
    type: "string",
    format: "email",
  });
  expectSchemaType(emailSchema).toBe<S.JSON, string>();
  const result = S.safe(() => S.assert(emailSchema, "example.com"));

  t.expect(result.error?.message).toBe(
    `Expected email, received "example.com"`,
  );
});

test("fromJSONSchema: assertion keywords bind without an explicit `type`", (t) => {
  const parse = (js: object) => S.parser(S.fromJSONSchema(js as never)) as (d: unknown) => unknown;

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
});

test("fromJSONSchema: composition keywords constrain in addition to the base shape", (t) => {
  const schema = S.fromJSONSchema({
    type: "object",
    properties: { bar: { type: "integer" } },
    required: ["bar"],
    allOf: [{ properties: { foo: { type: "string" } }, required: ["foo"] }],
  } as never);
  const parse = S.parser(schema) as (d: unknown) => unknown;

  // The allOf branch sees the whole document, while the base object schema
  // still strips what it doesn't declare — so `foo` validates, then drops.
  t.expect(parse({ bar: 2, foo: "x" })).toEqual({ bar: 2 });
  // Fails the base shape.
  t.expect(S.safe(() => parse({ bar: "no", foo: "x" })).error).toBeDefined();
  // Fails only the allOf branch — the base shape alone used to win.
  t.expect(S.safe(() => parse({ bar: 2 })).error).toBeDefined();
});

test("fromJSONSchema: oneOf counts matches, `not` and if/then/else layer on", (t) => {
  const one = S.parser(
    S.fromJSONSchema({ oneOf: [{ type: "number" }, { type: "string" }] } as never),
  ) as (d: unknown) => unknown;
  t.expect(one(1)).toBe(1);
  t.expect(S.safe(() => one(true)).error).toBeDefined();

  const not = S.parser(S.fromJSONSchema({ not: { type: "string" } } as never)) as (
    d: unknown,
  ) => unknown;
  t.expect(not(1)).toBe(1);
  t.expect(S.safe(() => not("x")).error).toBeDefined();

  // `then`/`else` are each optional and default to "always passes".
  const ite = S.parser(
    S.fromJSONSchema({ if: { type: "number" }, then: { minimum: 5 } } as never),
  ) as (d: unknown) => unknown;
  t.expect(ite(7)).toBe(7);
  t.expect(ite("anything")).toBe("anything");
  t.expect(S.safe(() => ite(3)).error).toBeDefined();
});

test("fromJSONSchema: an unmodelled assertion keyword fails at creation", (t) => {
  // Ignoring it would widen the schema — the validator would accept data the
  // author wrote the keyword to reject — so this must not silently succeed.
  const result = S.safe(() => S.fromJSONSchema({ type: "number", multipleOf: 2 } as never));
  t.expect(result.error?.message).toContain("Unsupported JSON Schema keyword: multipleOf");

  t.expect(
    S.safe(() => S.fromJSONSchema({ type: "array", uniqueItems: true } as never)).error?.message,
  ).toContain("uniqueItems");
});

test("fromJSONSchema: exclusiveMaximum bounds the maximum, not the minimum", (t) => {
  const parse = S.parser(
    S.fromJSONSchema({ type: "integer", exclusiveMaximum: 5 } as never),
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
    S.to(
      schema,
      S.string,
      (v) => {
        if (v.startsWith(prefix)) {
          return v.slice(1);
        } else {
          throw new Error(`String must start with ${prefix}`);
        }
      },
      (v) => prefix + v,
    );

  const schema = S.schema({
    nested: {
      tag: S.string.with(stripPrefix, "_").with(S.to, S.schema("foo")),
      numberTag: S.string.with(stripPrefix, "~").with(S.to, S.schema(1)),
    },
  }).with(S.shape, (_) => undefined);

  const fn = S.encoder(schema);

  t.expect(fn.toString()).toEqual(
    `i=>{i===void 0||e[4](i);let v0;try{v0=e[0]("foo")}catch(x){e[1](x)}let v1;try{v1=e[2]("1")}catch(x){e[3](x)}return {"nested":{"tag":v0,"numberTag":v1,},}}`,
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
    `i=>{return JSON.stringify(e[1].decode(e[0].encode(i)))}`,
  );
  t.expect(S.decoder(S.unknown, S.uint8Array, S.jsonString).toString()).toEqual(
    `i=>{i instanceof e[1]||e[2](i);return JSON.stringify(e[0].decode(i))}`,
  );
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
  // `empty`/`nonEmpty` desugar to length bounds, and report as those rather
  // than naming a constructor the caller didn't write.
  t.expect(() => S.string.with(S.minLength, 2).with(S.empty)).toThrow(
    `[Sury] string.length <= 0 contradicts string.length >= 2`,
  );
  // A format's range is a bound like any other, so a value outside it conflicts.
  t.expect(() => S.int32.with(S.gte, 3000000000)).toThrow(
    `[Sury] int32 >= 3000000000 contradicts int32 <= 2147483647`,
  );
  t.expect(() => S.port.with(S.lte, -1)).toThrow(
    `[Sury] port <= -1 contradicts port >= 0`,
  );

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
});

test("An unsatisfiable JSON Schema document loads as never", (t) => {
  // Legal JSON Schema — it just describes a type nothing inhabits — so it has
  // to load rather than fail the way the hand-written equivalent does.
  for (const definition of [
    { type: "number", minimum: 5, maximum: 1 },
    { type: "integer", minimum: 5, maximum: 1 },
    { type: "number", exclusiveMinimum: 5, maximum: 5 },
    { type: "string", minLength: 5, maxLength: 1 },
    { type: "array", minItems: 5, maxItems: 1 },
  ] as const) {
    const schema = S.fromJSONSchema(definition);
    t.expect(S.inputExpression(schema)).toEqual("never");
  }
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

// FIXME: S.record takes no key schema, so keys are never validated. The
// generated loop is `for (let v0 in i)`, which skips symbol keys entirely — a
// value under one is never reached, whatever the value schema says. Lives here
// rather than in specs/record.yaml because the spec harness cannot serialize an
// object with symbol keys back to source (see CONTRIBUTING.md).
test("S.record does not validate values under symbol keys", (t) => {
  const key = Symbol.for("sury-test-symbol-key");
  const input: Record<symbol, unknown> = { [key]: 123 };

  const result = S.parser(S.record(S.string))(
    input as unknown as Record<string, string>,
  );

  // 123 is not a string, yet this neither throws nor strips the property.
  t.expect(result).toBe(input);
  t.expect((result as unknown as Record<symbol, unknown>)[key]).toBe(123);
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
  expectSchemaType(S.array(S.number).with(S.empty)).toBe<[]>();
  expectSchemaType(S.string.with(S.empty)).toBe<"">();
  // length picks up an earlier bound's subsumption unchanged
  expectSchemaType(S.array(S.string).with(S.minLength, 1).with(S.length, 2)).toBe<
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

// The bound binds the array, and a codec's input is a different value reachable
// from it — pinning the array's arity says nothing about the string it decodes
// from, which is why the input side is rewritten only when it is the same type.
test("A length bound leaves the other side of a codec alone", () => {
  const csv = S.string.with(
    S.to,
    S.array(S.string),
    (s) => s.split(","),
    (a) => a.join(","),
  );
  expectSchemaType(csv.with(S.empty)).toBe<string, []>();
  expectSchemaType(csv.with(S.length, 2)).toBe<string, [string, string]>();
});
