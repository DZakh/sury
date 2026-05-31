import { test } from "vitest";
import { expectType, TypeEqual } from "ts-expect";

import * as S from "../src/S.js";

// FIXME: S.max should be applied to output
// From https://x.com/dzakh_dev/status/1963982551208309222
// const PixelSchema = S.pattern(/^\d{1,3}px$/)
//   .with(S.to, S.number, parseInt)
//   .with(S.max, 100)
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

type SchemaEqual<
  Schema extends S.Schema<unknown, unknown>,
  Output,
  Input = Output,
> = TypeEqual<S.Output<Schema>, Output> & TypeEqual<S.Input<Schema>, Input>;

// Can use genType schema
// expectType<SchemaEqual<typeof stringSchema, string, unknown>>(true);

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

test("Successfully parses string", (t) => {
  const schema = S.string;
  const value = S.parser(schema)("123");

  t.expect(value).toEqual("123");

  expectType<SchemaEqual<typeof schema, string, string>>(true);
  expectType<TypeEqual<typeof value, string>>(true);
});

test("Successfully parses string with built-in refinement", (t) => {
  const schema = S.string.with(S.length, 5);
  const result = S.safe(() => S.parser(schema)("123"));

  expectType<TypeEqual<typeof result, S.Result<string>>>(true);

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe(
    "String must be exactly 5 characters long",
  );

  expectType<SchemaEqual<typeof schema, string, string>>(true);
  expectType<
    TypeEqual<
      typeof result,
      {
        readonly success: false;
        readonly error: S.Error;
      }
    >
  >(true);
});

test("Successfully parses string with built-in refinement and custom message", (t) => {
  const schema = S.string.with(S.length, 5, "Postcode must have 5 symbols");
  const result = S.safe(() => S.parser(schema)("123"));

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe("Postcode must have 5 symbols");

  expectType<SchemaEqual<typeof schema, string, string>>(true);
});

test("Successfully parses string with built-in transform", (t) => {
  const schema = S.trim(S.string);
  const value = S.parser(schema)("  123");

  t.expect(value).toEqual("123");

  expectType<SchemaEqual<typeof schema, string, string>>(true);
  expectType<TypeEqual<typeof value, string>>(true);
});

test("Successfully parses string to Date via S.to(S.date)", (t) => {
  const schema = S.to(S.string, S.date);
  const value = S.parser(schema)("2020-01-01T00:00:00Z");

  t.expect(value).toEqual(new Date("2020-01-01T00:00:00Z"));

  expectType<SchemaEqual<typeof schema, Date, string>>(true);
  expectType<TypeEqual<typeof value, Date>>(true);
});

test("Successfully parses string to Date with S.to", (t) => {
  const schema = S.string.with(S.to, S.date);
  const value = S.parser(schema)("2024-01-01T00:00:00.000Z");

  t.expect(value).toEqual(new Date("2024-01-01T00:00:00.000Z"));

  expectType<SchemaEqual<typeof schema, Date, string>>(true);
  expectType<TypeEqual<typeof value, Date>>(true);
});

test("Successfully converts Date to string with S.to", (t) => {
  const schema = S.date.with(S.to, S.string);
  const value = S.decoder(schema)(new Date("2024-01-01T00:00:00.000Z"));

  t.expect(value).toBe("2024-01-01T00:00:00.000Z");

  expectType<SchemaEqual<typeof schema, string, Date>>(true);
  expectType<TypeEqual<typeof value, string>>(true);
});

test("Successfully parses int", (t) => {
  const schema = S.int32;
  const value = S.parser(schema)(123);

  t.expect(value).toEqual(123);

  expectType<SchemaEqual<typeof schema, number, number>>(true);
  expectType<TypeEqual<typeof value, number>>(true);
});

test("Successfully parses float", (t) => {
  const schema = S.number;
  const value = S.parser(schema)(123.4);

  t.expect(value).toEqual(123.4);

  expectType<SchemaEqual<typeof schema, number, number>>(true);
  expectType<TypeEqual<typeof value, number>>(true);
});

test("Successfully parses BigInt", (t) => {
  const schema = S.bigint;
  const value = S.parser(schema)(123n);

  t.expect(value).toEqual(123n);

  expectType<SchemaEqual<typeof schema, bigint, bigint>>(true);
  expectType<TypeEqual<typeof value, bigint>>(true);
});

test("Successfully parses symbol", (t) => {
  const schema = S.symbol;
  const data = Symbol("foo");
  const value = S.parser(schema)(data);

  t.expect(value).toEqual(data);
  t.expect(value).not.toEqual(Symbol("foo")); // Because this is how symbols work

  expectType<SchemaEqual<typeof schema, symbol, symbol>>(true);
  expectType<TypeEqual<typeof value, symbol>>(true);
});

test("Function literal schema", (t) => {
  const fn = function () {};

  const schema = S.schema(fn);

  expectType<SchemaEqual<typeof schema, () => void, () => void>>(true);
  if (schema.type !== "function") {
    t.expect.fail("Schema should be a function");
    return;
  }
  t.expect(schema.const).toBe(fn);

  const value = S.parser(schema)(fn);

  t.expect(value).toEqual(fn);
  t.expect(value).not.toEqual(function () {});
});

test("Fails to parse float when NaN is provided", (t) => {
  const schema = S.number;

  t.expect(() => {
    const value = S.parser(schema)(NaN);

    expectType<SchemaEqual<typeof schema, number, number>>(true);
    expectType<TypeEqual<typeof value, number>>(true);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected number, received NaN",
    }),
  );
});

test("Successfully parses float when NaN is provided and NaN check disabled in global config", (t) => {
  S.global({
    disableNanNumberValidation: true,
  });
  const schema = S.number;
  const value = S.parser(schema)(NaN);
  S.global({});

  t.expect(value).toEqual(NaN);

  expectType<SchemaEqual<typeof schema, number, number>>(true);
  expectType<TypeEqual<typeof value, number>>(true);
});

test("Successfully parses bool", (t) => {
  const schema = S.boolean;
  const value = S.parser(schema)(true);

  t.expect(value).toEqual(true);

  expectType<SchemaEqual<typeof schema, boolean, boolean>>(true);
  expectType<TypeEqual<typeof value, boolean>>(true);
});

test("Successfully parses unknown", (t) => {
  const schema = S.unknown;
  const value = S.parser(schema)(true);

  t.expect(value).toEqual(true);

  expectType<SchemaEqual<typeof schema, unknown, unknown>>(true);
  expectType<TypeEqual<typeof value, unknown>>(true);
});

test("Successfully parses any", (t) => {
  const schema = S.any;
  const value = S.parser(schema)(true);

  t.expect(value).toEqual(true);

  expectType<SchemaEqual<typeof schema, any, any>>(true);
  expectType<TypeEqual<typeof value, any>>(true);
});

test("Successfully parses json", (t) => {
  const schema = S.json;
  const value = S.parser(schema)(true);

  t.expect(value).toEqual(true);

  expectType<SchemaEqual<typeof schema, S.JSON, S.JSON>>(true);
  expectType<TypeEqual<typeof value, S.JSON>>(true);
});

test("Successfully parses invalid json without validation", (t) => {
  const schema = S.json.with(S.noValidation, true);

  let fn = S.parser(schema);

  const value = fn(undefined);
  t.expect(value).toEqual(undefined);

  t.expect(fn.name).toEqual(`noopOperation`);

  t.expect(fn([undefined])).toEqual([undefined]);

  expectType<SchemaEqual<typeof schema, S.JSON, S.JSON>>(true);
  expectType<TypeEqual<typeof value, S.JSON>>(true);
});

test("Successfully parses undefined", (t) => {
  const schema = S.schema(undefined);
  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual(undefined);

  expectType<SchemaEqual<typeof schema, undefined, undefined>>(true);
  expectType<TypeEqual<typeof value, undefined>>(true);
});

test("Successfully parses void", (t) => {
  const schema = S.void;
  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual(undefined);

  expectType<SchemaEqual<typeof schema, void, void>>(true);
  expectType<TypeEqual<typeof value, void>>(true);
});

test("Fails to parse never", (t) => {
  const schema = S.never;

  t.expect(() => {
    const value = S.parser(schema)(true);

    expectType<SchemaEqual<typeof schema, never, never>>(true);
    expectType<TypeEqual<typeof value, never>>(true);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected never, received true",
    }),
  );
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

  expectType<SchemaEqual<typeof schema, string[], string[]>>(true);
  expectType<TypeEqual<typeof value, string[]>>(true);
});

test("Transforms array of bigint to array of string", (t) => {
  const fn = S.decoder(S.array(S.bigint), S.array(S.string));

  t.expect(fn.toString()).toEqual(
    `i=>{let v2=new Array(i.length);for(let v1=0;v1<i.length;++v1){v2[v1]=""+i[v1]}return v2}`,
  );
  t.expect(fn([123n])).toEqual(["123"]);
});

test("Successfully parses array with min and max refinements", (t) => {
  const schema = S.array(S.string).with(S.min, 1).with(S.max, 2);
  const value = S.parser(schema)(["foo"]);
  t.expect(value).toEqual(["foo"]);

  const result = S.safe(() => S.parser(schema)([]));
  t.expect(result.error?.message).toEqual("Array must be 1 or more items long");

  expectType<SchemaEqual<typeof schema, string[], string[]>>(true);
  expectType<TypeEqual<typeof value, string[]>>(true);
});

test("Successfully parses record", (t) => {
  const schema = S.record(S.string);
  const value = S.parser(schema)({ foo: "bar" });

  t.expect(value).toEqual({ foo: "bar" });

  expectType<SchemaEqual<typeof schema, Record<string, string>>>(true);
  expectType<TypeEqual<typeof value, Record<string, string>>>(true);
});

test("Successfully parses JSON string", (t) => {
  const schema = S.jsonString.with(S.to, S.boolean);
  const value = S.parser(schema)(`true`);

  t.expect(value).toEqual(true);
  t.expect(schema.type === "string" && schema.format === "json").toEqual(true);

  expectType<SchemaEqual<typeof schema, boolean, string>>(true);
  expectType<TypeEqual<typeof value, boolean>>(true);
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

  expectType<SchemaEqual<typeof schema, string, string>>(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: number[];
      },
      string
    >
  >(true);
  expectType<
    SchemaEqual<
      typeof schema,
      S.Output<typeof schemaWithSpace>,
      S.Input<typeof schemaWithSpace>
    >
  >(true);
  expectType<TypeEqual<typeof value, string>>(true);
});

test("Successfully parses optional string", (t) => {
  const schema = S.optional(S.string);
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(undefined);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);

  expectType<
    TypeEqual<S.Schema<string | undefined, string | undefined>, typeof schema>
  >(true);
  expectType<TypeEqual<typeof value1, string | undefined>>(true);
  expectType<TypeEqual<typeof value2, string | undefined>>(true);
});

test("Optional enum", (t) => {
  const statuses = S.union(["Win", "Draw", "Loss"]);
  const schema = S.optional(statuses);

  t.expect(S.parser(schema)("Win")).toEqual("Win");
  t.expect(S.parser(schema)(undefined)).toEqual(undefined);

  expectType<
    TypeEqual<
      S.Schema<
        "Win" | "Draw" | "Loss" | undefined,
        "Win" | "Draw" | "Loss" | undefined
      >,
      typeof schema
    >
  >(true);

  const brokenInfer = S.optional(S.union(["Win", "Draw", "Loss"]));
  expectType<
    TypeEqual<
      S.Schema<string | undefined, string | undefined>,
      typeof brokenInfer
    >
  >(true);
});

test("Successfully parses schema wrapped in optional multiple times", (t) => {
  const schema = S.optional(S.optional(S.optional(S.string)));
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(undefined);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);

  expectType<
    TypeEqual<S.Schema<string | undefined, string | undefined>, typeof schema>
  >(true);
  expectType<TypeEqual<typeof value1, string | undefined>>(true);
  expectType<TypeEqual<typeof value2, string | undefined>>(true);
});

test("Successfully parses nullable string", (t) => {
  const schema = S.nullable(S.string);
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);

  expectType<
    TypeEqual<S.Schema<string | undefined, string | null>, typeof schema>
  >(true);
  expectType<TypeEqual<typeof value1, string | undefined>>(true);
});

test("Successfully parses nullable of array with default", (t) => {
  const schema = S.nullable(S.array(S.string), []);
  const value1 = S.parser(schema)(["foo"]);
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual(["foo"]);
  t.expect(value2).toEqual([]);

  expectType<TypeEqual<S.Schema<string[], string[] | null>, typeof schema>>(
    true,
  );
  expectType<TypeEqual<typeof value1, string[]>>(true);
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

  expectType<TypeEqual<S.Schema<string, string | null>, typeof schema>>(true);
  expectType<TypeEqual<typeof value1, string>>(true);
});

test("Successfully parses nullable string with dynamic default", (t) => {
  const schema = S.nullable(S.string, () => "bar");
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual("bar");

  expectType<TypeEqual<S.Schema<string, string | null>, typeof schema>>(true);
  expectType<TypeEqual<typeof value1, string>>(true);
});

test("Successfully parses nullish string", (t) => {
  const schema = S.nullish(S.string);
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(undefined);
  const value3 = S.parser(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);
  t.expect(value3).toEqual(null);

  expectType<
    TypeEqual<
      S.Schema<string | undefined | null, string | undefined | null>,
      typeof schema
    >
  >(true);
  expectType<TypeEqual<typeof value1, string | undefined | null>>(true);
});

test("Successfully parses schema wrapped in nullable multiple times", (t) => {
  const nullable = S.nullable(S.string);
  const schema = S.nullable(S.nullable(nullable));
  const value1 = S.parser(schema)("foo");
  const value2 = S.parser(schema)(null);

  // TODO: Test that it should flatten nested nullable schemas

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual(undefined);

  expectType<
    TypeEqual<S.Schema<string | undefined, string | null>, typeof schema>
  >(true);
  expectType<TypeEqual<typeof value1, string | undefined>>(true);
  expectType<TypeEqual<typeof value2, string | undefined>>(true);
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

test("Test JSON Schema of int32", (t) => {
  const schema = S.int32;

  t.expect(S.toJSONSchema(schema)).toEqual({
    type: "integer",
    minimum: -2147483648,
    maximum: 2147483647,
  });
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

  expectType<TypeEqual<typeof result, string>>(true);
});

test("Successfully reverse converts to Json with valid value", (t) => {
  const schema = S.string;
  const result = S.encoder(schema, S.json)("123");

  t.expect(result).toEqual("123");

  expectType<TypeEqual<typeof result, S.JSON>>(true);
});

test("Successfully reverse converts to Json string with valid value", (t) => {
  const result = S.encoder(S.int32, S.jsonString)(123);

  t.expect(result).toEqual(`123`);

  expectType<TypeEqual<typeof result, string>>(true);
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

  expectType<TypeEqual<typeof value, number>>(true);
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
  expectType<TypeEqual<typeof value, number>>(true);

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
    expectType<TypeEqual<typeof number, number>>(true);
    return number.toString();
  });
  const result = S.encoder(schema)(123);

  t.expect(result).toEqual("123");

  expectType<TypeEqual<typeof result, string>>(true);
});

test("Successfully parses with refine", (t) => {
  const schema = S.string.with(S.refine, (string) => {
    expectType<TypeEqual<typeof string, string>>(true);
    return true;
  });
  const value = S.parser(schema)("123");

  t.expect(value).toEqual("123");

  expectType<TypeEqual<typeof value, string>>(true);
});

test("Successfully reverse converts with refine", (t) => {
  const schema = S.string.with(S.refine, (string) => {
    expectType<TypeEqual<typeof string, string>>(true);
    return true;
  });
  const result = S.encoder(schema)("123");

  t.expect(result).toEqual("123");

  expectType<TypeEqual<typeof result, string>>(true);
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
    expectType<TypeEqual<typeof string, string>>(true);
  });
  const value = await S.safeAsync(() => S.asyncParser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  expectType<TypeEqual<typeof value, S.Result<string>>>(true);
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

  expectType<
    TypeEqual<
      typeof result.error.code,
      | "invalid_input"
      | "invalid_operation"
      | "unsupported_decode"
      | "invalid_conversion"
      | "unrecognized_keys"
    >
  >(true);

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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: string;
        bar: boolean;
      },
      {
        foo: string;
        bar: boolean;
      }
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        foo: string;
        bar: boolean;
      }
    >
  >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        '"': string;
        "'": string;
        "`": string;
      }
    >
  >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        tag: "block";
        bar: boolean;
      },
      {
        tag: "block";
        bar: boolean;
      }
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        tag: "block";
        bar: boolean;
      }
    >
  >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        bar?: boolean | undefined;
        baz: boolean;
      },
      {
        bar?: boolean | undefined;
        baz: boolean;
      }
    >
  >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: string;
        bar: boolean;
      },
      Record<string, unknown>
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        foo: string;
        bar: boolean;
      }
    >
  >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        nested: number;
        flattened: {
          id: string;
        };
        foo: string;
        bar: boolean;
      },
      Record<string, unknown>
    >
  >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: number;
        bar: boolean;
      },
      {
        foo: string;
        bar: boolean;
      }
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        foo: number;
        bar: boolean;
      }
    >
  >(true);
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
    expectType<
      TypeEqual<
        typeof schema,
        S.Schema<
          {
            foo: string;
          },
          {
            foo: string;
          }
        >
      >
    >(true);
    expectType<
      TypeEqual<
        typeof value,
        {
          foo: string;
        }
      >
    >(true);
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
    expectType<
      SchemaEqual<
        typeof schema,
        {
          foo: {
            a: string;
          };
        }
      >
    >(true);
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
    expectType<
      TypeEqual<
        typeof schema,
        S.Schema<
          {
            foo: string;
          },
          {
            foo: string;
          }
        >
      >
    >(true);
    expectType<
      TypeEqual<
        typeof value,
        {
          foo: string;
        }
      >
    >(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: string;
      },
      {
        foo: string;
      }
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        foo: string;
      }
    >
  >(true);
});

test("Successfully parses intersected objects", (t) => {
  const schema = S.merge(
    S.schema({
      foo: S.string,
      bar: S.boolean,
    }),
    S.schema({
      baz: S.string,
    }),
  );

  t.expect(S.parser(schema).toString()).toEqual(
    `i=>{typeof i==="object"&&i||e[3](i);let v0=i["foo"],v1=i["bar"],v2=i["baz"];typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);typeof v2==="string"||e[2](v2);return {"foo":v0,"bar":v1,"baz":v2,}}`,
  );

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: string;
        bar: boolean;
        baz: string;
      },
      Record<string, unknown>
    >
  >(true);

  const result = S.safe(() =>
    S.parser(schema)({
      foo: "bar",
      bar: true,
    }),
  );
  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe(
    `Failed at ["baz"]: Expected string, received undefined`,
  );

  const value = S.parser(schema)({
    foo: "bar",
    baz: "baz",
    bar: true,
  });
  t.expect(value).toEqual({
    foo: "bar",
    baz: "baz",
    bar: true,
  });
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

  // expectType<
  //   SchemaEqual<
  //     typeof schema,
  //     {
  //       abc: string;
  //       baz: string;
  //     },
  //     Record<string, unknown>
  //   >
  // >(true);

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

test("Successfully serializes S.merge", (t) => {
  const schema = S.merge(
    S.schema({
      foo: S.string,
      bar: S.boolean,
    }),
    S.schema({
      baz: S.string,
    }),
  );

  t.expect(S.parser(S.reverse(schema)).toString()).toEqual(
    `i=>{typeof i==="object"&&i||e[3](i);let v0=i["foo"],v1=i["bar"],v2=i["baz"];typeof v0==="string"||e[0](v0);typeof v1==="boolean"||e[1](v1);typeof v2==="string"||e[2](v2);return {"foo":v0,"bar":v1,"baz":v2,}}`,
  );
  t.expect(
    S.encoder(schema).toString().startsWith("function noopOperation(i) {"),
  ).toEqual(true);

  const value = S.encoder(schema)({
    foo: "bar",
    baz: "baz",
    bar: true,
  });
  expectType<TypeEqual<typeof value, Record<string, unknown>>>(true);

  t.expect(value).toEqual({
    foo: "bar",
    baz: "baz",
    bar: true,
  });
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

  expectType<
    SchemaEqual<
      typeof fooSchema,
      {
        type: "foo";
        name: string;
        fooCount: number;
      },
      Record<string, unknown>
    >
  >(true);

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

  t.expect(S.toExpression(schema)).toBe(
    `{ foo: string; bar: boolean; baz: string; }`,
  );
});

// https://github.com/DZakh/sury/issues/157
test("Merge preserves optional properties", (t) => {
  const docMetaSchema = S.schema({
    _id: S.optional(S.string),
    _rev: S.optional(S.string),
    _deleted: S.optional(S.boolean),
  });

  const productSchema = S.merge(
    docMetaSchema,
    S.schema({
      code: S.min(S.string, 1, "Product Code is required"),
      name: S.min(S.string, 1, "Product Name is required"),
    }),
  );

  expectType<
    SchemaEqual<
      typeof productSchema,
      {
        _id?: string | undefined;
        _rev?: string | undefined;
        _deleted?: boolean | undefined;
        code: string;
        name: string;
      },
      Record<string, unknown>
    >
  >(true);

  const value = S.parser(productSchema)({
    code: "P001",
    name: "Widget",
  });
  t.expect(value).toEqual({
    _id: undefined,
    _rev: undefined,
    _deleted: undefined,
    code: "P001",
    name: "Widget",
  });

  const valueWithOptionals = S.parser(productSchema)({
    _id: "123",
    code: "P001",
    name: "Widget",
  });
  t.expect(valueWithOptionals).toEqual({
    _id: "123",
    _rev: undefined,
    _deleted: undefined,
    code: "P001",
    name: "Widget",
  });
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: string;
        bar: boolean;
      }
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        foo: string;
        bar: boolean;
      }
    >
  >(true);
});

test("Successfully parses tuple using S.schema", (t) => {
  const schema = S.schema([S.string, S.boolean] as const);
  const value = S.parser(schema)(["bar", true]);

  t.expect(value).toEqual(["bar", true]);

  expectType<SchemaEqual<typeof schema, [string, boolean]>>(true);
  expectType<TypeEqual<typeof value, [string, boolean]>>(true);
});

test("Successfully parses primitive schema passed to S.schema", (t) => {
  const schema = S.schema(S.string);
  const value = S.parser(schema)("bar");

  t.expect(value).toEqual("bar");

  expectType<SchemaEqual<typeof schema, string, string>>(true);
  expectType<TypeEqual<typeof value, string>>(true);
});

test("Successfully parses literal using S.schema with as cost", (t) => {
  const schema = S.schema("foo" as const);

  const value = S.parser(schema)("foo");

  t.expect(value).toEqual("foo");

  expectType<SchemaEqual<typeof schema, "foo">>(true);
  expectType<TypeEqual<typeof value, "foo">>(true);
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

  expectType<
    SchemaEqual<
      typeof schema,
      {
        foo: { bar: number };
      }
    >
  >(true);
  expectType<
    TypeEqual<
      typeof value,
      {
        foo: { bar: number };
      }
    >
  >(true);
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

  expectType<TypeEqual<typeof circleSchema, S.Schema<Shape, Shape>>>(true);
  expectType<TypeEqual<typeof value, Shape>>(true);
});

test("S.name", (t) => {
  t.expect(S.toExpression(S.unknown.with(S.meta, { name: "BlaBla" }))).toBe(
    `BlaBla`,
  );
});

test("Successfully parses and returns result", (t) => {
  const schema = S.string;
  const value = S.safe(() => S.parser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  expectType<TypeEqual<typeof value, S.Result<string>>>(true);
  if (value.success) {
    expectType<
      TypeEqual<
        typeof value,
        {
          readonly success: true;
          readonly value: string;
          readonly error?: undefined;
        }
      >
    >(true);
  } else {
    expectType<
      TypeEqual<
        typeof value,
        {
          readonly success: false;
          readonly error: S.Error;
        }
      >
    >(true);
  }
});

test("Successfully reverse converts and returns result", (t) => {
  const schema = S.string;
  const value = S.safe(() => S.encoder(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  if (value.success) {
    expectType<
      TypeEqual<
        typeof value,
        {
          readonly success: true;
          readonly value: string;
          readonly error?: undefined;
        }
      >
    >(true);
  } else {
    expectType<
      TypeEqual<
        typeof value,
        {
          readonly success: false;
          readonly error: S.Error;
        }
      >
    >(true);
  }
});

test("Successfully parses union", (t) => {
  const schema = S.union([S.string, S.number]);
  const value = S.safe(() => S.parser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: "123" });

  expectType<SchemaEqual<typeof schema, string | number>>(true);
});

test("Successfully parses union of literals", (t) => {
  const schema = S.union(["foo", 123, true]);
  const value = S.safe(() => S.parser(schema)("foo"));

  t.expect(value).toEqual({ success: true, value: "foo" });

  expectType<SchemaEqual<typeof schema, "foo" | 123 | true>>(true);
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

  expectType<
    TypeEqual<
      typeof shapeSchema,
      S.Schema<
        | {
            kind: "circle";
            radius: number;
          }
        | {
            kind: "square";
            x: number;
          }
        | {
            kind: "triangle";
            x: number;
            y: number;
          },
        | {
            kind: "circle";
            radius: number;
          }
        | {
            kind: "square";
            x: number;
          }
        | {
            kind: "triangle";
            x: number;
            y: number;
          }
      >
    >
  >(true);
});

test("Successfully parses union with transformed items", (t) => {
  const schema = S.union([
    S.string.with(S.to, S.number, (string) => Number(string)),
    S.number,
  ]);
  const value = S.safe(() => S.parser(schema)("123"));

  t.expect(value).toEqual({ success: true, value: 123 });

  expectType<SchemaEqual<typeof schema, number, string | number>>(true);
});

test("String literal", (t) => {
  const schema = S.schema("tuna");

  t.expect(S.parser(schema)("tuna")).toEqual("tuna");

  expectType<SchemaEqual<typeof schema, "tuna">>(true);
});

test("Nested string literal", (t) => {
  const schema = S.schema({
    nested: "tuna" as const,
    withoutAsConst: "tuna",
    inSchema: S.schema("tuna"),
  });

  t.expect(
    S.parser(schema)({
      nested: "tuna",
      withoutAsConst: "tuna",
      inSchema: "tuna",
    }),
  ).toEqual({ nested: "tuna", withoutAsConst: "tuna", inSchema: "tuna" });

  expectType<
    SchemaEqual<
      typeof schema,
      { nested: "tuna"; withoutAsConst: string; inSchema: "tuna" }
    >
  >(true);
});

test("Boolean literal", (t) => {
  const schema = S.schema(true);

  t.expect(S.parser(schema)(true)).toEqual(true);

  expectType<SchemaEqual<typeof schema, true, true>>(true);
});

test("Number literal", (t) => {
  const schema = S.schema(123);

  t.expect(S.parser(schema)(123)).toEqual(123);

  expectType<SchemaEqual<typeof schema, 123, 123>>(true);
});

test("Undefined literal", (t) => {
  const schema = S.schema(undefined);

  t.expect(S.parser(schema)(undefined)).toEqual(undefined);

  expectType<SchemaEqual<typeof schema, undefined, undefined>>(true);
});

test("Null literal", (t) => {
  const schema = S.schema(null);

  t.expect(S.parser(schema)(null)).toEqual(null);

  expectType<SchemaEqual<typeof schema, null, null>>(true);
});

test("Symbol literal", (t) => {
  let symbol = Symbol();
  const schema = S.schema(symbol);

  t.expect(S.parser(schema)(symbol)).toEqual(symbol);

  expectType<SchemaEqual<typeof schema, symbol, symbol>>(true);
});

test("BigInt literal", (t) => {
  const schema = S.schema(123n);

  t.expect(S.parser(schema)(123n)).toEqual(123n);

  expectType<SchemaEqual<typeof schema, 123n, 123n>>(true);
});

test("NaN literal", (t) => {
  const schema = S.schema(NaN);

  t.expect(S.parser(schema)(NaN)).toEqual(NaN);

  expectType<SchemaEqual<typeof schema, number, number>>(true);
});

test("Tuple literal", (t) => {
  const cliArgsSchema = S.schema(["help", "lint"] as const);

  t.expect(S.parser(cliArgsSchema)(["help", "lint"])).toEqual(["help", "lint"]);

  expectType<
    TypeEqual<
      typeof cliArgsSchema,
      S.Schema<["help", "lint"], ["help", "lint"]>
    >
  >(true);
});

test("Correctly infers type", (t) => {
  const schema = S.string.with(S.to, S.number, Number);
  expectType<SchemaEqual<typeof schema, number, string>>(true);
  expectType<TypeEqual<S.Input<typeof schema>, string>>(true);
  expectType<TypeEqual<S.Output<typeof schema>, number>>(true);
});

test("Successfully parses undefined using the default value", (t) => {
  const schema = S.string.with(S.optional, "foo");

  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual("foo");
  t.expect(schema.default).toEqual("foo");

  expectType<TypeEqual<typeof schema.default, string | undefined>>(true);
  expectType<SchemaEqual<typeof schema, string, string | undefined>>(true);
});

test("Successfully parses undefined using the default value for transformed schema", (t) => {
  // FIXME: Test that it works correctly:
  // const schema = S.boolean.with(S.optional, false).with(S.to, S.string);
  const schema = S.boolean.with(S.to, S.string).with(S.optional, "false");

  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual("false");
  t.expect(schema.default).toEqual(false);

  expectType<TypeEqual<typeof schema.default, boolean | undefined>>(true);
  expectType<SchemaEqual<typeof schema, string, boolean | undefined>>(true);
});

test("Successfully parses undefined using the default value from callback", (t) => {
  const schema = S.string.with(S.optional, () => "foo");

  const value = S.parser(schema)(undefined);

  t.expect(value).toEqual("foo");
  t.expect(schema.default).toEqual(undefined);

  //FIXME: This is broken
  // @ts-expect-error
  expectType<SchemaEqual<typeof schema, string, string | undefined>>(true);
});

test("Creates schema with description and title", (t) => {
  const undocumentedStringSchema = S.string;

  expectType<
    TypeEqual<typeof undocumentedStringSchema, S.Schema<string, string>>
  >(true);

  const documentedStringSchema = undocumentedStringSchema.with(S.meta, {
    title: "My schema",
    description: "A useful bit of text, if you know what to do with it.",
  });

  expectType<
    TypeEqual<typeof documentedStringSchema, S.Schema<string, string>>
  >(true);

  expectType<
    TypeEqual<typeof documentedStringSchema.title, string | undefined>
  >(true);
  expectType<
    TypeEqual<typeof documentedStringSchema.description, string | undefined>
  >(true);

  t.expect(undocumentedStringSchema.description).toEqual(undefined);
  t.expect(documentedStringSchema.description).toEqual(
    "A useful bit of text, if you know what to do with it.",
  );
  t.expect(undocumentedStringSchema.title).toEqual(undefined);
  t.expect(documentedStringSchema.title).toEqual("My schema");
});

test("Creates schema with deprecation", (t) => {
  const schema = S.string;

  expectType<TypeEqual<typeof schema, S.Schema<string, string>>>(true);

  const deprecatedStringSchema = schema.with(S.meta, {
    deprecated: true,
    description: "Use number instead.",
  });

  expectType<
    TypeEqual<typeof deprecatedStringSchema, S.Schema<string, string>>
  >(true);

  expectType<
    TypeEqual<typeof deprecatedStringSchema.deprecated, boolean | undefined>
  >(true);
  expectType<
    TypeEqual<typeof deprecatedStringSchema.description, string | undefined>
  >(true);

  t.expect(schema.deprecated).toEqual(undefined);
  t.expect(deprecatedStringSchema.deprecated).toEqual(true);
  t.expect(deprecatedStringSchema.description).toEqual("Use number instead.");
});

test("Tuple with single element", (t) => {
  const schema = S.schema([S.string.with(S.to, S.number, (s) => Number(s))]);

  t.expect(S.parser(schema)(["123"])).toEqual([123]);

  expectType<SchemaEqual<typeof schema, [number], [string]>>(true);
});

test("Tuple with multiple elements", (t) => {
  const schema = S.schema([S.string, S.number, true]);

  t.expect(S.parser(schema)(["123", 123, true])).toEqual(["123", 123, true]);

  expectType<SchemaEqual<typeof schema, [string, number, true]>>(true);
});

test("Tuple types", (t) => {
  const emptyTuple = S.schema([]);
  expectType<SchemaEqual<typeof emptyTuple, []>>(true);

  const tuple1WithLiteral = S.schema(["foo"]);
  expectType<SchemaEqual<typeof tuple1WithLiteral, ["foo"]>>(true);

  const tuple1WithSchema = S.schema([S.string]);
  expectType<SchemaEqual<typeof tuple1WithSchema, [string]>>(true);

  const tuple1WithObject = S.schema([{ foo: S.string }]);
  expectType<
    SchemaEqual<
      typeof tuple1WithObject,
      [
        {
          foo: string;
        },
      ]
    >
  >(true);

  const tuple2WithLiterals = S.schema(["foo", 123]);
  expectType<SchemaEqual<typeof tuple2WithLiterals, ["foo", 123]>>(true);

  const tuple2WithSchemas = S.schema([S.string, S.boolean]);
  expectType<SchemaEqual<typeof tuple2WithSchemas, [string, boolean]>>(true);

  const tuple2LiteralAndSchema = S.schema(["foo", S.boolean]);
  expectType<SchemaEqual<typeof tuple2LiteralAndSchema, ["foo", boolean]>>(
    true,
  );

  const tuple2LiteralAsCosntAndSchema = S.schema(["foo" as const, S.boolean]);
  expectType<
    SchemaEqual<typeof tuple2LiteralAsCosntAndSchema, ["foo", boolean]>
  >(true);

  const tuple2LiteralSchemaAndSchema = S.schema([S.schema("foo"), S.boolean]);
  expectType<
    SchemaEqual<typeof tuple2LiteralSchemaAndSchema, ["foo", boolean]>
  >(true);
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
    value: undefined,
  });

  expectType<
    TypeEqual<S.StandardSchemaV1.InferInput<typeof schema>, string | null>
  >(true);
  expectType<
    TypeEqual<S.StandardSchemaV1.InferOutput<typeof schema>, string | undefined>
  >(true);
});

test("Env schema: Reggression version", (t) => {
  const env = <T>(schema: S.Schema<T>): S.Schema<T, string> => {
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
    `i=>{if(typeof i==="string"){if(i==="t"){i=true}else if(i==="1"){i=true}else if(i==="f"){i=false}else if(i==="0"){i=false}else{try{let v0;(v0=i==="true")||i==="false"||e[0](i);i=v0}catch(e4){e[1](i,e4)}}}else{e[2](i)}return i}`,
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
  // S.nullable converts null to undefined on parsing
  const parse = S.parser(schema);
  const parsed = parse([
    ["0", "1"],
    ["Hello", null],
    [false, true],
  ] as unknown[][]);
  t.expect(parsed).toEqual([
    { id: "0", name: "Hello", deleted: false },
    { id: "1", name: undefined, deleted: true },
  ]);

  // Test encoding row objects back to columnar data
  // S.nullable converts undefined back to null on encoding
  const encode = S.encoder(schema);
  const encoded = encode([
    { id: "0", name: "Hello", deleted: false },
    { id: "1", name: undefined, deleted: true },
  ] as any);
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

  expectType<SchemaEqual<typeof schema, Set<unknown>, Set<unknown>>>(true);
  if (schema.type === "instance") {
    expectType<TypeEqual<typeof schema.class, S.Class<Set<unknown>>>>(true);
    t.expect(schema.class).toBe(Set);
  }

  const parser = S.parser(schema);
  expectType<TypeEqual<typeof parser, (input: unknown) => Set<unknown>>>(true);

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
  const mySet = <T>(itemSchema: S.Schema<T>): S.Schema<Set<T>> =>
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
        name: `Set<${S.toExpression(itemSchema)}>`,
      });

  const numberSetSchema = mySet(S.number);

  expectType<SchemaEqual<typeof numberSetSchema, Set<number>, unknown>>(true);

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

  expectType<SchemaEqual<typeof schema, number, string>>(true);
  expectType<TypeEqual<typeof schema.to, S.Schema<unknown> | undefined>>(true);

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

  expectType<
    SchemaEqual<
      typeof pointSchema,
      {
        x: number;
        y: number;
      },
      unknown[]
    >
  >(true);
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
  expectType<TypeEqual<typeof data, unknown>>(true);
  S.assert(schema, data);
  expectType<TypeEqual<typeof data, string>>(true);
});

test("Schema of object with empty prototype", (t) => {
  const obj = Object.create(null) as { foo: S.Schema<string, string> };
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

  let nodeSchema = S.recursive<Node, Node>("Node", (nodeSchema) =>
    S.schema({
      id: S.string,
      children: S.array(nodeSchema),
    }),
  );

  expectType<SchemaEqual<typeof nodeSchema, Node, Node>>(true);

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

  const makeUserSchema = (postSchema: S.Schema<Post>) =>
    S.schema({
      email: S.string,
      posts: S.array(postSchema),
    });
  const makePostSchema = (userSchema: S.Schema<User>) =>
    S.schema({
      Title: S.string,
      Author: userSchema,
    }).with(S.shape, (post) => ({
      title: post.Title,
      author: post.Author,
    }));

  const userSchema = S.recursive<User>("User", (userSchema) =>
    makeUserSchema(
      S.recursive<Post>("Post", (_) => makePostSchema(userSchema)),
    ),
  );
  const postSchema = S.recursive<Post>("Post", (postSchema) =>
    makePostSchema(
      S.recursive<User>("User", (_) => makeUserSchema(postSchema)),
    ),
  );

  expectType<SchemaEqual<typeof userSchema, User, unknown>>(true);
  expectType<SchemaEqual<typeof postSchema, Post, unknown>>(true);

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

  let nodeSchema = S.recursive<Node>("Node", (nodeSchema) =>
    S.schema({
      ID: S.string,
      CHILDREN: S.array(nodeSchema),
    }).with(S.shape, (input) => ({
      id: input.ID,
      children: input.CHILDREN,
    })),
  );

  expectType<SchemaEqual<typeof nodeSchema, Node, unknown>>(true);

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
    let nodeSchema = S.recursive<Node, string>("Node", (self) =>
      S.string.with(S.to, S.array(self)),
    );
    expectType<SchemaEqual<typeof nodeSchema, Node, string>>(true);

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

  expectType<SchemaEqual<typeof portSchema, number, number>>(true);

  t.expect(() => {
    S.parser(portSchema)(10.2);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected port, received 10.2",
    }),
  );

  const portCoercedFromString = S.string.with(S.to, S.port);
  expectType<SchemaEqual<typeof portCoercedFromString, number, string>>(true);

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
    password: S.string.with(S.min, 8),
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

  expectType<
    SchemaEqual<
      typeof loginSchema,
      { email: string; password: string },
      { email: string; password: string }
    >
  >(true);
  expectType<TypeEqual<LoginData, { email: string; password: string }>>(true);
});

test("Decode from json", async (t) => {
  t.expect(S.decoder(S.json, S.array(S.bigint))(["123"])).toEqual([123n]);
  t.expect(S.decoder(S.array(S.bigint), S.json)([123n])).toEqual(["123"]);

  const schema = S.string.with(S.nullable);

  t.expect(S.decoder(S.json, schema)("hello")).toEqual("hello");
  t.expect(S.decoder(S.json, schema)(null)).toEqual(undefined);

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
  t.expect(S.decoder(S.jsonString, schema)("null")).toEqual(undefined);
});

test("Decode from json string, convert to number", async (t) => {
  const fn = S.decoder(S.jsonString, S.string, S.number);

  expectType<TypeEqual<typeof fn, (data: string) => number>>(true);

  t.expect(fn(`"123"`)).toEqual(123);
});

test("Decode from json string to array of bigints", async (t) => {
  const fn = S.decoder(S.jsonString, S.array(S.bigint));

  expectType<TypeEqual<typeof fn, (data: string) => bigint[]>>(true);

  t.expect(fn(`["123"]`)).toEqual([123n]);
});

test("Parse to literal with no validation to emulate assert", async (t) => {
  const fn = S.parser(
    S.schema({ foo: S.string }),
    S.schema(true).with(S.noValidation, true),
  );

  expectType<TypeEqual<typeof fn, (data: unknown) => true>>(true);
  t.expect(fn({ foo: "bar" })).toEqual(true);
  t.expect(fn.toString()).toEqual(
    `i=>{typeof i==="object"&&i||e[1](i);let v0=i["foo"];typeof v0==="string"||e[0](v0);return true}`,
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
  expectType<SchemaEqual<typeof schema, S.Brand<string, "Foo">, string>>(true);
  const result = S.parser(schema)("hello");
  expectType<S.Brand<string, "Foo">>(result);
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
  expectType<SchemaEqual<typeof emailSchema, string, S.JSON>>(true);
  const result = S.safe(() => S.assert(emailSchema, "example.com"));

  t.expect(result.error?.message).toBe(
    `Expected email, received "example.com"`,
  );
});

test("Compile types", async (t) => {
  const schema = S.union([
    S.string,
    S.schema(null).with(S.to, S.schema(undefined)),
  ]);

  const fn1 = S.decoder(schema);
  expectType<
    TypeEqual<typeof fn1, (input: string | null) => string | undefined>
  >(true);
  t.expect(fn1("hello")).toEqual("hello");
  t.expect(fn1(null)).toEqual(undefined);

  const fn2 = S.encoder(schema);
  expectType<
    TypeEqual<typeof fn2, (input: string | undefined) => string | null>
  >(true);
  t.expect(fn2("hello")).toEqual("hello");
  t.expect(fn2(undefined)).toEqual(null);

  const fn3 = S.parser(schema);
  expectType<TypeEqual<typeof fn3, (input: unknown) => string | undefined>>(
    true,
  );
  t.expect(fn3("hello")).toEqual("hello");
  t.expect(fn3(null)).toEqual(undefined);

  const fn4 = S.decoder(S.json, schema);
  expectType<TypeEqual<typeof fn4, (input: S.JSON) => string | undefined>>(
    true,
  );
  t.expect(fn4("hello")).toEqual("hello");
  t.expect(fn4(null)).toEqual(undefined);

  const fn5 = S.decoder(S.jsonString, schema);
  expectType<TypeEqual<typeof fn5, (input: string) => string | undefined>>(
    true,
  );
  t.expect(fn5(`"hello"`)).toEqual("hello");
  t.expect(fn5("null")).toEqual(undefined);

  const fn6 = S.encoder(schema, S.json);
  expectType<TypeEqual<typeof fn6, (input: string | undefined) => S.JSON>>(
    true,
  );
  t.expect(fn6("hello")).toEqual("hello");
  t.expect(fn6(undefined)).toEqual(null);

  const fn7 = S.encoder(schema, S.jsonString);
  expectType<TypeEqual<typeof fn7, (input: string | undefined) => string>>(
    true,
  );
  t.expect(fn7("hello")).toEqual(`"hello"`);
  t.expect(fn7(undefined)).toEqual("null");

  // FIXME:
  // const fn8 = S.compile(schema, "Output", "Assert", "Sync", true);
  // expectType<TypeEqual<typeof fn8, (input: string | undefined) => void>>(true);
  // t.deepEqual(fn8("hello"), undefined);
  // t.deepEqual(fn8(undefined), undefined);

  // const fn9 = S.compile(schema, "Output", "JsonString", "Async");
  // expectType<
  //   TypeEqual<typeof fn9, (input: string | undefined) => Promise<string>>
  // >(true);
  // t.deepEqual(await fn9("hello"), `"hello"`);
  // t.deepEqual(await fn9(undefined), "null");
});

test("Preprocess nested fields", (t) => {
  const stripPrefix = <Input>(
    schema: S.Schema<string, Input>,
    prefix: string,
  ): S.Schema<string, Input> =>
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
  expectType<SchemaEqual<typeof schema, string, string>>(true);
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
  expectType<
    SchemaEqual<typeof schema2, "USD" | "BGP" | "EUR", "USD" | "BGP" | "EUR">
  >(true);
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
  expectType<
    SchemaEqual<typeof schema3, "USD" | "BGP" | "EUR", "USD" | "BGP" | "EUR">
  >(true);
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

  expectType<SchemaEqual<typeof schema, "a" | "b" | "c", "a" | "b" | "c">>(
    true,
  );
  t.expect(S.parser(schema)("a")).toEqual("a");
  t.expect(() => S.parser(schema)("d")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "a" | "b" | "c", received "d"`,
    }),
  );
});

test("Overwrite error message", (t) => {
  const schema = S.string.with(S.min, 3, "Invalid string");

  const fieldSchema = <O, I>(schema: S.Schema<O, I>): S.Schema<O, I> => {
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
