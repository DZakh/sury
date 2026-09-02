import { test } from "vitest";

import * as S from "../index.mjs";

const field = <TInput, TOutput>(
  schema: S.Schema<TInput, TOutput>,
  number: number,
  type: S.ProtobufType,
) => schema.with(S.protobufField, { number, type });

test("protobuf encodes and decodes scalar fields with Sury coercion", (t) => {
  const Message = S.schema({
    id: field(S.string, 1, "uint32"),
    name: field(S.string, 2, "string"),
    active: field(S.boolean, 3, "bool"),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  const bytes = encode({ id: "150", name: "Ada", active: true });

  t.expect([...bytes]).toEqual([8, 150, 1, 18, 3, 65, 100, 97, 24, 1]);
  t.expect(decode(bytes)).toEqual({ id: "150", name: "Ada", active: true });
});

test("protobuf supports all scalar wire forms", (t) => {
  const Message = S.schema({
    int32: field(S.int32, 1, "int32"),
    int64: field(S.bigint, 2, "int64"),
    uint32: field(S.integer, 3, "uint32"),
    uint64: field(S.bigint, 4, "uint64"),
    sint32: field(S.int32, 5, "sint32"),
    sint64: field(S.bigint, 6, "sint64"),
    fixed32: field(S.integer, 7, "fixed32"),
    fixed64: field(S.bigint, 8, "fixed64"),
    sfixed32: field(S.int32, 9, "sfixed32"),
    sfixed64: field(S.bigint, 10, "sfixed64"),
    float: field(S.number, 11, "float"),
    double: field(S.number, 12, "double"),
    bytes: field(S.uint8Array, 13, "bytes"),
    enum: field(S.int32, 14, "enum"),
  });
  const value = {
    int32: -1,
    int64: -2n,
    uint32: 4294967295,
    uint64: 18446744073709551615n,
    sint32: -2147483648,
    sint64: -9223372036854775808n,
    fixed32: 4294967295,
    fixed64: 18446744073709551615n,
    sfixed32: -2147483648,
    sfixed64: -9223372036854775808n,
    float: 1.5,
    double: -2.25,
    bytes: new Uint8Array([0, 255]),
    enum: -1,
  };

  t.expect(S.decoder(S.protobuf, Message)(S.decoder(Message, S.protobuf)(value))).toEqual(value);
});

test("protobuf emits packed repeated scalars and accepts packed and expanded values", (t) => {
  const Message = S.schema({ values: field(S.array(S.int32), 1, "sint32") });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);

  t.expect([...encode({ values: [-1, 0, 2] })]).toEqual([10, 3, 1, 0, 4]);
  t.expect(decode(new Uint8Array([8, 1, 8, 0, 8, 4]))).toEqual({ values: [-1, 0, 2] });
  t.expect(decode(new Uint8Array([10, 1, 1, 8, 4]))).toEqual({ values: [-1, 2] });
});

test("protobuf preserves optional scalar presence", (t) => {
  const Message = S.schema({ value: field(S.optional(S.int32), 1, "int32") });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);

  t.expect([...encode({})]).toEqual([]);
  t.expect([...encode({ value: 0 })]).toEqual([8, 0]);
  t.expect(decode(new Uint8Array())).toEqual({});
  t.expect(decode(new Uint8Array([8, 0]))).toEqual({ value: 0 });
});

test("protobuf decodes nested messages and merges repeated occurrences", (t) => {
  const Child = S.schema({
    first: field(S.optional(S.int32), 1, "int32"),
    second: field(S.optional(S.string), 2, "string"),
  });
  const Parent = S.schema({ child: field(Child, 1, "message") });
  const decode = S.decoder(S.protobuf, Parent);

  t.expect(decode(new Uint8Array([10, 2, 8, 1, 10, 3, 18, 1, 120]))).toEqual({
    child: { first: 1, second: "x" },
  });
});

test("protobuf strips unknown fields and strict rejects them", (t) => {
  const Message = S.schema({ value: field(S.int32, 1, "int32") });
  const StrictMessage = Message.with(S.strict);
  const bytes = new Uint8Array([8, 1, 16, 2]);

  t.expect(S.decoder(S.protobuf, Message)(bytes)).toEqual({ value: 1 });
  t.expect(() => S.decoder(S.protobuf, StrictMessage)(bytes)).toThrow("unknown protobuf field 2");
});

test("protobuf skips every legal unknown wire type including groups", (t) => {
  const Message = S.schema({ value: field(S.int32, 1, "int32") });
  const decode = S.decoder(S.protobuf, Message);
  const bytes = new Uint8Array([
    19,
      29, 1, 2, 3, 4,
      35, 40, 1, 36,
    20,
    9, 1, 2, 3, 4, 5, 6, 7, 8,
    18, 2, 9, 9,
    8, 7,
  ]);

  t.expect(decode(bytes)).toEqual({ value: 7 });
  t.expect(() => decode(new Uint8Array([19, 28]))).toThrow("mismatched protobuf end group");
});

test("protobuf uses last-one-wins and treats a known field with the wrong wire type as unknown", (t) => {
  const Message = S.schema({ value: field(S.int32, 1, "int32") });
  const StrictMessage = Message.with(S.strict);
  const bytes = new Uint8Array([10, 1, 99, 8, 1, 8, 2]);

  t.expect(S.decoder(S.protobuf, Message)(bytes)).toEqual({ value: 2 });
  t.expect(() => S.decoder(S.protobuf, StrictMessage)(bytes)).toThrow("unknown protobuf field 1");
});

test("protobuf preserves IEEE-754 special values and rejects float32 overflow", (t) => {
  const Message = S.schema({
    float: field(S.number, 1, "float"),
    double: field(S.number, 2, "double"),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);

  const negativeZero = decode(encode({ float: -0, double: -0 }));
  t.expect(Object.is(negativeZero.float, -0)).toBe(true);
  t.expect(Object.is(negativeZero.double, -0)).toBe(true);
  const specials = decode(encode({ float: Number.NaN, double: Number.POSITIVE_INFINITY }));
  t.expect(Number.isNaN(specials.float)).toBe(true);
  t.expect(specials.double).toBe(Number.POSITIVE_INFINITY);
  t.expect(() => encode({ float: Number.MAX_VALUE, double: 0 })).toThrow("invalid float");
});

test("protobuf rejects malformed wire data", (t) => {
  const Message = S.schema({ value: field(S.string, 1, "string") });
  const decode = S.decoder(S.protobuf, Message);

  t.expect(() => decode(new Uint8Array([10, 2, 65]))).toThrow();
  t.expect(() => decode(new Uint8Array([0]))).toThrow();
  t.expect(() => decode(new Uint8Array([10, 1, 255]))).toThrow();
  t.expect(() => decode(new Uint8Array([128, 128, 128, 128, 128, 128, 128, 128, 128, 2]))).toThrow();
  t.expect(() => decode(new Uint8Array([128, 128, 128, 128, 16]))).toThrow("invalid protobuf tag");
  t.expect(() => decode(new Uint8Array([136, 128, 128, 128, 128, 0, 1]))).toThrow("invalid protobuf tag");
  t.expect(() => decode(new Uint8Array([136, 128, 128, 128, 128, 128, 128, 128, 0, 1]))).toThrow("invalid protobuf tag");
  t.expect(decode(new Uint8Array([248, 255, 255, 255, 15, 1, 10, 0]))).toEqual({ value: "" });
});

test("protobuf accepts overlong and 64-bit varints in value position", (t) => {
  const Message = S.schema({
    int32: field(S.int32, 1, "int32"),
    uint32: field(S.integer, 2, "uint32"),
    bool: field(S.boolean, 3, "bool"),
    sint32: field(S.int32, 4, "sint32"),
  });
  const decode = S.decoder(S.protobuf, Message);
  t.expect(decode(new Uint8Array([8, 185, 224, 128, 128, 128, 128, 128, 128, 0]))).toMatchObject({ int32: 12345 });
  t.expect(decode(new Uint8Array([8, 255, 255, 255, 255, 31]))).toMatchObject({ int32: -1 });
  t.expect(decode(new Uint8Array([16, 129, 128, 128, 128, 32]))).toMatchObject({ uint32: 1 });
  t.expect(decode(new Uint8Array([24, 128, 128, 128, 128, 32]))).toMatchObject({ bool: true });
  t.expect(decode(new Uint8Array([32, 130, 128, 128, 128, 16]))).toMatchObject({ sint32: 1 });
  t.expect(() => decode(new Uint8Array([8, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1]))).toThrow("exceeds 10 bytes");
});

test("protobuf encodes negative int32 and enum as ten bytes without BigInt", (t) => {
  const Message = S.schema({ a: field(S.int32, 1, "int32"), e: field(S.int32, 2, "enum") });
  t.expect([...S.decoder(Message, S.protobuf)({ a: -2147483648, e: -1 })]).toEqual([
    8, 128, 128, 128, 128, 248, 255, 255, 255, 255, 1, 16, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1,
  ]);
});

test("protobuf writes a repeated scalar expanded with packed: false and reads both forms", (t) => {
  const Message = S.schema({ a: S.array(S.integer).with(S.protobufField, { number: 1, type: "uint32", packed: false }) });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  t.expect([...encode({ a: [1, 2, 3] })]).toEqual([8, 1, 8, 2, 8, 3]);
  t.expect(decode(new Uint8Array([10, 3, 1, 2, 3]))).toEqual({ a: [1, 2, 3] });
  t.expect(decode(new Uint8Array([8, 1, 8, 2, 8, 3]))).toEqual({ a: [1, 2, 3] });
});

test("protobuf maps a record to map<K, V> entries", (t) => {
  const Inner = S.schema({ key: S.string.with(S.protobufField, 1), values: S.array(S.string).with(S.protobufField, 2) });
  const Message = S.schema({
    value: S.record(Inner).with(S.protobufField, 1),
    ints: S.record(S.int32).with(S.protobufField, { number: 2, key: "int32" }),
    flags: S.record(S.boolean).with(S.protobufField, { number: 3, key: "bool" }),
    big: S.record(S.string).with(S.protobufField, { number: 4, key: "sint64" }),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  const value = {
    value: { b: { key: "1", values: ["c", "d"] }, a: { key: "2", values: ["a", "b"] } },
    ints: { "-1": 5 },
    flags: { false: true },
    big: { "-9223372036854775808": "min" },
  };
  const bytes = encode(value);
  t.expect([...bytes.subarray(0, 32)]).toEqual([
    10, 14, 10, 1, 98, 18, 9, 10, 1, 49, 18, 1, 99, 18, 1, 100, 10, 14, 10, 1, 97, 18, 9, 10, 1, 50, 18, 1, 97, 18, 1, 98,
  ]);
  t.expect(decode(bytes)).toEqual(value);
  t.expect(decode(new Uint8Array([10, 0]))).toEqual({ value: { "": { key: "", values: [] } }, ints: {}, flags: {}, big: {} });
  t.expect(decode(new Uint8Array([18, 2, 16, 7]))).toMatchObject({ ints: { "0": 7 } });
  t.expect(() => encode({ ...value, ints: { x: 1 } })).toThrow("invalid int32 key");
});

test("protobuf stores a __proto__ map key as an own property", (t) => {
  const Message = S.schema({ map: S.record(S.int32).with(S.protobufField, 1) });
  const decode = S.decoder(S.protobuf, Message);
  const encode = S.decoder(Message, S.protobuf);
  const bytes = new Uint8Array([10, 13, 10, 9, 95, 95, 112, 114, 111, 116, 111, 95, 95, 16, 5]);
  const result = decode(bytes) as { map: Record<string, number> };
  t.expect(Object.hasOwn(result.map, "__proto__")).toBe(true);
  t.expect(Object.getPrototypeOf(result.map)).toBe(Object.prototype);
  t.expect([...encode(result)]).toEqual([...bytes]);
});

test("protobuf oneof keeps the last member on the wire and emits a zero member", (t) => {
  const Message = S.schema({
    str: S.optional(S.string).with(S.protobufField, { number: 1, oneof: "kind" }),
    num: S.optional(S.int32).with(S.protobufField, { number: 2, oneof: "kind" }),
    other: S.boolean.with(S.protobufField, 3),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  t.expect(decode(new Uint8Array([10, 1, 97, 16, 1]))).toEqual({ num: 1, other: false });
  t.expect([...encode({ num: 0, other: false })]).toEqual([16, 0]);
  t.expect(decode(encode({ str: "a", other: true }))).toEqual({ str: "a", other: true });
  t.expect(() => S.string.with(S.protobufField, { number: 1, oneof: "kind" })).toThrow("oneof member");
});

test("protobuf decodes an absent required message to its default instance and keeps optional presence", (t) => {
  const Child = S.schema({ n: S.int32.with(S.protobufField, 1), s: S.string.with(S.protobufField, 2) });
  const Message = S.schema({
    required: Child.with(S.protobufField, 1),
    maybe: S.optional(Child).with(S.protobufField, 2),
    items: S.array(Child).with(S.protobufField, 3),
  });
  const decode = S.decoder(S.protobuf, Message);
  const encode = S.decoder(Message, S.protobuf);
  t.expect(decode(new Uint8Array())).toEqual({ required: { n: 0, s: "" }, items: [] });
  t.expect([...encode({ required: { n: 0, s: "" }, items: [] })]).toEqual([10, 0]);
  t.expect(decode(new Uint8Array([18, 0]))).toEqual({ required: { n: 0, s: "" }, maybe: { n: 0, s: "" }, items: [] });
});

test("protobuf converts nested fields through the schema's own coercions", (t) => {
  const Child = S.schema({ id: S.string.with(S.protobufField, { number: 1, type: "uint32" }) });
  const Message = S.schema({ child: S.optional(Child).with(S.protobufField, 1), kids: S.array(Child).with(S.protobufField, 2) });
  const decode = S.decoder(S.protobuf, Message);
  const encode = S.decoder(Message, S.protobuf);
  const value = { child: { id: "7" }, kids: [{ id: "8" }] };
  t.expect(decode(encode(value))).toEqual(value);
  t.expect(decode(new Uint8Array([18, 2, 8, 9]))).toEqual({ kids: [{ id: "9" }] });
});

test("protobuf infers enum for a union of integer literals and keeps unknown values", (t) => {
  const Message = S.schema({
    kind: S.union([0, 1, 2]).with(S.protobufField, 1),
    maybe: S.optional(S.union([0, 5])).with(S.protobufField, 2),
    list: S.array(S.union([-1, 1])).with(S.protobufField, 3),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  t.expect([...encode({ kind: 2, maybe: 0, list: [-1, 1] })]).toEqual([8, 2, 16, 0, 26, 11, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 1]);
  t.expect(decode(encode({ kind: 2, maybe: 0, list: [-1, 1] }))).toEqual({ kind: 2, maybe: 0, list: [-1, 1] });
  t.expect(decode(new Uint8Array())).toEqual({ kind: 0, list: [] });
  t.expect(decode(new Uint8Array([8, 7]))).toEqual({ kind: 7, list: [] });
});

test("protobuf encode stays valid when a message outgrows the writer's slab", (t) => {
  const Message = S.schema({
    items: S.array(S.schema({ blob: S.uint8Array.with(S.protobufField, 1) })).with(S.protobufField, 1),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  const value = { items: Array.from({ length: 3 }, (_, i) => ({ blob: new Uint8Array(3000).fill(i + 1) })) };
  const first = encode(value);
  for (let i = 0; i < 5; i++) {
    const bytes = encode(value);
    t.expect(bytes.length).toBe(first.length);
    t.expect(decode(bytes)).toEqual(value);
  }
  t.expect(decode(first)).toEqual(value);
});

test("protobuf reports wire and value failures as Sury errors", (t) => {
  const Message = S.schema({ s: S.string.with(S.protobufField, 1), f: S.number.with(S.protobufField, { number: 2, type: "float" }) });
  const decode = S.decoder(S.protobuf, Message);
  const encode = S.decoder(Message, S.protobuf);
  const invalid = (fn: () => unknown) => {
    try {
      fn();
    } catch (error) {
      return error as S.Error;
    }
    throw new Error("expected a throw");
  };
  t.expect(invalid(() => decode(new Uint8Array([10, 1, 255])))).toBeInstanceOf(S.Error);
  t.expect(invalid(() => decode(new Uint8Array([10, 1, 255]))).code).toBe("invalid_conversion");
  t.expect(invalid(() => decode(new Uint8Array([10, 1, 255]))).message).toContain("not valid UTF-8");
  t.expect(invalid(() => decode(new Uint8Array([10]))).message).toContain("truncated protobuf message");
  t.expect(invalid(() => encode({ s: "", f: 1e40 })).message).toContain("invalid float");
  const Wrapped = S.schema({ inner: Message.with(S.protobufField, 1) });
  t.expect(invalid(() => S.decoder(S.protobuf, Wrapped)(new Uint8Array([10, 3, 10, 1, 255]))).code).toBe("invalid_conversion");
});

test("protobuf names the field that keeps a schema from being a message", (t) => {
  t.expect(() => S.decoder(S.protobuf, S.schema({ id: S.int32 }))).toThrow('field "id" has no field number');
  t.expect(() =>
    S.decoder(S.protobuf, S.schema({ a: S.int32.with(S.protobufField, 1), b: S.int32.with(S.protobufField, 1) })),
  ).toThrow('field number 1 of "b" is already taken');
  t.expect(() => S.decoder(S.protobuf, S.schema({ a: S.optional(S.array(S.int32)).with(S.protobufField, 1) }))).toThrow(
    "can't be optional",
  );
  t.expect(() => S.decoder(S.protobuf, S.schema({ a: S.optional(S.record(S.int32)).with(S.protobufField, 1) }))).toThrow(
    "can't be optional",
  );
  t.expect(() =>
    S.decoder(S.protobuf, S.schema({ a: S.string.with(S.protobufField, { number: 1, type: "message" }) })),
  ).toThrow("is a message but its schema is not an object");
});

test("protobuf requires an adjacent fully annotated object schema", (t) => {
  t.expect(() => S.parser(S.protobuf)).toThrow("Can't decode unknown to Uint8Array");
  t.expect(() => S.decoder(S.uint8Array, S.protobuf)).toThrow();
  t.expect(() => S.decoder(S.protobuf, S.string)).toThrow();
  t.expect(() => S.decoder(S.schema({ value: S.int32 }), S.protobuf)).toThrow();
  t.expect(() =>
    S.decoder(
      S.schema({ map: S.record(S.int32).with(S.protobufField, { number: 1, type: "message" }) }),
      S.protobuf,
    ),
  ).toThrow();
});

test("protobuf keeps a UTF-8 BOM as a string character", (t) => {
  const Message = S.schema({ value: field(S.string, 1, "string") });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  t.expect(decode(encode({ value: "\uFEFFhi" }))).toEqual({ value: "\uFEFFhi" });
});

test("protobuf stores __proto__ as a data property", (t) => {
  const Message = S.schema({ ["__proto__"]: field(S.int32, 1, "int32") });
  const decode = S.decoder(S.protobuf, Message);
  const result = decode(new Uint8Array([8, 1])) as { ["__proto__"]: number };
  t.expect(Object.hasOwn(result, "__proto__")).toBe(true);
  t.expect(result["__proto__"]).toBe(1);
});

test("protobuf applies array minLength on repeated message fields", (t) => {
  const Child = S.schema({ n: field(S.int32, 1, "int32") });
  const Message = S.schema({
    items: field(S.array(Child).with(S.minLength, 1), 1, "message"),
  });
  t.expect(() => S.decoder(S.protobuf, Message)(new Uint8Array())).toThrow();
});

test("protobufField validates field descriptors", (t) => {
  t.expect(() => field(S.int32, 0, "int32")).toThrow();
  t.expect(() => field(S.int32, 19000, "int32")).toThrow();
  t.expect(() => field(S.int32, 536870912, "int32")).toThrow();
  t.expect(() => field(S.int32, 1.5, "int32")).toThrow();
});

test("protobufField infers type from the schema", (t) => {
  const Child = S.schema({ n: S.int32.with(S.protobufField, 1) });
  const Message = S.schema({
    id: S.int32.with(S.protobufField, 1),
    name: S.string.with(S.protobufField, 2),
    active: S.boolean.with(S.protobufField, 3),
    blob: S.uint8Array.with(S.protobufField, 4),
    nested: Child.with(S.protobufField, 5),
    zig: S.int32.with(S.protobufField, { number: 6, type: "sint32" }),
  });
  const encode = S.decoder(Message, S.protobuf);
  const decode = S.decoder(S.protobuf, Message);
  const value = {
    id: 7,
    name: "Ada",
    active: true,
    blob: new Uint8Array([1]),
    nested: { n: 2 },
    zig: -1,
  };
  t.expect(decode(encode(value))).toEqual(value);
  t.expect(() => S.integer.with(S.protobufField, 1)).toThrow("S.protobufField requires a protobuf type");
});
