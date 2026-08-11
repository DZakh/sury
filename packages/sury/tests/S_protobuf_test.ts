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
  t.expect(decode(new Uint8Array([248, 255, 255, 255, 15, 1, 10, 0]))).toEqual({ value: "" });
});

test("protobuf requires an adjacent fully annotated object schema", (t) => {
  t.expect(() => S.parser(S.protobuf)).toThrow("Can't decode unknown to Uint8Array");
  t.expect(() => S.decoder(S.uint8Array, S.protobuf)).toThrow();
  t.expect(() => S.decoder(S.protobuf, S.string)).toThrow();
  t.expect(() => S.decoder(S.schema({ value: S.int32 }), S.protobuf)).toThrow();
});

test("protobufField validates field descriptors", (t) => {
  t.expect(() => field(S.int32, 0, "int32")).toThrow();
  t.expect(() => field(S.int32, 19000, "int32")).toThrow();
  t.expect(() => field(S.int32, 536870912, "int32")).toThrow();
  t.expect(() => field(S.int32, 1.5, "int32")).toThrow();
});
