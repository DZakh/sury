// google/protobuf/test_messages_proto3.proto as Sury schemas - the message
// every binary conformance case is about. Written against the `.proto` in the
// pinned upstream checkout (see upstream.ts). Every run diffs this against
// that file, field number by field number - `pnpm protobuf:conformance schema`
// on its own - so a corpus bump cannot silently outrun it.
//
// Field names are the proto's own, not camelCase: the conformance runner only
// ever compares bytes, and keeping the wire names makes a failure readable
// against the `.proto` without a translation step.
import * as S from "sury";

const f = <TIn, TOut>(schema: S.Schema<TIn, TOut>, number: number, type?: S.ProtobufType) =>
  schema.with(S.protobufField, type === undefined ? number : { number, type });

// A singular scalar in proto3 has implicit presence: absent and default are the
// same value, which is exactly what a non-optional Sury field means here.
const i32 = (n: number, t: S.ProtobufType = "int32") => f(S.int32, n, t);
// uint32 and fixed32 reach 4294967295, past what `S.int32` accepts. `S.integer`
// is an integer with no 32-bit bound of its own; the wire type supplies the
// 0..4294967295 range check.
const u32 = (n: number, t: S.ProtobufType) => f(S.integer, n, t);
const i64 = (n: number, t: S.ProtobufType = "int64") => f(S.bigint, n, t);
const dbl = (n: number, t: S.ProtobufType = "double") => f(S.number, n, t);
const str = (n: number) => f(S.string, n);
const byt = (n: number) => f(S.uint8Array, n);
const bol = (n: number) => f(S.boolean, n);

const rep = <TIn, TOut>(schema: S.Schema<TIn, TOut>, number: number, type?: S.ProtobufType) =>
  S.array(schema).with(
    S.protobufField,
    type === undefined ? number : { number, type },
  );

// `[packed = false]` on the wire: a tag per item rather than one packed run.
// Decoding accepts both either way; this is about what we write.
const unpacked = <TIn, TOut>(schema: S.Schema<TIn, TOut>, number: number, type?: S.ProtobufType) =>
  S.array(schema).with(S.protobufField, { number, packed: false, ...(type ? { type } : {}) });

const map = <TIn, TOut>(
  value: S.Schema<TIn, TOut>,
  number: number,
  key: S.ProtobufType,
  type?: S.ProtobufType,
) => S.record(value).with(S.protobufField, { number, key, ...(type ? { type } : {}) });

// Enums are open in proto3: a number the schema does not list decodes as that
// number, so an `int32` with an `enum` wire type is the whole of it.
const enm = (n: number) => f(S.int32, n, "enum");

export const nestedMessage = S.schema({
  a: i32(1),
  // `corecursive` is TestAllTypesProto3 - a cycle S.protobuf cannot express, so
  // it stays undeclared and rides through as an unknown field. Named in the
  // failure list.
});

export const foreignMessage = S.schema({ c: i32(1) });

// The google.protobuf wrappers are ordinary one-field messages on the wire.
const wrapper = <TIn, TOut>(value: S.Schema<TIn, TOut>, type?: S.ProtobufType) =>
  S.schema({ value: f(value, 1, type) });

const boolValue = wrapper(S.boolean);
const int32Value = wrapper(S.int32);
const int64Value = wrapper(S.bigint);
const uint32Value = wrapper(S.integer, "uint32");
const uint64Value = wrapper(S.bigint, "uint64");
const floatValue = wrapper(S.number, "float");
const doubleValue = wrapper(S.number);
const stringValue = wrapper(S.string);
const bytesValue = wrapper(S.uint8Array);

const duration = S.schema({ seconds: i64(1), nanos: i32(2) });
const timestamp = S.schema({ seconds: i64(1), nanos: i32(2) });
const fieldMask = S.schema({ paths: rep(S.string, 1) });
// Any is two scalars on the wire; its payload stays opaque bytes, which is all
// the binary suite needs.
const any = S.schema({ type_url: str(1), value: byt(2) });

export const testAllTypesProto3 = S.schema({
  optional_int32: i32(1),
  optional_int64: i64(2),
  optional_uint32: u32(3, "uint32"),
  optional_uint64: i64(4, "uint64"),
  optional_sint32: i32(5, "sint32"),
  optional_sint64: i64(6, "sint64"),
  optional_fixed32: u32(7, "fixed32"),
  optional_fixed64: i64(8, "fixed64"),
  optional_sfixed32: i32(9, "sfixed32"),
  optional_sfixed64: i64(10, "sfixed64"),
  optional_float: dbl(11, "float"),
  optional_double: dbl(12),
  optional_bool: bol(13),
  optional_string: str(14),
  optional_bytes: byt(15),

  optional_nested_message: S.optional(nestedMessage).with(S.protobufField, 18),
  optional_foreign_message: S.optional(foreignMessage).with(S.protobufField, 19),

  optional_nested_enum: enm(21),
  optional_foreign_enum: enm(22),
  optional_aliased_enum: enm(23),

  optional_string_piece: str(24),
  optional_cord: str(25),

  // recursive_message = 27 is TestAllTypesProto3 itself. Undeclared, so it
  // reads as an unknown field; see the failure list.

  repeated_int32: rep(S.int32, 31),
  repeated_int64: rep(S.bigint, 32),
  repeated_uint32: rep(S.integer, 33, "uint32"),
  repeated_uint64: rep(S.bigint, 34, "uint64"),
  repeated_sint32: rep(S.int32, 35, "sint32"),
  repeated_sint64: rep(S.bigint, 36, "sint64"),
  repeated_fixed32: rep(S.integer, 37, "fixed32"),
  repeated_fixed64: rep(S.bigint, 38, "fixed64"),
  repeated_sfixed32: rep(S.int32, 39, "sfixed32"),
  repeated_sfixed64: rep(S.bigint, 40, "sfixed64"),
  repeated_float: rep(S.number, 41, "float"),
  repeated_double: rep(S.number, 42),
  repeated_bool: rep(S.boolean, 43),
  repeated_string: rep(S.string, 44),
  repeated_bytes: rep(S.uint8Array, 45),

  repeated_nested_message: rep(nestedMessage, 48),
  repeated_foreign_message: rep(foreignMessage, 49),

  repeated_nested_enum: rep(S.int32, 51, "enum"),
  repeated_foreign_enum: rep(S.int32, 52, "enum"),

  repeated_string_piece: rep(S.string, 54),
  repeated_cord: rep(S.string, 55),

  map_int32_int32: map(S.int32, 56, "int32"),
  map_int64_int64: map(S.bigint, 57, "int64"),
  map_uint32_uint32: map(S.integer, 58, "uint32", "uint32"),
  map_uint64_uint64: map(S.bigint, 59, "uint64", "uint64"),
  map_sint32_sint32: map(S.int32, 60, "sint32", "sint32"),
  map_sint64_sint64: map(S.bigint, 61, "sint64", "sint64"),
  map_fixed32_fixed32: map(S.integer, 62, "fixed32", "fixed32"),
  map_fixed64_fixed64: map(S.bigint, 63, "fixed64", "fixed64"),
  map_sfixed32_sfixed32: map(S.int32, 64, "sfixed32", "sfixed32"),
  map_sfixed64_sfixed64: map(S.bigint, 65, "sfixed64", "sfixed64"),
  map_int32_float: map(S.number, 66, "int32", "float"),
  map_int32_double: map(S.number, 67, "int32"),
  map_bool_bool: map(S.boolean, 68, "bool"),
  map_string_string: map(S.string, 69, "string"),
  map_string_bytes: map(S.uint8Array, 70, "string"),
  map_string_nested_message: map(nestedMessage, 71, "string"),
  map_string_foreign_message: map(foreignMessage, 72, "string"),
  map_string_nested_enum: map(S.int32, 73, "string", "enum"),
  map_string_foreign_enum: map(S.int32, 74, "string", "enum"),

  packed_int32: rep(S.int32, 75),
  packed_int64: rep(S.bigint, 76),
  packed_uint32: rep(S.integer, 77, "uint32"),
  packed_uint64: rep(S.bigint, 78, "uint64"),
  packed_sint32: rep(S.int32, 79, "sint32"),
  packed_sint64: rep(S.bigint, 80, "sint64"),
  packed_fixed32: rep(S.integer, 81, "fixed32"),
  packed_fixed64: rep(S.bigint, 82, "fixed64"),
  packed_sfixed32: rep(S.int32, 83, "sfixed32"),
  packed_sfixed64: rep(S.bigint, 84, "sfixed64"),
  packed_float: rep(S.number, 85, "float"),
  packed_double: rep(S.number, 86),
  packed_bool: rep(S.boolean, 87),
  packed_nested_enum: rep(S.int32, 88, "enum"),

  unpacked_int32: unpacked(S.int32, 89),
  unpacked_int64: unpacked(S.bigint, 90),
  unpacked_uint32: unpacked(S.integer, 91, "uint32"),
  unpacked_uint64: unpacked(S.bigint, 92, "uint64"),
  unpacked_sint32: unpacked(S.int32, 93, "sint32"),
  unpacked_sint64: unpacked(S.bigint, 94, "sint64"),
  unpacked_fixed32: unpacked(S.integer, 95, "fixed32"),
  unpacked_fixed64: unpacked(S.bigint, 96, "fixed64"),
  unpacked_sfixed32: unpacked(S.int32, 97, "sfixed32"),
  unpacked_sfixed64: unpacked(S.bigint, 98, "sfixed64"),
  unpacked_float: unpacked(S.number, 99, "float"),
  unpacked_double: unpacked(S.number, 100),
  unpacked_bool: unpacked(S.boolean, 101),
  unpacked_nested_enum: unpacked(S.int32, 102, "enum"),

  oneof_uint32: S.optional(S.integer).with(S.protobufField, { number: 111, type: "uint32", oneof: "oneof_field" }),
  oneof_nested_message: S.optional(nestedMessage).with(S.protobufField, { number: 112, oneof: "oneof_field" }),
  oneof_string: S.optional(S.string).with(S.protobufField, { number: 113, oneof: "oneof_field" }),
  oneof_bytes: S.optional(S.uint8Array).with(S.protobufField, { number: 114, oneof: "oneof_field" }),
  oneof_bool: S.optional(S.boolean).with(S.protobufField, { number: 115, oneof: "oneof_field" }),
  oneof_uint64: S.optional(S.bigint).with(S.protobufField, { number: 116, type: "uint64", oneof: "oneof_field" }),
  oneof_float: S.optional(S.number).with(S.protobufField, { number: 117, type: "float", oneof: "oneof_field" }),
  oneof_double: S.optional(S.number).with(S.protobufField, { number: 118, oneof: "oneof_field" }),
  oneof_enum: S.optional(S.int32).with(S.protobufField, { number: 119, type: "enum", oneof: "oneof_field" }),
  oneof_null_value: S.optional(S.int32).with(S.protobufField, { number: 120, type: "enum", oneof: "oneof_field" }),

  optional_bool_wrapper: S.optional(boolValue).with(S.protobufField, 201),
  optional_int32_wrapper: S.optional(int32Value).with(S.protobufField, 202),
  optional_int64_wrapper: S.optional(int64Value).with(S.protobufField, 203),
  optional_uint32_wrapper: S.optional(uint32Value).with(S.protobufField, 204),
  optional_uint64_wrapper: S.optional(uint64Value).with(S.protobufField, 205),
  optional_float_wrapper: S.optional(floatValue).with(S.protobufField, 206),
  optional_double_wrapper: S.optional(doubleValue).with(S.protobufField, 207),
  optional_string_wrapper: S.optional(stringValue).with(S.protobufField, 208),
  optional_bytes_wrapper: S.optional(bytesValue).with(S.protobufField, 209),

  repeated_bool_wrapper: rep(boolValue, 211),
  repeated_int32_wrapper: rep(int32Value, 212),
  repeated_int64_wrapper: rep(int64Value, 213),
  repeated_uint32_wrapper: rep(uint32Value, 214),
  repeated_uint64_wrapper: rep(uint64Value, 215),
  repeated_float_wrapper: rep(floatValue, 216),
  repeated_double_wrapper: rep(doubleValue, 217),
  repeated_string_wrapper: rep(stringValue, 218),
  repeated_bytes_wrapper: rep(bytesValue, 219),

  optional_duration: S.optional(duration).with(S.protobufField, 301),
  optional_timestamp: S.optional(timestamp).with(S.protobufField, 302),
  optional_field_mask: S.optional(fieldMask).with(S.protobufField, 303),
  // optional_struct = 304, optional_value = 306, repeated_struct = 324,
  // repeated_value = 316 and repeated_list_value = 317 are Struct/Value/
  // ListValue, which are mutually recursive. Undeclared; see the failure list.
  optional_any: S.optional(any).with(S.protobufField, 305),
  optional_null_value: enm(307),

  repeated_duration: rep(duration, 311),
  repeated_timestamp: rep(timestamp, 312),
  repeated_fieldmask: rep(fieldMask, 313),
  repeated_any: rep(any, 315),

  fieldname1: i32(401),
  field_name2: i32(402),
  _field_name3: i32(403),
  field__name4_: i32(404),
  field0name5: i32(405),
  field_0_name6: i32(406),
  fieldName7: i32(407),
  FieldName8: i32(408),
  field_Name9: i32(409),
  Field_Name10: i32(410),
  FIELD_NAME11: i32(411),
  FIELD_name12: i32(412),
  __field_name13: i32(413),
  __Field_name14: i32(414),
  field__name15: i32(415),
  field__Name16: i32(416),
  field_name17__: i32(417),
  Field_name18__: i32(418),
});
