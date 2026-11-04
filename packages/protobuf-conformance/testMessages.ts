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
import * as wkt from "sury/protobuf/wkt";

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
// A double holds NaN like any other value, which `S.number` alone refuses.
const number = S.union([S.number, S.schema(NaN)]);
const dbl = (n: number, t: S.ProtobufType = "double") => f(number, n, t);
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

// Named, as ProtoJSON reads and prints them. Open in proto3: a number the
// schema does not list still decodes as that number.
const nestedEnum = S.protobufEnum({ FOO: 0, BAR: 1, BAZ: 2, NEG: -1 });
const aliasedEnum = S.protobufEnum({ ALIAS_FOO: 0, ALIAS_BAR: 1, ALIAS_BAZ: 2, MOO: 2, moo: 2, bAz: 2 });
const foreignEnum = S.protobufEnum({ FOREIGN_FOO: 0, FOREIGN_BAR: 1, FOREIGN_BAZ: 2 });
const enm = (schema: S.Schema<number, number>, n: number) => schema.with(S.protobufField, n);

// ProtoJSON refuses a key the message doesn't declare unless asked to ignore it,
// which is S.strict's answer and the default's. Built twice for that.
type Shape = Record<string, S.Schema<unknown, unknown>>;
type Message = (shape: Shape) => S.Schema<unknown, unknown>;
const lenient: Message = (shape) => S.schema(shape);
const strict: Message = (shape) => S.strict(S.schema(shape));

// The well-known types, each through the `type` that names it: a wrapper is
// an optional scalar, a Duration or Timestamp the `{ seconds, nanos }` message
// itself (a Date would drop nanos), a Struct and a ListValue JSON.
type WellKnown = S.ProtobufWellKnownType;
const wk = <TIn, TOut>(schema: S.Schema<TIn, TOut>, number: number, type: WellKnown) =>
  schema.with(S.protobufField, { number, type });
const secondsNanos = S.schema({ seconds: S.bigint, nanos: S.int32 });
// An Any holds the types the suite packs in one, each by name, which is what
// lets ProtoJSON print it. Anything else stays its type URL and bytes.
const anyOf = (self: S.Schema<unknown, unknown>) =>
  S.union(
    [
      S.schema({ type: "self", value: self }),
      ...Object.entries({
        duration: wkt.DurationSchema,
        timestamp: wkt.TimestampSchema,
        fieldMask: wkt.FieldMaskSchema,
        struct: wkt.StructSchema,
        value: wkt.ValueSchema,
        listValue: wkt.ListValueSchema,
        empty: wkt.EmptySchema,
        int32: wkt.Int32ValueSchema,
        int64: wkt.Int64ValueSchema,
        uint32: wkt.UInt32ValueSchema,
        uint64: wkt.UInt64ValueSchema,
        float: wkt.FloatValueSchema,
        double: wkt.DoubleValueSchema,
        bool: wkt.BoolValueSchema,
        string: wkt.StringValueSchema,
        bytes: wkt.BytesValueSchema,
      }).map(([type, value]) => S.schema({ type, value: value as S.Schema<unknown, unknown> })),
      S.schema({ type: "other", value: S.protobufAny }),
    ],
  );

// `corecursive` and `recursive_message` are TestAllTypesProto3 itself, so the
// message is its own definition and `NestedMessage` is declared inside it,
// where the self-reference is in scope.
const build = (message: Message): S.Schema<unknown, unknown> => S.recursive(
  "TestAllTypesProto3",
  (selfRef) => {
    const foreignMessage = message({ c: i32(1) });
    const self = selfRef.with(S.protobufTypeName, "protobuf_test_messages.proto3.TestAllTypesProto3");
    const any = anyOf(self);
    const nestedMessage = S.recursive("NestedMessage", () =>
      message({ a: i32(1), corecursive: S.optional(self).with(S.protobufField, 2) }),
    );
    return message({
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

      optional_nested_enum: enm(nestedEnum, 21),
      optional_foreign_enum: enm(foreignEnum, 22),
      optional_aliased_enum: enm(aliasedEnum, 23),

      optional_string_piece: str(24),
      optional_cord: str(25),

      recursive_message: S.optional(self).with(S.protobufField, 27),

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
      repeated_float: rep(number, 41, "float"),
      repeated_double: rep(number, 42, "double"),
      repeated_bool: rep(S.boolean, 43),
      repeated_string: rep(S.string, 44),
      repeated_bytes: rep(S.uint8Array, 45),

      repeated_nested_message: rep(nestedMessage, 48),
      repeated_foreign_message: rep(foreignMessage, 49),

      repeated_nested_enum: rep(nestedEnum, 51),
      repeated_foreign_enum: rep(foreignEnum, 52),

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
      map_int32_float: map(number, 66, "int32", "float"),
      map_int32_double: map(number, 67, "int32", "double"),
      map_bool_bool: map(S.boolean, 68, "bool"),
      map_string_string: map(S.string, 69, "string"),
      map_string_bytes: map(S.uint8Array, 70, "string"),
      map_string_nested_message: map(nestedMessage, 71, "string"),
      map_string_foreign_message: map(foreignMessage, 72, "string"),
      map_string_nested_enum: map(nestedEnum, 73, "string"),
      map_string_foreign_enum: map(foreignEnum, 74, "string"),

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
      packed_float: rep(number, 85, "float"),
      packed_double: rep(number, 86, "double"),
      packed_bool: rep(S.boolean, 87),
      packed_nested_enum: rep(nestedEnum, 88),

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
      unpacked_float: unpacked(number, 99, "float"),
      unpacked_double: unpacked(number, 100, "double"),
      unpacked_bool: unpacked(S.boolean, 101),
      unpacked_nested_enum: unpacked(nestedEnum, 102),

      oneof_uint32: S.optional(S.integer).with(S.protobufField, { number: 111, type: "uint32", oneof: "oneof_field" }),
      oneof_nested_message: S.optional(nestedMessage).with(S.protobufField, { number: 112, oneof: "oneof_field" }),
      oneof_string: S.optional(S.string).with(S.protobufField, { number: 113, oneof: "oneof_field" }),
      oneof_bytes: S.optional(S.uint8Array).with(S.protobufField, { number: 114, oneof: "oneof_field" }),
      oneof_bool: S.optional(S.boolean).with(S.protobufField, { number: 115, oneof: "oneof_field" }),
      oneof_uint64: S.optional(S.bigint).with(S.protobufField, { number: 116, type: "uint64", oneof: "oneof_field" }),
      oneof_float: S.optional(number).with(S.protobufField, { number: 117, type: "float", oneof: "oneof_field" }),
      oneof_double: S.optional(number).with(S.protobufField, { number: 118, type: "double", oneof: "oneof_field" }),
      oneof_enum: S.optional(nestedEnum).with(S.protobufField, { number: 119, oneof: "oneof_field" }),
      oneof_null_value: S.optional(wkt.NullValueSchema).with(S.protobufField, { number: 120, type: "enum", oneof: "oneof_field" }),

      optional_bool_wrapper: wk(S.optional(S.boolean), 201, "google.protobuf.BoolValue"),
      optional_int32_wrapper: wk(S.optional(S.int32), 202, "google.protobuf.Int32Value"),
      optional_int64_wrapper: wk(S.optional(S.bigint), 203, "google.protobuf.Int64Value"),
      optional_uint32_wrapper: wk(S.optional(S.integer), 204, "google.protobuf.UInt32Value"),
      optional_uint64_wrapper: wk(S.optional(S.bigint), 205, "google.protobuf.UInt64Value"),
      optional_float_wrapper: wk(S.optional(S.number), 206, "google.protobuf.FloatValue"),
      optional_double_wrapper: wk(S.optional(S.number), 207, "google.protobuf.DoubleValue"),
      optional_string_wrapper: wk(S.optional(S.string), 208, "google.protobuf.StringValue"),
      optional_bytes_wrapper: wk(S.optional(S.uint8Array), 209, "google.protobuf.BytesValue"),

      repeated_bool_wrapper: wk(S.array(S.boolean), 211, "google.protobuf.BoolValue"),
      repeated_int32_wrapper: wk(S.array(S.int32), 212, "google.protobuf.Int32Value"),
      repeated_int64_wrapper: wk(S.array(S.bigint), 213, "google.protobuf.Int64Value"),
      repeated_uint32_wrapper: wk(S.array(S.integer), 214, "google.protobuf.UInt32Value"),
      repeated_uint64_wrapper: wk(S.array(S.bigint), 215, "google.protobuf.UInt64Value"),
      repeated_float_wrapper: wk(S.array(S.number), 216, "google.protobuf.FloatValue"),
      repeated_double_wrapper: wk(S.array(S.number), 217, "google.protobuf.DoubleValue"),
      repeated_string_wrapper: wk(S.array(S.string), 218, "google.protobuf.StringValue"),
      repeated_bytes_wrapper: wk(S.array(S.uint8Array), 219, "google.protobuf.BytesValue"),

      optional_duration: wk(S.optional(secondsNanos), 301, "google.protobuf.Duration"),
      optional_timestamp: wk(S.optional(secondsNanos), 302, "google.protobuf.Timestamp"),
      optional_field_mask: wk(S.optional(S.array(S.string)), 303, "google.protobuf.FieldMask"),
      optional_struct: wk(S.optional(S.record(S.json)), 304, "google.protobuf.Struct"),
      optional_value: S.optional(S.json).with(S.protobufField, 306),
      optional_any: S.optional(any).with(S.protobufField, 305),
      optional_null_value: wkt.NullValueSchema.with(S.protobufField, { number: 307, type: "enum" }),

      repeated_duration: wk(S.array(secondsNanos), 311, "google.protobuf.Duration"),
      repeated_timestamp: wk(S.array(secondsNanos), 312, "google.protobuf.Timestamp"),
      repeated_fieldmask: wk(S.array(S.array(S.string)), 313, "google.protobuf.FieldMask"),
      repeated_any: rep(any, 315),
      repeated_value: rep(S.json, 316),
      repeated_list_value: wk(S.array(S.array(S.json)), 317, "google.protobuf.ListValue"),
      repeated_struct: wk(S.array(S.record(S.json)), 324, "google.protobuf.Struct"),

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
  },
);

export const testAllTypesProto3 = build(lenient);
export const testAllTypesProto3Strict = build(strict);
