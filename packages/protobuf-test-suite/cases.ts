import * as S from "sury";
import {
  type Bytes,
  dbl,
  delim,
  field as wireField,
  flt,
  group,
  INT32_MAX,
  INT32_MIN,
  INT64_MAX,
  INT64_MIN,
  len,
  longvarint,
  str,
  tag,
  u32,
  u64,
  UINT32_MAX,
  UINT64_MAX,
  utf8,
  varint,
  zz32,
  zz64,
} from "./wire";

export type FieldDef = {
  key: string;
  number: number;
  type: S.ProtobufType;
  repeated?: boolean;
  optional?: boolean;
  packed?: boolean;
  // Key type of a map field; `type` is then the value type.
  map?: S.ProtobufType;
  oneof?: string;
  fields?: FieldDef[];
};

export type RoundTripCase = {
  id: string;
  fields: FieldDef[];
  value: Record<string, unknown>;
  wire?: Bytes;
};

export type DecodeOnlyCase = {
  id: string;
  fields: FieldDef[];
  wire: Bytes;
  value: Record<string, unknown>;
  // Bytes the decoded value must encode back to (the suite's
  // `ValidDataScalarBinary` and `*.PackedOutput` families).
  reencoded?: Bytes;
};

export type RejectCase = {
  id: string;
  fields: FieldDef[];
  wire: Bytes;
};

const field = (
  key: string,
  number: number,
  type: S.ProtobufType,
  extra: Partial<FieldDef> = {},
): FieldDef => ({ key, number, type, ...extra });

// Field numbers follow TestAllTypesProto3 so a case id maps onto the
// conformance test it mirrors.
type Scalar = Exclude<S.ProtobufType, "message">;
const scalarTypes: Scalar[] = [
  "int32",
  "int64",
  "uint32",
  "uint64",
  "sint32",
  "sint64",
  "fixed32",
  "fixed64",
  "sfixed32",
  "sfixed64",
  "float",
  "double",
  "bool",
  "string",
  "bytes",
  "enum",
];
const singularNumber = (type: S.ProtobufType): number =>
  type === "message" ? 18 : type === "enum" ? 21 : scalarTypes.indexOf(type as Scalar) + 1;
const repeatedNumber = (type: S.ProtobufType): number =>
  type === "message" ? 48 : type === "enum" ? 51 : singularNumber(type) + 30;
const unpackedNumber = (type: S.ProtobufType): number => (type === "enum" ? 102 : singularNumber(type) + 88);
const UNKNOWN_FIELD = 666;

const wireOf = (type: S.ProtobufType): number => {
  if (type === "double" || type === "fixed64" || type === "sfixed64") return 1;
  if (type === "string" || type === "bytes" || type === "message") return 2;
  if (type === "float" || type === "fixed32" || type === "sfixed32") return 5;
  return 0;
};

const packable = (type: S.ProtobufType): boolean => wireOf(type) !== 2;

const nested: FieldDef[] = [field("a", 1, "int32")];

const singular = (type: S.ProtobufType): FieldDef =>
  field(type, singularNumber(type), type, type === "message" ? { fields: nested } : {});
const repeated = (type: S.ProtobufType): FieldDef =>
  field(`repeated_${type}`, repeatedNumber(type), type, {
    repeated: true,
    ...(type === "message" ? { fields: nested } : {}),
  });
const unpacked = (type: S.ProtobufType): FieldDef =>
  field(`unpacked_${type}`, unpackedNumber(type), type, { repeated: true, packed: false });

// ValidDataScalar tables: [input bytes, decoded value, canonical bytes].
type Row = [Bytes, unknown, Bytes];
const same = (bytes: Bytes, value: unknown): Row => [bytes, value, bytes];

const scalarRows: Record<Scalar, Row[]> = {
  double: [
    same(dbl(0), 0),
    same(dbl(0.1), 0.1),
    same(dbl(1.7976931348623157e308), 1.7976931348623157e308),
    same(dbl(2.2250738585072014e-308), 2.2250738585072014e-308),
  ],
  float: [
    same(flt(0), 0),
    same(flt(0.1), Math.fround(0.1)),
    same(flt(1.00000075e-36), Math.fround(1.00000075e-36)),
    same(flt(3.402823e38), Math.fround(3.402823e38)),
    same(flt(1.17549435e-38), Math.fround(1.17549435e-38)),
  ],
  int64: [
    same(varint(0), 0n),
    same(varint(12345), 12345n),
    same(varint(INT64_MAX), INT64_MAX),
    same(varint(INT64_MIN), INT64_MIN),
  ],
  uint64: [same(varint(0), 0n), same(varint(12345), 12345n), same(varint(UINT64_MAX), UINT64_MAX)],
  int32: [
    same(varint(0), 0),
    same(varint(12345), 12345),
    [longvarint(12345, 2), 12345, varint(12345)],
    [longvarint(12345, 7), 12345, varint(12345)],
    same(varint(INT32_MAX), INT32_MAX),
    same(varint(INT32_MIN), INT32_MIN),
    [varint(1n << 33n), 0, varint(0)],
    [varint((1n << 33n) - 1n), -1, varint(-1)],
    [varint(INT64_MAX), -1, varint(-1)],
    [varint(INT64_MIN + 1n), 1, varint(1)],
  ],
  uint32: [
    same(varint(0), 0),
    same(varint(12345), 12345),
    [longvarint(12345, 2), 12345, varint(12345)],
    [longvarint(12345, 7), 12345, varint(12345)],
    same(varint(UINT32_MAX), UINT32_MAX),
    [varint(1n << 33n), 0, varint(0)],
    [varint((1n << 33n) + 1n), 1, varint(1)],
    [varint((1n << 33n) - 1n), UINT32_MAX, varint(UINT32_MAX)],
    [varint(INT64_MAX), UINT32_MAX, varint(UINT32_MAX)],
    [varint(INT64_MIN + 1n), 1, varint(1)],
  ],
  fixed64: [same(u64(0n), 0n), same(u64(12345n), 12345n), same(u64(UINT64_MAX), UINT64_MAX)],
  fixed32: [same(u32(0), 0), same(u32(12345), 12345), same(u32(UINT32_MAX), UINT32_MAX)],
  sfixed64: [
    same(u64(0n), 0n),
    same(u64(12345n), 12345n),
    same(u64(INT64_MAX), INT64_MAX),
    same(u64(INT64_MIN), INT64_MIN),
  ],
  sfixed32: [
    same(u32(0), 0),
    same(u32(12345), 12345),
    same(u32(INT32_MAX), INT32_MAX),
    same(u32(INT32_MIN), INT32_MIN),
  ],
  bool: [
    same(varint(0), false),
    same(varint(1), true),
    [varint(-1), true, varint(1)],
    [varint(12345678), true, varint(1)],
    [varint(1n << 33n), true, varint(1)],
    [varint(INT64_MAX), true, varint(1)],
    [varint(INT64_MIN), true, varint(1)],
  ],
  sint32: [
    same(zz32(0), 0),
    same(zz32(12345), 12345),
    same(zz32(INT32_MAX), INT32_MAX),
    same(zz32(INT32_MIN), INT32_MIN),
    [zz64((1n << 31n) + 1n), 1, zz32(1)],
  ],
  sint64: [
    same(zz64(0n), 0n),
    same(zz64(12345n), 12345n),
    same(zz64(INT64_MAX), INT64_MAX),
    same(zz64(INT64_MIN), INT64_MIN),
  ],
  string: [
    same(str(""), ""),
    same(str("Hello world!"), "Hello world!"),
    same(str("'\"?\\\x07\b\f\n\r\t\v"), "'\"?\\\x07\b\f\n\r\t\v"),
    same(str("谷歌"), "谷歌"),
    same(str("😁"), "😁"),
  ],
  bytes: [
    same(delim([]), new Uint8Array()),
    same(delim(utf8("Hello world!")), new Uint8Array(utf8("Hello world!"))),
    same(delim([1, 2]), new Uint8Array([1, 2])),
    same(delim([0xfb]), new Uint8Array([0xfb])),
  ],
  enum: [
    same(varint(0), 0),
    same(varint(1), 1),
    same(varint(2), 2),
    same(varint(-1), -1),
    [varint(INT64_MAX), -1, varint(-1)],
    [varint(INT64_MIN + 1n), 1, varint(1)],
  ],
};

const isDefault = (type: Scalar, value: unknown): boolean =>
  value === 0 || value === 0n || value === false || value === "" || (value instanceof Uint8Array && value.length === 0);

export const officialVectors: RoundTripCase[] = [
  {
    id: "official/string-bom",
    fields: [field("b", 1, "string")],
    value: { b: "﻿" },
    wire: [0x0a, 0x03, 0xef, 0xbb, 0xbf],
  },
  {
    id: "official/int32-150",
    fields: [field("a", 1, "int32")],
    value: { a: 150 },
    wire: [0x08, 0x96, 0x01],
  },
  {
    id: "official/string-testing",
    fields: [field("b", 2, "string")],
    value: { b: "testing" },
    wire: [0x12, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67],
  },
  {
    id: "official/embedded-message",
    fields: [field("c", 3, "message", { fields: [field("a", 1, "int32")] })],
    value: { c: { a: 150 } },
    wire: [0x1a, 0x03, 0x08, 0x96, 0x01],
  },
  {
    id: "official/long-tag-max-field-number",
    fields: [field("val", 0x1fffffff, "uint32")],
    value: { val: 1 },
    wire: [0xf8, 0xff, 0xff, 0xff, 0x0f, 0x01],
  },
];

// ValidDataScalar / ValidDataScalarBinary: every table row decodes to its
// value and re-encodes canonically; a proto3 zero re-encodes to nothing.
export const validDataScalar: DecodeOnlyCase[] = scalarTypes.flatMap((type) =>
  scalarRows[type].map(([input, value, canonical], i): DecodeOnlyCase => ({
    id: `ValidDataScalar.${type}[${i}]`,
    fields: [singular(type)],
    wire: [...tag(singularNumber(type), wireOf(type)), ...input],
    value: { [type]: value },
    reencoded: isDefault(type, value) ? [] : [...tag(singularNumber(type), wireOf(type)), ...canonical],
  })),
);

export const validDataMessage: DecodeOnlyCase[] = [
  {
    id: "ValidDataScalar.message[0]",
    fields: [singular("message")],
    wire: [...len(18)],
    value: { message: { a: 0 } },
    reencoded: [...len(18)],
  },
  {
    id: "ValidDataScalar.message[1]",
    fields: [singular("message")],
    wire: [...len(18, wireField(1, 0, varint(1234)))],
    value: { message: { a: 1234 } },
    reencoded: [...len(18, wireField(1, 0, varint(1234)))],
  },
];

// RepeatedScalarSelectsLast: every row on one singular field; the last wins.
export const selectsLast: DecodeOnlyCase[] = scalarTypes.map((type) => {
  const rows = scalarRows[type];
  const last = rows[rows.length - 1]!;
  return {
    id: `RepeatedScalarSelectsLast.${type}`,
    fields: [singular(type)],
    wire: rows.flatMap(([input]) => [...tag(singularNumber(type), wireOf(type)), ...input]),
    value: { [type]: last[1] },
    reencoded: [...tag(singularNumber(type), wireOf(type)), ...last[2]],
  };
});

// ValidDataRepeated: packed and unpacked input both decode; a packed field
// always re-encodes packed, an unpacked one always expanded.
export const validDataRepeated: DecodeOnlyCase[] = scalarTypes.flatMap((type): DecodeOnlyCase[] => {
  const rows = scalarRows[type];
  const values = rows.map(([, value]) => value);
  const wire = wireOf(type);
  if (!packable(type)) {
    return [
      {
        id: `ValidDataRepeated.${type}`,
        fields: [repeated(type)],
        wire: rows.flatMap(([input]) => [...tag(repeatedNumber(type), 2), ...input]),
        value: { [`repeated_${type}`]: values },
        reencoded: rows.flatMap(([, , canonical]) => [...tag(repeatedNumber(type), 2), ...canonical]),
      },
    ];
  }
  const rep = repeatedNumber(type);
  const unp = unpackedNumber(type);
  const unpackedInput = (n: number) => rows.flatMap(([input]) => [...tag(n, wire), ...input]);
  const packedInput = (n: number) => [...tag(n, 2), ...delim(...rows.map(([input]) => input))];
  const packedOutput = [...tag(rep, 2), ...delim(...rows.map(([, , canonical]) => canonical))];
  const unpackedOutput = rows.flatMap(([, , canonical]) => [...tag(unp, wire), ...canonical]);
  return [
    {
      id: `ValidDataRepeated.${type}.UnpackedInput.PackedOutput`,
      fields: [repeated(type)],
      wire: unpackedInput(rep),
      value: { [`repeated_${type}`]: values },
      reencoded: packedOutput,
    },
    {
      id: `ValidDataRepeated.${type}.PackedInput.PackedOutput`,
      fields: [repeated(type)],
      wire: packedInput(rep),
      value: { [`repeated_${type}`]: values },
      reencoded: packedOutput,
    },
    {
      id: `ValidDataRepeated.${type}.UnpackedInput.UnpackedOutput`,
      fields: [unpacked(type)],
      wire: unpackedInput(unp),
      value: { [`unpacked_${type}`]: values },
      reencoded: unpackedOutput,
    },
    {
      id: `ValidDataRepeated.${type}.PackedInput.UnpackedOutput`,
      fields: [unpacked(type)],
      wire: packedInput(unp),
      value: { [`unpacked_${type}`]: values },
      reencoded: unpackedOutput,
    },
  ];
});

const corecursive: FieldDef[] = [
  field("optional_int32", 1, "int32"),
  field("optional_int64", 2, "int64"),
  field("optional_uint32", 3, "uint32"),
  field("repeated_int32", 31, "int32", { repeated: true }),
];
const nestedWithCorecursive: FieldDef[] = [
  field("a", 1, "int32"),
  field("corecursive", 2, "message", { optional: true, fields: corecursive }),
];

export const mergeCases: DecodeOnlyCase[] = [
  {
    id: "RepeatedScalarMessageMerge",
    fields: [field("optional_nested_message", 18, "message", { fields: nestedWithCorecursive })],
    wire: [
      ...len(18, len(2, wireField(1, 0, varint(1234)), wireField(2, 0, varint(1234)), wireField(31, 0, varint(1234)))),
      ...len(18, len(2, wireField(1, 0, varint(4321)), wireField(3, 0, varint(4321)), wireField(31, 0, varint(4321)))),
    ],
    value: {
      optional_nested_message: {
        a: 0,
        corecursive: {
          optional_int32: 4321,
          optional_int64: 1234n,
          optional_uint32: 4321,
          repeated_int32: [1234, 4321],
        },
      },
    },
  },
  {
    id: "ValidDataRepeated.message",
    fields: [repeated("message")],
    wire: [...len(48), ...len(48, wireField(1, 0, varint(1234)))],
    value: { repeated_message: [{ a: 0 }, { a: 1234 }] },
    reencoded: [...len(48), ...len(48, wireField(1, 0, varint(1234)))],
  },
  {
    id: "UnknownVarint.stripped",
    fields: [singular("int32")],
    wire: [...wireField(501, 0, varint(1))],
    value: { int32: 0 },
    reencoded: [],
  },
  {
    id: "UnknownOrdering.stripped",
    fields: [singular("int32")],
    wire: [
      ...len(UNKNOWN_FIELD, utf8("abc")),
      ...wireField(UNKNOWN_FIELD, 0, varint(123)),
      ...len(UNKNOWN_FIELD, utf8("def")),
      ...wireField(UNKNOWN_FIELD, 0, varint(456)),
    ],
    value: { int32: 0 },
    reencoded: [],
  },
  {
    id: "UnknownGroup.skipped",
    fields: [singular("int32")],
    wire: [
      ...group(1234, wireField(1, 5, u32(7)), group(5, wireField(6, 1, u64(1n))), len(2, utf8("x"))),
      ...wireField(1, 0, varint(9)),
    ],
    value: { int32: 9 },
    reencoded: [...wireField(1, 0, varint(9))],
  },
];

export const scalarCases: RoundTripCase[] = [
  ...scalarTypes.map((type): RoundTripCase => {
    const rows = scalarRows[type];
    const row = rows.find(([, value]) => !isDefault(type, value))!;
    return {
      id: `scalar/${type}`,
      fields: [singular(type)],
      value: { [type]: row[1] },
      wire: [...tag(singularNumber(type), wireOf(type)), ...row[2]],
    };
  }),
  {
    id: "scalar/int32-negative-ten-bytes",
    fields: [field("a", 1, "int32")],
    value: { a: -1 },
    wire: [8, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1],
  },
  {
    id: "scalar/enum-negative-ten-bytes",
    fields: [field("a", 1, "enum")],
    value: { a: -1 },
    wire: [8, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1],
  },
  {
    id: "scalar/uint32-max",
    fields: [field("a", 1, "uint32")],
    value: { a: 4294967295 },
    wire: [8, 255, 255, 255, 255, 15],
  },
  {
    id: "scalar/sint32-minus-one",
    fields: [field("a", 1, "sint32")],
    value: { a: -1 },
    wire: [8, 1],
  },
  {
    id: "scalar/int64-min",
    fields: [field("a", 1, "int64")],
    value: { a: -9223372036854775808n },
    wire: [8, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1],
  },
  {
    id: "scalar/uint64-six-bytes",
    fields: [field("a", 1, "uint64")],
    value: { a: 549755813887n },
    wire: [8, 255, 255, 255, 255, 255, 15],
  },
  {
    id: "scalar/sint64-min",
    fields: [field("a", 1, "sint64")],
    value: { a: -9223372036854775808n },
    wire: [8, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1],
  },
  {
    id: "scalar/fixed64-grpc",
    fields: [field("int_64", 1, "fixed64")],
    value: { int_64: 314159265358979n },
    wire: [9, ...u64(314159265358979n)],
  },
  {
    id: "scalar/sfixed64-grpc",
    fields: [field("int_64", 1, "sfixed64")],
    value: { int_64: -9095674951825889465n },
    wire: [9, ...u64(-9095674951825889465n)],
  },
  {
    id: "scalar/sfixed32-minus-two",
    fields: [field("a", 1, "sfixed32")],
    value: { a: -2 },
    wire: [13, 254, 255, 255, 255],
  },
  {
    id: "scalar/string-utf8-two-byte",
    fields: [field("a", 1, "string")],
    value: { a: "ä" },
    wire: [10, 2, 195, 164],
  },
  {
    id: "scalar/string-ascii-long",
    fields: [field("a", 1, "string")],
    value: { a: "x".repeat(5000) },
  },
  {
    id: "scalar/string-utf8-two-byte-length-boundary",
    fields: [field("a", 1, "string")],
    value: { a: "ä".repeat(64) },
  },
  {
    id: "scalar/string-surrogate-pairs",
    fields: [field("a", 1, "string")],
    value: { a: "😀".repeat(32) },
  },
  {
    id: "scalar/string-mixed-long",
    fields: [field("a", 1, "string")],
    value: { a: "🎉mixed-ä-€-text".repeat(20) },
  },
  {
    id: "scalar/bytes-300",
    fields: [field("a", 1, "bytes")],
    value: { a: new Uint8Array(Array.from({ length: 300 }, (_, i) => (i * 5) & 255)) },
  },
];

export const presenceCases: RoundTripCase[] = [
  {
    id: "presence/required-default-omitted",
    fields: [field("value", 1, "int32")],
    value: { value: 0 },
    wire: [],
  },
  {
    id: "presence/optional-zero-emitted",
    fields: [field("value", 1, "int32", { optional: true })],
    value: { value: 0 },
    wire: [0x08, 0x00],
  },
  {
    id: "presence/optional-absent",
    fields: [field("value", 1, "int32", { optional: true })],
    value: {},
    wire: [],
  },
  {
    id: "presence/empty-string-omitted",
    fields: [field("value", 1, "string")],
    value: { value: "" },
    wire: [],
  },
  {
    id: "presence/empty-bytes-omitted",
    fields: [field("value", 1, "bytes")],
    value: { value: new Uint8Array() },
    wire: [],
  },
  {
    id: "presence/false-omitted",
    fields: [field("value", 1, "bool")],
    value: { value: false },
    wire: [],
  },
  {
    id: "presence/zero-bigint-omitted",
    fields: [field("value", 1, "int64")],
    value: { value: 0n },
    wire: [],
  },
  {
    id: "presence/zero-double-omitted",
    fields: [field("value", 1, "double")],
    value: { value: 0 },
    wire: [],
  },
  {
    id: "presence/empty-message-emitted",
    fields: [field("value", 1, "message", { fields: nested })],
    value: { value: { a: 0 } },
    wire: [0x0a, 0x00],
  },
  {
    id: "presence/optional-empty-string-emitted",
    fields: [field("value", 1, "string", { optional: true })],
    value: { value: "" },
    wire: [0x0a, 0x00],
  },
  {
    id: "presence/optional-empty-bytes-emitted",
    fields: [field("value", 1, "bytes", { optional: true })],
    value: { value: new Uint8Array() },
    wire: [0x0a, 0x00],
  },
];

export const repeatedCases: RoundTripCase[] = [
  {
    id: "repeated/packed-sint32",
    fields: [field("values", 1, "sint32", { repeated: true })],
    value: { values: [-1, 0, 2] },
    wire: [0x0a, 0x03, 0x01, 0x00, 0x04],
  },
  {
    id: "repeated/packed-uint32",
    fields: [field("a", 1, "uint32", { repeated: true })],
    value: { a: [1, 2, 3] },
    wire: [0x0a, 0x03, 0x01, 0x02, 0x03],
  },
  {
    id: "repeated/unpacked-uint32",
    fields: [field("a", 1, "uint32", { repeated: true, packed: false })],
    value: { a: [1, 2, 3] },
    wire: [0x08, 0x01, 0x08, 0x02, 0x08, 0x03],
  },
  {
    id: "repeated/packed-int32-negative",
    fields: [field("a", 1, "int32", { repeated: true })],
    value: { a: [0, 1, -1, 2147483647, -2147483648, 127, 128, -2] },
  },
  {
    id: "repeated/packed-bool",
    fields: [field("a", 1, "bool", { repeated: true })],
    value: { a: [true, false, true, true, false, true] },
    wire: [0x0a, 0x06, 1, 0, 1, 1, 0, 1],
  },
  {
    id: "repeated/packed-fixed32",
    fields: [field("a", 1, "fixed32", { repeated: true })],
    value: { a: [0, 1, 4294967295, 123456, 255] },
  },
  {
    id: "repeated/packed-sfixed32",
    fields: [field("a", 1, "sfixed32", { repeated: true })],
    value: { a: [0, -1, 1, 2147483647, -2147483648] },
  },
  {
    id: "repeated/packed-float",
    fields: [field("a", 1, "float", { repeated: true })],
    value: { a: [0, 1.5, -2.25, 0.125, 100] },
  },
  {
    id: "repeated/packed-double",
    fields: [field("a", 1, "double", { repeated: true })],
    value: { a: [0, 1.5, -2.25, 204.8, 12345.678] },
  },
  {
    id: "repeated/packed-uint64",
    fields: [field("a", 1, "uint64", { repeated: true })],
    value: { a: [0n, 1n, 1000n, 4294967295n, 123456789n, 18446744073709551615n] },
  },
  {
    id: "repeated/packed-int64",
    fields: [field("a", 1, "int64", { repeated: true })],
    value: { a: [0n, 1n, -1n, 1000n, 4294967295n, -9223372036854775808n] },
  },
  {
    id: "repeated/packed-sint64",
    fields: [field("a", 1, "sint64", { repeated: true })],
    value: { a: [0n, 1n, -1n, 1000n, -1000n, 123456789n] },
  },
  {
    id: "repeated/packed-fixed64",
    fields: [field("a", 1, "fixed64", { repeated: true })],
    value: { a: [0n, 1n, 4294967295n, 123456789n] },
  },
  {
    id: "repeated/packed-sfixed64",
    fields: [field("a", 1, "sfixed64", { repeated: true })],
    value: { a: [0n, -1n, 1n, 123456789n] },
  },
  {
    id: "repeated/packed-double-unaligned",
    fields: [field("s", 1, "string"), field("a", 2, "double", { repeated: true })],
    value: { s: "x", a: [0.5, -1.25, 1e300, Number.NaN, -0] },
  },
  {
    id: "repeated/packed-double-aligned",
    fields: [field("s", 1, "string"), field("a", 2, "double", { repeated: true })],
    value: { s: "abcd", a: [0.5, -1.25, 1e300] },
  },
  {
    id: "repeated/packed-fixed64-unaligned",
    fields: [field("s", 1, "string"), field("a", 2, "fixed64", { repeated: true })],
    value: { s: "x", a: [0n, 1n, 18446744073709551615n] },
  },
  {
    id: "repeated/packed-sfixed64-aligned",
    fields: [field("s", 1, "string"), field("a", 2, "sfixed64", { repeated: true })],
    value: { s: "abcd", a: [-1n, 9223372036854775807n, -9223372036854775808n] },
  },
  {
    id: "repeated/packed-float-unaligned",
    fields: [field("s", 1, "string"), field("a", 2, "float", { repeated: true })],
    value: { s: "xy", a: [0.5, -1.25, 3.4028234663852886e38] },
  },
  {
    id: "repeated/packed-sfixed32-aligned",
    fields: [field("a", 1, "sfixed32", { repeated: true })],
    value: { a: [-1, 2147483647, -2147483648, 0] },
  },
  {
    id: "repeated/packed-300-elements",
    fields: [field("a", 1, "int32", { repeated: true })],
    value: { a: Array.from({ length: 300 }, (_, i) => [0, 1, -1, 2147483647, -2147483648, 127, 128, -2][i % 8]!) },
  },
  {
    id: "repeated/strings",
    fields: [field("values", 1, "string", { repeated: true })],
    value: { values: ["a", "bc"] },
  },
  {
    id: "repeated/bytes",
    fields: [field("values", 1, "bytes", { repeated: true })],
    value: { values: [new Uint8Array([1]), new Uint8Array(), new Uint8Array([2, 3])] },
  },
  {
    id: "repeated/empty",
    fields: [field("values", 1, "int32", { repeated: true })],
    value: { values: [] },
    wire: [],
  },
  {
    id: "repeated/empty-messages",
    fields: [field("inner", 1, "message", { repeated: true, fields: [] })],
    value: { inner: [{}, {}, {}] },
    wire: [0x0a, 0x00, 0x0a, 0x00, 0x0a, 0x00],
  },
];

export const nestedCases: RoundTripCase[] = [
  {
    id: "nested/child",
    fields: [
      field("child", 1, "message", {
        fields: [
          field("first", 1, "int32", { optional: true }),
          field("second", 2, "string", { optional: true }),
        ],
      }),
    ],
    value: { child: { first: 1, second: "x" } },
  },
  {
    id: "nested/over-128-bytes",
    fields: [
      field("inner", 1, "message", { fields: [field("data", 1, "bytes")] }),
      field("tail", 2, "bytes"),
    ],
    value: {
      inner: { data: new Uint8Array(Array.from({ length: 300 }, (_, i) => (i * 5) & 255)) },
      tail: new Uint8Array(Array.from({ length: 200 }, (_, j) => (j * 9) & 255)),
    },
  },
  {
    id: "nested/larger-than-a-slab",
    fields: [
      field("items", 1, "message", { repeated: true, fields: [field("blob", 1, "bytes"), field("n", 2, "int32")] }),
      field("tail", 2, "string"),
    ],
    value: {
      items: Array.from({ length: 4 }, (_, i) => ({ blob: new Uint8Array(3000).fill(i + 1), n: i })),
      tail: "end",
    },
  },
  {
    id: "nested/three-levels",
    fields: [
      field("mid", 1, "message", {
        fields: [
          field("inner", 1, "message", { fields: [field("ix", 1, "int32"), field("iy", 2, "int32")] }),
          field("mx", 2, "int32"),
        ],
      }),
      field("ox", 2, "int32"),
    ],
    value: { mid: { inner: { ix: 7, iy: 101 }, mx: 120 }, ox: 100 },
  },
];

export const specialFloatCases: RoundTripCase[] = [
  {
    id: "float/negative-zero",
    fields: [field("float", 1, "float"), field("double", 2, "double")],
    value: { float: -0, double: -0 },
  },
  {
    id: "float/nan-and-infinity",
    fields: [field("float", 1, "float"), field("double", 2, "double")],
    value: { float: Number.NaN, double: Number.POSITIVE_INFINITY },
  },
  {
    id: "float/negative-infinity",
    fields: [field("float", 1, "float"), field("double", 2, "double")],
    value: { float: Number.NEGATIVE_INFINITY, double: Number.NEGATIVE_INFINITY },
  },
  {
    id: "float/subnormal",
    fields: [field("float", 1, "float"), field("double", 2, "double")],
    value: { float: Math.fround(1.1754946310819804e-39), double: 2.2250738585072014e-309 },
  },
  {
    id: "float/max",
    fields: [field("float", 1, "float"), field("double", 2, "double")],
    value: { float: 3.4028234663852886e38, double: 1.7976931348623157e308 },
  },
];

export const mixedCases: RoundTripCase[] = [
  {
    id: "mixed/typical-message",
    fields: [
      field("id", 1, "uint32"),
      field("name", 2, "string"),
      field("active", 3, "bool"),
      field("tags", 4, "string", { repeated: true }),
      field("score", 5, "double", { optional: true }),
      field("payload", 6, "bytes", { optional: true }),
    ],
    value: {
      id: 42,
      name: "Ada",
      active: true,
      tags: ["ml", "fp"],
      score: 0.5,
      payload: new Uint8Array([1, 2, 3]),
    },
  },
  {
    id: "mixed/declaration-order-differs-from-numbers",
    fields: [field("b", 2, "string"), field("a", 1, "int32"), field("c", 3, "bool")],
    value: { b: "x", a: 1, c: true },
    wire: [8, 1, 18, 1, 120, 24, 1],
  },
];

const incompleteFor = (wire: number): Bytes =>
  wire === 0 ? [0x80] : wire === 1 ? utf8("abcdefg") : wire === 2 ? [0x80] : utf8("abc");

const prematureEof: RejectCase[] = scalarTypes.concat(["message" as never]).flatMap((type): RejectCase[] => {
  const wire = wireOf(type);
  const fields = [singular(type), repeated(type)];
  const s = singularNumber(type);
  const r = repeatedNumber(type);
  const incomplete = incompleteFor(wire);
  const cases: RejectCase[] = [
    { id: `PrematureEofBeforeKnownNonRepeatedValue.${type}`, fields, wire: tag(s, wire) },
    { id: `PrematureEofBeforeKnownRepeatedValue.${type}`, fields, wire: tag(r, wire) },
    { id: `PrematureEofBeforeUnknownValue.${type}`, fields, wire: tag(UNKNOWN_FIELD, wire) },
    { id: `PrematureEofInsideKnownNonRepeatedValue.${type}`, fields, wire: [...tag(s, wire), ...incomplete] },
    { id: `PrematureEofInsideKnownRepeatedValue.${type}`, fields, wire: [...tag(r, wire), ...incomplete] },
    { id: `PrematureEofInsideUnknownValue.${type}`, fields, wire: [...tag(UNKNOWN_FIELD, wire), ...incomplete] },
  ];
  if (wire === 2) {
    cases.push(
      { id: `PrematureEofInDelimitedDataForKnownNonRepeatedValue.${type}`, fields, wire: [...tag(s, 2), 1] },
      { id: `PrematureEofInDelimitedDataForKnownRepeatedValue.${type}`, fields, wire: [...tag(r, 2), 1] },
      { id: `PrematureEofInDelimitedDataForUnknownValue.${type}`, fields, wire: [...tag(UNKNOWN_FIELD, 2), 1] },
    );
    if (type === "message") {
      cases.push({ id: "PrematureEofInSubmessageValue.message", fields, wire: [...tag(s, 2), 2, 0x28, 0x80] });
    }
  } else {
    cases.push(
      {
        id: `PrematureEofInPackedFieldValue.${type}`,
        fields,
        wire: [...tag(r, 2), ...delim(incomplete)],
      },
      { id: `PrematureEofInPackedField.${type}`, fields, wire: [...tag(r, 2), 1] },
    );
  }
  return cases;
});

const allTypes: FieldDef[] = scalarTypes.map(singular);

const illegalTags: RejectCase[] = [
  { id: "IllegalZeroFieldNum_Case_0", fields: allTypes, wire: [1, ...utf8("DEADBEEF")] },
  { id: "IllegalZeroFieldNum_Case_1", fields: allTypes, wire: [2, 1, 1] },
  { id: "IllegalZeroFieldNum_Case_2", fields: allTypes, wire: [3, 4] },
  { id: "IllegalZeroFieldNum_Case_3", fields: allTypes, wire: [5, ...utf8("DEAD")] },
  { id: "IllegalZeroFieldNum_Varint", fields: allTypes, wire: [0, 1] },
  {
    id: "BadTag_FieldNumberTooHigh",
    fields: allTypes,
    wire: [0x88, 0x80, 0x80, 0x80, 0x80, 0x80, 0x0f, 0xd2, 0x09],
  },
  { id: "BadTag_FieldNumberSlightlyTooHigh", fields: allTypes, wire: [0x88, 0x80, 0x80, 0x80, 0x40, 0xd2, 0x09] },
  {
    id: "BadTag_OverlongVarint",
    fields: allTypes,
    wire: [0x88, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00, 0xd2, 0x09],
  },
  {
    id: "BadTag_VarintMoreThanTenBytes",
    fields: allTypes,
    wire: [0x88, ...Array<number>(11).fill(0x80), 0x00, 0xd2, 0x09],
  },
];

const unknownWireTypes: RejectCase[] = [6, 7].flatMap((type) =>
  [0, 1, 2, 3].flatMap((f) =>
    [0, 1, 2, 3].map((value): RejectCase => ({
      id: `UnknownWireType${type}_Field${f}_Version${value}`,
      fields: allTypes,
      wire: [(f << 3) | type, value],
    })),
  ),
);

const hello = len(2, utf8("hello world"));
const unmatchedGroups: RejectCase[] = [
  { id: "UnmatchedEndGroup", fields: allTypes, wire: tag(201, 4) },
  { id: "UnmatchedEndGroupUnknown", fields: allTypes, wire: tag(1234, 4) },
  { id: "UnmatchedEndGroupWrongType", fields: allTypes, wire: tag(1, 4) },
  { id: "UnmatchedEndGroupNestedLen", fields: [singular("message")], wire: len(18, tag(1234, 4)) },
  { id: "UnmatchedEndGroupNested", fields: allTypes, wire: group(201, tag(202, 4)) },
  { id: "UnmatchedEndGroupWithData", fields: allTypes, wire: [...tag(1, 4), ...hello] },
  { id: "UnmatchedStartGroup", fields: allTypes, wire: tag(201, 3) },
  { id: "UnmatchedStartGroupUnknown", fields: allTypes, wire: tag(1234, 3) },
  { id: "UnmatchedStartGroupWrongType", fields: allTypes, wire: tag(1, 3) },
  { id: "UnmatchedStartGroupNestedLen", fields: [singular("message")], wire: len(18, tag(1234, 3)) },
  { id: "UnmatchedStartGroupNested", fields: allTypes, wire: group(201, tag(202, 3)) },
  { id: "UnmatchedStartGroupWithData", fields: allTypes, wire: [...tag(1, 3), ...hello] },
  { id: "MismatchedGroupTags", fields: allTypes, wire: [...tag(201, 3), ...hello, ...tag(202, 4)] },
  {
    id: "MismatchedNestedGroupTags",
    fields: allTypes,
    wire: group(201, tag(202, 3), hello, tag(203, 4)),
  },
  {
    id: "GroupCrossingSubmessageBoundary",
    fields: [singular("message"), field("after", 2, "int32")],
    wire: [...len(18, tag(1234, 3)), ...tag(1234, 4), ...wireField(2, 0, varint(7))],
  },
  {
    id: "GroupRecursionLimit",
    fields: allTypes,
    wire: Array<number>(50000).fill(0x7b),
  },
];

const invalid = [0xa0, 0xb0, 0xc0, 0xd0];
const invalidUtf8: RejectCase[] = [
  { id: "RejectInvalidUtf8.String.Singular", fields: allTypes, wire: len(14, invalid) },
  { id: "RejectInvalidUtf8.String.Repeated", fields: [repeated("string")], wire: len(44, invalid) },
  { id: "RejectInvalidUtf8.String.Overlong", fields: allTypes, wire: len(14, [0xc0, 0x80]) },
  { id: "RejectInvalidUtf8.String.Surrogate", fields: allTypes, wire: len(14, [0xed, 0xa0, 0x80]) },
  { id: "RejectInvalidUtf8.String.Truncated", fields: allTypes, wire: len(14, [0xe2, 0x82]) },
];

const bounds: RejectCase[] = [
  {
    id: "decoder-bounds/nested-length-swallows-parent",
    fields: [
      field("mid", 1, "message", {
        fields: [
          field("inner", 1, "message", { fields: [field("ix", 1, "int32"), field("iy", 2, "int32"), field("iz", 3, "int32")] }),
          field("mx", 2, "int32"),
          field("my", 3, "int32"),
        ],
      }),
      field("ox", 2, "int32"),
      field("oy", 3, "int32"),
    ],
    wire: [0x0a, 0x08, 0x0a, 0x08, 0x08, 0x07, 0x10, 0x65, 0x18, 0x78, 0x10, 0x64, 0x18, 0x7b],
  },
  {
    id: "decoder-bounds/varint-crosses-nested-boundary",
    fields: [
      field("mid", 1, "message", {
        fields: [field("inner", 1, "message", { fields: [field("ix", 1, "int32")] }), field("mx", 2, "int32")],
      }),
    ],
    wire: [0x0a, 0x04, 0x0a, 0x02, 0x08, 0x80, 0x10, 0x64],
  },
  {
    id: "decoder-bounds/packed-varint-crosses-packed-length",
    fields: [field("values", 1, "uint32", { repeated: true }), field("after", 2, "uint32"), field("tail", 3, "uint32")],
    wire: [0x0a, 0x01, 0x80, 0x10, 0x18, 0x07],
  },
  {
    id: "decoder-bounds/unknown-fixed64-crosses-parent",
    fields: [field("inner", 1, "message", { fields: [] }), field("after", 2, "uint32"), field("tail", 3, "uint32")],
    wire: [0x0a, 0x01, 0x09, 0x10, 1, 0x10, 2, 0x10, 3, 0x10, 4, 0x18, 7],
  },
  {
    id: "decoder-bounds/packed-fixed32-leftover-byte",
    fields: [field("a", 1, "fixed32", { repeated: true })],
    wire: [0x0a, 5, 0, 0, 0, 0, 0],
  },
  {
    id: "decoder-bounds/packed-fixed64-leftover-bytes",
    fields: [field("a", 1, "fixed64", { repeated: true })],
    wire: [0x0a, 12, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4],
  },
  {
    id: "decoder-bounds/packed-double-leftover-byte",
    fields: [field("a", 1, "double", { repeated: true })],
    wire: [0x0a, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  },
  {
    id: "decoder-bounds/varint-eleven-bytes",
    fields: [field("a", 1, "uint64")],
    wire: [8, ...Array<number>(10).fill(0x80), 0x01],
  },
  {
    id: "decoder-bounds/message-nesting-limit",
    fields: (() => {
      const self: FieldDef[] = [];
      self.push(field("child", 1, "message", { optional: true, fields: self }));
      return self;
    })(),
    wire: (() => {
      let bytes: Bytes = [];
      for (let i = 0; i < 150; i++) bytes = len(1, bytes);
      return bytes;
    })(),
  },
];


// ValidDataMap: each key/value pair the suite exercises, with the entry
// layouts it requires a parser to accept.
const mapPairs: [S.ProtobufType, S.ProtobufType][] = [
  ["int32", "int32"],
  ["int64", "int64"],
  ["uint32", "uint32"],
  ["uint64", "uint64"],
  ["sint32", "sint32"],
  ["sint64", "sint64"],
  ["fixed32", "fixed32"],
  ["fixed64", "fixed64"],
  ["sfixed32", "sfixed32"],
  ["sfixed64", "sfixed64"],
  ["int32", "float"],
  ["int32", "double"],
  ["bool", "bool"],
  ["string", "string"],
  ["string", "bytes"],
  ["string", "enum"],
  ["string", "message"],
];

const defaultValue = (type: S.ProtobufType): unknown =>
  type === "string" ? ""
  : type === "bytes" ? new Uint8Array()
  : type === "bool" ? false
  : type === "message" ? { a: 0 }
  : type.includes("64") ? 0n
  : 0;
const nonDefaultValue = (type: S.ProtobufType): unknown =>
  type === "string" ? "a"
  : type === "bytes" ? new Uint8Array([97])
  : type === "bool" ? true
  : type === "message" ? { a: 1234 }
  : type.includes("64") ? 1n
  : 1;
const encodeScalar = (type: S.ProtobufType, value: unknown): Bytes => {
  if (type === "string") return str(value as string);
  if (type === "bytes") return delim([...(value as Uint8Array)]);
  if (type === "bool") return varint(value ? 1 : 0);
  if (type === "message") return delim((value as { a: number }).a ? wireField(1, 0, varint((value as { a: number }).a)) : []);
  if (type === "float") return flt(value as number);
  if (type === "double") return dbl(value as number);
  if (type === "fixed32" || type === "sfixed32") return u32(Number(value));
  if (type === "fixed64" || type === "sfixed64") return u64(BigInt(value as bigint));
  if (type === "sint32") return zz32(Number(value));
  if (type === "sint64") return zz64(BigInt(value as bigint));
  return varint(value as number | bigint);
};
const keyName = (value: unknown): string => String(value);

export const validDataMap: DecodeOnlyCase[] = mapPairs.flatMap(([keyType, valueType]): DecodeOnlyCase[] => {
  const fields = [field("map", 1, valueType, { map: keyType, ...(valueType === "message" ? { fields: nested } : {}) })];
  const key1 = wireField(1, wireOf(keyType), encodeScalar(keyType, defaultValue(keyType)));
  const key2 = wireField(1, wireOf(keyType), encodeScalar(keyType, nonDefaultValue(keyType)));
  const value1 = wireField(2, wireOf(valueType), encodeScalar(valueType, defaultValue(valueType)));
  const value2 = wireField(2, wireOf(valueType), encodeScalar(valueType, nonDefaultValue(valueType)));
  const k1 = keyName(defaultValue(keyType));
  const k2 = keyName(nonDefaultValue(keyType));
  const prefix = `ValidDataMap.${keyType}.${valueType}`;
  return [
    { id: `${prefix}.Default`, fields, wire: len(1, key1, value1), value: { map: { [k1]: defaultValue(valueType) } }, reencoded: len(1, key1, value1) },
    { id: `${prefix}.MissingDefault`, fields, wire: len(1), value: { map: { [k1]: defaultValue(valueType) } }, reencoded: len(1, key1, value1) },
    { id: `${prefix}.NonDefault`, fields, wire: len(1, key2, value2), value: { map: { [k2]: nonDefaultValue(valueType) } }, reencoded: len(1, key2, value2) },
    { id: `${prefix}.Unordered`, fields, wire: len(1, value2, key2), value: { map: { [k2]: nonDefaultValue(valueType) } }, reencoded: len(1, key2, value2) },
    { id: `${prefix}.DuplicateKey`, fields, wire: [...len(1, key2, value1), ...len(1, key2, value2)], value: { map: { [k2]: nonDefaultValue(valueType) } } },
    { id: `${prefix}.DuplicateKeyInMapEntry`, fields, wire: len(1, key1, key2, value2), value: { map: { [k2]: nonDefaultValue(valueType) } } },
    { id: `${prefix}.DuplicateValueInMapEntry`, fields, wire: len(1, key2, value1, value2), value: { map: { [k2]: nonDefaultValue(valueType) } } },
  ];
});

const mapMessageFields = [field("map", 71, "message", { map: "string", fields: nestedWithCorecursive })];
export const mapMergeCases: DecodeOnlyCase[] = [
  {
    id: "ValidDataMap.string.message.MergeValue",
    fields: mapMessageFields,
    wire: [
      ...len(71, len(1), len(2, len(2, wireField(1, 0, varint(1)), wireField(31, 0, varint(1))))),
      ...len(71, len(1), len(2, len(2, wireField(2, 0, varint(1)), wireField(31, 0, varint(1))))),
    ],
    value: {
      map: { "": { a: 0, corecursive: { optional_int32: 1, optional_int64: 1n, optional_uint32: 0, repeated_int32: [1, 1] } } },
    },
  },
  {
    id: "map/proto-key-is-own-property",
    fields: [field("map", 1, "int32", { map: "string" })],
    wire: len(1, len(1, utf8("__proto__")), wireField(2, 0, varint(5))),
    value: { map: { ["__proto__"]: 5 } },
    reencoded: len(1, len(1, utf8("__proto__")), wireField(2, 0, varint(5))),
  },
  {
    id: "map/int32-key-omitted-value",
    fields: [field("value", 1, "string", { map: "int32" })],
    wire: [0x0a, 0x02, 0x08, 0x00],
    value: { value: { "0": "" } },
  },
  {
    id: "map/int32-key-omitted-key",
    fields: [field("value", 1, "string", { map: "int32" })],
    wire: [0x0a, 0x02, 0x12, 0x00],
    value: { value: { "0": "" } },
  },
  {
    id: "map/unknown-field-in-entry-skipped",
    fields: [field("map", 1, "int32", { map: "string" })],
    wire: len(1, len(1, utf8("k")), wireField(7, 0, varint(9)), wireField(2, 0, varint(5))),
    value: { map: { k: 5 } },
  },
];

export const mapRoundTrips: RoundTripCase[] = [
  {
    id: "map/string-message-entry-layout",
    fields: [
      field("value", 1, "message", {
        map: "string",
        fields: [field("key", 1, "string"), field("values", 2, "string", { repeated: true })],
      }),
    ],
    value: { value: { b: { key: "1", values: ["c", "d"] }, a: { key: "2", values: ["a", "b"] } } },
    wire: [10, 14, 10, 1, 98, 18, 9, 10, 1, 49, 18, 1, 99, 18, 1, 100, 10, 14, 10, 1, 97, 18, 9, 10, 1, 50, 18, 1, 97, 18, 1, 98],
  },
  {
    id: "map/bool-key-false-written",
    fields: [field("value", 1, "int32", { map: "bool" })],
    value: { value: { false: 0 } },
    wire: [0x0a, 0x04, 0x08, 0x00, 0x10, 0x00],
  },
  {
    id: "map/int64-key-negative",
    fields: [field("value", 1, "string", { map: "int64" })],
    value: { value: { "-1": "x" } },
  },
  {
    id: "map/uint64-key-max",
    fields: [field("value", 1, "string", { map: "uint64" })],
    value: { value: { "18446744073709551615": "x" } },
  },
  {
    id: "map/sint64-key-negative",
    fields: [field("value", 1, "string", { map: "sint64" })],
    value: { value: { "-1": "x" } },
  },
  {
    id: "map/empty",
    fields: [field("value", 1, "int32", { map: "string" })],
    value: { value: {} },
    wire: [],
  },
  {
    id: "map/string-int32-many",
    fields: [field("value", 1, "int32", { map: "string" })],
    value: { value: Object.fromEntries(Array.from({ length: 50 }, (_, i) => [`k${i}`, i - 25])) },
  },
];

// ValidDataOneof: a member keeps its zero value on the wire, the last member
// wins, and a repeated message member merges.
const oneofTypes: S.ProtobufType[] = ["uint32", "bool", "uint64", "float", "double", "string", "bytes", "enum", "message"];
const oneofNumber: Record<string, number> = { uint32: 111, message: 112, string: 113, bytes: 114, bool: 115, uint64: 116, float: 117, double: 118, enum: 119 };
const oneofFields: FieldDef[] = oneofTypes.map((type) =>
  field(`oneof_${type}`, oneofNumber[type]!, type, { oneof: "oneof_field", ...(type === "message" ? { fields: nestedWithCorecursive } : {}) }),
);
export const validDataOneof: DecodeOnlyCase[] = oneofTypes.flatMap((type): DecodeOnlyCase[] => {
  const n = oneofNumber[type]!;
  const wire = wireOf(type);
  const key = `oneof_${type}`;
  const dv = type === "message" ? { a: 0 } : defaultValue(type);
  const nv = type === "message" ? { a: 1234 } : nonDefaultValue(type);
  const defaultBytes = wireField(n, wire, encodeScalar(type, dv));
  const nonDefaultBytes = wireField(n, wire, encodeScalar(type, nv));
  const otherNumber = type === "uint32" ? 112 : 111;
  const otherBytes = type === "uint32" ? len(112) : wireField(111, 0, varint(0));
  const prefix = `ValidDataOneof.${type}`;
  return [
    { id: `${prefix}.DefaultValue`, fields: oneofFields, wire: defaultBytes, value: { [key]: dv }, reencoded: defaultBytes },
    { id: `${prefix}.NonDefaultValue`, fields: oneofFields, wire: nonDefaultBytes, value: { [key]: nv }, reencoded: nonDefaultBytes },
    { id: `${prefix}.MultipleValuesForSameField`, fields: oneofFields, wire: [...defaultBytes, ...nonDefaultBytes], value: { [key]: nv }, reencoded: nonDefaultBytes },
    { id: `${prefix}.MultipleValuesForDifferentField`, fields: oneofFields, wire: [...otherBytes, ...nonDefaultBytes], value: { [key]: nv }, reencoded: nonDefaultBytes },
    ...(otherNumber ? [] : []),
  ];
});

export const oneofMergeCases: DecodeOnlyCase[] = [
  {
    id: "ValidDataOneof.message.Merge",
    fields: oneofFields,
    wire: [
      ...len(112, len(2, wireField(1, 0, varint(1)), wireField(2, 0, varint(1)), wireField(31, 0, varint(1)))),
      ...len(112, len(2, wireField(2, 0, varint(1)), wireField(31, 0, varint(1)))),
    ],
    value: {
      oneof_message: { a: 0, corecursive: { optional_int32: 1, optional_int64: 1n, optional_uint32: 0, repeated_int32: [1, 1] } },
    },
    reencoded: len(112, len(2, wireField(1, 0, varint(1)), wireField(2, 0, varint(1)), len(31, varint(1), varint(1)))),
  },
  {
    id: "oneof/last-field-wins-clears-earlier",
    fields: [
      field("str", 1, "string", { oneof: "kind" }),
      field("num", 2, "int32", { oneof: "kind" }),
      field("other", 3, "bool"),
    ],
    wire: [10, 1, 97, 16, 1],
    value: { num: 1, other: false },
    reencoded: [16, 1],
  },
  {
    id: "oneof/zero-member-emitted",
    fields: [field("str", 1, "string", { oneof: "kind" }), field("num", 2, "int32", { oneof: "kind" })],
    wire: [16, 0],
    value: { num: 0 },
    reencoded: [16, 0],
  },
  {
    id: "oneof/empty-inner-message-present",
    fields: [field("inner", 1, "message", { oneof: "child", fields: [] })],
    wire: [0x0a, 0x00],
    value: { inner: {} },
    reencoded: [0x0a, 0x00],
  },
];

export const oneofRoundTrips: RoundTripCase[] = [
  {
    id: "oneof/string-member",
    fields: [field("str", 1, "string", { oneof: "kind" }), field("num", 2, "int32", { oneof: "kind" }), field("other", 3, "bool")],
    value: { str: "a", other: true },
    wire: [10, 1, 97, 24, 1],
  },
  {
    id: "oneof/no-member-set",
    fields: [field("str", 1, "string", { oneof: "kind" }), field("num", 2, "int32", { oneof: "kind" })],
    value: {},
    wire: [],
  },
];

const oneofReject: RejectCase[] = [
  { id: "RejectInvalidUtf8.String.Oneof", fields: oneofFields, wire: len(113, invalid) },
  {
    id: "RejectInvalidUtf8.String.MapKey",
    fields: [field("map", 69, "string", { map: "string" })],
    wire: len(69, len(1, invalid), len(2, utf8("foo"))),
  },
  {
    id: "RejectInvalidUtf8.String.MapValue",
    fields: [field("map", 69, "string", { map: "string" })],
    wire: len(69, len(1, utf8("foo")), len(2, invalid)),
  },
  {
    id: "decoder-bounds/map-key-length-consumes-following-field",
    fields: [field("values", 1, "uint32", { map: "string" }), field("after", 2, "uint32"), field("tail", 3, "uint32")],
    wire: [0x0a, 0x03, 0x0a, 0x02, 0x41, 0x10, 0x18, 0x07],
  },
  {
    id: "decoder-bounds/map-entry-truncated",
    fields: [field("values", 1, "uint32", { map: "string" })],
    wire: [0x0a, 0x04, 0x0a, 0x01, 0x41],
  },
];

export const roundTripCases: RoundTripCase[] = [
  ...officialVectors,
  ...scalarCases,
  ...presenceCases,
  ...repeatedCases,
  ...nestedCases,
  ...specialFloatCases,
  ...mixedCases,
  ...mapRoundTrips,
  ...oneofRoundTrips,
];

export const decodeOnlyCases: DecodeOnlyCase[] = [
  ...validDataScalar,
  ...validDataMessage,
  ...selectsLast,
  ...validDataRepeated,
  ...mergeCases,
  ...validDataMap,
  ...mapMergeCases,
  ...validDataOneof,
  ...oneofMergeCases,
  {
    id: "decode/unpacked-repeated-sint32",
    fields: [field("values", 1, "sint32", { repeated: true })],
    wire: [0x08, 0x01, 0x08, 0x00, 0x08, 0x04],
    value: { values: [-1, 0, 2] },
  },
  {
    id: "decode/mixed-packed-and-unpacked",
    fields: [field("values", 1, "sint32", { repeated: true })],
    wire: [0x0a, 0x01, 0x01, 0x08, 0x04],
    value: { values: [-1, 2] },
  },
  {
    id: "decode/unknown-field-stripped",
    fields: [field("value", 1, "int32")],
    wire: [0x08, 0x01, 0x10, 0x02],
    value: { value: 1 },
  },
  {
    id: "decode/last-one-wins",
    fields: [field("value", 1, "int32")],
    wire: [0x08, 0x01, 0x08, 0x02],
    value: { value: 2 },
  },
  {
    id: "decode/later-default-clears",
    fields: [field("value", 1, "int32")],
    wire: [0x08, 0x01, 0x08, 0x00],
    value: { value: 0 },
    reencoded: [],
  },
  {
    id: "decode/nested-merge",
    fields: [
      field("child", 1, "message", {
        fields: [
          field("first", 1, "int32", { optional: true }),
          field("second", 2, "string", { optional: true }),
        ],
      }),
    ],
    wire: [0x0a, 0x02, 0x08, 0x01, 0x0a, 0x03, 0x12, 0x01, 0x78],
    value: { child: { first: 1, second: "x" } },
  },
  {
    id: "decode/nested-merge-recursive",
    fields: [
      field("inner", 1, "message", {
        fields: [
          field("a", 1, "int32"),
          field("b", 2, "int32"),
          field("child", 3, "message", { fields: [field("a", 1, "int32"), field("b", 2, "int32")] }),
        ],
      }),
    ],
    wire: [10, 2, 8, 1, 10, 2, 16, 2, 10, 4, 26, 2, 8, 3, 10, 4, 26, 2, 16, 4],
    value: { inner: { a: 1, b: 2, child: { a: 3, b: 4 } } },
  },
  {
    id: "decode/wrong-wire-type-is-unknown",
    fields: [field("value", 1, "int32")],
    wire: [10, 1, 99, 8, 1, 8, 2],
    value: { value: 2 },
  },
  {
    id: "decode/non-minimal-uint32-six-bytes",
    fields: [field("a", 1, "uint32"), field("b", 2, "uint32")],
    wire: [8, 128, 128, 128, 128, 128, 0, 16, 1],
    value: { a: 0, b: 1 },
  },
  {
    id: "decode/bool-wide-varint",
    fields: [field("a", 1, "bool")],
    wire: [8, 128, 128, 128, 128, 16],
    value: { a: true },
  },
  {
    id: "decode/skip-every-wire-type",
    fields: [field("value", 1, "int32")],
    wire: [19, 29, 1, 2, 3, 4, 35, 40, 1, 36, 20, 9, 1, 2, 3, 4, 5, 6, 7, 8, 18, 2, 9, 9, 8, 7],
    value: { value: 7 },
  },
  {
    id: "decode/packed-appends-across-occurrences",
    fields: [field("a", 1, "uint32", { repeated: true })],
    wire: [10, 2, 1, 2, 10, 1, 3],
    value: { a: [1, 2, 3] },
  },
  {
    id: "decode/repeated-message-with-optional-nested",
    fields: [field("items", 1, "message", { repeated: true, fields: nestedWithCorecursive })],
    wire: [...len(1, wireField(1, 0, varint(1))), ...len(1, len(2, wireField(1, 0, varint(2))))],
    value: {
      items: [
        { a: 1 },
        { a: 0, corecursive: { optional_int32: 2, optional_int64: 0n, optional_uint32: 0, repeated_int32: [] } },
      ],
    },
  },
  {
    id: "decode/repeated-messages-append",
    fields: [field("a", 1, "message", { repeated: true, fields: nested })],
    wire: [10, 2, 8, 1, 10, 2, 8, 2],
    value: { a: [{ a: 1 }, { a: 2 }] },
  },
];

export const rejectCases: RejectCase[] = [
  ...oneofReject,
  ...prematureEof,
  ...illegalTags,
  ...unknownWireTypes,
  ...unmatchedGroups,
  ...invalidUtf8,
  ...bounds,
];

export const skipped = ["extensions", "proto2-groups", "proto-json", "message-sets", "unknown-field-retention"];

const scalarSchema = (type: S.ProtobufType): S.Schema<unknown, unknown> => {
  if (type === "string") return S.string;
  if (type === "bytes") return S.uint8Array;
  if (type === "bool") return S.boolean;
  if (
    type === "int64" ||
    type === "uint64" ||
    type === "sint64" ||
    type === "fixed64" ||
    type === "sfixed64"
  ) {
    return S.bigint;
  }
  if (type === "float" || type === "double") return S.number;
  if (type === "int32" || type === "sint32" || type === "sfixed32" || type === "enum") {
    return S.int32;
  }
  return S.integer;
};

export const suryMessage = (fields: FieldDef[], seen = new Map<FieldDef[], S.Schema<unknown, unknown>>()): S.Schema<unknown, unknown> => {
  const cached = seen.get(fields);
  if (cached) return cached;
  let schema: S.Schema<unknown, unknown> | undefined;
  const build = () => {
    const properties: Record<string, S.Schema<unknown, unknown>> = {};
    for (const def of fields) {
      let property: S.Schema<unknown, unknown> =
        def.type === "message"
          ? def.fields === fields
            ? S.recursive("Self", () => schema!)
            : suryMessage(def.fields ?? [], seen)
          : scalarSchema(def.type);
      if (def.repeated) property = S.array(property);
      else if (def.map) property = S.record(property);
      else if (def.optional || def.oneof) property = S.optional(property);
      const descriptor: S.ProtobufField = { number: def.number, type: def.type };
      if (def.packed === false) descriptor.packed = false;
      if (def.map) descriptor.key = def.map;
      if (def.oneof) descriptor.oneof = def.oneof;
      properties[def.key] = property.with(S.protobufField, descriptor);
    }
    return S.schema(properties);
  };
  schema = build();
  seen.set(fields, schema);
  return schema;
};
