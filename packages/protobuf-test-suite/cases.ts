import * as S from "sury";

export type FieldDef = {
  key: string;
  number: number;
  type: S.ProtobufType;
  repeated?: boolean;
  optional?: boolean;
  fields?: FieldDef[];
};

export type RoundTripCase = {
  id: string;
  fields: FieldDef[];
  value: Record<string, unknown>;
  wire?: number[];
};

export type DecodeOnlyCase = {
  id: string;
  fields: FieldDef[];
  wire: number[];
  value: Record<string, unknown>;
};

export type RejectCase = {
  id: string;
  fields: FieldDef[];
  wire: number[];
};

const field = (
  key: string,
  number: number,
  type: S.ProtobufType,
  extra: Partial<FieldDef> = {},
): FieldDef => ({ key, number, type, ...extra });

const scalar = (type: S.ProtobufType, value: unknown, wire?: number[]): RoundTripCase => ({
  id: `scalar/${type}`,
  fields: [field("value", 1, type)],
  value: { value },
  wire,
});

export const officialVectors: RoundTripCase[] = [
  {
    id: "official/string-bom",
    fields: [field("b", 1, "string")],
    value: { b: "\uFEFF" },
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
];

export const scalarCases: RoundTripCase[] = [
  scalar("double", -2.25),
  scalar("float", 1.5),
  scalar("int32", -1),
  scalar("int64", -2n),
  scalar("uint32", 4294967295),
  scalar("uint64", 18446744073709551615n),
  scalar("sint32", -2147483648),
  scalar("sint64", -9223372036854775808n),
  scalar("fixed32", 4294967295),
  scalar("fixed64", 18446744073709551615n),
  scalar("sfixed32", -2147483648),
  scalar("sfixed64", -9223372036854775808n),
  scalar("bool", true),
  scalar("string", "Ada"),
  scalar("bytes", new Uint8Array([0, 255])),
  scalar("enum", -1),
  {
    id: "scalar/uint32-150-coerced-string",
    fields: [field("id", 1, "uint32")],
    value: { id: 150 },
    wire: [0x08, 0x96, 0x01],
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
];

export const repeatedCases: RoundTripCase[] = [
  {
    id: "repeated/packed-sint32",
    fields: [field("values", 1, "sint32", { repeated: true })],
    value: { values: [-1, 0, 2] },
    wire: [0x0a, 0x03, 0x01, 0x00, 0x04],
  },
  {
    id: "repeated/strings",
    fields: [field("values", 1, "string", { repeated: true })],
    value: { values: ["a", "bc"] },
  },
  {
    id: "repeated/empty",
    fields: [field("values", 1, "int32", { repeated: true })],
    value: { values: [] },
    wire: [],
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
];

export const roundTripCases: RoundTripCase[] = [
  ...officialVectors,
  ...scalarCases,
  ...presenceCases,
  ...repeatedCases,
  ...nestedCases,
  ...specialFloatCases,
  ...mixedCases,
];

export const decodeOnlyCases: DecodeOnlyCase[] = [
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
];

export const rejectCases: RejectCase[] = [
  {
    id: "reject/truncated-length",
    fields: [field("value", 1, "string")],
    wire: [0x0a, 0x02, 0x41],
  },
  {
    id: "reject/field-number-zero",
    fields: [field("value", 1, "string")],
    wire: [0x00],
  },
  {
    id: "reject/invalid-utf8",
    fields: [field("value", 1, "string")],
    wire: [0x0a, 0x01, 0xff],
  },
  {
    id: "reject/overlong-tag",
    fields: [field("value", 1, "string")],
    wire: [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x02],
  },
];

export const skipped = [
  "maps",
  "oneofs",
  "extensions",
  "proto2-groups",
  "proto-json",
  "message-sets",
];

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

const annotate = (
  schema: S.Schema<unknown, unknown>,
  def: FieldDef,
): S.Schema<unknown, unknown> =>
  schema.with(S.protobufField, { number: def.number, type: def.type });

export const suryMessage = (fields: FieldDef[]): S.Schema<unknown, unknown> => {
  const properties: Record<string, S.Schema<unknown, unknown>> = {};
  for (const def of fields) {
    let schema: S.Schema<unknown, unknown> =
      def.type === "message" ? suryMessage(def.fields ?? []) : scalarSchema(def.type);
    if (def.repeated) schema = S.array(schema);
    else if (def.optional) schema = S.optional(schema);
    properties[def.key] = annotate(schema, def);
  }
  return S.schema(properties);
};
