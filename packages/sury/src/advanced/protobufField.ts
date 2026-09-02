import {
  anyOfTag,
  arrayTag,
  bigintTag,
  booleanTag,
  instanceTag,
  type Internal,
  numberTag,
  objectTag,
  panic,
  stringTag,
  U,
  undefinedTag,
  updateOutput
} from "../base";

export type ProtobufType =
  | "double"
  | "float"
  | "int32"
  | "int64"
  | "uint32"
  | "uint64"
  | "sint32"
  | "sint64"
  | "fixed32"
  | "fixed64"
  | "sfixed32"
  | "sfixed64"
  | "bool"
  | "string"
  | "bytes"
  | "enum"
  | "message";

export type ProtobufField = {
  number: number;
  type?: ProtobufType;
  packed?: boolean;
  key?: ProtobufType;
  oneof?: string;
};

const protobufTypes: Record<ProtobufType, true> = {
  double: true,
  float: true,
  int32: true,
  int64: true,
  uint32: true,
  uint64: true,
  sint32: true,
  sint64: true,
  fixed32: true,
  fixed64: true,
  sfixed32: true,
  sfixed64: true,
  bool: true,
  string: true,
  bytes: true,
  enum: true,
  message: true,
};

const outputOf = (schema: Internal): Internal => {
  while (schema.to) schema = schema.to;
  return schema;
};

const mapKeyTypes: Partial<Record<ProtobufType, true>> = {
  int32: true,
  int64: true,
  uint32: true,
  uint64: true,
  sint32: true,
  sint64: true,
  fixed32: true,
  fixed64: true,
  sfixed32: true,
  sfixed64: true,
  bool: true,
  string: true,
};

const isRecord = (schema: Internal): boolean =>
  schema.type === objectTag && typeof schema.additionalItems === objectTag;

// The shape a field's wire type is inferred from: past `S.optional`, into a
// repeated field's item or a map's value.
const peel = (schema: Internal): Internal => {
  let output = outputOf(schema);
  if (output.type === anyOfTag && output.anyOf !== U) {
    let value: Internal | undefined = U;
    let hasUndefined = false;
    for (let idx = 0; idx < output.anyOf.length; idx++) {
      const member = outputOf(output.anyOf[idx]!);
      if (member.type === undefinedTag) hasUndefined = true;
      else if (value === U) value = member;
      else return output;
    }
    if (hasUndefined && value !== U) output = value;
  }
  if ((output.type === arrayTag || isRecord(output)) && typeof output.additionalItems === objectTag) {
    return outputOf(output.additionalItems as Internal);
  }
  return output;
};

const inferType = (schema: Internal): ProtobufType | undefined => {
  const shape = peel(schema);
  if (shape.type === stringTag) return "string";
  if (shape.type === booleanTag) return "bool";
  if (shape.type === instanceTag && shape.class === Uint8Array) return "bytes";
  if (shape.type === objectTag) return "message";
  if (shape.type === bigintTag) return "int64";
  if (shape.type === numberTag) {
    if (shape.format === "int32") return "int32";
    if (shape.format === "integer") return U;
    return "double";
  }
  return U;
};

// @__NO_SIDE_EFFECTS__
export const protobufField = (schema: Internal, field: number | ProtobufField): Internal => {
  const number = typeof field === "number" ? field : field?.number;
  if (
    !Number.isInteger(number) ||
    number < 1 ||
    number > 536870911 ||
    (number >= 19000 && number <= 19999)
  ) {
    return panic(`S.protobufField requires a legal protobuf field number`);
  }
  const type = typeof field === "number" || field.type === U ? inferType(schema) : field.type;
  if (type === U || protobufTypes[type] !== true) {
    return panic(`S.protobufField requires a protobuf type`);
  }
  const key = typeof field === "number" || field.key === U ? "string" : field.key;
  if (mapKeyTypes[key] !== true) {
    return panic(`S.protobufField requires an integral, bool or string map key type`);
  }
  const oneof = typeof field === "number" ? U : field.oneof;
  if (oneof !== U && !(outputOf(schema).type === anyOfTag || peel(schema).type === objectTag)) {
    return panic(`S.protobufField requires a oneof member to be S.optional or a message`);
  }
  let current: Internal | undefined = schema;
  while (current !== U) {
    if (current.pb !== U) {
      return panic(`S.protobufField is already applied to this schema`);
    }
    current = current.to;
  }
  const packed = typeof field === "number" || field.packed !== false;
  return updateOutput(schema, (mut) => {
    mut.pb = { number, type, packed, key, oneof } satisfies ProtobufField;
  });
};
