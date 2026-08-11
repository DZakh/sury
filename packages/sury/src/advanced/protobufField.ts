import { panic, type Internal, U, updateOutput } from "../base";

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
  type: ProtobufType;
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

// @__NO_SIDE_EFFECTS__
export const protobufField = (schema: Internal, field: ProtobufField): Internal => {
  const number = field?.number;
  if (
    !Number.isInteger(number) ||
    number < 1 ||
    number > 536870911 ||
    (number >= 19000 && number <= 19999)
  ) {
    return panic(`S.protobufField requires a legal protobuf field number`);
  }
  if (!field || protobufTypes[field.type] !== true) {
    return panic(`S.protobufField requires a protobuf type`);
  }
  let current: Internal | undefined = schema;
  while (current !== U) {
    if (current.pb !== U) {
      return panic(`S.protobufField is already applied to this schema`);
    }
    current = current.to;
  }
  return updateOutput(schema, (mut) => {
    mut.pb = { number, type: field.type } satisfies ProtobufField;
  });
};
