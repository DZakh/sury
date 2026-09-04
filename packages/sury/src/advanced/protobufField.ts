import { getOutputSchema, optionalMembers } from "../parse";
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

// What `S.protobufField` stores: the field, plus the schema as it was when
// numbered. Meta set before the number belongs to the type the schema
// declares, meta layered on after to the field, and `toProto` tells them
// apart by that schema.
export type StoredField = {
  number: number;
  type: ProtobufType;
  packed: boolean;
  key: ProtobufType;
  oneof?: string;
  m: Internal;
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

// The shape a field's wire type is inferred from: a repeated field's item or
// a map's value, else the value itself.
const peel = (value: Internal): Internal =>
  (value.type === arrayTag || isRecord(value)) && typeof value.additionalItems === objectTag
    ? getOutputSchema(value.additionalItems as Internal)
    : value;

const isInt32Literal = (schema: Internal): boolean =>
  schema.type === numberTag &&
  Number.isInteger(schema.const) &&
  (schema.const as number) >= -2147483648 &&
  (schema.const as number) <= 2147483647;

// `S.enum([0, 1, 2])` and its optional form: every member an int32 literal
// or such a union itself (a named one nests rather than flattens),
// `undefined` aside.
const isIntegerEnum = (schema: Internal): boolean => {
  if (schema.type !== anyOfTag || schema.anyOf === U) return false;
  let members = 0;
  for (let idx = 0; idx < schema.anyOf.length; idx++) {
    const member = getOutputSchema(schema.anyOf[idx]!);
    if (member.type === undefinedTag) continue;
    if (!isInt32Literal(member) && !isIntegerEnum(member)) return false;
    members++;
  }
  return members > 0;
};

const inferType = (shape: Internal, literalEnum: boolean): ProtobufType | undefined => {
  if (literalEnum) return "enum";
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
  const output = getOutputSchema(schema);
  const [members, hasUndefined] = optionalMembers(output);
  // Past `S.optional`: the schema a field's value has when present.
  const value = hasUndefined && members.length === 1 ? getOutputSchema(members[0]!) : output;
  const shape = peel(value);
  // A union of int32 literals is an enum; a lone literal is a number, since
  // a one-member enum could accept neither its zero nor an unknown value.
  const literalEnum = isIntegerEnum(shape);
  const type = typeof field === "number" || field.type === U ? inferType(shape, literalEnum) : field.type;
  if (type === U || protobufTypes[type] !== true) {
    return panic(`S.protobufField requires a protobuf type`);
  }
  const key = typeof field === "number" || field.key === U ? "string" : field.key;
  if (mapKeyTypes[key] !== true) {
    return panic(`S.protobufField requires an integral, bool or string map key type`);
  }
  if (type === "enum" && !literalEnum && (shape.const !== U || shape.type !== numberTag)) {
    return panic(`S.protobufField requires an enum to be a number schema or a union of int32 literals`);
  }
  const oneof = typeof field === "number" ? U : field.oneof;
  if (oneof !== U) {
    if (value.type === arrayTag || isRecord(value)) {
      return panic(`S.protobufField requires a oneof member to be singular, not repeated or a map`);
    }
    if (!(hasUndefined || value.type === objectTag)) {
      return panic(`S.protobufField requires a oneof member to be S.optional or a message`);
    }
    // A default would be supplied whenever another arm is set.
    if (output.anyOf?.some((member) => member.type === undefinedTag && member.to !== U)) {
      return panic(`S.protobufField requires a oneof member without a default`);
    }
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
    mut.pb = { number, type, packed, key, oneof, m: schema } satisfies StoredField;
  });
};
