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
  refTag,
  stringTag,
  U,
  undefinedTag,
  updateOutput
} from "../base";
import type { Reader, Writer } from "./protobuf";

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

// A field's type as stored: a scalar or message the caller can name, or the
// full name of a well-known type the value's schema carries a codec for.
export type FieldType = ProtobufType | `google.protobuf.${string}`;

// How a schema that stands for a well-known type (`S.protobufTimestamp`,
// `S.protobufValue`) is read and written: the two functions its message's
// fields would otherwise compile to, and called the same way - the caller
// frames the length, and reading nothing is the default instance. It lives on
// the schema, so the codec ships only with the export that carries it.
export type ProtobufCodec = {
  type: `google.protobuf.${string}`;
  // The file a printed `.proto` imports the type from.
  file: string;
  // Reads up to the reader's limit, merging into `prev` when the field was
  // seen before, as a message does.
  read: (reader: Reader, depth: number, prev?: unknown) => unknown;
  write: (writer: Writer, value: unknown) => void;
};

export type ProtobufField = {
  number: number;
  type?: ProtobufType;
  packed?: boolean;
  key?: ProtobufType;
  oneof?: string;
};

// What `S.protobufField` stores: the field, plus the schema as it was when
// numbered. Meta set before the number belongs to the type the schema
// declares, meta layered on after to the field, and `toProtoOrThrow` tells them
// apart by that schema.
export type StoredField = {
  number: number;
  type: FieldType;
  packed: boolean;
  key: ProtobufType;
  oneof?: string;
  /** The schema as it was when numbered. */
  numberedAs: Internal;
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

// The input side past `S.optional`, as `protobufField` reads the output side.
const present = (schema: Internal): Internal => {
  const [members, hasUndefined] = optionalMembers(schema);
  return hasUndefined && members.length === 1 ? members[0]! : schema;
};

// A repeated field's item or a map's value, else the value itself.
const itemOf = (value: Internal): Internal =>
  (value.type === arrayTag || isRecord(value)) && typeof value.additionalItems === objectTag
    ? (value.additionalItems as Internal)
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

const isMessageShape = (schema: Internal): boolean =>
  schema.type === objectTag || (schema.type === refTag && !schema.isJson);

const inferType = (shape: Internal, literalEnum: boolean): FieldType | undefined => {
  if (literalEnum) return "enum";
  if (shape.type === stringTag) return "string";
  if (shape.type === booleanTag) return "bool";
  if (shape.type === instanceTag && shape.class === Uint8Array) return "bytes";
  if (shape.protobufCodec !== U) return (shape.protobufCodec as ProtobufCodec).type;
  if (shape.isJson) return U;
  // A `$ref` is `S.recursive`, whose definition is not built yet while the
  // definer runs; a message is the only thing the wire can make of one.
  if (shape.type === objectTag || shape.type === refTag) return "message";
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
  // What the wire type is inferred from.
  const shape = getOutputSchema(itemOf(value));
  // A union of int32 literals is an enum; a lone literal is a number, since
  // a one-member enum could accept neither its zero nor an unknown value.
  const literalEnum = isIntegerEnum(shape);
  const inferred = inferType(shape, literalEnum);
  const type: FieldType | undefined = typeof field === "number" || field.type === U ? inferred : field.type;
  // A well-known type is only ever what the value's codec says.
  if (type === U || (protobufTypes[type as ProtobufType] !== true && type !== inferred)) {
    // The two JS values a message of Google's own stands for.
    if (shape.protobufCodec === U) {
      if (shape.class === Date) return panic(`S.protobufField requires S.protobufTimestamp for a Date, which is a google.protobuf.Timestamp on the wire`);
      if (shape.isJson) return panic(`S.protobufField requires S.protobufValue for JSON, which is a google.protobuf.Value on the wire`);
    }
    return panic(`S.protobufField requires a protobuf type`);
  }
  const key = typeof field === "number" || field.key === U ? "string" : field.key;
  if (mapKeyTypes[key] !== true) {
    return panic(`S.protobufField requires an integral, bool or string map key type`);
  }
  if (type === "enum" && !literalEnum && (shape.const !== U || shape.type !== numberTag)) {
    return panic(`S.protobufField requires an enum to be a number schema or a union of int32 literals`);
  }
  // Declared as a scalar, the writer would take the object for a number and
  // write whatever that coerces to. Only where both sides are one: which side
  // faces the wire depends on the direction the chain runs, and a field that
  // converts bytes to an object (`S.uint8Array.with(S.to, S.schema(...))`) is
  // a bytes field. `S.json` is a ref but no message.
  if (type !== "message" && isMessageShape(shape) && isMessageShape(itemOf(present(schema)))) {
    return panic(`S.protobufField requires an object or S.recursive schema to be a message, not ${type}`);
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
    if (current.protobufField !== U) {
      return panic(`S.protobufField is already applied to this schema`);
    }
    current = current.to;
  }
  const packed = typeof field === "number" || field.packed !== false;
  return updateOutput(schema, (mut) => {
    mut.protobufField = { number, type, packed, key, oneof, numberedAs: schema } satisfies StoredField;
  });
};
