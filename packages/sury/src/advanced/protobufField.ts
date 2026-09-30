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

// A Google well-known type a field is declared as: a message on the wire,
// printed as an import (`ProtobufWellKnownType` in index.d.ts lists them).
export type WellKnownType = `google.protobuf.${string}`;

export type FieldType = ProtobufType | WellKnownType;

export type ProtobufField = {
  number: number;
  type?: FieldType;
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
  schema.type === objectTag || (schema.type === refTag && !(schema.flags & 16));

// The field a schema was numbered as, found down its `.to` chain.
export const fieldMetadata = (schema: Internal): StoredField | undefined => {
  let current: Internal | undefined = schema;
  while (current !== U) {
    if (current.protobufField !== U) return current.protobufField as StoredField;
    current = current.to;
  }
  return U;
};

// A wrapper is a message holding its scalar as field 1.
export const wrappers: Record<string, ProtobufType> = {
  "google.protobuf.DoubleValue": "double",
  "google.protobuf.FloatValue": "float",
  "google.protobuf.Int64Value": "int64",
  "google.protobuf.UInt64Value": "uint64",
  "google.protobuf.Int32Value": "int32",
  "google.protobuf.UInt32Value": "uint32",
  "google.protobuf.BoolValue": "bool",
  "google.protobuf.StringValue": "string",
  "google.protobuf.BytesValue": "bytes",
};

const isContainer = (schema: Internal): boolean =>
  (schema.type === arrayTag || schema.type === objectTag) && typeof schema.additionalItems === objectTag;

// `{ seconds, nanos }` is the Timestamp and Duration message itself: its keys
// number it, as 1 and 2, and a key numbered by hand has to agree.
const isSecondsNanos = (shape: Internal): boolean => {
  const properties = shape.properties;
  if (shape.type !== objectTag || properties === U || isContainer(shape) || Object.keys(properties).length !== 2) return false;
  const seconds = properties["seconds"];
  const nanos = properties["nanos"];
  if (seconds === U || nanos === U) return false;
  const nanosShape = getOutputSchema(nanos);
  const secondsField = fieldMetadata(seconds);
  const nanosField = fieldMetadata(nanos);
  return (
    getOutputSchema(seconds).type === bigintTag &&
    nanosShape.type === numberTag &&
    nanosShape.format === "int32" &&
    (secondsField === U || (secondsField.number === 1 && secondsField.type === "int64")) &&
    (nanosField === U || (nanosField.number === 2 && nanosField.type === "int32"))
  );
};

// The value each well-known type takes, and how an error names it.
const wellKnown: Record<string, [(shape: Internal, item: Internal | undefined) => boolean, string]> = {
  "google.protobuf.Timestamp": [
    (shape) => (shape.type === instanceTag && shape.class === Date) || isSecondsNanos(shape),
    "a Date or { seconds: S.bigint, nanos: S.int32 }",
  ],
  "google.protobuf.Duration": [isSecondsNanos, "{ seconds: S.bigint, nanos: S.int32 }"],
  "google.protobuf.Value": [(shape) => !!(shape.flags & 16), "S.json"],
  "google.protobuf.Struct": [(shape, item) => shape.type === objectTag && !!(item && item.flags & 16), "S.record(S.json)"],
  "google.protobuf.ListValue": [(shape, item) => shape.type === arrayTag && !!(item && item.flags & 16), "S.array(S.json)"],
  "google.protobuf.FieldMask": [(shape, item) => shape.type === arrayTag && item?.type === stringTag, "S.array(S.string)"],
  "google.protobuf.Empty": [
    (shape) => shape.type === objectTag && !isContainer(shape) && Object.keys(shape.properties ?? {}).length === 0,
    "S.schema({})",
  ],
};

// Whether a value is one of the well-known type itself, not a list or map of
// it: `S.array(S.string)` is one FieldMask, `S.array(S.array(S.string))` a
// repeated one.
export const wellKnownTakes = (type: WellKnownType, value: Internal): boolean => {
  const shape = getOutputSchema(value);
  const takes = wellKnown[type]?.[0];
  if (takes !== U) return takes(shape, isContainer(shape) ? getOutputSchema(shape.additionalItems as Internal) : U);
  return !isContainer(shape) && !isMessageShape(shape);
};

const inferType = (shape: Internal, literalEnum: boolean): FieldType | undefined => {
  if (literalEnum) return "enum";
  if (shape.type === stringTag) return "string";
  if (shape.type === booleanTag) return "bool";
  if (shape.type === instanceTag && shape.class === Uint8Array) return "bytes";
  // The two values with no other wire form.
  if (shape.type === instanceTag && shape.class === Date) return "google.protobuf.Timestamp";
  if (shape.flags & 16) return "google.protobuf.Value";
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
  const type = typeof field === "number" || field.type === U ? inferType(shape, literalEnum) : field.type;
  // The prefix first: the tables are plain objects, and `"constructor"` is a
  // key of every one.
  const known =
    typeof type === "string" && type.startsWith("google.protobuf.") && (wellKnown[type] !== U || wrappers[type] !== U);
  if (type === U || (!known && protobufTypes[type as ProtobufType] !== true)) {
    return panic(`S.protobufField requires a protobuf type`);
  }
  if (known) {
    const wellKnownType = type as WellKnownType;
    const one = wellKnownTakes(wellKnownType, value);
    if (!one && !(isContainer(value) && wellKnownTakes(wellKnownType, value.additionalItems as Internal))) {
      return panic(`S.protobufField requires ${wellKnown[type]?.[1] ?? `an S.optional ${wrappers[type]} value`} for ${type}`);
    }
    if (one && wrappers[type] !== U && !hasUndefined) {
      return panic(`S.protobufField requires S.optional for ${type}: presence is what a wrapper is for`);
    }
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
  if (!known && type !== "message" && isMessageShape(shape) && isMessageShape(itemOf(present(schema)))) {
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
