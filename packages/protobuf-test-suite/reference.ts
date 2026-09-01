import protobuf from "protobufjs";
import type { FieldDef } from "./cases";

const ident = (name: string): string => name.replace(/[^A-Za-z0-9_]/g, "_");

const pbjsTypeName = (field: FieldDef, parent: string): string => {
  if (field.type === "message") return ident(`${parent}_${field.key}`);
  if (field.type === "enum") return "int32";
  return field.type;
};

const emitMessage = (name: string, fields: FieldDef[]): string => {
  const nested = fields
    .filter((field) => field.type === "message")
    .map((field) => emitMessage(pbjsTypeName(field, name), field.fields ?? []));
  const body = fields.map((field) => {
    const rule = field.repeated ? "repeated " : field.optional ? "optional " : "";
    return `  ${rule}${pbjsTypeName(field, name)} ${ident(field.key)} = ${field.number};`;
  });
  return `${nested.join("\n")}\nmessage ${ident(name)} {\n${body.join("\n")}\n}\n`;
};

export const protobufjsType = (fields: FieldDef[]): protobuf.Type => {
  const src = `syntax = "proto3";\n${emitMessage("M", fields)}`;
  const parsed = protobuf.parse(src);
  return parsed.root.lookupType("M");
};

const is64 = (type: FieldDef["type"]): boolean =>
  type === "int64" ||
  type === "uint64" ||
  type === "sint64" ||
  type === "fixed64" ||
  type === "sfixed64";

const defaultOf = (type: FieldDef["type"]): unknown => {
  if (type === "string") return "";
  if (type === "bytes") return new Uint8Array();
  if (type === "bool") return false;
  if (is64(type)) return 0n;
  if (type === "message") return undefined;
  return 0;
};

const asBytes = (value: unknown): Uint8Array => {
  if (value instanceof Uint8Array) return value;
  if (ArrayBuffer.isView(value)) {
    const view = value as ArrayBufferView;
    return new Uint8Array(view.buffer, view.byteOffset, view.byteLength);
  }
  if (typeof value === "string") {
    const out = new Uint8Array(value.length);
    for (let i = 0; i < value.length; i++) out[i] = value.charCodeAt(i) & 255;
    return out;
  }
  throw new Error(`expected bytes, got ${typeof value}`);
};

const asBigInt = (value: unknown): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number") return BigInt(value);
  if (typeof value === "string") return BigInt(value);
  if (typeof value === "object" && value !== null && "toString" in value) {
    return BigInt((value as { toString(): string }).toString());
  }
  throw new Error(`expected 64-bit int, got ${typeof value}`);
};

const toPbjsField = (field: FieldDef, value: unknown): unknown => {
  if (field.repeated) {
    return (value as unknown[]).map((item) => toPbjsField({ ...field, repeated: false }, item));
  }
  if (field.type === "message") return toPbjsValue(field.fields ?? [], value as Record<string, unknown>);
  if (is64(field.type)) return asBigInt(value).toString();
  return value;
};

export const toPbjsValue = (
  fields: FieldDef[],
  value: Record<string, unknown>,
): Record<string, unknown> => {
  const out: Record<string, unknown> = {};
  for (const field of fields) {
    const raw = value[field.key];
    if (raw === undefined) continue;
    out[field.key] = toPbjsField(field, raw);
  }
  return out;
};

const convertField = (field: FieldDef, value: unknown): unknown => {
  if (field.repeated) {
    return (value as unknown[]).map((item) => convertField({ ...field, repeated: false }, item));
  }
  if (field.type === "message") return walk(field.fields ?? [], value as Record<string, unknown>);
  if (field.type === "bytes") return asBytes(value);
  if (is64(field.type)) return asBigInt(value);
  return value;
};

const walk = (fields: FieldDef[], value: Record<string, unknown>): Record<string, unknown> => {
  const out: Record<string, unknown> = {};
  for (const field of fields) {
    const raw = value[field.key];
    if (raw === undefined || raw === null) {
      if (field.repeated) out[field.key] = [];
      else if (!field.optional && field.type !== "message") out[field.key] = defaultOf(field.type);
      continue;
    }
    out[field.key] = convertField(field, raw);
  }
  return out;
};

export const encodeProtobufjs = (fields: FieldDef[], value: Record<string, unknown>): Uint8Array => {
  const type = protobufjsType(fields);
  return type.encode(toPbjsValue(fields, value)).finish();
};

export const decodeProtobufjs = (fields: FieldDef[], bytes: Uint8Array): Record<string, unknown> => {
  const type = protobufjsType(fields);
  const raw = type.toObject(type.decode(bytes), {
    longs: String,
    bytes: Uint8Array,
    defaults: false,
  }) as Record<string, unknown>;
  return walk(fields, raw);
};
