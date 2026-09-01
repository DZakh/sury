import {
  anyOfTag,
  arrayTag,
  baseSchema,
  copySchema,
  initSchema,
  instanceTag,
  type Internal,
  objectTag,
  U,
  undefinedTag,
  type Val,
} from "../base";
import { B_conversion, B_refine, B_unsupportedDecode } from "../builder";
import { arrayFactory, objectDecoder } from "../composites";
import { getDecoder, getOutputSchema, instanceDecoder } from "../parse";
import { bigint, bool, float, int, integer, string } from "../primitives";
import type { ProtobufType } from "./protobufField";

type Field = {
  number: number;
  type: ProtobufType;
  key: string;
  repeated: boolean;
  optional: boolean;
  message?: Message;
};

type Message = {
  fields: Field[];
  byNumber: Record<number, Field>;
  strict: boolean;
  raw: Internal;
  schema: Internal;
};

const packable: Record<ProtobufType, boolean> = {
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
  string: false,
  bytes: false,
  enum: true,
  message: false,
};

const wireType = (type: ProtobufType): number => {
  if (type === "double" || type === "fixed64" || type === "sfixed64") return 1;
  if (type === "string" || type === "bytes" || type === "message") return 2;
  if (type === "float" || type === "fixed32" || type === "sfixed32") return 5;
  return 0;
};

const fieldMetadata = (schema: Internal): { number: number; type: ProtobufType } | undefined => {
  let current: Internal | undefined = schema;
  while (current !== U) {
    if (current.pb !== U) return current.pb as { number: number; type: ProtobufType };
    current = current.to;
  }
  return U;
};

const unwrapOptional = (schema: Internal): [Internal, boolean] => {
  const output = getOutputSchema(schema);
  if (output.type !== anyOfTag || output.anyOf === U) return [schema, false];
  let value: Internal | undefined = U;
  let hasUndefined = false;
  for (let idx = 0; idx < output.anyOf.length; idx++) {
    const member = output.anyOf[idx]!;
    if (getOutputSchema(member).type === undefinedTag) hasUndefined = true;
    else if (value === U) value = member;
    else return [output, false];
  }
  return hasUndefined && value !== U ? [value, true] : [output, false];
};

const bytesSchema: Internal = /* @__PURE__ */ initSchema(instanceTag, instanceDecoder, (s) => {
  s.class = Uint8Array;
});

const scalarSchema = (type: ProtobufType): Internal => {
  if (type === "string") return string;
  if (type === "bytes") return bytesSchema;
  if (type === "bool") return bool;
  if (
    type === "int64" ||
    type === "uint64" ||
    type === "sint64" ||
    type === "fixed64" ||
    type === "sfixed64"
  ) return bigint;
  if (type === "float" || type === "double") return float;
  if (type === "int32" || type === "sint32" || type === "sfixed32" || type === "enum") return int;
  return integer;
};

const compileMessage = (schema: Internal, seen = new Set<Internal>()): Message | undefined => {
  let output = schema;
  while (output.type !== objectTag && output.to !== U) output = output.to;
  if (output.type !== objectTag || output.properties === U || seen.has(output)) return U;
  if (typeof output.additionalItems === objectTag) return U;
  seen.add(output);
  const fields: Field[] = [];
  const byNumber: Record<number, Field> = Object.create(null);
  const rawProperties: Record<string, Internal> = Object.create(null);
  const normalizedProperties: Record<string, Internal> = Object.create(null);
  const rawRequired: string[] = [];
  const keys = Object.keys(output.properties);
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const property = output.properties[key]!;
    const metadata = fieldMetadata(property);
    if (metadata === U || byNumber[metadata.number] !== U) return U;
    const [propertyValue, optional] = unwrapOptional(property);
    let shape = getOutputSchema(propertyValue);
    let repeated = false;
    if (shape.type === arrayTag && typeof shape.additionalItems === objectTag) {
      if (optional) return U;
      repeated = true;
      shape = getOutputSchema(shape.additionalItems as Internal);
    }
    let message: Message | undefined;
    let raw: Internal;
    let normalizedProperty = optional ? propertyValue : property;
    if (metadata.type === "message") {
      message = compileMessage(shape, new Set(seen));
      if (message === U) return U;
      raw = message.raw;
      normalizedProperty = message.schema;
    } else {
      raw = scalarSchema(metadata.type);
    }
    if (repeated) {
      raw = arrayFactory(raw);
      normalizedProperty = property;
    } else if (!optional) rawRequired.push(key);
    rawProperties[key] = raw;
    normalizedProperties[key] = normalizedProperty;
    const field: Field = { ...metadata, key, repeated, optional, message };
    fields.push(field);
    byNumber[field.number] = field;
  }
  fields.sort((a, b) => a.number - b.number);
  const raw = baseSchema(objectTag, false, objectDecoder);
  raw.properties = rawProperties;
  raw.required = rawRequired;
  raw.additionalItems = output.additionalItems === "strict" ? "strict" : "strip";
  const normalized = copySchema(output);
  normalized.properties = normalizedProperties;
  normalized.required = rawRequired;
  delete normalized.to;
  return { fields, byNumber, strict: output.additionalItems === "strict", raw, schema: normalized };
};

class Reader {
  pos = 0;
  constructor(readonly bytes: Uint8Array, readonly limit = bytes.length) {}
  varint32(): number {
    let value = 0;
    for (let shift = 0; shift < 35; shift += 7) {
      if (this.pos >= this.limit) throw Error("truncated varint");
      const byte = this.bytes[this.pos++]!;
      value |= (byte & 127) << shift;
      if (byte < 128) return value >>> 0;
    }
    throw Error("varint exceeds 64 bits");
  }
  varint64(): bigint {
    let value = 0n;
    for (let shift = 0n; shift < 70n; shift += 7n) {
      if (this.pos >= this.limit) throw Error("truncated varint");
      const byte = this.bytes[this.pos++]!;
      if (shift === 63n && byte > 1) throw Error("varint exceeds 64 bits");
      value |= BigInt(byte & 127) << shift;
      if (byte < 128) return value;
    }
    throw Error("varint exceeds 64 bits");
  }
  tag(): number {
    const start = this.pos;
    const tag = Number(this.varint64());
    if (this.pos - start > 5 || tag > 4294967295) throw Error("invalid protobuf tag");
    return tag;
  }
  fixed(size: number): Uint8Array {
    const end = this.pos + size;
    if (end > this.limit) throw Error("truncated protobuf field");
    const value = this.bytes.subarray(this.pos, end);
    this.pos = end;
    return value;
  }
  length(): Reader {
    const length = this.varint32();
    if (this.pos + length > this.limit) throw Error("truncated protobuf field");
    const reader = new Reader(this.bytes, this.pos + length);
    reader.pos = this.pos;
    this.pos += length;
    return reader;
  }
}

const scratch = /* @__PURE__ */ new Uint8Array(8);
const scratchView = /* @__PURE__ */ new DataView(scratch.buffer);

class Writer {
  buf = new Uint8Array(64);
  pos = 0;
  ensure(n: number): void {
    if (this.pos + n <= this.buf.length) return;
    const next = new Uint8Array(Math.max(this.buf.length * 2, this.pos + n));
    next.set(this.buf);
    this.buf = next;
  }
  varint32(value: number): void {
    this.ensure(5);
    value >>>= 0;
    while (value > 127) {
      this.buf[this.pos++] = (value & 127) | 128;
      value >>>= 7;
    }
    this.buf[this.pos++] = value;
  }
  varint64(value: bigint): void {
    value = BigInt.asUintN(64, value);
    this.ensure(10);
    while (value > 127n) {
      this.buf[this.pos++] = Number(value & 127n) | 128;
      value >>= 7n;
    }
    this.buf[this.pos++] = Number(value);
  }
  fixed(value: Uint8Array): void {
    this.ensure(value.length);
    this.buf.set(value, this.pos);
    this.pos += value.length;
  }
  float32(value: number): void {
    if (Number.isFinite(value) && Math.abs(value) > 3.4028234663852886e38) throw Error("invalid float");
    scratchView.setFloat32(0, value, true);
    this.ensure(4);
    this.buf[this.pos++] = scratch[0]!;
    this.buf[this.pos++] = scratch[1]!;
    this.buf[this.pos++] = scratch[2]!;
    this.buf[this.pos++] = scratch[3]!;
  }
  float64(value: number): void {
    scratchView.setFloat64(0, value, true);
    this.ensure(8);
    this.buf.set(scratch, this.pos);
    this.pos += 8;
  }
  bits32(value: number): void {
    scratchView.setUint32(0, value, true);
    this.ensure(4);
    this.buf[this.pos++] = scratch[0]!;
    this.buf[this.pos++] = scratch[1]!;
    this.buf[this.pos++] = scratch[2]!;
    this.buf[this.pos++] = scratch[3]!;
  }
  bits64(value: bigint): void {
    scratchView.setBigUint64(0, value, true);
    this.ensure(8);
    this.buf.set(scratch, this.pos);
    this.pos += 8;
  }
  finish(): Uint8Array {
    return this.buf.slice(0, this.pos);
  }
}

const dataView = (bytes: Uint8Array): DataView => new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
const textDecoder = /* @__PURE__ */ new TextDecoder("utf-8", { fatal: true });
const textEncoder = /* @__PURE__ */ new TextEncoder();

const readScalar = (reader: Reader, type: ProtobufType): unknown => {
  if (wireType(type) === 0) {
    if (type === "int64") return BigInt.asIntN(64, reader.varint64());
    if (type === "uint64") return BigInt.asUintN(64, reader.varint64());
    if (type === "sint64") {
      const value = reader.varint64();
      return BigInt.asIntN(64, (value >> 1n) ^ -(value & 1n));
    }
    if (type === "int32" || type === "enum") return Number(BigInt.asIntN(32, reader.varint64()));
    const value = reader.varint32();
    if (type === "bool") return value !== 0;
    if (type === "sint32") return ((value >>> 1) ^ -(value & 1)) | 0;
    return value >>> 0;
  }
  if (wireType(type) === 1) {
    const bytes = reader.fixed(8);
    const view = dataView(bytes);
    if (type === "double") return view.getFloat64(0, true);
    const value = view.getBigUint64(0, true);
    return type === "sfixed64" ? BigInt.asIntN(64, value) : value;
  }
  if (wireType(type) === 5) {
    const bytes = reader.fixed(4);
    const view = dataView(bytes);
    if (type === "float") return view.getFloat32(0, true);
    const value = view.getUint32(0, true);
    return type === "sfixed32" ? value | 0 : value;
  }
  const child = reader.length();
  const bytes = child.fixed(child.limit - child.pos);
  if (type === "string") {
    const text = textDecoder.decode(bytes);
    // Node 24 drops a leading U+FEFF even with ignoreBOM: false, and keeps it
    // with ignoreBOM: true, which is the opposite of WHATWG. Re-attach the BOM
    // when the bytes still have one and the decoder ate it.
    if (
      bytes.length >= 3 &&
      bytes[0] === 239 &&
      bytes[1] === 187 &&
      bytes[2] === 191 &&
      !text.startsWith("\uFEFF")
    ) {
      return "\uFEFF" + text;
    }
    return text;
  }
  return new Uint8Array(bytes);
};

const skip = (reader: Reader, wire: number, fieldNumber: number, depth: number): void => {
  if (wire === 0) reader.varint64();
  else if (wire === 1) reader.fixed(8);
  else if (wire === 2) {
    const child = reader.length();
    child.pos = child.limit;
  } else if (wire === 5) reader.fixed(4);
  else if (wire === 3) {
    if (depth >= 100) throw Error("protobuf group nesting limit exceeded");
    while (reader.pos < reader.limit) {
      const tag = reader.tag();
      const number = tag >>> 3;
      const nestedWire = tag & 7;
      if (number === 0) throw Error("invalid protobuf field number");
      if (nestedWire === 4) {
        if (number !== fieldNumber) throw Error("mismatched protobuf end group");
        return;
      }
      skip(reader, nestedWire, number, depth + 1);
    }
    throw Error("unterminated protobuf group");
  } else throw Error("invalid protobuf wire type");
};

const defaultValue = (field: Field): unknown => {
  if (field.repeated) return [];
  if (field.optional || field.type === "message") return U;
  if (field.type === "string") return "";
  if (field.type === "bytes") return new Uint8Array();
  if (field.type === "bool") return false;
  if (field.type.includes("64")) return 0n;
  return 0;
};

const mergeMessage = (into: Record<string, unknown>, value: Record<string, unknown>, message: Message): void => {
  for (let idx = 0; idx < message.fields.length; idx++) {
    const field = message.fields[idx]!;
    const next = value[field.key];
    if (next === U) continue;
    if (field.repeated) (into[field.key] as unknown[]).push(...(next as unknown[]));
    else if (field.type === "message" && into[field.key] !== U)
      mergeMessage(into[field.key] as Record<string, unknown>, next as Record<string, unknown>, field.message!);
    else into[field.key] = next;
  }
};

const decodeMessage = (reader: Reader, message: Message, depth = 0): Record<string, unknown> => {
  if (depth >= 100) throw Error("protobuf message nesting limit exceeded");
  const output: Record<string, unknown> = Object.create(null);
  for (let idx = 0; idx < message.fields.length; idx++) {
    const field = message.fields[idx]!;
    const value = defaultValue(field);
    if (value !== U) output[field.key] = value;
  }
  while (reader.pos < reader.limit) {
    const tag = reader.tag();
    const number = tag >>> 3;
    const wire = tag & 7;
    if (number === 0) throw Error("invalid protobuf field number");
    const field = message.byNumber[number];
    if (field === U || (wire !== wireType(field.type) && !(field.repeated && packable[field.type] && wire === 2))) {
      if (message.strict) throw Error(`unknown protobuf field ${number}`);
      skip(reader, wire, number, 0);
      continue;
    }
    if (field.repeated && packable[field.type] && wire === 2) {
      const packed = reader.length();
      while (packed.pos < packed.limit) (output[field.key] as unknown[]).push(readScalar(packed, field.type));
      continue;
    }
    let value: unknown;
    if (field.type === "message") {
      const child = reader.length();
      value = decodeMessage(child, field.message!, depth + 1);
      if (child.pos !== child.limit) throw Error("invalid nested protobuf message");
    } else value = readScalar(reader, field.type);
    if (field.repeated) (output[field.key] as unknown[]).push(value);
    else if (field.type === "message" && output[field.key] !== U)
      mergeMessage(output[field.key] as Record<string, unknown>, value as Record<string, unknown>, field.message!);
    else output[field.key] = value;
  }
  return output;
};

const checkedNumber = (value: unknown, min: number, max: number, type: string): number => {
  if (typeof value !== "number" || !Number.isInteger(value) || value < min || value > max) throw Error(`invalid ${type}`);
  return value;
};

const checkedBigint = (value: unknown, min: bigint, max: bigint, type: string): bigint => {
  if (typeof value !== "bigint" || value < min || value > max) throw Error(`invalid ${type}`);
  return value;
};

const writeCall = (type: ProtobufType, w: string, v: string): string => {
  if (type === "bool") return `${w}.varint32(${v}?1:0)`;
  if (type === "uint32") return `${w}.varint32(${v}>>>0)`;
  if (type === "int32" || type === "enum") return `${w}.varint64(BigInt(${v}))`;
  if (type === "sint32") return `s=${v};${w}.varint32(((s<<1)^(s>>31))>>>0)`;
  if (type === "int64") return `${w}.varint64(e[2](${v},-9223372036854775808n,9223372036854775807n,"int64"))`;
  if (type === "uint64") return `${w}.varint64(e[2](${v},0n,18446744073709551615n,"uint64"))`;
  if (type === "sint64") return `s=e[2](${v},-9223372036854775808n,9223372036854775807n,"sint64");${w}.varint64((s<<1n)^(s>>63n))`;
  if (type === "fixed32") return `${w}.bits32(e[1](${v},0,4294967295,"fixed32"))`;
  if (type === "sfixed32") return `${w}.bits32(e[1](${v},-2147483648,2147483647,"sfixed32"))`;
  if (type === "fixed64") return `${w}.bits64(BigInt.asUintN(64,e[2](${v},0n,18446744073709551615n,"fixed64")))`;
  if (type === "sfixed64") return `${w}.bits64(BigInt.asUintN(64,e[2](${v},-9223372036854775808n,9223372036854775807n,"sfixed64")))`;
  if (type === "float") return `${w}.float32(${v})`;
  if (type === "double") return `${w}.float64(${v})`;
  if (type === "string") return `b=e[3].encode(${v});${w}.varint32(b.length);${w}.fixed(b)`;
  return `s=${v};${w}.varint32(s.length);${w}.fixed(s)`;
};

const compileEncode = (message: Message): ((value: Record<string, unknown>) => Uint8Array) => {
  const extras: unknown[] = [Writer, checkedNumber, checkedBigint, textEncoder];
  const use = (value: unknown): string => {
    extras.push(value);
    return `e[${extras.length - 1}]`;
  };
  const body: string[] = ["var w=new e[0]", "var v", "var i", "var n", "var p", "var b", "var s"];
  for (let idx = 0; idx < message.fields.length; idx++) {
    const field = message.fields[idx]!;
    const key = JSON.stringify(field.key);
    const tag = field.number * 8 + wireType(field.type);
    const packedTag = field.number * 8 + 2;
    if (field.repeated) {
      body.push(`v=value[${key}];n=v.length;if(n){`);
      if (packable[field.type]) {
        body.push(`p=new e[0];i=0;while(i<n){${writeCall(field.type, "p", "v[i++]")}}b=p.finish();w.varint32(${packedTag});w.varint32(b.length);w.fixed(b)`);
      } else if (field.type === "message") {
        const nested = use(compileEncode(field.message!));
        body.push(`i=0;while(i<n){b=${nested}(v[i++]);w.varint32(${tag});w.varint32(b.length);w.fixed(b)}`);
      } else {
        body.push(`i=0;while(i<n){w.varint32(${tag});${writeCall(field.type, "w", "v[i++]")}}`);
      }
      body.push(`}`);
    } else if (field.type === "message") {
      const nested = use(compileEncode(field.message!));
      body.push(`v=value[${key}];if(v!=null){b=${nested}(v);w.varint32(${tag});w.varint32(b.length);w.fixed(b)}`);
    } else {
      const live =
        field.optional ? "v!=null"
        : field.type === "bytes" ? "v.length"
        : field.type === "float" || field.type === "double" ? "v||v!==v||Object.is(v,-0)"
        : "v";
      body.push(`v=value[${key}];if(${live}){w.varint32(${tag});${writeCall(field.type, "w", "v")}}`);
    }
  }
  body.push("return w.finish()");
  return (new Function("e", `return function(value){${body.join(";")}}`) as (e: unknown[]) => (value: Record<string, unknown>) => Uint8Array)(extras);
};

const bridge = (input: Val, target: Internal, fn: (value: unknown) => unknown): Val => {
  const expected = copySchema(input.e);
  expected.to = target;
  const output = B_conversion(fn, false, true)(B_refine(input, U, U, expected));
  output.s = target;
  output.e = target;
  return output;
};

const protobufDecoder = (input: Val): Val => {
  if (input.s.encoder === protobufEncoder) return instanceDecoder(input);
  const message = compileMessage(input.s);
  if (message === U) return B_unsupportedDecode(input, input.s, input.e);
  const convert = getDecoder(message.schema, message.raw);
  const encode = compileEncode(message);
  return bridge(input, input.e, (value) =>
    encode(convert(value) as Record<string, unknown>)
  );
};

const protobufEncoder = (input: Val, target: Internal): Val => {
  const message = compileMessage(target);
  if (message === U) return B_unsupportedDecode(input, input.s, target);
  const convert = getDecoder(message.raw, message.schema);
  return bridge(
    input,
    message.schema,
    (value) => convert(decodeMessage(new Reader(value as Uint8Array), message)),
  );
};

export const protobuf: Internal = /* @__PURE__ */ initSchema(instanceTag, protobufDecoder, (schema) => {
  schema.class = Uint8Array;
  schema.encoder = protobufEncoder;
});
