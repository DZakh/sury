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
import {
  _var,
  B_embed,
  B_embedPure,
  B_next,
  B_unsupportedDecode,
  B_varWithoutAllocation,
} from "../builder";
import { arrayFactory, objectDecoder } from "../composites";
import { getOutputSchema, instanceDecoder } from "../parse";
import { bigint, bool, float, int, integer, string } from "../primitives";
import type { ProtobufType } from "./protobufField";

type Field = {
  number: number;
  type: ProtobufType;
  key: string;
  repeated: boolean;
  optional: boolean;
  wire: number;
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
    const field: Field = { ...metadata, key, repeated, optional, message, wire: wireType(metadata.type) };
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

const textDecoder = /* @__PURE__ */ new TextDecoder("utf-8", { fatal: true });
const textEncoder = /* @__PURE__ */ new TextEncoder();

class Reader {
  pos = 0;
  buf: Uint8Array;
  limit: number;
  constructor(buf: Uint8Array, limit = buf.length) {
    this.buf = buf;
    this.limit = limit;
  }
  reset(buf: Uint8Array): Reader {
    this.buf = buf;
    this.pos = 0;
    this.limit = buf.length;
    return this;
  }
  varint32(): number {
    const buf = this.buf;
    let pos = this.pos;
    if (pos >= this.limit) throw Error("truncated varint");
    let byte = buf[pos]!;
    if (byte < 128) {
      this.pos = pos + 1;
      return byte;
    }
    if (this.limit - pos > 4) {
      let value = (byte & 127) >>> 0;
      byte = buf[++pos]!;
      value = (value | (byte & 127) << 7) >>> 0;
      if (byte < 128) {
        this.pos = pos + 1;
        return value;
      }
      byte = buf[++pos]!;
      value = (value | (byte & 127) << 14) >>> 0;
      if (byte < 128) {
        this.pos = pos + 1;
        return value;
      }
      byte = buf[++pos]!;
      value = (value | (byte & 127) << 21) >>> 0;
      if (byte < 128) {
        this.pos = pos + 1;
        return value;
      }
      byte = buf[++pos]!;
      value = (value | (byte & 15) << 28) >>> 0;
      if (byte < 128) {
        this.pos = pos + 1;
        return value;
      }
      throw Error("varint exceeds 64 bits");
    }
    let value = byte & 127;
    pos++;
    for (let shift = 7; shift < 35; shift += 7) {
      if (pos >= this.limit) throw Error("truncated varint");
      byte = buf[pos++]!;
      value |= (byte & 127) << shift;
      if (byte < 128) {
        this.pos = pos;
        return value >>> 0;
      }
    }
    throw Error("varint exceeds 64 bits");
  }
  varint64(): bigint {
    let value = 0n;
    for (let shift = 0n; shift < 70n; shift += 7n) {
      if (this.pos >= this.limit) throw Error("truncated varint");
      const byte = this.buf[this.pos++]!;
      if (shift === 63n && byte > 1) throw Error("varint exceeds 64 bits");
      value |= BigInt(byte & 127) << shift;
      if (byte < 128) return value;
    }
    throw Error("varint exceeds 64 bits");
  }
  tag(): number {
    if (this.pos < this.limit) {
      const byte = this.buf[this.pos]!;
      if (byte < 128) {
        this.pos++;
        return byte;
      }
    }
    const start = this.pos;
    const tag = Number(this.varint64());
    if (this.pos - start > 5 || tag > 4294967295) throw Error("invalid protobuf tag");
    return tag;
  }
  fixed(size: number): Uint8Array {
    const end = this.pos + size;
    if (end > this.limit) throw Error("truncated protobuf field");
    const value = this.buf.subarray(this.pos, end);
    this.pos = end;
    return value;
  }
  length(): Reader {
    const length = this.varint32();
    if (this.pos + length > this.limit) throw Error("truncated protobuf field");
    const reader = new Reader(this.buf, this.pos + length);
    reader.pos = this.pos;
    this.pos += length;
    return reader;
  }
  string(): string {
    const len = this.varint32();
    const start = this.pos;
    const end = start + len;
    if (end > this.limit) throw Error("truncated protobuf field");
    const buf = this.buf;
    this.pos = end;
    if (len < 32) {
      let s = "";
      let i = start;
      for (; i < end; i++) {
        const c = buf[i]!;
        if (c > 127) break;
        s += String.fromCharCode(c);
      }
      if (i === end) return s;
    }
    const bytes = buf.subarray(start, end);
    const text = textDecoder.decode(bytes);
    if (
      len >= 3 &&
      bytes[0] === 239 &&
      bytes[1] === 187 &&
      bytes[2] === 191 &&
      !text.startsWith("\uFEFF")
    ) {
      return "\uFEFF" + text;
    }
    return text;
  }
  bytes(): Uint8Array {
    const len = this.varint32();
    const end = this.pos + len;
    if (end > this.limit) throw Error("truncated protobuf field");
    const value = new Uint8Array(this.buf.subarray(this.pos, end));
    this.pos = end;
    return value;
  }
}

const scratchReader = /* @__PURE__ */ new Reader(new Uint8Array());

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
  string(v: string): void {
    const len = v.length;
    if (len < 32) {
      let i = 0;
      for (; i < len; i++) {
        if (v.charCodeAt(i) > 127) break;
      }
      if (i === len) {
        this.ensure(1 + len);
        this.buf[this.pos++] = len;
        for (i = 0; i < len; i++) this.buf[this.pos++] = v.charCodeAt(i);
        return;
      }
    }
    const b = textEncoder.encode(v);
    const n = b.length;
    this.ensure(5 + n);
    if (n < 128) this.buf[this.pos++] = n;
    else this.varint32(n);
    this.buf.set(b, this.pos);
    this.pos += n;
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
  begin(): number {
    this.ensure(5);
    const hole = this.pos;
    this.pos += 5;
    return hole;
  }
  end(hole: number): void {
    const start = hole + 5;
    const len = this.pos - start;
    let n = len >>> 0;
    let used = 1;
    while (n > 127) {
      used++;
      n >>>= 7;
    }
    const extra = 5 - used;
    if (extra) {
      this.buf.copyWithin(hole + used, start, this.pos);
      this.pos -= extra;
    }
    n = len >>> 0;
    let p = hole;
    while (n > 127) {
      this.buf[p++] = (n & 127) | 128;
      n >>>= 7;
    }
    this.buf[p] = n;
  }
  reset(): Writer {
    this.pos = 0;
    return this;
  }
}

const scratchWriter = /* @__PURE__ */ new Writer();

const dataView = (bytes: Uint8Array): DataView => new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);

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

const mergeMessage = (into: Record<string, unknown>, value: Record<string, unknown>): void => {
  const keys = Object.keys(value);
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const next = value[key];
    if (next === U) continue;
    const prev = into[key];
    if (Array.isArray(next)) {
      if (prev === U) into[key] = (next as unknown[]).slice();
      else (prev as unknown[]).push(...(next as unknown[]));
    } else if (
      next !== null &&
      typeof next === "object" &&
      !(next instanceof Uint8Array) &&
      prev !== U &&
      typeof prev === "object" &&
      !Array.isArray(prev) &&
      !(prev instanceof Uint8Array)
    ) {
      mergeMessage(prev as Record<string, unknown>, next as Record<string, unknown>);
    } else into[key] = next;
  }
};

const checkedNumber = (value: unknown, min: number, max: number, type: string): number => {
  if (typeof value !== "number" || !Number.isInteger(value) || value < min || value > max) throw Error(`invalid ${type}`);
  return value;
};

const checkedBigint = (value: unknown, min: bigint, max: bigint, type: string): bigint => {
  if (typeof value !== "bigint" || value < min || value > max) throw Error(`invalid ${type}`);
  return value;
};

type Embeds = {
  writer: string;
  wscratch: string;
  reader: string;
  skip: string;
  view: string;
  merge: string;
  num: string;
  big: string;
};

const writeTag = (w: string, tag: number): string =>
  tag < 128
    ? `${w}.pos<${w}.buf.length?${w}.buf[${w}.pos++]=${tag}:${w}.varint32(${tag})`
    : `${w}.varint32(${tag})`;

const writeVarint32 = (w: string, expr: string): string =>
  `${expr}<128&&${w}.pos<${w}.buf.length?${w}.buf[${w}.pos++]=${expr}:${w}.varint32(${expr})`;

const writeCall = (type: ProtobufType, w: string, v: string, e: Embeds): string => {
  if (type === "bool") return `s=${v}?1:0;${w}.pos<${w}.buf.length?${w}.buf[${w}.pos++]=s:${w}.varint32(s)`;
  if (type === "uint32") return `s=${v}>>>0;${writeVarint32(w, "s")}`;
  if (type === "int32" || type === "enum") return `${w}.varint64(BigInt(${v}))`;
  if (type === "sint32") return `s=${v};${w}.varint32(((s<<1)^(s>>31))>>>0)`;
  if (type === "int64") return `${w}.varint64(${e.big}(${v},-9223372036854775808n,9223372036854775807n,"int64"))`;
  if (type === "uint64") return `${w}.varint64(${e.big}(${v},0n,18446744073709551615n,"uint64"))`;
  if (type === "sint64") return `s=${e.big}(${v},-9223372036854775808n,9223372036854775807n,"sint64");${w}.varint64((s<<1n)^(s>>63n))`;
  if (type === "fixed32") return `${w}.bits32(${e.num}(${v},0,4294967295,"fixed32"))`;
  if (type === "sfixed32") return `${w}.bits32(${e.num}(${v},-2147483648,2147483647,"sfixed32"))`;
  if (type === "fixed64") return `${w}.bits64(BigInt.asUintN(64,${e.big}(${v},0n,18446744073709551615n,"fixed64")))`;
  if (type === "sfixed64") return `${w}.bits64(BigInt.asUintN(64,${e.big}(${v},-9223372036854775808n,9223372036854775807n,"sfixed64")))`;
  if (type === "float") return `${w}.float32(${v})`;
  if (type === "double") return `${w}.float64(${v})`;
  if (type === "string") return `${w}.string(${v})`;
  return `s=${v};${writeVarint32(w, "s.length")};${w}.fixed(s)`;
};

const readCall = (type: ProtobufType, r: string, e: Embeds): string => {
  if (type === "bool") return `${r}.varint32()!==0`;
  if (type === "uint32") return `${r}.varint32()`;
  if (type === "int32" || type === "enum") return `Number(BigInt.asIntN(32,${r}.varint64()))`;
  if (type === "sint32") return `((t=${r}.varint32())>>>1^-(t&1))|0`;
  if (type === "int64") return `BigInt.asIntN(64,${r}.varint64())`;
  if (type === "uint64") return `BigInt.asUintN(64,${r}.varint64())`;
  if (type === "sint64") return `(t=${r}.varint64(),BigInt.asIntN(64,(t>>1n)^-(t&1n)))`;
  if (type === "double") return `${e.view}(${r}.fixed(8)).getFloat64(0,true)`;
  if (type === "float") return `${e.view}(${r}.fixed(4)).getFloat32(0,true)`;
  if (type === "fixed64") return `${e.view}(${r}.fixed(8)).getBigUint64(0,true)`;
  if (type === "sfixed64") return `BigInt.asIntN(64,${e.view}(${r}.fixed(8)).getBigUint64(0,true))`;
  if (type === "fixed32") return `${e.view}(${r}.fixed(4)).getUint32(0,true)`;
  if (type === "sfixed32") return `${e.view}(${r}.fixed(4)).getUint32(0,true)|0`;
  if (type === "string") return `${r}.string()`;
  return `${r}.bytes()`;
};

const emitDefault = (field: Field, key: string): string => {
  if (field.repeated) return `o[${key}]=[]`;
  if (field.optional || field.type === "message") return "";
  if (field.type === "string") return `o[${key}]=""`;
  if (field.type === "bytes") return `o[${key}]=new Uint8Array`;
  if (field.type === "bool") return `o[${key}]=!1`;
  if (field.type.includes("64")) return `o[${key}]=0n`;
  return `o[${key}]=0`;
};

const fieldLive = (field: Field): string =>
  field.optional ? "v!=null"
  : field.type === "bytes" ? "v.length"
  : field.type === "float" || field.type === "double" ? "v||v!==v||Object.is(v,-0)"
  : field.type === "string" || field.type === "bool" || field.type.includes("64") ? "v"
  : "(v=+v)";

const encodeBody = (msg: Message, e: Embeds, fns: Map<Message, string>, read: (key: string) => string): string => {
  const body: string[] = [];
  for (let idx = 0; idx < msg.fields.length; idx++) {
    const field = msg.fields[idx]!;
    const key = field.key;
    const tag = field.number * 8 + field.wire;
    const packedTag = field.number * 8 + 2;
    const src = read(key);
    if (field.repeated) {
      let packed = "";
      if (packable[field.type]) {
        packed = `p=new ${e.writer};j=0;while(j<n){${writeCall(field.type, "p", "v[j++]", e)}}b=p.finish();${writeTag("w", packedTag)};${writeVarint32("w", "b.length")};w.fixed(b)`;
      } else if (field.type === "message") {
        const nested = fns.get(field.message!)!;
        packed = `j=0;while(j<n){${writeTag("w", tag)};h=w.begin();${nested}(w,v[j++]);w.end(h)}`;
      } else {
        packed = `j=0;while(j<n){${writeTag("w", tag)};${writeCall(field.type, "w", "v[j++]", e)}}`;
      }
      body.push(`v=${src};n=v.length;if(n){${packed}}`);
    } else if (field.type === "message") {
      const nested = fns.get(field.message!)!;
      body.push(`v=${src};if(v!=null){${writeTag("w", tag)};h=w.begin();${nested}(w,v);w.end(h)}`);
    } else {
      body.push(`v=${src};if(${fieldLive(field)}){${writeTag("w", tag)};${writeCall(field.type, "w", "v", e)}}`);
    }
  }
  return body.join(";");
};

const emitEncodeFn = (input: Val, message: Message, e: Embeds, fns: Map<Message, string>): string => {
  const cached = fns.get(message);
  if (cached !== U) return cached;
  const name = B_varWithoutAllocation(input.g);
  fns.set(message, name);
  for (let idx = 0; idx < message.fields.length; idx++) {
    const nested = message.fields[idx]!.message;
    if (nested) emitEncodeFn(input, nested, e, fns);
  }
  return name;
};

const finishEncodeFn = (message: Message, e: Embeds, fns: Map<Message, string>, input: Val): string => {
  emitEncodeFn(input, message, e, fns);
  const parts: string[] = [];
  fns.forEach((fnName, msg) => {
    if (msg === message) return;
    const fromValue = (key: string) => `value[${JSON.stringify(key)}]`;
    parts.push(`let ${fnName}=function(w,value){var v,j,n,p,b,s,h;${encodeBody(msg, e, fns, fromValue)}};`);
  });
  return parts.join("");
};

const emitDecodeFn = (input: Val, message: Message, e: Embeds, fns: Map<Message, string>): string => {
  const cached = fns.get(message);
  if (cached !== U) return cached;
  const name = B_varWithoutAllocation(input.g);
  fns.set(message, name);
  for (let idx = 0; idx < message.fields.length; idx++) {
    const field = message.fields[idx]!;
    if (field.message) emitDecodeFn(input, field.message, e, fns);
  }
  return name;
};

const finishDecodeFn = (message: Message, e: Embeds, fns: Map<Message, string>): string => {
  const parts: string[] = [];
  fns.forEach((fnName, msg) => {
    const body: string[] = [
      "if(d>=100)throw Error(\"protobuf message nesting limit exceeded\")",
      "var o=Object.create(null),t,w,n,p,c,v",
    ];
    for (let idx = 0; idx < msg.fields.length; idx++) {
      const def = emitDefault(msg.fields[idx]!, JSON.stringify(msg.fields[idx]!.key));
      if (def) body.push(def);
    }
    const cases: string[] = [];
    for (let idx = 0; idx < msg.fields.length; idx++) {
      const field = msg.fields[idx]!;
      const key = JSON.stringify(field.key);
      const expected = field.wire;
      let arm = `case ${field.number}:`;
      if (field.repeated && packable[field.type]) {
        arm += `if(w===2){p=r.length();while(p.pos<p.limit)o[${key}].push(${readCall(field.type, "p", e)});continue}if(w===${expected}){o[${key}].push(${readCall(field.type, "r", e)});continue}break;`;
      } else if (field.type === "message") {
        const nested = fns.get(field.message!)!;
        arm += `if(w===2){c=r.length();v=${nested}(c,d+1);if(c.pos!==c.limit)throw Error("invalid nested protobuf message");`;
        arm += field.repeated
          ? `o[${key}].push(v);continue}break;`
          : `if(o[${key}]!=null)${e.merge}(o[${key}],v);else o[${key}]=v;continue}break;`;
      } else if (field.repeated) {
        arm += `if(w===${expected}){o[${key}].push(${readCall(field.type, "r", e)});continue}break;`;
      } else {
        arm += `if(w===${expected}){o[${key}]=${readCall(field.type, "r", e)};continue}break;`;
      }
      cases.push(arm);
    }
    const miss = msg.strict ? `throw Error("unknown protobuf field "+n)` : `${e.skip}(r,w,n,0)`;
    body.push(`while(r.pos<r.limit){t=r.tag();n=t>>>3;w=t&7;if(!n)throw Error("invalid protobuf field number");switch(n){${cases.join("")}}${miss}}`);
    body.push("return o");
    parts.push(`let ${fnName}=function(r,d){${body.join(";")}};`);
  });
  return parts.join(";");
};

const objectSchemaOf = (input: Val): Internal => {
  if (input.s !== U && input.s.type === objectTag && input.s.properties !== U) return input.s;
  let prev: Val | undefined = input.prev;
  while (prev !== U) {
    if (prev.s !== U && prev.s.type === objectTag && prev.s.properties !== U) return prev.s;
    prev = prev.prev;
  }
  return input.s;
};

const fieldValsOf = (input: Val): Record<string, Val> | undefined => {
  let current: Val | undefined = input;
  while (current !== U) {
    if (current.d !== U) return current.d;
    current = current.prev;
  }
  return U;
};

const protobufDecoder = (input: Val): Val => {
  if (input.s.encoder === protobufEncoder) return instanceDecoder(input);
  const message = compileMessage(objectSchemaOf(input));
  if (message === U) return B_unsupportedDecode(input, input.s, input.e);
  const e: Embeds = {
    writer: B_embedPure(input, Writer),
    wscratch: B_embedPure(input, scratchWriter),
    reader: "",
    skip: "",
    view: "",
    merge: "",
    num: B_embed(input, checkedNumber),
    big: B_embed(input, checkedBigint),
  };
  const d = fieldValsOf(input);
  const readRoot = (key: string): string => {
    const fv = d !== U ? d[key] : U;
    return fv !== U ? fv.i : `${input.v()}[${JSON.stringify(key)}]`;
  };
  const outVar = B_varWithoutAllocation(input.g);
  const fns = new Map<Message, string>();
  emitEncodeFn(input, message, e, fns);
  const nestedCode = finishEncodeFn(message, e, fns, input);
  const body = encodeBody(message, e, fns, readRoot);
  const output = B_next(input, outVar, input.e, input.e);
  output.v = _var;
  output.cp = `${nestedCode}let w=${e.wscratch}.reset(),v,j,n,p,b,s,h;${body};let ${outVar}=w.finish();`;
  output.io = true;
  return output;
};

const protobufEncoder = (input: Val, target: Internal): Val => {
  const message = compileMessage(target);
  if (message === U) return B_unsupportedDecode(input, input.s, target);
  const e: Embeds = {
    writer: "",
    wscratch: "",
    reader: B_embedPure(input, scratchReader),
    skip: B_embed(input, skip),
    view: B_embedPure(input, dataView),
    merge: B_embedPure(input, mergeMessage),
    num: "",
    big: "",
  };
  const bytes = input.v();
  const outVar = B_varWithoutAllocation(input.g);
  const fns = new Map<Message, string>();
  const root = emitDecodeFn(input, message, e, fns);
  const fnsCode = finishDecodeFn(message, e, fns);
  const output = B_next(input, outVar, message.raw, message.schema);
  output.v = _var;
  output.cp = `${fnsCode}let ${outVar}=${root}(${e.reader}.reset(${bytes}),0);`;
  return output;
};

export const protobuf: Internal = /* @__PURE__ */ initSchema(instanceTag, protobufDecoder, (schema) => {
  schema.class = Uint8Array;
  schema.encoder = protobufEncoder;
});
