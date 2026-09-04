import {
  anyOfTag,
  arrayTag,
  baseSchema,
  copySchema,
  type Encoder,
  getOrRethrow,
  initSchema,
  instanceTag,
  type Internal,
  noopDecoder,
  numberTag,
  objectTag,
  panic,
  refTag,
  setHas,
  tagFlags,
  U,
  undefinedTag,
  unknown,
  type Val,
} from "../base";
import {
  _var,
  B_embed,
  B_embedPure,
  B_failWithArg,
  B_makeInvalidConversionDetails,
  B_merge,
  B_next,
  B_scope,
  B_unsupportedDecode,
  B_varWithoutAllocation,
} from "../builder";
import { arrayFactory, dictFactory, objectDecoder } from "../composites";
import { getOutputSchema, instanceDecoder, optionalMembers, parse } from "../parse";
import { bigint, bool, float, int, integer, string, unit } from "../primitives";
import type { ProtobufType, StoredField } from "./protobufField";

type Field = {
  number: number;
  type: ProtobufType;
  packed: boolean;
  key: string;
  repeated: boolean;
  optional: boolean;
  wire: number;
  message?: Message;
  // Key type of a map field; unset for anything else.
  map?: ProtobufType;
  oneof?: string;
};

type Message = {
  fields: Field[];
  strict: boolean;
  raw: Internal;
  schema: Internal;
};

// The object a schema's chain starts with: what the wire speaks, the rest of
// the chain running on the value after.
const firstObject = (schema: Internal): Internal | undefined => {
  let current: Internal | undefined = schema;
  while (current !== U && current.type !== objectTag) current = current.to;
  return current;
};

// The object the wire speaks for a schema `toProto` is given: beside
// `S.protobuf` when the chain reaches it (`A.with(S.to, B).with(S.to,
// S.protobuf)` converts to B before the wire), else the chain's first.
const wireObject = (schema: Internal): Internal | undefined => {
  let last: Internal | undefined = U;
  for (let current: Internal | undefined = schema; current !== U; current = current.to) {
    if (current.w) return last || firstObject(current.to!);
    if (current.type === objectTag) last = current;
  }
  return firstObject(schema);
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

const fieldMetadata = (schema: Internal): StoredField | undefined => {
  let current: Internal | undefined = schema;
  while (current !== U) {
    if (current.pb !== U) return current.pb as StoredField;
    current = current.to;
  }
  return U;
};

// Splits `T | undefined` into T, the presence flag, and the arm that
// supplies a default on absence, if any. Several members left over (an
// optional enum) become a union of their own.
const unwrapOptional = (schema: Internal): [Internal, boolean, Internal | undefined] => {
  const output = getOutputSchema(schema);
  if (output.type !== anyOfTag || output.anyOf === U) return [schema, false, U];
  const [values, hasUndefined] = optionalMembers(output);
  if (!hasUndefined || values.length === 0) return [output, false, U];
  const absent = output.anyOf.find((member) => member.type === undefinedTag && member.to !== U);
  return [values.length === 1 ? values[0]! : anyOf(values), true, absent];
};

// A union as a type descriptor only: the raw and normalized sides share it,
// so nothing ever converts through it, and the union compiler stays out of
// the bundle.
const anyOf = (members: Internal[]): Internal => {
  const mut = baseSchema(anyOfTag, false, noopDecoder);
  mut.anyOf = members;
  mut.has = {};
  for (let idx = 0; idx < members.length; idx++) setHas(mut.has, members[idx]!.type);
  return mut;
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

// Parses the present variant of `input` into `target`: the code, the
// expression it leaves the value in, and whether it transformed the value.
// Nothing is materialized, so a parse with nothing to do costs nothing.
const presentParse = (input: Val, source: Internal, target: Internal): [string, string, boolean] => {
  const presentIn = B_scope(input);
  presentIn.io = false;
  presentIn.s = source;
  presentIn.e = target;
  presentIn.u = true;
  const presentOut = parse(presentIn);
  return [B_merge(presentOut), presentOut.i, !!presentOut.t];
};

const optionalMessageEncoder: Encoder = (input, target) => {
  const source = input.s.anyOf![0]!;
  // Probed first, so a present side with nothing to do materializes no var;
  // one with work is generated again against the var it then assigns.
  const [probe, probeResult, transformed] = presentParse(input, source, target);
  if (probe === "" && probeResult === input.i && !transformed) {
    const output = B_next(input, input.i, getOutputSchema(target), target);
    output.t = U;
    return output;
  }
  const v = input.v();
  const [code, result] = presentParse(input, source, target);
  const body = code + (result === v ? "" : `${v}=${result};`);
  const output = B_next(input, v, getOutputSchema(target), target);
  output.v = _var;
  output.io = true;
  output.cp = body === "" ? "" : `if(${v}!==void 0){${body}}`;
  // Reads the nested parse materialized without converting anything are
  // not a transform; reporting one would make the parent rebuild itself.
  if (!transformed) output.t = U;
  return output;
};

// A present nested object converts field-wise into the (non-optional)
// normalized target and an absent one stays absent. A real `T | undefined`
// union on either side would make the pipeline re-validate the whole nested
// object, or refuse the object-to-union conversion outright.
const optionalMessage = (raw: Internal): Internal => {
  const mut = baseSchema(anyOfTag, false, noopDecoder);
  mut.anyOf = [raw, unit];
  mut.has = { [undefinedTag]: true };
  setHas(mut.has, raw.type);
  mut.encoder = optionalMessageEncoder;
  mut.perVariant = true;
  return mut;
};

// `S.optional(T, default)`: the raw side is `T | undefined` whose encoder
// parses a present value into `present` and an absent one through the
// union's default arm, so the decoded value is never re-validated and an
// enum stays open.
const defaultedOptional = (raw: Internal, present: Internal, absent: Internal): Internal => {
  const mut = optionalMessage(raw);
  mut.encoder = (input, target) => {
    const v = input.v();
    const [code, result] = presentParse(input, raw, present);
    const body = code + (result === v ? "" : `${v}=${result};`);
    const absentIn = B_scope(input);
    absentIn.io = false;
    absentIn.s = unit;
    absentIn.e = absent;
    absentIn.u = true;
    const absentOut = parse(absentIn);
    const supply = `${B_merge(absentOut)}${v}=${absentOut.i};`;
    const output = B_next(input, v, getOutputSchema(target), target);
    output.v = _var;
    output.io = true;
    output.cp = body === "" ? `if(${v}===void 0){${supply}}` : `if(${v}!==void 0){${body}}else{${supply}}`;
    return output;
  };
  return mut;
};

const compileMessage = (schema: Internal, seen = new Set<Internal>()): Message | undefined => {
  const output = firstObject(schema);
  if (output === U || output.properties === U || seen.has(output)) return U;
  if (typeof output.additionalItems === objectTag) return U;
  seen.add(output);
  const fields: Field[] = [];
  const numbers = new Set<number>();
  const rawProperties: Record<string, Internal> = Object.create(null);
  const normalizedProperties: Record<string, Internal> = Object.create(null);
  const rawRequired: string[] = [];
  const keys = Object.keys(output.properties);
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const property = output.properties[key]!;
    const metadata = fieldMetadata(property);
    if (metadata === U) return panic(`S.protobuf: field "${key}" has no field number. Give it one with S.protobufField`);
    if (numbers.has(metadata.number)) return panic(`S.protobuf: field number ${metadata.number} of "${key}" is already taken`);
    numbers.add(metadata.number);
    const [propertyValue, optional, absent] = unwrapOptional(property);
    let shape = getOutputSchema(propertyValue);
    const container = shape;
    let repeated = false;
    let map: ProtobufType | undefined;
    if (shape.type === arrayTag && typeof shape.additionalItems === objectTag) {
      if (optional) return panic(`S.protobuf: repeated field "${key}" can't be optional. An absent list decodes to []`);
      repeated = true;
      shape = getOutputSchema(shape.additionalItems as Internal);
    } else if (shape.type === objectTag && typeof shape.additionalItems === objectTag) {
      if (optional) return panic(`S.protobuf: map field "${key}" can't be optional. An absent map decodes to {}`);
      map = metadata.key;
      shape = getOutputSchema(shape.additionalItems as Internal);
    }
    let message: Message | undefined;
    let raw: Internal;
    let normalizedProperty = optional ? propertyValue : property;
    if (metadata.type === "message") {
      const first = firstObject(repeated || map !== U ? (container.additionalItems as Internal) : propertyValue);
      message = first === U ? U : compileMessage(first, new Set(seen));
      if (message === U) return panic(`S.protobuf: field "${key}" is a message but its schema is not an object`);
      // A nested value converts field by field, output to output, so a chain
      // past the object has nothing to run it: only the root's does.
      if (getOutputSchema(first!) !== first) {
        return panic(`S.protobuf: field "${key}" is a message that converts further with S.to, which a nested field can't`);
      }
      raw = optional && absent === U ? optionalMessage(message.raw) : message.raw;
      normalizedProperty = message.schema;
    } else {
      // An enum declared as integer literals keeps its own schema on the raw
      // side: the wire value lands as is, unknown numbers included, the open
      // enum proto3 specifies.
      raw = metadata.type === "enum" && shape.type === anyOfTag ? shape : scalarSchema(metadata.type);
    }
    // A repeated or map message keeps the user's container (its length
    // checks included) around the normalized nested schema, not the user's:
    // an optional nested message inside must stay a light wrap.
    if (absent !== U) {
      raw = defaultedOptional(raw, message ? message.schema : getOutputSchema(propertyValue), absent);
      normalizedProperty = property;
    } else if (optional && message === U && raw !== getOutputSchema(propertyValue)) {
      // A literal or refined scalar has a check to run, which only a present
      // value can pass.
      raw = optionalMessage(raw);
    }
    if (repeated || map !== U) {
      raw = repeated ? arrayFactory(raw) : dictFactory(raw);
      if (message) {
        const normalizedContainer = copySchema(container);
        normalizedContainer.additionalItems = message.schema;
        delete normalizedContainer.to;
        normalizedProperty = normalizedContainer;
      } else normalizedProperty = property;
    } else if (!optional) rawRequired.push(key);
    rawProperties[key] = raw;
    normalizedProperties[key] = normalizedProperty;
    const field: Field = {
      number: metadata.number,
      type: metadata.type,
      packed: metadata.packed,
      key,
      repeated,
      optional,
      message,
      map,
      oneof: metadata.oneof,
      wire: wireType(metadata.type),
    };
    fields.push(field);
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
  return { fields, strict: output.additionalItems === "strict", raw, schema: normalized };
};

const textDecoder = /* @__PURE__ */ new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const textEncoder = /* @__PURE__ */ new TextEncoder();

// Every `new` here is annotated: esbuild keeps an unannotated constructor
// call, and with it this whole module, in every consumer's bundle.
const scratch = /* @__PURE__ */ new Uint8Array(8);
const scratchView = /* @__PURE__ */ new DataView(scratch.buffer);

const truncated = (): never => {
  throw Error("truncated protobuf message");
};

// Every varint reader accepts the full 10-byte form and keeps the bits it
// has room for: that is what makes `int32` -1 (10 bytes on the wire) and a
// `uint32` written from a 64-bit value decode the way C++ does.
class Reader {
  pos = 0;
  buf: Uint8Array;
  limit: number;
  constructor(buf: Uint8Array) {
    this.buf = buf;
    this.limit = buf.length;
  }
  busy = false;
  // Same reentrancy rule as Writer.acquire: a decode started from inside
  // another gets its own reader. Released by the generated code.
  acquire(buf: Uint8Array): Reader {
    const reader = this.busy ? new Reader(buf) : this;
    reader.busy = true;
    reader.buf = buf;
    reader.pos = 0;
    reader.limit = buf.length;
    return reader;
  }
  varint32(): number {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    if (pos >= limit) truncated();
    let byte = buf[pos++]!;
    let value = byte & 127;
    if (byte > 127) {
      if (pos >= limit) truncated();
      byte = buf[pos++]!;
      value |= (byte & 127) << 7;
      if (byte > 127) {
        if (pos >= limit) truncated();
        byte = buf[pos++]!;
        value |= (byte & 127) << 14;
        if (byte > 127) {
          if (pos >= limit) truncated();
          byte = buf[pos++]!;
          value |= (byte & 127) << 21;
          if (byte > 127) {
            if (pos >= limit) truncated();
            byte = buf[pos++]!;
            value |= (byte & 15) << 28;
            let extra = 5;
            while (byte > 127) {
              if (pos >= limit) truncated();
              if (++extra > 10) throw Error("varint exceeds 10 bytes");
              byte = buf[pos++]!;
            }
          }
        }
      }
    }
    this.pos = pos;
    return value >>> 0;
  }
  // Unlike a value, a tag is held to 5 bytes and 32 bits: the conformance
  // suite rejects an overlong tag that a lenient read would accept.
  tag(): number {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let value = 0;
    let shift = 0;
    let byte: number;
    do {
      if (pos >= limit) truncated();
      if (shift > 28) throw Error("invalid protobuf tag");
      byte = buf[pos++]!;
      value |= (byte & 127) << shift;
      shift += 7;
    } while (byte > 127);
    if (shift > 28 && byte > 15) throw Error("invalid protobuf tag");
    this.pos = pos;
    return value >>> 0;
  }
  varint64(): bigint {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let lo = 0;
    let hi = 0;
    let shift = 0;
    let byte: number;
    do {
      if (pos >= limit) truncated();
      if (shift > 63) throw Error("varint exceeds 10 bytes");
      byte = buf[pos++]!;
      if (shift < 28) lo |= (byte & 127) << shift;
      else if (shift === 28) {
        lo |= (byte & 15) << 28;
        hi = (byte & 127) >>> 4;
      } else hi |= (byte & 127) << (shift - 32);
      shift += 7;
    } while (byte > 127);
    this.pos = pos;
    return hi >>> 21 === 0 ? BigInt(hi * 4294967296 + (lo >>> 0)) : (BigInt(hi >>> 0) << 32n) | BigInt(lo >>> 0);
  }
  // Signed reading of the same varint: a value within 2^53 either way is
  // built from one BigInt of a float instead of a shift and an or.
  int64(): bigint {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let lo = 0;
    let hi = 0;
    let shift = 0;
    let byte: number;
    do {
      if (pos >= limit) truncated();
      if (shift > 63) throw Error("varint exceeds 10 bytes");
      byte = buf[pos++]!;
      if (shift < 28) lo |= (byte & 127) << shift;
      else if (shift === 28) {
        lo |= (byte & 15) << 28;
        hi = (byte & 127) >>> 4;
      } else hi |= (byte & 127) << (shift - 32);
      shift += 7;
    } while (byte > 127);
    this.pos = pos;
    if (hi >>> 21 === 0) return BigInt(hi * 4294967296 + (lo >>> 0));
    if (hi >>> 21 === 2047) return BigInt(hi * 4294967296 + (lo >>> 0));
    return BigInt.asIntN(64, (BigInt(hi >>> 0) << 32n) | BigInt(lo >>> 0));
  }
  bool(): boolean {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let set = 0;
    let byte: number;
    let count = 0;
    do {
      if (pos >= limit) truncated();
      if (++count > 10) throw Error("varint exceeds 10 bytes");
      byte = buf[pos++]!;
      set |= byte & 127;
    } while (byte > 127);
    this.pos = pos;
    return set !== 0;
  }
  u32(): number {
    const pos = this.pos;
    if (pos + 4 > this.limit) truncated();
    const buf = this.buf;
    this.pos = pos + 4;
    return (buf[pos]! | (buf[pos + 1]! << 8) | (buf[pos + 2]! << 16) | (buf[pos + 3]! << 24)) >>> 0;
  }
  f32(): number {
    const pos = this.pos;
    if (pos + 4 > this.limit) truncated();
    const buf = this.buf;
    scratch[0] = buf[pos]!;
    scratch[1] = buf[pos + 1]!;
    scratch[2] = buf[pos + 2]!;
    scratch[3] = buf[pos + 3]!;
    this.pos = pos + 4;
    return scratchView.getFloat32(0, true);
  }
  load64(): void {
    const pos = this.pos;
    if (pos + 8 > this.limit) truncated();
    const buf = this.buf;
    scratch[0] = buf[pos]!;
    scratch[1] = buf[pos + 1]!;
    scratch[2] = buf[pos + 2]!;
    scratch[3] = buf[pos + 3]!;
    scratch[4] = buf[pos + 4]!;
    scratch[5] = buf[pos + 5]!;
    scratch[6] = buf[pos + 6]!;
    scratch[7] = buf[pos + 7]!;
    this.pos = pos + 8;
  }
  f64(): number {
    this.load64();
    return scratchView.getFloat64(0, true);
  }
  u64(): bigint {
    this.load64();
    return scratchView.getBigUint64(0, true);
  }
  i64(): bigint {
    this.load64();
    return scratchView.getBigInt64(0, true);
  }
  // A packed varint field read in one method per kind: the loop keeps `pos`
  // in a local where generated code would pay a property read and write on
  // the reader per element, and each kind owns its `push` site so the
  // arrays it fills stay monomorphic. A multi-byte element takes the
  // ordinary reader path.
  u32s(out: number[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      const byte = buf[pos]!;
      if (byte < 128) {
        pos++;
        out.push(byte);
      } else {
        this.pos = pos;
        out.push(this.varint32());
        pos = this.pos;
      }
    }
    this.pos = pos;
  }
  i32s(out: number[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      const byte = buf[pos]!;
      if (byte < 128) {
        pos++;
        out.push(byte);
      } else {
        this.pos = pos;
        out.push(this.varint32() | 0);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }
  s32s(out: number[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      let value = buf[pos]!;
      if (value < 128) pos++;
      else {
        this.pos = pos;
        value = this.varint32();
        pos = this.pos;
      }
      out.push(((value >>> 1) ^ -(value & 1)) | 0);
    }
    this.pos = pos;
  }
  bools(out: boolean[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      const byte = buf[pos]!;
      if (byte < 128) {
        pos++;
        out.push(byte !== 0);
      } else {
        this.pos = pos;
        out.push(this.bool());
        pos = this.pos;
      }
    }
    this.pos = pos;
  }
  // Packed fixed-width fields: an aligned span is read through a typed array
  // over the input, anything else through a DataView, both created once per
  // field instead of a scratch copy per element. `kind` is 0 double, 1 float,
  // 2 fixed32, 3 sfixed32, 4 fixed64, 5 sfixed64.
  fixeds(out: unknown[], kind: number): void {
    const buf = this.buf;
    const start = this.pos;
    const end = this.limit;
    const size = kind === 1 || kind === 2 || kind === 3 ? 4 : 8;
    const len = end - start;
    if (len % size !== 0) truncated();
    const n = len / size;
    const offset = buf.byteOffset + start;
    this.pos = end;
    if (offset % size === 0) {
      const view =
        kind === 0 ? new Float64Array(buf.buffer, offset, n)
        : kind === 1 ? new Float32Array(buf.buffer, offset, n)
        : kind === 2 ? new Uint32Array(buf.buffer, offset, n)
        : kind === 3 ? new Int32Array(buf.buffer, offset, n)
        : kind === 4 ? new BigUint64Array(buf.buffer, offset, n)
        : new BigInt64Array(buf.buffer, offset, n);
      for (let i = 0; i < n; i++) out.push(view[i]);
      return;
    }
    const view = new DataView(buf.buffer, offset, len);
    for (let i = 0; i < len; i += size) {
      out.push(
        kind === 0 ? view.getFloat64(i, true)
        : kind === 1 ? view.getFloat32(i, true)
        : kind === 2 ? view.getUint32(i, true)
        : kind === 3 ? view.getInt32(i, true)
        : kind === 4 ? view.getBigUint64(i, true)
        : view.getBigInt64(i, true),
      );
    }
  }
  // Enters a length-delimited field: narrows `limit` to it and returns the
  // outer limit for the caller to restore. Bounds checks against `limit`
  // are what stop a nested read escaping its field.
  sub(): number {
    const len = this.varint32();
    const end = this.pos + len;
    if (end > this.limit) truncated();
    const outer = this.limit;
    this.limit = end;
    return outer;
  }
  string(): string {
    const len = this.varint32();
    const start = this.pos;
    const end = start + len;
    if (end > this.limit) truncated();
    const buf = this.buf;
    this.pos = end;
    // ASCII up to 48 bytes builds the string eight chars a call, which beats
    // TextDecoder's fixed cost; a byte over 127 hands the span to it.
    if (len < 48) {
      let s = "";
      let i = start;
      let ascii = 0;
      for (; i + 8 <= end; i += 8) {
        const a = buf[i]!, b = buf[i + 1]!, c = buf[i + 2]!, d = buf[i + 3]!;
        const e = buf[i + 4]!, f = buf[i + 5]!, g = buf[i + 6]!, h = buf[i + 7]!;
        ascii |= a | b | c | d | e | f | g | h;
        if (ascii > 127) break;
        s += String.fromCharCode(a, b, c, d, e, f, g, h);
      }
      if (ascii < 128) {
        for (; i < end; i++) {
          const c = buf[i]!;
          if (c > 127) break;
          s += String.fromCharCode(c);
        }
        if (i === end) return s;
      }
    }
    try {
      return textDecoder.decode(buf.subarray(start, end));
    } catch {
      throw Error("protobuf string is not valid UTF-8");
    }
  }
  bytes(): Uint8Array {
    const len = this.varint32();
    const end = this.pos + len;
    if (end > this.limit) truncated();
    const value = this.buf.slice(this.pos, end);
    this.pos = end;
    return value;
  }
}

const scratchReader = /* @__PURE__ */ new Reader(/* @__PURE__ */ new Uint8Array(0));

// Messages are written back to back into one slab and handed out as views
// of it, the way Node's Buffer pool works: a typed array over 64 bytes is
// allocated off-heap, and that allocation cost more than encoding the
// message it held. `base` is where the message being written starts.
class Writer {
  buf = new Uint8Array(8192);
  pos = 0;
  base = 0;
  // Growth keeps every position: a length hole a caller holds is an absolute
  // index, so the buffer is copied from 0 rather than from `base`.
  ensure(n: number): void {
    if (this.pos + n <= this.buf.length) return;
    const next = new Uint8Array(Math.max(this.buf.length * 2, this.pos + n));
    next.set(this.buf.subarray(0, this.pos));
    this.buf = next;
  }
  varint32(value: number): void {
    this.ensure(5);
    const buf = this.buf;
    let pos = this.pos;
    value >>>= 0;
    while (value > 127) {
      buf[pos++] = (value & 127) | 128;
      value >>>= 7;
    }
    buf[pos++] = value;
    this.pos = pos;
  }
  // A negative int32 is sign-extended to 64 bits on the wire: the low 32
  // bits, then 0xF0-masked byte 5, four 0xFF bytes and a closing 0x01.
  int32(value: number): void {
    if (value >= 0) return this.varint32(value);
    this.ensure(10);
    const buf = this.buf;
    let pos = this.pos;
    buf[pos++] = (value & 127) | 128;
    buf[pos++] = ((value >>> 7) & 127) | 128;
    buf[pos++] = ((value >>> 14) & 127) | 128;
    buf[pos++] = ((value >>> 21) & 127) | 128;
    buf[pos++] = (value >>> 28) | 240;
    buf[pos++] = 255;
    buf[pos++] = 255;
    buf[pos++] = 255;
    buf[pos++] = 255;
    buf[pos++] = 1;
    this.pos = pos;
  }
  varint64(value: bigint): void {
    if (value >= 0n && value < 2147483648n) return this.varint32(Number(value));
    let lo: number;
    let hi: number;
    // Within 2^53 the halves come from float arithmetic; `>>> 0` on the
    // negative high word is the two's complement the wire wants.
    if (value >= -9007199254740992n && value <= 9007199254740992n) {
      const num = Number(value);
      hi = Math.floor(num / 4294967296);
      lo = num - hi * 4294967296;
      hi >>>= 0;
    } else {
      value = BigInt.asUintN(64, value);
      lo = Number(value & 4294967295n);
      hi = Number(value >> 32n);
    }
    this.ensure(10);
    const buf = this.buf;
    let pos = this.pos;
    while (hi) {
      buf[pos++] = (lo & 127) | 128;
      lo = ((lo >>> 7) | (hi << 25)) >>> 0;
      hi >>>= 7;
    }
    while (lo > 127) {
      buf[pos++] = (lo & 127) | 128;
      lo >>>= 7;
    }
    buf[pos++] = lo;
    this.pos = pos;
  }
  // The packed-loop twin of Reader.varints, with the buffer sized once for
  // the whole field. `kind` is 0 uint32, 1 int32/enum, 2 sint32, 3 bool.
  varints(values: ArrayLike<number | boolean>, kind: number): void {
    const n = values.length;
    this.ensure(n * 10);
    const buf = this.buf;
    let pos = this.pos;
    for (let i = 0; i < n; i++) {
      let value = values[i] as number;
      if (kind === 3) value = value ? 1 : 0;
      else if (kind === 0) {
        if (value < 0 || value > 4294967295) checkedNumber(value, 0, 4294967295, "uint32");
      } else if (value < -2147483648 || value > 2147483647) {
        checkedNumber(value, -2147483648, 2147483647, kind === 2 ? "sint32" : "int32");
      }
      if (kind === 2) value = ((value << 1) ^ (value >> 31)) >>> 0;
      else if (kind === 1 && value < 0) {
        buf[pos++] = (value & 127) | 128;
        buf[pos++] = ((value >>> 7) & 127) | 128;
        buf[pos++] = ((value >>> 14) & 127) | 128;
        buf[pos++] = ((value >>> 21) & 127) | 128;
        buf[pos++] = (value >>> 28) | 240;
        buf[pos++] = 255;
        buf[pos++] = 255;
        buf[pos++] = 255;
        buf[pos++] = 255;
        buf[pos++] = 1;
        continue;
      } else value >>>= 0;
      while (value > 127) {
        buf[pos++] = (value & 127) | 128;
        value >>>= 7;
      }
      buf[pos++] = value;
    }
    this.pos = pos;
  }
  // Packed fixed-width twin of Reader.fixeds: one typed-array `set` when the
  // slab position is aligned, a DataView loop otherwise.
  fixeds(values: ArrayLike<number | bigint>, kind: number): void {
    const n = values.length;
    const size = kind === 1 || kind === 2 || kind === 3 ? 4 : 8;
    this.ensure(n * size);
    const buf = this.buf;
    const offset = buf.byteOffset + this.pos;
    if (kind === 1) {
      for (let i = 0; i < n; i++) {
        const value = values[i] as number;
        if (Number.isFinite(value) && Math.abs(value) > 3.4028234663852886e38) throw Error("invalid float");
      }
    } else if (kind === 2) for (let i = 0; i < n; i++) checkedNumber(values[i], 0, 4294967295, "fixed32");
    else if (kind === 3) for (let i = 0; i < n; i++) checkedNumber(values[i], -2147483648, 2147483647, "sfixed32");
    else if (kind === 4) for (let i = 0; i < n; i++) checkedBigint(values[i], 0n, 18446744073709551615n, "fixed64");
    else if (kind === 5) for (let i = 0; i < n; i++) checkedBigint(values[i], -9223372036854775808n, 9223372036854775807n, "sfixed64");
    if (offset % size === 0) {
      (kind === 0 ? new Float64Array(buf.buffer, offset, n)
        : kind === 1 ? new Float32Array(buf.buffer, offset, n)
        : kind === 2 ? new Uint32Array(buf.buffer, offset, n)
        : kind === 3 ? new Int32Array(buf.buffer, offset, n)
        : kind === 4 ? new BigUint64Array(buf.buffer, offset, n)
        : new BigInt64Array(buf.buffer, offset, n)
      ).set(values as never);
    } else {
      const view = new DataView(buf.buffer, offset, n * size);
      for (let i = 0; i < n; i++) {
        const value = values[i]!;
        if (kind === 0) view.setFloat64(i * 8, value as number, true);
        else if (kind === 1) view.setFloat32(i * 4, value as number, true);
        else if (kind === 2) view.setUint32(i * 4, value as number, true);
        else if (kind === 3) view.setInt32(i * 4, value as number, true);
        else if (kind === 4) view.setBigUint64(i * 8, value as bigint, true);
        else view.setBigInt64(i * 8, value as bigint, true);
      }
    }
    this.pos += n * size;
  }
  bytes(value: Uint8Array): void {
    const n = value.length;
    this.ensure(5 + n);
    if (n < 128) this.buf[this.pos++] = n;
    else this.varint32(n);
    this.buf.set(value, this.pos);
    this.pos += n;
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
        const buf = this.buf;
        let pos = this.pos;
        buf[pos++] = len;
        for (i = 0; i < len; i++) buf[pos++] = v.charCodeAt(i);
        this.pos = pos;
        return;
      }
    }
    // The length prefix is written after the text, so the text goes in at
    // the widest prefix it could need and slides back when it is shorter.
    const max = len * 3;
    const width = max < 128 ? 1 : max < 16384 ? 2 : max < 2097152 ? 3 : max < 268435456 ? 4 : 5;
    this.ensure(width + max);
    const start = this.pos + width;
    const n = textEncoder.encodeInto(v, this.buf.subarray(start, start + max)).written;
    let used = 1;
    for (let rest = n >>> 7; rest; rest >>>= 7) used++;
    if (used !== width) this.buf.copyWithin(this.pos + used, start, start + n);
    this.varint32(n);
    this.pos += n;
  }
  float32(value: number): void {
    if (Number.isFinite(value) && Math.abs(value) > 3.4028234663852886e38) throw Error("invalid float");
    scratchView.setFloat32(0, value, true);
    this.ensure(4);
    const buf = this.buf;
    const pos = this.pos;
    buf[pos] = scratch[0]!;
    buf[pos + 1] = scratch[1]!;
    buf[pos + 2] = scratch[2]!;
    buf[pos + 3] = scratch[3]!;
    this.pos = pos + 4;
  }
  float64(value: number): void {
    scratchView.setFloat64(0, value, true);
    this.store64();
  }
  bits32(value: number): void {
    this.ensure(4);
    const buf = this.buf;
    const pos = this.pos;
    buf[pos] = value & 255;
    buf[pos + 1] = (value >>> 8) & 255;
    buf[pos + 2] = (value >>> 16) & 255;
    buf[pos + 3] = value >>> 24;
    this.pos = pos + 4;
  }
  bits64(value: bigint): void {
    scratchView.setBigUint64(0, BigInt.asUintN(64, value), true);
    this.store64();
  }
  store64(): void {
    this.ensure(8);
    const buf = this.buf;
    const pos = this.pos;
    buf[pos] = scratch[0]!;
    buf[pos + 1] = scratch[1]!;
    buf[pos + 2] = scratch[2]!;
    buf[pos + 3] = scratch[3]!;
    buf[pos + 4] = scratch[4]!;
    buf[pos + 5] = scratch[5]!;
    buf[pos + 6] = scratch[6]!;
    buf[pos + 7] = scratch[7]!;
    this.pos = pos + 8;
  }
  finish(): Uint8Array {
    const out = this.buf.subarray(this.base, this.pos);
    this.base = this.pos;
    this.busy = false;
    return out;
  }
  // Opens a length-delimited field with a 5-byte hole for the prefix;
  // `end` writes the real prefix and slides the payload back over the slack.
  begin(): number {
    this.ensure(5);
    const hole = this.pos;
    this.pos += 5;
    return hole;
  }
  end(hole: number): void {
    const start = hole + 5;
    const len = this.pos - start;
    let used = 1;
    for (let rest = len >>> 7; rest; rest >>>= 7) used++;
    if (used !== 5) {
      this.buf.copyWithin(hole + used, start, this.pos);
      this.pos -= 5 - used;
    }
    this.pos = hole;
    this.varint32(len);
    this.pos += len;
  }
  busy = false;
  // A field getter or a custom coder may run another protobuf encode in
  // the middle of this one, so a busy scratch writer hands out a fresh one
  // rather than resetting under the outer call.
  acquire(): Writer {
    if (this.busy) return new Writer();
    this.busy = true;
    if (this.buf.length - this.base < 1024) {
      this.buf = new Uint8Array(8192);
      this.base = 0;
    }
    this.pos = this.base;
    return this;
  }
}

const scratchWriter = /* @__PURE__ */ new Writer();

const skip = (reader: Reader, wire: number, fieldNumber: number, depth: number): void => {
  if (wire === 0) reader.varint64();
  else if (wire === 1) reader.load64();
  else if (wire === 2) {
    const outer = reader.sub();
    reader.pos = reader.limit;
    reader.limit = outer;
  } else if (wire === 5) reader.u32();
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

const checkedNumber = (value: unknown, min: number, max: number, type: string): number => {
  if (typeof value !== "number" || !Number.isInteger(value) || value < min || value > max) throw Error(`invalid ${type}`);
  return value;
};

const checkedBigint = (value: unknown, min: bigint, max: bigint, type: string): bigint => {
  if (typeof value !== "bigint" || value < min || value > max) throw Error(`invalid ${type}`);
  return value;
};

const writeTag = (tag: number): string =>
  tag < 128 ? `w.pos<w.buf.length?w.buf[w.pos++]=${tag}:w.varint32(${tag})` : `w.varint32(${tag})`;

const writeVarint32 = (expr: string): string =>
  `${expr}<128&&w.pos<w.buf.length?w.buf[w.pos++]=${expr}:w.varint32(${expr})`;

// `num`/`big` name the range checks in scope: closure params inside a
// hoisted message encoder, `e[N]` embeds in the operation body.
const writeCall = (type: ProtobufType, v: string, num: string, big: string): string => {
  if (type === "bool") return `s=${v}?1:0;w.pos<w.buf.length?w.buf[w.pos++]=s:w.varint32(s)`;
  if (type === "uint32") return `s=${v};if(s<0||s>4294967295)${num}(s,0,4294967295,"uint32");${writeVarint32("s")}`;
  if (type === "int32" || type === "enum") return `s=${v};if(s<-2147483648||s>2147483647)${num}(s,-2147483648,2147483647,"${type}");s>=0?${writeVarint32("s")}:w.int32(s)`;
  if (type === "sint32") return `s=${v};if(s<-2147483648||s>2147483647)${num}(s,-2147483648,2147483647,"sint32");s=((s<<1)^(s>>31))>>>0;${writeVarint32("s")}`;
  if (type === "int64") return `w.varint64(${big}(${v},-9223372036854775808n,9223372036854775807n,"int64"))`;
  if (type === "uint64") return `w.varint64(${big}(${v},0n,18446744073709551615n,"uint64"))`;
  if (type === "sint64") return `s=${big}(${v},-9223372036854775808n,9223372036854775807n,"sint64");w.varint64((s<<1n)^(s>>63n))`;
  if (type === "fixed32") return `w.bits32(${num}(${v},0,4294967295,"fixed32"))`;
  if (type === "sfixed32") return `w.bits32(${num}(${v},-2147483648,2147483647,"sfixed32"))`;
  if (type === "fixed64") return `w.bits64(${big}(${v},0n,18446744073709551615n,"fixed64"))`;
  if (type === "sfixed64") return `w.bits64(${big}(${v},-9223372036854775808n,9223372036854775807n,"sfixed64"))`;
  if (type === "float") return `w.float32(${v})`;
  if (type === "double") return `w.float64(${v})`;
  if (type === "string") return `w.string(${v})`;
  return `w.bytes(${v})`;
};

const fixedKind = (type: ProtobufType): number | undefined =>
  type === "double" ? 0
  : type === "float" ? 1
  : type === "fixed32" ? 2
  : type === "sfixed32" ? 3
  : type === "fixed64" ? 4
  : type === "sfixed64" ? 5
  : U;

const varintKind = (type: ProtobufType): number | undefined =>
  type === "uint32" ? 0
  : type === "int32" || type === "enum" ? 1
  : type === "sint32" ? 2
  : type === "bool" ? 3
  : U;

// A one-byte varint is read inline.
const readCall = (type: ProtobufType): string => {
  const varint32 = "(r.pos<r.limit&&(t=r.buf[r.pos])<128?(r.pos++,t):r.varint32())";
  if (type === "bool") return "r.bool()";
  if (type === "uint32") return varint32;
  if (type === "int32" || type === "enum") return `${varint32}|0`;
  if (type === "sint32") return `((t=${varint32})>>>1^-(t&1))|0`;
  if (type === "int64") return "r.int64()";
  if (type === "uint64") return "r.varint64()";
  if (type === "sint64") return "(t=r.varint64(),(t>>1n)^-(t&1n))";
  if (type === "double") return "r.f64()";
  if (type === "float") return "r.f32()";
  if (type === "fixed64") return "r.u64()";
  if (type === "sfixed64") return "r.i64()";
  if (type === "fixed32") return "r.u32()";
  if (type === "sfixed32") return "r.u32()|0";
  if (type === "string") return "r.string()";
  return "r.bytes()";
};

// A field named after an Object.prototype member (`constructor`,
// `toString`, `__proto__`) reads an inherited value when absent; an own-key
// read keeps it absent.
const readKey = (obj: string, key: string): string => {
  const k = JSON.stringify(key);
  return key in Object.prototype ? `(Object.hasOwn(${obj},${k})?${obj}[${k}]:void 0)` : `${obj}[${k}]`;
};

const scalarDefault = (type: ProtobufType): string =>
  type === "string" ? '""'
  : type === "bytes" ? "new Uint8Array"
  : type === "bool" ? "!1"
  : type.includes("64") ? "0n"
  : "0";

const emitDefault = (field: Field): string => {
  if (field.repeated) return "[]";
  if (field.map !== U) return "{}";
  if (field.optional || field.type === "message") return "void 0";
  return scalarDefault(field.type);
};

// A map key travels as a string property name; these convert it to and
// from the key type on the wire.
const keyToWire = (type: ProtobufType, num: string): string => {
  if (type === "string") return "";
  if (type === "bool") return 'k=k==="true";';
  if (type === "int32" || type === "sint32" || type === "sfixed32") return `k=${num}(+k,-2147483648,2147483647,"${type} key");`;
  if (type === "uint32" || type === "fixed32") return `k=${num}(+k,0,4294967295,"${type} key");`;
  return "k=BigInt(k);";
};

// `numeric`: the value is known to be a number already (a validated field
// val), so the write skips the coercion a nested encoder's untyped read needs.
const fieldLive = (field: Field, numeric: boolean): string =>
  field.optional ? "v!=null"
  : field.type === "bytes" ? "v.length"
  : field.type === "float" || field.type === "double" ? "v||v!==v||Object.is(v,-0)"
  : field.type === "string" || field.type === "bool" || field.type.includes("64") || numeric ? "v"
  : "(v=+v)";

type Read = (key: string) => { expr: string; numeric: boolean };

const encodeBody = (
  msg: Message,
  fns: Map<Message, string>,
  read: Read,
  num: string,
  big: string,
): string => {
  const body: string[] = [];
  for (let idx = 0; idx < msg.fields.length; idx++) {
    const field = msg.fields[idx]!;
    const tag = field.number * 8 + field.wire;
    const { expr: src, numeric } = read(field.key);
    if (field.map !== U) {
      const keyType = field.map;
      const entryTag = writeTag(field.number * 8 + 2);
      const keyPart = `${keyToWire(keyType, num)}${writeTag(8 + wireType(keyType))};${writeCall(keyType, "k", num, big)}`;
      const valuePart = field.type === "message"
        ? `${writeTag(16 + 2)};g=w.begin();${fns.get(field.message!)!}(w,c);w.end(g)`
        : `${writeTag(16 + field.wire)};${writeCall(field.type, "c", num, big)}`;
      body.push(`v=${src};a=Object.keys(v);n=a.length;j=0;while(j<n){k=a[j++];c=v[k];${entryTag};h=w.begin();${keyPart};${valuePart};w.end(h)}`);
    } else if (field.repeated) {
      let loop: string;
      if (packable[field.type] && field.packed) {
        const kind = varintKind(field.type);
        const fixed = fixedKind(field.type);
        const packed = kind !== U ? `w.varints(v,${kind});`
          : fixed !== U ? `w.fixeds(v,${fixed});`
          : `j=0;while(j<n){${writeCall(field.type, "v[j++]", num, big)}}`;
        loop = `${writeTag(field.number * 8 + 2)};h=w.begin();${packed}w.end(h)`;
      } else if (field.type === "message") {
        loop = `j=0;while(j<n){${writeTag(tag)};h=w.begin();${fns.get(field.message!)!}(w,v[j++]);w.end(h)}`;
      } else {
        loop = `j=0;while(j<n){${writeTag(tag)};${writeCall(field.type, "v[j++]", num, big)}}`;
      }
      body.push(`v=${src};n=v.length;if(n){${loop}}`);
    } else if (field.type === "message") {
      body.push(`v=${src};if(v!=null){${writeTag(tag)};h=w.begin();${fns.get(field.message!)!}(w,v);w.end(h)}`);
    } else {
      body.push(`v=${src};if(${fieldLive(field, numeric)}){${writeTag(tag)};${writeCall(field.type, "v", num, big)}}`);
    }
  }
  return body.join(";");
};

const nameMessages = (message: Message, fns: Map<Message, string>): void => {
  if (fns.has(message)) return;
  fns.set(message, `m${fns.size}`);
  for (let idx = 0; idx < message.fields.length; idx++) {
    const nested = message.fields[idx]!.message;
    if (nested) nameMessages(nested, fns);
  }
};

// Message codecs are built once per operation with `Function` and embedded
// as values, so the operation body calls a top-level function instead of
// allocating a closure per call.
const compileEncoders = (root: Message, fns: Map<Message, string>): Record<string, Function> => {
  let src = "";
  fns.forEach((name, msg) => {
    if (msg === root) return;
    src += `function ${name}(w,value){var v,j,n,s,h,a,k,g,c;${encodeBody(msg, fns, (key) => ({ expr: readKey("value", key), numeric: false }), "num", "big")}}`;
  });
  const names = [...fns.values()].filter((name) => name !== fns.get(root));
  return new Function("num", "big", `${src}return {${names.join(",")}}`)(checkedNumber, checkedBigint);
};

// A nested field seen twice merges: the second decode starts from the
// first's fields (`o`) instead of defaults, so scalars last-win, lists
// append and messages merge recursively without a separate merge pass.
const decodeFnSource = (msg: Message, fns: Map<Message, string>): string => {
  const fields = msg.fields;
  const locals: string[] = [];
  const fromPrev: string[] = [];
  const cases: string[] = [];
  let literal = "";
  let optional = "";
  let fill = "";
  for (let idx = 0; idx < fields.length; idx++) {
    const field = fields[idx]!;
    const local = `f${idx}`;
    const key = JSON.stringify(field.key);
    const read = readKey("o", field.key);
    locals.push(`${local}=${emitDefault(field)}`);
    fromPrev.push(`${local}=${read}`);
    let arm = `case ${field.number}:`;
    if (field.oneof !== U) {
      for (let other = 0; other < fields.length; other++) {
        if (other !== idx && fields[other]!.oneof === field.oneof) arm += `f${other}=void 0;`;
      }
    }
    if (field.map !== U) {
      const keyType = field.map;
      const keyWire = wireType(keyType);
      const entryLoop = (body: string) =>
        `while(r.pos<r.limit){t=r.buf[r.pos];if(t<128)r.pos++;else t=r.tag();n=t>>>3;w=t&7;if(!n)throw Error("invalid protobuf field number");${body}else skip(r,w,n,0)}`;
      const readKey = `if(n===1&&w===${keyWire})k=${readCall(keyType)};`;
      const store = keyType === "string"
        ? `k==="__proto__"?Object.defineProperty(${local},k,{value:c,enumerable:!0,writable:!0,configurable:!0}):${local}[k]=c`
        : `${local}[k]=c`;
      let entry: string;
      if (field.type === "message") {
        // The key can follow the value, and a repeated key merges its
        // messages, so the value is decoded after a first pass finds the key.
        const nested = fns.get(field.message!)!;
        entry = `k=${scalarDefault(keyType)};q=r.pos;${entryLoop(readKey)}r.pos=q;c=${local}[k];${entryLoop(`${readKey}else if(n===2&&w===2){g=r.sub();c=${nested}(r,d+1,c);r.limit=g}`)}if(c===void 0){g=r.limit;r.limit=r.pos;c=${nested}(r,d+1);r.limit=g}`;
      } else {
        entry = `k=${scalarDefault(keyType)};c=${scalarDefault(field.type)};${entryLoop(`${readKey}else if(n===2&&w===${field.wire})c=${readCall(field.type)};`)}`;
      }
      arm += `if(w===2){p=r.sub();${entry};r.limit=p;${store};continue}break;`;
    } else if (field.type === "message") {
      const nested = fns.get(field.message!)!;
      arm += field.repeated
        ? `if(w===2){p=r.sub();${local}.push(${nested}(r,d+1));r.limit=p;continue}break;`
        : `if(w===2){p=r.sub();${local}=${nested}(r,d+1,${local});r.limit=p;continue}break;`;
    } else if (field.repeated && packable[field.type]) {
      const kind = varintKind(field.type);
      const fixed = fixedKind(field.type);
      const packedLoop = kind !== U ? `r.${["u32s", "i32s", "s32s", "bools"][kind]}(${local})`
        : fixed !== U ? `r.fixeds(${local},${fixed})`
        : `while(r.pos<r.limit)${local}.push(${readCall(field.type)})`;
      arm += `if(w===2){p=r.sub();${packedLoop};r.limit=p;continue}if(w===${field.wire}){${local}.push(${readCall(field.type)});continue}break;`;
    } else if (field.repeated) {
      arm += `if(w===${field.wire}){${local}.push(${readCall(field.type)});continue}break;`;
    } else {
      arm += `if(w===${field.wire}){${local}=${readCall(field.type)};continue}break;`;
    }
    cases.push(arm);
    if (field.type === "message" && !field.repeated && !field.optional && field.map === U) {
      // A required message absent on the wire is its default instance, so
      // the schema's type holds; `S.optional` is how presence is asked for.
      fill += `if(${local}===void 0){g=r.limit;r.limit=r.pos;${local}=${fns.get(field.message!)!}(r,d+1);r.limit=g}`;
    }
    if (field.map === U && field.optional) {
      optional += field.key === "__proto__"
        ? `if(${local}!==void 0)o={...o,["__proto__"]:${local}};`
        : `if(${local}!==void 0)o[${key}]=${local};`;
    } else literal += `${field.key === "__proto__" ? '["__proto__"]' : key}:${local},`;
  }
  const miss = msg.strict ? 'throw Error("unknown protobuf field "+n)' : "skip(r,w,n,0)";
  const vars = locals.length ? `${locals.join(",")},` : "";
  const merge = fromPrev.length ? `if(o!==void 0){${fromPrev.join(";")}}` : "";
  return `function ${fns.get(msg)!}(r,d,o){if(d>=100)throw Error("protobuf message nesting limit exceeded");var ${vars}t,w,n,p,k,c,q,g;${merge}while(r.pos<r.limit){t=r.buf[r.pos];if(t<128)r.pos++;else t=r.tag();n=t>>>3;w=t&7;switch(n){case 0:throw Error("invalid protobuf field number");${cases.join("")}}${miss}}${fill}o={${literal.slice(0, -1)}};${optional}return o}`;
};

const compileDecoder = (root: Message, fns: Map<Message, string>): Function => {
  let src = "";
  fns.forEach((_, msg) => {
    src += decodeFnSource(msg, fns);
  });
  return new Function("skip", `${src}return ${fns.get(root)!}`)(skip);
};

// The declared object, with the field metadata on its properties. A parsed
// val's `s` is the object rebuilt from what each field parsed to, and a
// union field parses to a fresh schema that lost its `pb`, so the expected
// side of the chain is searched first.
const isMessageShape = (schema: Internal | undefined): schema is Internal =>
  schema !== U && schema.type === objectTag && schema.properties !== U;

const objectSchemaOf = (input: Val): Internal => {
  let current: Val | undefined = input.prev;
  while (current !== U) {
    if (isMessageShape(current.e)) return current.e;
    current = current.prev;
  }
  current = input;
  while (current !== U) {
    if (isMessageShape(current.s)) return current.s;
    current = current.prev;
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
  const fns = new Map<Message, string>();
  nameMessages(message, fns);
  const encoders = compileEncoders(message, fns);
  const names = new Map<Message, string>();
  fns.forEach((name, msg) => {
    names.set(msg, msg === message ? name : B_embedPure(input, encoders[name]));
  });
  const d = fieldValsOf(input);
  const readRoot: Read = (key) => {
    const fv = d !== U ? d[key] : U;
    return fv !== U
      ? { expr: fv.i, numeric: fv.s.type === numberTag && fv.s.format !== U }
      : { expr: readKey(input.v(), key), numeric: false };
  };
  const body = encodeBody(message, names, readRoot, B_embed(input, checkedNumber), B_embed(input, checkedBigint));
  const outVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outVar, input.e, input.e);
  output.v = _var;
  output.cp = `let ${outVar},w;${guarded(input, output, input.e, `w=${B_embedPure(input, scratchWriter)}.acquire();let v,j,n,s,h,a,k,g,c;${body};${outVar}=w.finish()`, "w&&(w.busy=false);")}`;
  output.io = true;
  return output;
};

// A wire or value failure surfaces as a Sury conversion error with the
// operation's path, the way B_conversion reports a coder's throw.
const guarded = (input: Val, output: Val, target: Internal, code: string, release: string): string => {
  const unionContext = input.g.o & 4;
  const rethrow = unionContext ? `${B_embed(input, getOrRethrow)}(x);` : "";
  const failure = B_failWithArg(output, (e: unknown) => B_makeInvalidConversionDetails(input, target, e), "x");
  return `try{${code}}catch(x){${release}${rethrow}${failure}}`;
};

const protobufEncoder = (input: Val, target: Internal): Val => {
  const message = compileMessage(target);
  // Another instance (`S.arrayBuffer`, say) takes the bytes as they are.
  if (message === U) return (tagFlags[target.type]! & 8192) ? input : B_unsupportedDecode(input, input.s, target);
  const fns = new Map<Message, string>();
  nameMessages(message, fns);
  const decoder = B_embed(input, compileDecoder(message, fns));
  const outVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outVar, message.raw, message.schema);
  output.v = _var;
  output.cp = `let ${outVar},r;${guarded(input, output, target, `r=${B_embedPure(input, scratchReader)}.acquire(${input.v()});${outVar}=${decoder}(r,0);r.busy=false`, "r&&(r.busy=false);")}`;
  const object = firstObject(target)!;
  if (getOutputSchema(object) === object) return output;
  // A chain past the object runs on the decoded value once it is in the
  // object's shape (the wire's coercions applied), parsed whole as the
  // chain's input. Not field-wise from the normalized schema: the union
  // compiler refuses `T` into the input's `T | undefined` for an optional
  // field, so a chained root pays a second check, and a literal enum is
  // closed there.
  const normIn = B_scope(output);
  normIn.io = false;
  normIn.s = message.raw;
  normIn.e = message.schema;
  normIn.u = true;
  const normOut = parse(normIn);
  const chainIn = B_scope(normOut);
  chainIn.io = false;
  chainIn.s = unknown;
  chainIn.e = target;
  chainIn.u = true;
  // The normalized parse's field vals would be read as the input's.
  chainIn.d = U;
  const chainOut = parse(chainIn);
  const chained = B_next(output, chainOut.i, getOutputSchema(target), getOutputSchema(target));
  chained.cp = B_merge(normOut) + B_merge(chainOut);
  chained.io = true;
  return chained;
};

export const protobuf: Internal = /* @__PURE__ */ initSchema(instanceTag, protobufDecoder, (schema) => {
  schema.class = Uint8Array;
  schema.encoder = protobufEncoder;
  schema.w = true;
});

type ProtoOptions = { name?: string; package?: string };

// A printed line, or the comment lines a declaration settles once every use
// of it is known.
type Line = string | (() => string[]);

// A field with the user's property and its value schema past optional,
// repeated and map wrapping: what names, comments and enum members print from.
type Use = { field: Field; schema: Internal; shape: Internal; stored: StoredField };

// The meta each use says its type has: a direct use's is what the schema
// carried when it was numbered, a use through `S.optional`, `S.array` or
// `S.record` reaches the declaration and reads it there.
type Meta = { description?: string; deprecated?: boolean };
type TypeDecl = { metas: Meta[] };

type Proto = {
  top: Line[][];
  names: Map<unknown, string>;
  decls: Map<unknown, TypeDecl>;
  // The top-level names: protoc resolves a reference innermost-first, so a
  // nested name may not repeat one, while siblings under different parents
  // may share theirs.
  used: Set<string>;
  // Value names of the top-level enums: enum values live in the scope that
  // holds the enum, beside its fields and other enums' values.
  members: Set<string>;
  ids: Map<object, number>;
  uses: Map<Message, Use[]>;
};

const usesOf = (proto: Proto, message: Message, object: Internal): Use[] => {
  let uses = proto.uses.get(message);
  if (uses === U) {
    uses = message.fields.map((field) => {
      const schema = object.properties![field.key]!;
      let value = unwrapOptional(schema)[0];
      if (field.repeated || field.map !== U) value = getOutputSchema(value).additionalItems as Internal;
      // A message field speaks its chain's first object.
      const shape = field.message !== U ? firstObject(value)! : getOutputSchema(value);
      return { field, schema, shape, stored: fieldMetadata(schema)! };
    });
    proto.uses.set(message, uses);
  }
  return uses;
};

const snakeCase = (name: string): string =>
  name.replace(/([A-Z])([A-Z][a-z])/g, "$1_$2").replace(/([a-z0-9])([A-Z])/g, "$1_$2").toLowerCase();

// A camelCase key prints snake_case, which every generator turns back into the
// same lowerCamel property. Anything else is made an identifier verbatim.
const protoFieldName = (key: string): string =>
  /^[a-z][a-zA-Z0-9]*$/.test(key) ? snakeCase(key) : key.replace(/[^A-Za-z0-9_]/g, "_").replace(/^(?=[0-9])/, "_") || "_";

const protoTypeName = (name: string): string =>
  (
    protoFieldName(name)
      .split("_")
      .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
      .join("") || "Type"
  ).replace(/^(?=[0-9])/, "_");

const uniqueName = (name: string, used: Set<string>, taken?: (name: string) => boolean): string => {
  let candidate = name;
  for (let n = 2; used.has(candidate) || taken?.(candidate); n++) candidate = name + n;
  used.add(candidate);
  return candidate;
};

// The members of an enum shape, a union of integer literals.
const enumValues = (shape: Internal, values = new Set<number>()): number[] => {
  for (let idx = 0; idx < shape.anyOf!.length; idx++) {
    const member = getOutputSchema(shape.anyOf![idx]!);
    if (member.type === numberTag) values.add(member.const as number);
    else if (member.type === anyOfTag) enumValues(member, values);
  }
  return [...values].sort((a, b) => a - b);
};

const isEnumShape = ({ field, shape }: Use): boolean => field.type === "enum" && shape.type === anyOfTag;

// What a field's message or enum is the same type as. `.with` copies a schema
// but shares its properties, so a message is its properties object, or that
// and its name once renamed into another type. A named enum is one type
// wherever its name and members recur; an unnamed one belongs to its field,
// so the next field's identical members stay separate and can diverge without
// a breaking change.
const messageKey = (proto: Proto, properties: object, name: string | undefined): unknown => {
  if (!name) return properties;
  let id = proto.ids.get(properties);
  if (id === U) proto.ids.set(properties, (id = proto.ids.size));
  return `${id}:${name}`;
};

const typeKey = (proto: Proto, use: Use): unknown => {
  const { field, shape } = use;
  const name = nameOf(use);
  if (field.message !== U) return messageKey(proto, shape.properties!, name);
  if (!isEnumShape(use)) return U;
  return name ? `${name}:${enumValues(shape).join(",")}` : field;
};

// The meta a schema carries down its `.to` chain to the object it produces:
// `S.meta` writes to the schema it is called on, so meta set after a `.to`
// sits above the shape.
const chainMeta = (schema: Internal, shape: Internal): Meta & { name?: string } => {
  const meta: Meta & { name?: string } = {};
  for (let current: Internal | undefined = schema; current !== U; current = current.to) {
    if (meta.name === U) meta.name = current.name;
    if (meta.description === U) meta.description = current.description;
    if (meta.deprecated === U) meta.deprecated = current.deprecated;
    if (current === shape) break;
  }
  return meta;
};

// The schema a wrapper holds the type's declaration as, when the field's
// schema reaches the declaration through `S.optional`, `S.array` or
// `S.record` rather than being a copy of it.
const declaredThrough = ({ schema, shape }: Use): Internal | undefined => {
  const output = getOutputSchema(schema);
  if (output === shape) return U;
  const [members, hasUndefined] = optionalMembers(output);
  const member = hasUndefined ? (members.length === 1 ? members[0] : U) : (output.additionalItems as Internal | undefined);
  if (member === U || typeof member !== objectTag) return U;
  for (let current: Internal | undefined = member; current !== U; current = current.to) if (current === shape) return member;
  return U;
};

// The name a use gives its type: down the chain of its own schema for a
// direct use, of the wrapped declaration otherwise.
const nameOf = (use: Use): string | undefined => chainMeta(declaredThrough(use) || use.schema, use.shape).name;

const useType = (decl: TypeDecl, use: Use): void => {
  decl.metas.push(chainMeta(declaredThrough(use) || use.stored.m, use.shape));
};

// The value most of the uses that carry one agree on, the first on a tie;
// what a field carries beyond it prints on the field, so nothing is lost.
const typeMeta = <TKey extends keyof Meta>(decl: TypeDecl, key: TKey): Meta[TKey] => {
  const counts = new Map<Meta[TKey], number>();
  let best: Meta[TKey] = U;
  let bestCount = 0;
  for (const meta of decl.metas) {
    const value = meta[key];
    if (value === U) continue;
    const count = (counts.get(value) || 0) + 1;
    counts.set(value, count);
    if (count > bestCount) (best = value), (bestCount = count);
  }
  return best;
};

// A field's own meta: what its schema carries, or the declaration it wraps,
// beyond the type's.
const fieldMeta = <TKey extends keyof Meta>(use: Use, decl: TypeDecl | undefined, key: TKey): Meta[TKey] => {
  const own = use.schema[key] !== U ? use.schema[key] : use.shape[key];
  return decl !== U && own === typeMeta(decl, key) ? U : own;
};

const commentLines = (text: string | undefined, indent: string): string[] => {
  const trimmed = text ? text.replace(/\s+$/, "") : "";
  return trimmed
    ? trimmed.split("\n").map((line) => {
        const content = line.replace(/\s+$/, "");
        return `${indent}//${content ? ` ${content}` : ""}`;
      })
    : [];
};

const deprecatedLine = (decl: TypeDecl, indent: string): string[] =>
  typeMeta(decl, "deprecated") ? [`${indent}  option deprecated = true;`] : [];

// Named types claim their names before anything prints, so an unnamed type
// nested earlier can't take a name a schema asked for, and a copy used
// without the name still prints under it.
const reserveNames = (proto: Proto, message: Message, object: Internal, seen: Set<unknown>): void => {
  if (seen.has(object.properties)) return;
  seen.add(object.properties);
  for (const use of usesOf(proto, message, object)) {
    const key = typeKey(proto, use);
    const name = key !== U ? nameOf(use) : U;
    if (name && !proto.names.has(key)) proto.names.set(key, uniqueName(protoTypeName(name), proto.used));
    if (use.field.message !== U) reserveNames(proto, use.field.message, use.shape, seen);
  }
};

// Declares the message or enum a field refers to: a named schema goes to the
// top level and an unnamed one nests inside the message that first used it.
// Returns the name the field refers to it by.
const declareType = (
  proto: Proto,
  key: unknown,
  use: Use,
  parent: string,
  indent: string,
  nested: Line[],
  scope: Set<string>,
  fieldNames: Map<string, string>,
  members: Set<string>,
  body: (decl: TypeDecl, name: string, indent: string, members: Set<string>) => Line[]
): string => {
  let decl = proto.decls.get(key);
  if (decl !== U) {
    useType(decl, use);
    return proto.names.get(key)!;
  }
  const reserved = proto.names.get(key);
  decl = { metas: [] };
  useType(decl, use);
  proto.decls.set(key, decl);
  if (reserved !== U) {
    proto.top.push(body(decl, reserved, "", proto.members));
    return reserved;
  }
  // Fields and nested types share a message's scope.
  const typeName = uniqueName(protoTypeName(use.field.key), scope, (n) => proto.used.has(n) || fieldNames.has(n));
  const qualified = `${parent}.${typeName}`;
  proto.names.set(key, qualified);
  nested.push(...body(decl, typeName, `${indent}  `, members));
  return qualified;
};

const enumBody = (
  decl: TypeDecl,
  shape: Internal,
  name: string,
  indent: string,
  members: Set<string>,
  taken: (name: string) => boolean
): Line[] => {
  const values = enumValues(shape);
  // proto3 requires the first member to be zero, and buf lint the suffix.
  const entries: [string, number][] = [["UNSPECIFIED", 0]];
  for (const value of values) if (value !== 0) entries.push([value < 0 ? `MINUS_${-value}` : `${value}`, value]);
  const base = snakeCase(name).toUpperCase();
  let prefix = base;
  for (let n = 2; entries.some(([v]) => members.has(`${prefix}_${v}`) || taken(`${prefix}_${v}`)); n++) prefix = base + n;
  const lines: Line[] = [
    () => commentLines(typeMeta(decl, "description"), indent),
    `${indent}enum ${name} {`,
    () => deprecatedLine(decl, indent),
  ];
  for (const [suffix, value] of entries) {
    const member = `${prefix}_${suffix}`;
    members.add(member);
    lines.push(`${indent}  ${member} = ${value};`);
  }
  lines.push(`${indent}}`);
  return lines;
};

const messageBody = (
  proto: Proto,
  decl: TypeDecl,
  message: Message,
  object: Internal,
  name: string,
  qualified: string,
  indent: string
): Line[] => {
  const nested: Line[] = [];
  const entries: (string | Line[])[] = [];
  const oneofs = new Map<string, Line[]>();
  const fieldNames = new Map<string, string>();
  const jsonNames = new Map<string, string>();
  const scope = new Set<string>();
  const members = new Set<string>();
  // protoc keeps fields apart by their default JSON name, an underscore
  // capitalizing what follows, so `a_b` and `aB` are one field to it (protoc
  // 22 and later; the 3.x line also folded case); a oneof's name only has to
  // be a symbol of its own.
  const claim = (printed: string, key: string, json = printed): void => {
    const taken = fieldNames.get(printed) ?? jsonNames.get(json);
    if (taken !== U) {
      return panic(`S.toProto: "${taken}" and "${key}" of ${qualified} collide as "${json}"`);
    }
    jsonNames.set(json, key);
    fieldNames.set(printed, key);
  };
  const printedNames = new Map<Field, string>();
  const oneofNames = new Map<string, string>();
  for (const field of message.fields) {
    const printed = protoFieldName(field.key);
    printedNames.set(field, printed);
    claim(printed, field.key, printed.replace(/_+(.)?/g, (_, next: string | undefined) => (next ? next.toUpperCase() : "")));
  }
  for (const field of message.fields) {
    if (field.oneof !== U && !oneofs.has(field.oneof)) {
      oneofs.set(field.oneof, []);
      const printed = protoFieldName(field.oneof);
      oneofNames.set(field.oneof, printed);
      // Symbol only: protoc doesn't give a oneof a JSON name.
      const taken = fieldNames.get(printed);
      if (taken !== U) return panic(`S.toProto: "${taken}" and oneof "${field.oneof}" of ${qualified} collide as "${printed}"`);
      fieldNames.set(printed, `oneof ${field.oneof}`);
    }
  }
  for (const use of usesOf(proto, message, object)) {
    const { field } = use;
    const key = typeKey(proto, use);
    let type: string;
    if (field.message !== U) {
      type = declareType(proto, key, use, qualified, indent, nested, scope, fieldNames, members, (fieldDecl, typeName, inner) =>
        messageBody(proto, fieldDecl, field.message!, use.shape, typeName, proto.names.get(key)!, inner)
      );
    } else if (key !== U) {
      type = declareType(proto, key, use, qualified, indent, nested, scope, fieldNames, members, (fieldDecl, typeName, inner, values) =>
        enumBody(fieldDecl, use.shape, typeName, inner, values, values === members ? (n) => fieldNames.has(n) : (n) => proto.used.has(n))
      );
    } else type = field.type === "enum" ? "int32" : field.type;
    // A type nested in this message is referred to by its local name.
    if (type.startsWith(`${qualified}.`) && !type.includes(".", qualified.length + 1)) {
      type = type.slice(qualified.length + 1);
    }
    const fieldDecl = proto.decls.get(key);
    const inner = field.oneof === U ? `${indent}  ` : `${indent}    `;
    const fieldName = printedNames.get(field)!;
    const line = (): string[] => {
      const options: string[] = [];
      if (field.repeated && !field.packed && packable[field.type]) options.push("packed = false");
      if (fieldMeta(use, fieldDecl, "deprecated")) options.push("deprecated = true");
      const suffix = options.length ? ` [${options.join(", ")}]` : "";
      const rule =
        field.oneof !== U ? "" : field.map !== U ? `map<${field.map}, ` : field.repeated ? "repeated " : field.optional ? "optional " : "";
      const close = field.map !== U ? ">" : "";
      return [...commentLines(fieldMeta(use, fieldDecl, "description"), inner), `${inner}${rule}${type}${close} ${fieldName} = ${field.number}${suffix};`];
    };
    if (field.oneof !== U) {
      const oneof = oneofs.get(field.oneof)!;
      if (!oneof.length) entries.push(field.oneof);
      oneof.push(line);
    } else entries.push([line]);
  }
  const lines: Line[] = [
    () => commentLines(typeMeta(decl, "description"), indent),
    `${indent}message ${name} {`,
    () => deprecatedLine(decl, indent),
  ];
  if (nested.length) lines.push(...nested, "");
  for (const entry of entries) {
    if (typeof entry === "string") {
      lines.push(`${indent}  oneof ${oneofNames.get(entry)} {`, ...oneofs.get(entry)!, `${indent}  }`);
    } else lines.push(...entry);
  }
  lines.push(`${indent}}`);
  return lines;
};

const render = (lines: Line[]): string =>
  lines.flatMap((line) => (typeof line === "string" ? line : line())).join("\n");

// @__NO_SIDE_EFFECTS__
export const toProto = (schema: Internal, options?: ProtoOptions): string => {
  if (options?.package && !/^[A-Za-z_]\w*(\.[A-Za-z_]\w*)*$/.test(options.package)) {
    return panic(`S.toProto: "${options.package}" is not a package name`);
  }
  if (options?.name && !/^[A-Za-z_]\w*$/.test(options.name)) {
    return panic(`S.toProto: "${options.name}" is not a message name`);
  }
  const wire = wireObject(schema);
  const message = wire === U ? U : compileMessage(wire);
  if (message === U) {
    return panic(
      getOutputSchema(schema).type === refTag
        ? "S.toProto: a recursive message can't be printed, as S.protobuf can't encode one"
        : "S.toProto: the schema is not an object"
    );
  }
  const output = wire!;
  const rootMeta = chainMeta(schema, output);
  const proto: Proto = {
    top: [],
    names: new Map(),
    decls: new Map(),
    used: new Set(),
    members: new Set(),
    ids: new Map(),
    uses: new Map(),
  };
  const decl: TypeDecl = { metas: [rootMeta] };
  const key = messageKey(proto, output.properties!, rootMeta.name);
  proto.decls.set(key, decl);
  // A root named by the caller or its meta claims first; the default yields
  // to whatever the fields named.
  const given = options?.name || (rootMeta.name && protoTypeName(rootMeta.name));
  if (given) proto.names.set(key, uniqueName(given, proto.used));
  reserveNames(proto, message, output, new Set());
  const name = proto.names.get(key) || uniqueName("Message", proto.used);
  proto.names.set(key, name);
  const root = messageBody(proto, decl, message, output, name, name, "");
  const header = `syntax = "proto3";\n${options?.package ? `\npackage ${options.package};\n` : ""}`;
  return `${header}\n${[root, ...proto.top].map(render).join("\n\n")}\n`;
};
