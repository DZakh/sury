// Byte builders mirroring conformance/binary_wireformat.h, so a case reads
// like the conformance suite's source.

export type Bytes = number[];

export const varint = (value: bigint | number): Bytes => {
  let v = BigInt.asUintN(64, BigInt(value));
  const out: Bytes = [];
  while (v > 127n) {
    out.push(Number(v & 127n) | 128);
    v >>= 7n;
  }
  out.push(Number(v));
  return out;
};

// `value` in its shortest form, then `extra` continuation bytes of zero
// payload: a legal but non-minimal varint.
export const longvarint = (value: bigint | number, extra: number): Bytes => {
  const out = varint(value);
  out[out.length - 1]! |= 128;
  for (let i = 1; i < extra; i++) out.push(128);
  out.push(0);
  if (out.length > 10) throw new Error("longvarint exceeds 10 bytes");
  return out;
};

export const tag = (field: number, wire: number): Bytes => varint((field << 3) | wire);

export const delim = (...parts: Bytes[]): Bytes => {
  const body = parts.flat();
  return [...varint(body.length), ...body];
};

export const len = (field: number, ...parts: Bytes[]): Bytes => [...tag(field, 2), ...delim(...parts)];

export const field = (number: number, wire: number, ...parts: Bytes[]): Bytes => [...tag(number, wire), ...parts.flat()];

export const group = (number: number, ...parts: Bytes[]): Bytes => [
  ...tag(number, 3),
  ...parts.flat(),
  ...tag(number, 4),
];

export const u32 = (value: number): Bytes => {
  const view = new DataView(new ArrayBuffer(4));
  view.setUint32(0, value >>> 0, true);
  return [...new Uint8Array(view.buffer)];
};

export const u64 = (value: bigint): Bytes => {
  const view = new DataView(new ArrayBuffer(8));
  view.setBigUint64(0, BigInt.asUintN(64, value), true);
  return [...new Uint8Array(view.buffer)];
};

export const flt = (value: number): Bytes => {
  const view = new DataView(new ArrayBuffer(4));
  view.setFloat32(0, value, true);
  return [...new Uint8Array(view.buffer)];
};

export const dbl = (value: number): Bytes => {
  const view = new DataView(new ArrayBuffer(8));
  view.setFloat64(0, value, true);
  return [...new Uint8Array(view.buffer)];
};

export const zz32 = (value: number): Bytes => varint(((value << 1) ^ (value >> 31)) >>> 0);

export const zz64 = (value: bigint): Bytes => varint((value << 1n) ^ (value >> 63n));

export const utf8 = (text: string): Bytes => [...new TextEncoder().encode(text)];

export const str = (text: string): Bytes => delim(utf8(text));

export const INT32_MAX = 2147483647;
export const INT32_MIN = -2147483648;
export const UINT32_MAX = 4294967295;
export const INT64_MAX = 9223372036854775807n;
export const INT64_MIN = -9223372036854775808n;
export const UINT64_MAX = 18446744073709551615n;
