// `S.base64` / `S.base64url` — a base64 string on the JSON side, bytes on ours.

import {
  flagUnsafeHas,
  initSchema,
  type Internal,
  type StringFormat,
  stringTag,
  tagFlagInstance,
  tagFlags,
  U,
  type Val,
} from "../base";
import { B_embed, B_failWithErrorMessage, B_next } from "../builder";
import { stringDecoderFn } from "../primitives";

// The proposal's methods and Node's Buffer are both absent often enough to need
// a fallback, and neither is typed by the lib this package compiles against.
type NativeBase64 = {
  fromBase64?: (value: string, options?: unknown) => Uint8Array;
  prototype: { toBase64?: (options?: unknown) => string };
};
type MaybeBuffer = {
  Buffer?: {
    from: {
      (value: string, encoding: string): Uint8Array;
      (
        buffer: ArrayBufferLike,
        byteOffset: number,
        length: number,
      ): { toString: (encoding: string) => string };
    };
  };
};

const atobToBytes = (value: string): Uint8Array => {
  const binary = atob(value);
  const length = binary.length;
  const bytes = new Uint8Array(length);
  for (let i = 0; i < length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
};

// `String.fromCharCode(...bytes)` overflows the argument limit somewhere above
// 100k bytes, so the binary string is assembled in chunks. The chunk size can't
// affect the result the way it would when encoding chunk by chunk: btoa runs
// once, over the whole string.
const bytesToAtob = (bytes: Uint8Array): string => {
  let binary = "";
  for (let i = 0; i < bytes.length; i += 4096) {
    binary += String.fromCharCode.apply(U, bytes.subarray(i, i + 4096) as unknown as number[]);
  }
  return btoa(binary);
};

// atob only speaks the standard alphabet, so base64url is translated back to it
// first. A length of 1 mod 4 would leave the padding short, and the format's
// pattern already rejects it.
const urlAtobToBytes = (value: string): Uint8Array =>
  atobToBytes(value.replace(/-/g, "+").replace(/_/g, "/") + "===".slice((value.length + 3) % 4));

const bytesToUrlAtob = (bytes: Uint8Array): string =>
  bytesToAtob(bytes).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");

const urlDecodeOptions = { alphabet: `base64url` };
const urlEncodeOptions = { alphabet: `base64url`, omitPadding: true };

const nativeToBytes = (value: string): Uint8Array =>
  (Uint8Array as unknown as NativeBase64).fromBase64!(value);
const nativeUrlToBytes = (value: string): Uint8Array =>
  (Uint8Array as unknown as NativeBase64).fromBase64!(value, urlDecodeOptions);
const nativeFromBytes = (bytes: Uint8Array): string =>
  (bytes as unknown as { toBase64: (options?: unknown) => string }).toBase64();
const nativeFromUrlBytes = (bytes: Uint8Array): string =>
  (bytes as unknown as { toBase64: (options?: unknown) => string }).toBase64(urlEncodeOptions);

// Buffer.from copies into the shared pool, so the result is a view into a
// larger ArrayBuffer; the wrap hands back a plain Uint8Array whose `buffer` and
// `byteOffset` say what a reader expects.
const bufferToBytes = (value: string, encoding: string): Uint8Array =>
  new Uint8Array((globalThis as MaybeBuffer).Buffer!.from(value, encoding));

// Which primitive to use is resolved once, while the operation is compiled, so
// no value ever pays for a feature test. What is emitted is the same `e[n](…)`
// call either way — the choice rides in the embed, which keeps the generated
// code identical across runtimes and so keeps the specs' goldens honest.
//
// The three primitives disagree about padding, whitespace and alphabet, and
// that is safe here only because the format's pattern runs first and rejects
// every input they disagree about (specs/base64.yaml pins that acceptance). A
// path that reached these without the pattern would decode differently on
// different runtimes.
export const B_toBytes = (input: Val, url: boolean): string => {
  const impl =
    (Uint8Array as unknown as NativeBase64).fromBase64 !== U
      ? url
        ? nativeUrlToBytes
        : nativeToBytes
      : (globalThis as MaybeBuffer).Buffer !== U
        ? (value: string) => bufferToBytes(value, url ? `base64url` : `base64`)
        : url
          ? urlAtobToBytes
          : atobToBytes;
  return `${B_embed(input, impl)}(${input.i})`;
};

export const B_fromBytes = (input: Val, url: boolean): string => {
  const impl =
    (Uint8Array as unknown as NativeBase64).prototype.toBase64 !== U
      ? url
        ? nativeFromUrlBytes
        : nativeFromBytes
      : (globalThis as MaybeBuffer).Buffer !== U
        ? // A view over the same memory: `Buffer.from(bytes)` would copy every
          // byte before encoding it.
          (bytes: Uint8Array) =>
            (globalThis as MaybeBuffer).Buffer!.from(
              bytes.buffer,
              bytes.byteOffset,
              bytes.byteLength,
            ).toString(url ? `base64url` : `base64`)
        : url
          ? bytesToUrlAtob
          : bytesToAtob;
  return `${B_embed(input, impl)}(${input.i})`;
};

// @__NO_SIDE_EFFECTS__
const base64Format = (format: StringFormat, re: RegExp, url: boolean): Internal => {
  // Named rather than taken off the `init` callback's `s`: the decoder is built
  // before the schema exists, and only ever runs after.
  const schema: Internal = initSchema(
    stringTag,
    (input: Val): Val => {
      if (flagUnsafeHas(tagFlags[input.s.type]!, tagFlagInstance) && input.s.class === Uint8Array) {
        // Marked as output so B_markOutput leaves the refiner alone: the string
        // this produces is base64 by construction, and re-testing the pattern
        // against our own encoder's output can only ever pass.
        const output = B_next(input, B_fromBytes(input, url), schema);
        output.io = true;
        return output;
      }
      return stringDecoderFn(input);
    },
    (s) => {
      s.format = format;
      // The same RegExp is the refiner's check and the emitted JSON Schema
      // `pattern`, so a reader of the schema and the code it generates can't be
      // told two different things.
      s.pattern = re;
      s.refiner = (input) => [
        {
          c: (inputVar) => `${B_embed(input, re)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
      s.encoder = (input, target) => {
        if (flagUnsafeHas(tagFlags[target.type]!, tagFlagInstance) && target.class === Uint8Array) {
          return B_next(input, B_toBytes(input, url), target, target);
        }
        return input;
      };
    },
  );
  return schema;
};

export const base64: Internal = /* @__PURE__ */ base64Format(
  "base64",
  /^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/,
  false,
);

// The trailing `{2,3}` is what rejects a length of 1 mod 4, which no base64
// encoding can produce and no decoder can consume.
export const base64url: Internal = /* @__PURE__ */ base64Format(
  "base64url",
  /^(?:[A-Za-z0-9_-]{4})*(?:[A-Za-z0-9_-]{2,3})?$/,
  true,
);
