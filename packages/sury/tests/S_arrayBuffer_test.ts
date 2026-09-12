import { test } from "vitest";

import * as S from "../index.mjs";

// Buffer identity is what these check, which a spec example can't express.
test("arrayBuffer hands over a whole buffer and copies a partial view to size", (t) => {
  const toBuffer = S.decodeOrThrow(S.uint8Array, S.arrayBuffer);
  const whole = new Uint8Array([1, 2, 3]);
  t.expect(toBuffer(whole)).toBe(whole.buffer);
  const view = new Uint8Array(new ArrayBuffer(16), 4, 3);
  view.set([7, 8, 9]);
  const owned = toBuffer(view);
  t.expect(owned).not.toBe(view.buffer);
  t.expect(owned.byteLength).toBe(3);
  t.expect([...new Uint8Array(owned)]).toEqual([7, 8, 9]);
});

test("arrayBuffer to bytes is a view, not a copy", (t) => {
  const toBytes = S.decodeOrThrow(S.arrayBuffer, S.uint8Array);
  const buffer = new Uint8Array([4, 5]).buffer;
  const bytes = toBytes(buffer);
  t.expect(bytes.buffer).toBe(buffer);
  t.expect([...bytes]).toEqual([4, 5]);
});

test("protobuf output converts to an owned buffer and back", (t) => {
  const Message = S.schema({ id: S.int32.with(S.protobufField, 1) });
  const Wire = S.arrayBuffer.with(S.to, S.protobuf).with(S.to, Message);
  const encode = S.encodeOrThrow(Wire);
  const decode = S.decodeOrThrow(Wire);
  const buffer = encode({ id: 150 });
  t.expect(buffer.byteLength).toBe(3);
  t.expect([...new Uint8Array(buffer)]).toEqual([8, 150, 1]);
  t.expect(decode(buffer)).toEqual({ id: 150 });
});

test("arrayBuffer validates and rejects other instances", (t) => {
  t.expect(S.parseOrThrow(S.arrayBuffer)(new ArrayBuffer(1)).byteLength).toBe(1);
  t.expect(() => S.parseOrThrow(S.arrayBuffer)(new Uint8Array(1))).toThrow();
  t.expect(() => S.decodeOrThrow(S.arrayBuffer, S.string)).toThrow();
});
