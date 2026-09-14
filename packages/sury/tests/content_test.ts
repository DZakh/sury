import { expect, test } from "vitest";
import * as S from "sury";

// The value side of CONTENT_CODEC_SPEC.md. A `Blob` or `File` output IS
// writable as a golden now (the writer reads the bytes before rendering, and
// `blob.yaml` and the `codec-*` pairs carry them), so what stays here is what a
// spec still cannot hold: a chain built from several schemas at once, and the
// cases whose point is the async shape of the call rather than one schema's
// contract.
// Non-ASCII bytes throughout, deliberately - an ASCII-only fixture round-trips
// even through the broken UTF-8 path.
const png = new Uint8Array([137, 80, 78, 71]);

test("a File in a value position reads asynchronously and writes back", async () => {
  const schema = S.schema({ avatar: S.file });
  expect(await S.encodeAsPromiseOrReject(schema, S.jsonString)({ avatar: new File([png], "a.png") })).toBe(
    `{"avatar":"iVBORw=="}`,
  );
  const decoded = S.decodeOrThrow(S.jsonString, schema)(`{"avatar":"iVBORw=="}`);
  expect(new Uint8Array(await decoded.avatar.arrayBuffer())).toEqual(png);
  // Packing loses the name, so the reverse builds an unnamed file.
  expect(decoded.avatar.name).toBe("");
});

test("a payload transfer moves the bytes unchanged", async () => {
  const file = new File([png], "a.png");
  expect(await S.parseAsPromiseOrReject(S.file.with(S.to, S.uint8Array))(file)).toEqual(png);
  expect(new Uint8Array(await S.encodeOrThrow(S.file.with(S.to, S.uint8Array))(png).arrayBuffer())).toEqual(
    png,
  );
  const fromBase64 = S.parseOrThrow(S.base64.with(S.to, S.file))("iVBORw==");
  expect(new Uint8Array(await fromBase64.arrayBuffer())).toEqual(png);
});

test("a binary container hands over its text, and takes text back", async () => {
  expect(S.parseOrThrow(S.string.with(S.to, S.blob))("€").size).toBe(3);
  expect(await S.parseAsPromiseOrReject(S.file.with(S.to, S.string))(new File(["€"], "a.txt"))).toBe("€");
  expect(await S.encodeAsPromiseOrReject(S.string.with(S.to, S.blob))(new Blob(["€"]))).toBe("€");
  const file = S.encodeOrThrow(S.file.with(S.to, S.string))("€");
  expect(await file.text()).toBe("€");
  expect(file.name).toBe("");
});

test("the content slots pick a reading, and reverse trades them", async () => {
  const intoAFile = S.jsonString.with(S.to, S.file, { decode: "unpack", encode: "pack" });
  expect(new Uint8Array(await S.parseOrThrow(intoAFile)(`"iVBORw=="`).arrayBuffer())).toEqual(png);
  expect(await S.encodeAsPromiseOrReject(intoAFile)(new File([png], "a.png"))).toBe(`"iVBORw=="`);
});

test("a reading is only offered where there are two", () => {
  // Every rejection here is a panic at construction, so no spec can hold the
  // schema (see CONTRIBUTING.md's Spec Harness Suggestions).
  const readings = { decode: "pack", encode: "unpack" } as const;
  // Both sides store bytes as base64, so the link is a transfer and there is
  // nothing for a reading to pick.
  expect(() => S.base64.with(S.to, S.uint8Array, readings)).toThrow(
    "Can't pick a reading for this link",
  );
  expect(() => S.uint8Array.with(S.to, S.base64url, readings)).toThrow(
    "Can't pick a reading for this link",
  );
  expect(() => S.blob.with(S.to, S.file, readings)).toThrow(
    "Can't pick a reading for this link",
  );
  // One side carries no payload at all, and `S.json` has no opened form.
  expect(() => S.string.with(S.to, S.uint8Array, readings)).toThrow(
    "Can't pick a reading for this link",
  );
  expect(() => S.base64.with(S.to, S.json, readings)).toThrow(
    "Can't pick a reading for this link",
  );
  // A reading names what its own direction does, so the two can't agree.
  expect(() =>
    S.base64.with(S.to, S.jsonString, { decode: "pack", encode: "pack" }),
  ).toThrow(`Expected "pack" opposite "unpack"`);
  expect(() =>
    S.base64.with(S.to, S.jsonString, { decode: "pack", encode: "auto" }),
  ).toThrow(`Expected "pack" opposite "unpack"`);
});

test("a declared payload opens the carrier feeding it", async () => {
  const claims = S.schema({ sub: S.string });
  const config = S.file.with(S.to, S.jsonString.with(S.to, claims));
  expect(await S.parseAsPromiseOrReject(config)(new File([`{"sub":"a"}`], "c.json"))).toEqual({ sub: "a" });
  expect(await S.encodeOrThrow(config)({ sub: "a" }).text()).toBe(`{"sub":"a"}`);
});

// `S.assertInputOrThrow` and `S.isInput` compile through the same builder chain as the codecs
// above, against a result target that discards the value - and the spec format
// has no op block for either, so this is the only place the pair is pinned (see
// CONTRIBUTING.md's Spec Harness Suggestions).
test("a document still asserts, where nothing is re-represented at all", () => {
  expect(S.assertInputOrThrow(`"hi"`, S.jsonString)).toBe(undefined);
  expect(S.assertInputOrThrow({ a: 1 }, S.json)).toBe(undefined);
  expect(S.assertInputOrThrow("hi", S.string.with(S.to, S.jsonString))).toBe(undefined);
  expect(S.isInput(S.json)({ a: 1 })).toBe(true);
  expect(S.isInput(S.jsonString)(`{"a":1}`)).toBe(true);
  expect(S.isInput(S.jsonString)(42)).toBe(false);

  // The other half: the result target discards the value, so it cannot stand in
  // for the parse that says the text is a document.
  expect(S.isInput(S.jsonString)("nope")).toBe(false);
  expect(() => S.assertInputOrThrow("nope", S.jsonString)).toThrow(
    `Expected JSON string, received "nope"`,
  );
  expect(S.isInput(S.json)(function () {})).toBe(false);
});

test("a read that fails is not a case that didn't match", async () => {
  const unreadable = new File([""], "a.txt");
  unreadable.text = () => Promise.reject(new TypeError("read failed"));
  const either = S.union([S.file.with(S.to, S.string), S.blob]);
  await expect(S.parseAsPromiseOrReject(either)(unreadable)).rejects.toThrow("read failed");
});

test("a read inside a union arm reports rather than matching a sibling", async () => {
  const unreadable = () => {
    const f = new File([""], "a.txt");
    f.arrayBuffer = () => Promise.reject(new TypeError("read failed"));
    return f;
  };
  const bytes = S.file.with(S.to, S.uint8Array);
  // A required field's rejection is a SuryError and takes the path; inside a
  // union it stays raw, which is what keeps it out of the dispatch.
  await expect(S.parseAsPromiseOrReject(S.schema({ a: bytes }))({ a: unreadable() })).rejects.toThrow(
    `Failed at a: TypeError: read failed`,
  );
  await expect(
    S.parseAsPromiseOrReject(S.schema({ a: S.optional(bytes) }))({ a: unreadable() }),
  ).rejects.toThrow(TypeError);
});
