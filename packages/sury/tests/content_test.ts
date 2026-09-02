import { expect, test } from "vitest";
import * as S from "sury";

// The value side of CONTENT_CODEC_SPEC.md, for the operations whose output the
// spec format can't write down: a golden holding a `Uint8Array`, `Blob` or
// `File` raises "cannot represent a ... instance as spec source code", and a
// compiled op block must run at least one example — so a conversion that only
// ever produces one has no spec at all (see CONTRIBUTING.md's Spec Harness
// Suggestions, which is where the fix belongs). Everything a spec can still
// hold is one: what lands here is the values, which is where the corruption
// this axis fixes used to hide. Non-ASCII bytes throughout, deliberately — an
// ASCII-only fixture round-trips even through the broken UTF-8 path.
const png = new Uint8Array([137, 80, 78, 71]);
const euro = new Uint8Array([226, 130, 172]);

test("bytes in a value position are base64, both ways", () => {
  const schema = S.schema({ payload: S.uint8Array });
  expect(S.encoder(schema, S.jsonString)({ payload: png })).toBe(
    `{"payload":"iVBORw=="}`,
  );
  expect(S.decoder(S.jsonString, schema)(`{"payload":"iVBORw=="}`)).toEqual({
    payload: png,
  });
  // The reading is the same in a JSON value as in JSON text.
  expect(S.decoder(S.json, schema)({ payload: "iVBORw==" })).toEqual({ payload: png });
});

test("a File in a value position reads asynchronously and writes back", async () => {
  const schema = S.schema({ avatar: S.file });
  expect(await S.asyncEncoder(schema, S.jsonString)({ avatar: new File([png], "a.png") })).toBe(
    `{"avatar":"iVBORw=="}`,
  );
  const decoded = S.decoder(S.jsonString, schema)(`{"avatar":"iVBORw=="}`);
  expect(new Uint8Array(await decoded.avatar.arrayBuffer())).toEqual(png);
  // Packing loses the name, so the reverse builds an unnamed file.
  expect(decoded.avatar.name).toBe("");
});

test("a payload transfer moves the bytes unchanged", async () => {
  expect(S.parser(S.base64.with(S.to, S.uint8Array))("iVBORw==")).toEqual(png);
  expect(S.encoder(S.base64.with(S.to, S.uint8Array))(png)).toBe("iVBORw==");
  expect(S.parser(S.uint8Array.with(S.to, S.base64))(png)).toBe("iVBORw==");
  expect(S.parser(S.uint8Array.with(S.to, S.base64url))(png)).toBe("iVBORw");
  expect(S.encoder(S.uint8Array.with(S.to, S.base64url))("iVBORw")).toEqual(png);
  expect(S.parser(S.base64.with(S.to, S.base64url))("iVBORw==")).toBe("iVBORw");
  expect(S.encoder(S.base64.with(S.to, S.base64url))("iVBORw")).toBe("iVBORw==");

  const file = new File([png], "a.png");
  expect(await S.asyncParser(S.file.with(S.to, S.uint8Array))(file)).toEqual(png);
  expect(new Uint8Array(await S.encoder(S.file.with(S.to, S.uint8Array))(png).arrayBuffer())).toEqual(
    png,
  );
  const fromBase64 = S.parser(S.base64.with(S.to, S.file))("iVBORw==");
  expect(new Uint8Array(await fromBase64.arrayBuffer())).toEqual(png);
});

test("a plain string target is UTF-8, and says so about bytes that aren't", () => {
  expect(S.parser(S.uint8Array.with(S.to, S.string))(euro)).toBe("€");
  expect(S.encoder(S.uint8Array.with(S.to, S.string))("€")).toEqual(euro);
  // The reason a value position packs instead: UTF-8 is lossy for arbitrary
  // bytes, and silently so.
  expect(S.parser(S.uint8Array.with(S.to, S.string))(png)).toBe("�PNG");

  const blob = S.parser(S.string.with(S.to, S.blob))("€");
  expect(blob.size).toBe(3);
});

test("a binary container hands over its text, and takes text back", async () => {
  expect(await S.asyncParser(S.file.with(S.to, S.string))(new File(["€"], "a.txt"))).toBe("€");
  expect(await S.asyncEncoder(S.string.with(S.to, S.blob))(new Blob(["€"]))).toBe("€");
  const file = S.encoder(S.file.with(S.to, S.string))("€");
  expect(await file.text()).toBe("€");
  expect(file.name).toBe("");
});

test("a value position reads the field's own head, union arm or not", () => {
  // `S.string.with(S.to, S.uint8Array)` says how it is stored — as text — so a
  // union arm holding one keeps saying it, where the arm holding a bare
  // `S.uint8Array` is stored as base64. Adding `S.optional` must not change the
  // wire form.
  const text = S.jsonString.with(S.to, S.schema({ a: S.optional(S.string.with(S.to, S.uint8Array)) }));
  expect(S.encoder(text)({ a: euro })).toBe(`{"a":"€"}`);
  expect(S.parser(text)(`{"a":"€"}`)).toEqual({ a: euro });
  expect(S.encoder(S.jsonString.with(S.to, S.schema({ a: S.string.with(S.to, S.uint8Array) })))({ a: euro })).toBe(
    `{"a":"€"}`,
  );

  const bytes = S.jsonString.with(S.to, S.schema({ a: S.optional(S.uint8Array) }));
  expect(S.encoder(bytes)({ a: png })).toBe(`{"a":"iVBORw=="}`);
  expect(S.parser(bytes)(`{"a":"iVBORw=="}`)).toEqual({ a: png });
  expect(S.parser(bytes)(`{}`)).toEqual({ a: undefined });
});

test("the content slots pick a reading, and reverse trades them", async () => {
  const bytesAreTheDocument = S.uint8Array.with(S.to, S.jsonString, {
    decode: "unpack",
    encode: "pack",
  });
  expect(S.parser(bytesAreTheDocument)(new TextEncoder().encode(`{"a":1}`))).toBe(`{"a":1}`);
  expect(S.encoder(bytesAreTheDocument)(`{"a":1}`)).toEqual(new TextEncoder().encode(`{"a":1}`));

  const bytesAreAValue = S.uint8Array.with(S.to, S.jsonString, {
    decode: "pack",
    encode: "unpack",
  });
  expect(S.parser(bytesAreAValue)(png)).toBe(`"iVBORw=="`);
  expect(S.encoder(bytesAreAValue)(`"iVBORw=="`)).toEqual(png);

  expect(S.parser(S.uint8Array.with(S.to, S.jsonString, "pack"))(png)).toBe(`"iVBORw=="`);
  expect(S.encoder(S.uint8Array.with(S.to, S.jsonString, "pack"))(`"iVBORw=="`)).toEqual(png);

  const intoAFile = S.jsonString.with(S.to, S.file, { decode: "unpack", encode: "pack" });
  expect(new Uint8Array(await S.parser(intoAFile)(`"iVBORw=="`).arrayBuffer())).toEqual(png);
  expect(await S.asyncEncoder(intoAFile)(new File([png], "a.png"))).toBe(`"iVBORw=="`);
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
  expect(S.parser(S.base64.with(S.to, S.jsonString.with(S.to, claims)))("eyJzdWIiOiJhIn0=")).toEqual({
    sub: "a",
  });
  expect(
    S.parser(S.uint8Array.with(S.to, S.jsonString.with(S.to, claims)))(
      new TextEncoder().encode(`{"sub":"a"}`),
    ),
  ).toEqual({ sub: "a" });
  expect(
    S.encoder(S.uint8Array.with(S.to, S.jsonString.with(S.to, claims)))({ sub: "a" }),
  ).toEqual(new TextEncoder().encode(`{"sub":"a"}`));

  const config = S.file.with(S.to, S.jsonString.with(S.to, claims));
  expect(await S.asyncParser(config)(new File([`{"sub":"a"}`], "c.json"))).toEqual({ sub: "a" });
  expect(await S.encoder(config)({ sub: "a" }).text()).toBe(`{"sub":"a"}`);
});

test("a refinement that only reshapes the text keeps the carrier's marker", () => {
  const trimmed = S.base64.with(S.trim).with(S.to, S.uint8Array);
  expect(S.parser(trimmed)("4oKs")).toEqual(euro);
  expect(S.encoder(trimmed)(euro)).toBe("4oKs");
});

test("the text a string target checks is the one the bytes spell", () => {
  const address = S.email.with(S.to, S.uint8Array);
  expect(S.encoder(address)(new TextEncoder().encode("a@b.co"))).toBe("a@b.co");
  expect(() => S.encoder(address)(new TextEncoder().encode("nope"))).toThrow(
    'Expected email, received "nope"',
  );

  // `noValidation` drops the checks, not the conversion.
  const trusted = S.string.with(S.noValidation, true).with(S.to, S.uint8Array);
  expect(S.encoder(trusted)(euro)).toBe("€");
  expect(S.parser(S.uint8Array.with(S.to, S.string.with(S.noValidation, true)))(euro)).toBe("€");
});

// `S.assertInput` and `S.inputValidator` compile through the same builder chain as the codecs
// above, against a result target that discards the value — and the spec format
// has no op block for either, so this is the only place the pair is pinned (see
// CONTRIBUTING.md's Spec Harness Suggestions).
test("a document still asserts, where nothing is re-represented at all", () => {
  expect(S.assertInput(`"hi"`, S.jsonString)).toBe(undefined);
  expect(S.assertInput({ a: 1 }, S.json)).toBe(undefined);
  expect(S.assertInput("hi", S.string.with(S.to, S.jsonString))).toBe(undefined);
  expect(S.inputValidator({ a: 1 }, S.json)).toBe(true);
  expect(S.inputValidator(`{"a":1}`, S.jsonString)).toBe(true);
  expect(S.inputValidator(42, S.jsonString)).toBe(false);

  // The other half: the result target discards the value, so it cannot stand in
  // for the parse that says the text is a document.
  expect(S.inputValidator("nope", S.jsonString)).toBe(false);
  expect(() => S.assertInput("nope", S.jsonString)).toThrow(
    `Expected JSON string, received "nope"`,
  );
  expect(S.inputValidator(function () {}, S.json)).toBe(false);
});

test("a read that fails is not a case that didn't match", async () => {
  const unreadable = new File([""], "a.txt");
  unreadable.text = () => Promise.reject(new TypeError("read failed"));
  const either = S.union([S.file.with(S.to, S.string), S.blob]);
  await expect(S.asyncParser(either)(unreadable)).rejects.toThrow("read failed");
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
  await expect(S.asyncParser(S.schema({ a: bytes }))({ a: unreadable() })).rejects.toThrow(
    `Failed at ["a"]: TypeError: read failed`,
  );
  await expect(
    S.asyncParser(S.schema({ a: S.optional(bytes) }))({ a: unreadable() }),
  ).rejects.toThrow(TypeError);
});

test("a payload arm keeps its marker through the union narrow", () => {
  // The narrow the union dispatches on is what a carrier's encoder is handed,
  // so an arm that stores bytes as base64 has to still say so. Bytes spelling
  // base64-legal ASCII passed the arm's own pattern otherwise, and the wrong
  // string came back with no error at all.
  const packed = new Uint8Array([97, 98, 99, 100]);
  expect(S.parser(S.uint8Array.with(S.to, S.base64))(packed)).toBe("YWJjZA==");
  expect(S.parser(S.uint8Array.with(S.to, S.optional(S.base64)))(packed)).toBe("YWJjZA==");
  expect(S.parser(S.uint8Array.with(S.to, S.nullable(S.base64)))(packed)).toBe("YWJjZA==");
  // An arm with no payload of its own still takes the text.
  expect(S.parser(S.uint8Array.with(S.to, S.optional(S.string)))(packed)).toBe("abcd");
});
