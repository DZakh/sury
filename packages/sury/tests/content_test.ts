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

  const intoAFile = S.jsonString.with(S.to, S.file, { decode: "unpack", encode: "pack" });
  expect(new Uint8Array(await S.parser(intoAFile)(`"iVBORw=="`).arrayBuffer())).toEqual(png);
  expect(await S.asyncEncoder(intoAFile)(new File([png], "a.png"))).toBe(`"iVBORw=="`);
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
