import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { expect, test } from "vitest";
import * as S from "sury";

// The value side of `S.formData`, for what the spec format can't write down: a
// golden can't hold a `FormData` (see CONTRIBUTING.md's Spec Harness
// Suggestions), so every `codec-formdata-*` encode block carries only its
// failures, and the entries an encode produces are checked here. Codegen and
// the decode direction stay in the specs.

const form = (...entries: [string, string | Blob][]): FormData => {
  const f = new FormData();
  for (const [key, value] of entries) f.append(key, value);
  return f;
};

const entries = (f: FormData): [string, string | File][] => [...f.entries()];

test("an encode appends one entry per field, in field order, as text", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({
      name: S.string,
      age: S.number,
      agree: S.boolean,
      kind: "signup",
      since: S.date,
      id: S.bigint,
      site: S.url,
    }),
  );
  const encoded = S.encoder(schema)({
    name: "Ann",
    age: 42,
    agree: true,
    kind: "signup",
    since: new Date("2024-01-01T00:00:00.000Z"),
    id: 7n,
    site: new URL("https://sury.dev/"),
  });
  expect(encoded).toBeInstanceOf(FormData);
  expect(entries(encoded)).toEqual([
    ["name", "Ann"],
    ["age", "42"],
    ["agree", "on"],
    ["kind", "signup"],
    ["since", "2024-01-01T00:00:00.000Z"],
    ["id", "7"],
    ["site", "https://sury.dev/"],
  ]);
  // The same schema reads the entries back, coercions included.
  expect(S.decoder(schema)(encoded)).toEqual({
    name: "Ann",
    age: 42,
    agree: true,
    kind: "signup",
    since: new Date("2024-01-01T00:00:00.000Z"),
    id: 7n,
    site: new URL("https://sury.dev/"),
  });
});

test("an absent optional is no entry, and a default fills the absent one back", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ nick: S.optional(S.string), age: S.optional(S.number, 18) }),
  );
  expect(entries(S.encoder(schema)({ age: 18 }))).toEqual([["age", "18"]]);
  expect(entries(S.encoder(schema)({ nick: "nn", age: 42 }))).toEqual([
    ["nick", "nn"],
    ["age", "42"],
  ]);
  expect(S.decoder(schema)(new FormData())).toEqual({ nick: undefined, age: 18 });
  // The empty text input is the absent one.
  expect(S.decoder(schema)(form(["nick", ""], ["age", ""]))).toEqual({ nick: undefined, age: 18 });
});

test("a required boolean is a checkbox: on or nothing", () => {
  const schema = S.formData.with(S.to, S.schema({ agree: S.boolean, notify: S.optional(S.boolean) }));
  expect(entries(S.encoder(schema)({ agree: true, notify: false }))).toEqual([
    ["agree", "on"],
    ["notify", "false"],
  ]);
  expect(entries(S.encoder(schema)({ agree: false }))).toEqual([]);
  expect(S.decoder(schema)(S.encoder(schema)({ agree: false, notify: true }))).toEqual({
    agree: false,
    notify: true,
  });
  expect(S.decoder(schema)(form(["agree", "on"]))).toEqual({ agree: true, notify: undefined });
});

test("an array is a repeated key, and an empty array is no entry", () => {
  const schema = S.formData.with(S.to, S.schema({ tags: S.array(S.string), ids: S.array(S.number) }));
  expect(entries(S.encoder(schema)({ tags: ["a", "b"], ids: [1, 2] }))).toEqual([
    ["tags", "a"],
    ["tags", "b"],
    ["ids", "1"],
    ["ids", "2"],
  ]);
  expect(entries(S.encoder(schema)({ tags: [], ids: [] }))).toEqual([]);
  expect(S.decoder(schema)(form(["ids", "1"], ["tags", "x"], ["ids", "2"]))).toEqual({
    tags: ["x"],
    ids: [1, 2],
  });
});

test("a file travels as itself, name included, and a blob becomes a file", async () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ avatar: S.file, cover: S.optional(S.file), raw: S.blob }),
  );
  const avatar = new File(["a"], "a.png", { type: "image/png" });
  const encoded = S.encoder(schema)({ avatar, raw: new Blob(["r"]) });
  const sent = entries(encoded) as [string, File][];
  const sentAvatar = sent[0]![1];
  const sentRaw = sent[1]![1];
  expect(sentAvatar.name).toBe("a.png");
  expect(sentAvatar.type).toBe("image/png");
  expect(await sentAvatar.text()).toBe("a");
  // `append` wraps a bare blob in a File, which is what `S.blob` still accepts.
  expect(sentRaw).toBeInstanceOf(File);
  expect(await sentRaw.text()).toBe("r");
  const decoded = S.decoder(schema)(encoded);
  expect(decoded.avatar).toBe(sentAvatar);
  expect(decoded.cover).toBe(undefined);
  expect(decoded.raw).toBe(sentRaw);
});

test("a nested document is a JSON text field, both ways", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ prefs: S.jsonString.with(S.to, S.schema({ theme: S.string, size: S.number })) }),
  );
  const encoded = S.encoder(schema)({ prefs: { theme: "dark", size: 2 } });
  expect(entries(encoded)).toEqual([["prefs", `{"theme":"dark","size":2}`]]);
  expect(S.decoder(schema)(encoded)).toEqual({ prefs: { theme: "dark", size: 2 } });
});

test("the reverse is spelled the same as jsonString's", () => {
  const user = S.schema({ name: S.string, age: S.number });
  const value = { name: "Ann", age: 42 };
  expect(entries(S.encoder(user, S.formData)(value))).toEqual([
    ["name", "Ann"],
    ["age", "42"],
  ]);
  expect(S.decoder(S.formData, user)(S.encoder(user, S.formData)(value))).toEqual(value);
  expect(entries(S.parser(user.with(S.to, S.formData))(value))).toEqual([
    ["name", "Ann"],
    ["age", "42"],
  ]);
});

// `S.formData` binds its class at import, like `S.file`, so the
// runtime-missing case can only be observed in a process that never had the
// global.
const withoutGlobal = (body: string): string =>
  execFileSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `delete globalThis.FormData;
       const S = await import(${JSON.stringify(fileURLToPath(new URL("../index.mjs", import.meta.url)))});
       ${body}`,
    ],
    { encoding: "utf8" },
  ).trim();

test("a runtime without FormData says so on every route into the schema", () => {
  const message = "[Sury] S.formData is not supported in this runtime";
  for (const route of [
    `S.parser(S.formData)`,
    `S.inputExpression(S.formData)`,
    `S.encoder(S.schema({ a: S.string }), S.formData)`,
    `S.decoder(S.formData, S.schema({ a: S.string }))`,
  ]) {
    expect(withoutGlobal(`try { ${route} } catch (e) { console.log(e.message) }`), route).toBe(message);
  }
  expect(withoutGlobal(`console.log(typeof S.parser(S.file))`)).toBe("function");
});
