import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { expect, test } from "vitest";
import * as S from "sury";

// `S.blob`/`S.file` bind their class at import, so the runtime-missing case
// can only be observed in a process that never had the global — which is why
// this is a test and not a spec.
const withoutGlobal = (name: string, body: string): string =>
  execFileSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `delete globalThis.${name};
       const S = await import(${JSON.stringify(fileURLToPath(new URL("../index.mjs", import.meta.url)))});
       ${body}`,
    ],
    { encoding: "utf8" }
  ).trim();

test("every route into a schema the runtime can't support says so", () => {
  // `class` is the one thing all of them read — the decoder's `instanceof`,
  // the rendering and the JSON Schema emit via `.name`, and `copySchema`'s
  // `Object.assign` for `.with(…)` and `reverse`. Left undefined it produced a
  // TypeError naming neither the schema nor the reason; a schema that built
  // and failed later would be worse still.
  const message = "[Sury] S.file is not supported in this runtime";
  for (const route of [
    `S.parser(S.file)`,
    `S.file.with(S.minSize, 3)`,
    `S.parser(S.reverse(S.file))`,
    `S.inputExpression(S.file)`,
    `String(S.file)`,
    `S.toJSONSchema(S.file)`,
    `S.parser(S.union([S.file, S.string]))`,
  ]) {
    expect(withoutGlobal("File", `try { ${route} } catch (e) { console.log(e.message) }`), route).toBe(
      message
    );
  }
  // `reverse` of a self-reversing schema is that schema, so it copies nothing
  // and reads nothing — the report comes when the result is used, above.
  expect(withoutGlobal("File", `console.log(S.reverse(S.file) === S.file)`)).toBe("true");
  // Inspecting one is not using it — util.inspect reports the accessor rather
  // than invoking it, so a `console.log` of a schema never explodes.
  expect(
    withoutGlobal("File", `console.log((await import("node:util")).default.inspect(S.file).length > 0)`)
  ).toBe("true");
  // And the sibling the runtime does have is untouched.
  expect(withoutGlobal("File", `console.log(typeof S.parser(S.blob))`)).toBe("function");
});

// The content codecs are specced (`codec-file-string`, `codec-blob-uint8array`,
// `codec-file-jsonstring-object`) for their codegen and their types, but not for
// what they produce: a Blob only yields its bytes asynchronously, so the harness
// can neither run the async decode direction nor write a File down as the
// expected output of the sync one. That round trip lives here.
test("a blob's content decodes as text or bytes, and encoding builds it back", async () => {
  const text = S.file.with(S.to, S.string);
  expect(await S.asyncParser(text)(new File(["hello"], "a.txt"))).toBe("hello");

  const built = S.encoder(text)("hello");
  expect(built).toBeInstanceOf(File);
  // Nothing in a string names the file it came from, so the reverse leaves the
  // name to the caller rather than inventing one.
  expect(built.name).toBe("");
  expect(await built.text()).toBe("hello");

  const bytes = S.blob.with(S.to, S.uint8Array);
  expect(await S.asyncParser(bytes)(new Blob(["abc"]))).toEqual(new Uint8Array([97, 98, 99]));
  expect(await S.encoder(bytes)(new Uint8Array([97, 98, 99])).text()).toBe("abc");
});

test("a file of JSON parses into a typed value, and encoding rebuilds the upload", async () => {
  const configSchema = S.file.with(S.to, S.jsonString.with(S.to, S.schema({ port: S.number })));

  expect(await S.asyncParser(configSchema)(new File([`{"port":3000}`], "config.json"))).toEqual({
    port: 3000,
  });
  // The file's text is the document, so neither direction re-encodes it — the
  // encoder is a single `JSON.stringify` inside the constructor.
  expect(await S.encoder(configSchema)({ port: 3000 }).text()).toBe(`{"port":3000}`);
});

test("a file is a blob but a blob is not a file", () => {
  const file = new File(["abc"], "a.txt");
  expect(S.parser(S.blob)(file)).toBe(file);
  expect(() => S.parser(S.file)(new Blob(["abc"]))).toThrow("Expected File, received Blob");
});
