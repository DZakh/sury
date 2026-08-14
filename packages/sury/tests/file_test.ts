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
  // the rendering, and `copySchema`'s `Object.assign` for `.with(…)` and
  // `reverse`. Left undefined it produced a TypeError naming neither the
  // schema nor the reason; a schema that built and failed later would be worse
  // still.
  const message = "[Sury] S.file is not supported in this runtime";
  for (const route of [
    `S.parser(S.file)`,
    `S.file.with(S.minSize, 3)`,
    `S.parser(S.reverse(S.file))`,
    `S.inputExpression(S.file)`,
    `String(S.file)`,
    `S.parser(S.union([S.file, S.string]))`,
  ]) {
    expect(withoutGlobal("File", `try { ${route} } catch (e) { console.log(e.message) }`), route).toBe(
      message
    );
  }
  // The JSON Schema emit is the one route that doesn't read `class`: the
  // schema converts itself, and a document describing an upload is worth
  // generating on a runtime that could never hold one — a build step, or a
  // server emitting an OpenAPI spec it doesn't itself submit forms to.
  expect(withoutGlobal("File", `console.log(JSON.stringify(S.toJSONSchema(S.file)))`)).toBe(
    `{"type":"string","contentMediaType":"application/octet-stream"}`
  );
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

test("a file is a blob but a blob is not a file", () => {
  const file = new File(["abc"], "a.txt");
  expect(S.parser(S.blob)(file)).toBe(file);
  expect(() => S.parser(S.file)(new Blob(["abc"]))).toThrow("Expected File, received Blob");
});
