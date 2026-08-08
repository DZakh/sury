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

test("a schema whose class the runtime lacks says so when it is compiled", () => {
  // Not `i instanceof undefined`, which throws a TypeError naming neither the
  // schema nor the reason, and not at import, which would punish a consumer
  // who never touches these two.
  expect(withoutGlobal("File", `try { S.parser(S.file) } catch (e) { console.log(e.message) }`)).toBe(
    "[Sury] S.file is not supported in this runtime"
  );
  // A bound in front of it doesn't swallow the report.
  expect(
    withoutGlobal("File", `try { S.parser(S.file.with(S.minSize, 3)) } catch (e) { console.log(e.message) }`)
  ).toBe("[Sury] S.file is not supported in this runtime");
  // And the sibling that IS present still compiles.
  expect(withoutGlobal("File", `console.log(typeof S.parser(S.blob))`)).toBe("function");
});

test("a schema whose class the runtime lacks still introspects", () => {
  // `class` gets a stand-in rather than being left undefined: every reader of
  // it dereferences a `.name`, so without one these answered with a raw
  // TypeError naming neither the schema nor the reason.
  expect(withoutGlobal("File", `console.log(S.inputExpression(S.file))`)).toBe("File");
  expect(withoutGlobal("File", `console.log(String(S.file))`)).toBe("Schema<File>");
  expect(
    withoutGlobal("File", `try { S.toJSONSchema(S.file) } catch (e) { console.log(e.message) }`)
  ).toBe("Expected JSON, received File");
  // Deriving a schema from it is a copy, not a compile, so it still works.
  expect(withoutGlobal("File", `console.log(S.inputExpression(S.file.with(S.minSize, 3)))`)).toBe(
    "File.size >= 3"
  );
});

test("a file is a blob but a blob is not a file", () => {
  const file = new File(["abc"], "a.txt");
  expect(S.parser(S.blob)(file)).toBe(file);
  expect(() => S.parser(S.file)(new Blob(["abc"]))).toThrow("Expected File, received Blob");
});
