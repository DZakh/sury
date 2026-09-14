import { expect, test } from "vitest";
import * as S from "sury";
import { withoutGlobal } from "./withoutGlobal";

test("every route into a schema the runtime can't support says so", () => {
  // `class` is the one thing all of them read - the decoder's `instanceof`,
  // the rendering and the JSON Schema emit via `.name`, and `copySchema`'s
  // `Object.assign` for `.with(…)` and `reverse`. Left undefined it produced a
  // TypeError naming neither the schema nor the reason; a schema that built
  // and failed later would be worse still.
  const message = "[Sury] S.file is not supported in this runtime";
  for (const route of [
    `S.parseOrThrow(S.file)`,
    `S.file.with(S.minSize, 3)`,
    `S.parseOrThrow(S.reverse(S.file))`,
    `S.toInputExpression(S.file)`,
    `String(S.file)`,
    `S.toInputJSONSchemaOrThrow(S.file)`,
    // Even converting a carrier: the encode-reverse copies the target, and
    // `copySchema` reads `class` like every other route.
    `S.toInputJSONSchemaOrThrow(S.string.with(S.to, S.file))`,
    `S.parseOrThrow(S.union([S.file, S.string]))`,
  ]) {
    expect(withoutGlobal("File", `try { ${route} } catch (e) { console.log(e.message) }`), route).toBe(
      message
    );
  }
  // `reverse` of a self-reversing schema is that schema, so it copies nothing
  // and reads nothing - the report comes when the result is used, above.
  expect(withoutGlobal("File", `console.log(S.reverse(S.file) === S.file)`)).toBe("true");
  // Inspecting one is not using it - util.inspect reports the accessor rather
  // than invoking it, so a `console.log` of a schema never explodes.
  expect(
    withoutGlobal("File", `console.log((await import("node:util")).default.inspect(S.file).length > 0)`)
  ).toBe("true");
  // And the sibling the runtime does have is untouched.
  expect(withoutGlobal("File", `console.log(typeof S.parseOrThrow(S.blob))`)).toBe("function");
});

test("a file is a blob but a blob is not a file", () => {
  const file = new File(["abc"], "a.txt");
  expect(S.parseOrThrow(S.blob)(file)).toBe(file);
  expect(() => S.parseOrThrow(S.file)(new Blob(["abc"]))).toThrow("Expected File, received Blob");
});

// A variadic call is not a schema, so no spec can hold it.
test("FIXME: a three-schema chain decodes but does not encode", async () => {
  const user = S.schema({ name: S.string.with(S.nonEmpty) });
  const decoded = await S.decodeAsPromiseOrReject(S.file, S.jsonString, S.array(user))(
    new File(['[{"name":"Ann"}]'], "users.json"),
  );
  expect(decoded).toEqual([{ name: "Ann" }]);
  // The nested spelling of the same chain encodes.
  expect(
    S.encodeOrThrow(S.file.with(S.to, S.jsonString.with(S.to, S.array(user))))([{ name: "Ann" }]),
  ).toBeInstanceOf(File);
  // Written as its own pipeline, `jsonString -> file` declares nothing, so the
  // pair is the ambiguous one rather than the declared one the decode side has.
  expect(() => S.encodeOrThrow(S.array(user), S.jsonString, S.file)).toThrow(
    "Ambiguous JSON string -> File",
  );
});
