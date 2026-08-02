// Guards the `@__NO_SIDE_EFFECTS__` annotations on the public API.
//
// They are what lets a consumer's bundler drop schemas it never uses: without
// one, `export const adminSchema = S.schema({…})` in a shared module is an
// unanalyzable call, so it — and every part of Sury it reaches — survives into
// a page that only imports `userSchema`.
//
// bundleSize.yaml can't catch a lost annotation: it measures with esbuild,
// which honors `@__NO_SIDE_EFFECTS__` only within a single file and so is blind
// to it across the package boundary (Rollup >= 4 and Rolldown are not). Hence a
// direct assertion on the emitted index.mjs.
import { readFileSync } from "node:fs";
import { test, expect } from "vitest";
import * as S from "../index.mjs";

const source = readFileSync(new URL("../index.mjs", import.meta.url), "utf8");

// Exports whose whole point is the effect, so a bundler must never drop a call
// to them even when the result is unused.
const EFFECTFUL: Record<string, string> = {
  assert: "throws on invalid input — the call IS the assertion",
  is: "wraps assert",
  safe: "runs the callback it's given",
  safeAsync: "runs the callback it's given",
  global: "mutates the global config",
  enableStandardJSONSchema: "registers the converter singleton",
  $res_assertAsyncOrThrow: "throws on invalid input",
  $res_setExnId: "mutates the ReScript exception identity",
  Error: "a class, not a factory",
};

// Public name -> the local binding it resolves to in the bundle. Read off the
// emitted `export { … }` block rather than assumed: `enum` is emitted as
// `enum_`, `record` and `dict` share one binding, and an added alias would
// otherwise go unchecked.
const exportedLocals = (): Map<string, string> => {
  const block = /\nexport \{([^}]*)\};?\s*$/.exec(source);
  expect(block, "index.mjs should end with an export block").not.toBe(null);
  const locals = new Map<string, string>();
  for (const entry of block![1]!.split(",")) {
    const parts = entry.trim().split(/\s+as\s+/);
    if (parts[0]) locals.set(parts[1] ?? parts[0]!, parts[0]!);
  }
  return locals;
};

// esbuild normalizes the source-level `// @__NO_SIDE_EFFECTS__` line into
// whichever of these two forms the declaration takes.
const isAnnotated = (local: string): boolean => {
  const name = local.replace(/\$/g, "\\$");
  const decl = new RegExp(
    `^(?:// @__NO_SIDE_EFFECTS__\\n)?(?:var ${name} = |function ${name}\\().*$`,
    "m",
  ).exec(source);
  expect(decl, `no top-level declaration of ${local} in index.mjs`).not.toBe(null);
  return decl![0].includes("NO_SIDE_EFFECTS");
};

const locals = exportedLocals();
const publicFunctions = Object.keys(S)
  .filter((name) => typeof (S as Record<string, unknown>)[name] === "function")
  .sort();

test("every public factory is annotated pure", () => {
  const missing = publicFunctions.filter(
    (name) => !(name in EFFECTFUL) && !isAnnotated(locals.get(name) ?? name),
  );
  expect(missing).toEqual([]);
});

test("no stale entries in the effectful allowlist", () => {
  expect(Object.keys(EFFECTFUL).filter((name) => !publicFunctions.includes(name))).toEqual([]);
});

// An alias (`export const object = schemaObject`) makes the public name a
// variable that merely *holds* a function; the annotation only counts on the
// declaration that IS the function, so aliasing silently drops it.
test("no public name is an alias of another binding", () => {
  const aliases = publicFunctions
    .filter((name) => !(name in EFFECTFUL))
    .map((name) => locals.get(name) ?? name)
    .filter((local) =>
      new RegExp(`^var ${local.replace(/\$/g, "\\$")} = [A-Za-z_$][\\w$]*;$`, "m").test(source),
    );
  expect(aliases).toEqual([]);
});
