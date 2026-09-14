// Guards the `@__NO_SIDE_EFFECTS__` annotations on the public API.
//
// They are what lets a consumer's bundler drop schemas it never uses: without
// one, `export const adminSchema = S.schema({…})` in a shared module is an
// unanalyzable call, so it - and every part of Sury it reaches - survives into
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
// Every dual operation (§ operations.ts), with no exceptions: the rule is the
// family, not the member. `isEqual*` would qualify to be annotated - it neither
// validates nor runs a user transform or refinement - and deliberately is not,
// because one annotated member turns a rule anyone can check into a property
// each new operation has to be checked against.
// The immediate call forms execute, and
// a validation-only call discards its result, which an annotated pure call
// would let esbuild drop - silently deleting the validation.
const EFFECTFUL: Record<string, string> = {
  $parseAsResult: "the immediate call forms validate",
  $parseAsResultPromise: "the immediate call forms validate",
  $encodeAsResult: "the immediate call forms validate",
  $encodeAsResultPromise: "the immediate call forms validate",
  $makeAsResult: "the immediate call forms validate",
  $makeAsResultPromise: "the immediate call forms validate",
  parseOrThrow: "the immediate call forms validate",
  parseAsResult: "the immediate call forms validate",
  parseAsPromiseOrReject: "the immediate call forms validate",
  parseAsResultPromise: "the immediate call forms validate",
  parseAsPromisableResult: "the immediate call forms validate",
  decodeOrThrow: "the immediate call forms validate",
  decodeAsResult: "the immediate call forms validate",
  decodeAsPromiseOrReject: "the immediate call forms validate",
  decodeAsResultPromise: "the immediate call forms validate",
  decodeAsPromisableResult: "the immediate call forms validate",
  encodeOrThrow: "the immediate call forms validate",
  encodeAsResult: "the immediate call forms validate",
  encodeAsPromiseOrReject: "the immediate call forms validate",
  encodeAsResultPromise: "the immediate call forms validate",
  encodeAsPromisableResult: "the immediate call forms validate",
  makeInputOrThrow: "the immediate call forms validate",
  makeInputAsResult: "the immediate call forms validate",
  makeInputAsPromiseOrReject: "the immediate call forms validate",
  makeInputAsResultPromise: "the immediate call forms validate",
  makeInputAsPromisableResult: "the immediate call forms validate",
  makeOutputOrThrow: "the immediate call forms validate",
  makeOutputAsResult: "the immediate call forms validate",
  makeOutputAsPromiseOrReject: "the immediate call forms validate",
  makeOutputAsResultPromise: "the immediate call forms validate",
  makeOutputAsPromisableResult: "the immediate call forms validate",
  isInput: "the immediate call forms validate",
  isOutput: "the immediate call forms validate",
  isInputAsPromise: "the immediate call forms validate",
  isOutputAsPromise: "the immediate call forms validate",
  isEqualInput: "an operation, and operations are not annotated",
  isEqualOutput: "an operation, and operations are not annotated",
  assertInputOrThrow: "the immediate call forms validate",
  assertOutputOrThrow: "the immediate call forms validate",
  assertInputAsPromiseOrReject: "the immediate call forms validate",
  assertOutputAsPromiseOrReject: "the immediate call forms validate",
  global: "mutates the global config",
  enableStandardJSONSchema: "registers the converter singleton",
  $setExnId: "mutates the ReScript exception identity",
  Error: "a class, not a factory",
};

// Public name -> the local binding it resolves to in the bundle. Read off the
// emitted `export { … }` block rather than assumed: `enum` is emitted as
// `enum_`, and an added alias would otherwise go unchecked.
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

// A property write at module scope (`schema.encoder = …`) is a statement, not
// a declaration, so no annotation covers it: esbuild keeps the write and with
// it the schema and everything its value reaches - in every bundle, whether or
// not the schema is imported. A schema built by `initSchema` sets its hooks
// inside the initializer callback instead.
test("no module-scope property write on a schema", () => {
  const writes = source
    .split("\n")
    .filter((line) => /^[A-Za-z_$][\w$]*\.[\w$]+ = /.test(line))
    .filter((line) => !line.includes(".prototype = "));
  expect(writes).toEqual([]);
});
