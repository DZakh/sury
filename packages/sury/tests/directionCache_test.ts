// `isAsync` and `hasTransform` are caches of one schema's decode direction.
// A spec can't express what breaks them: the damage needs two calls in a
// particular order (probe a schema, then derive from it), and a spec's goldens
// are computed once per schema. It also crosses schemas — the probe that
// poisons the cache can be of a shared singleton, so the schema that answers
// wrong never had a probe of its own.
import { test, expect } from "vitest";
import * as S from "../index.mjs";

// `S.isAsync` is a runtime export that index.d.ts doesn't declare; the spec
// harness reaches it through the same cast.
const isAsync = (S as unknown as { isAsync: (schema: unknown) => boolean }).isAsync;

const asyncString = () =>
  S.string.with(S.asyncDecoderAssert, async (value) => {
    if (value === "bad") throw new Error("rejected");
  });

test("a probe of the shared singleton doesn't answer for what is built from it", () => {
  // Before: `S.string` cached `isAsync: false`, `.with(S.to, …)` copied it onto
  // the derived schema, and `S.isAsync` answered `false` for a schema whose
  // parse really is async — sending the caller to `S.parser`, which throws
  // `invalid_operation` at the async transform.
  expect(isAsync(S.string)).toBe(false);
  expect(isAsync(S.string.with(S.to, asyncString()))).toBe(true);
});

test("a probe doesn't answer for the reverse direction", () => {
  const schema = asyncString();
  expect(isAsync(schema)).toBe(true);
  // `S.asyncDecoderAssert` is async decoding and a sync pass encoding.
  expect(isAsync(S.reverse(schema))).toBe(false);
});

test("neither cache is enumerable, so no copy can inherit one", () => {
  const schema = asyncString();
  isAsync(schema);
  isAsync(S.reverse(schema));
  for (const key of ["isAsync", "hasTransform"]) {
    expect(Object.keys(schema)).not.toContain(key);
    expect(Object.keys(S.string)).not.toContain(key);
  }
});
