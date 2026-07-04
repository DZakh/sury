// @generated from specs/string.yaml — DO NOT EDIT.
// source-sha256: f8425fb289ec2f5d6f3a3f1238d70502cdf527b816baadc802a036139d91fc89
// Regenerate with: pnpm spec gen
import { test, expect, expectTypeOf } from "vitest";
import * as S from "../../src/S.js";

const schema = S.string;

test("string › types", () => {
  expectTypeOf(schema).toEqualTypeOf<S.Schema<string, string>>();
});
test("string › jsonSchema", () => {
  expect(S.toJSONSchema(schema)).toStrictEqual({"type":"string"});
  expect(S.toJSONSchema(S.reverse(schema))).toStrictEqual({"type":"string"});
});
test("string › parse › expression", () => {
  expect(S.parser(schema).toString()).toBe("i=>{typeof i===\"string\"||e[0](i);return i}");
});
test("string › parse › valid", () => {
  expect(S.parser(schema)("hello")).toStrictEqual("hello");
});
test("string › parse › empty", () => {
  expect(S.parser(schema)("")).toStrictEqual("");
});
test("string › parse › invalid-number", () => {
  expect(() => S.parser(schema)(42)).toThrow("Expected string, received 42");
});
test("string › parse › invalid-null", () => {
  expect(() => S.parser(schema)(null)).toThrow("Expected string, received null");
});
test("string › decode › expression", () => {
  expect(S.decoder(schema).toString()).toBe("function noopOperation(i) {\n  return i;\n}");
});
test("string › decode › identity", () => {
  expect(S.decoder(schema)("hello")).toStrictEqual("hello");
});
test("string › encode › expression", () => {
  expect(S.encoder(schema).toString()).toBe("function noopOperation(i) {\n  return i;\n}");
});
test("string › encode › identity", () => {
  expect(S.encoder(schema)("hello")).toStrictEqual("hello");
});
