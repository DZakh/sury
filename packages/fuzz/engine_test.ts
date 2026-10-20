import assert from "node:assert/strict";
import { compileSchema, Coverage, renderSchema } from "./schema";
import { baselineCases, generateCases } from "./generate";
import type { SchemaAst } from "./types";

const generatedCount = baselineCases().length + 20;
const first = generateCases(42, generatedCount, 3);
const second = generateCases(42, generatedCount, 3);
assert.deepEqual(first, second, "the same seed must produce the same compiler cases");
assert.notDeepEqual(
  first,
  generateCases(43, generatedCount, 3),
  "different seeds should vary the corpus",
);

const modes = baselineCases()
  .flatMap((testCase) => testCase.schemas)
  .filter((schema): schema is Extract<SchemaAst, { kind: "to" }> => schema.kind === "to")
  .map((schema) => schema.codec.kind);
assert.ok(modes.includes("builtin"));
assert.ok(modes.includes("custom-decoder"));
assert.ok(modes.includes("custom-bidirectional"));

const custom: SchemaAst = {
  kind: "to",
  source: { kind: "primitive", name: "string" },
  target: { kind: "primitive", name: "number" },
  codec: {
    kind: "custom-bidirectional",
    decoder: "to-number",
    encoder: "to-string",
  },
};
let argumentsSeen: unknown[] | undefined;
const stub = {
  string: { name: "string" },
  number: { name: "number" },
  to: (...args: unknown[]) => {
    argumentsSeen = args;
    return { args };
  },
};
compileSchema(custom, stub, new Coverage());
assert.equal(argumentsSeen?.length, 4);
assert.equal((argumentsSeen?.[2] as (value: unknown) => unknown)("12"), 12);
assert.equal((argumentsSeen?.[3] as (value: unknown) => unknown)(12), "12");
assert.match(renderSchema(custom), /S\.to\(.+<to-number>, <to-string>\)/);

console.log("fuzz engine tests passed");
