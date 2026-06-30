import { bench, describe } from "vitest";
import * as S from "../src/S.js";

// Sury's own runtime hot-path benchmarks across object, union, and recursive
// schemas. Instrumented by CodSpeed in CI; run locally with
// `pnpm --filter=sury bench`.

// Skip NaN number validation so float parsing isn't penalized — matches the
// config used by the cross-library comparison benchmark.
S.global({ disableNanNumberValidation: true });

const data = Object.freeze({
  number: 1,
  negNumber: -1,
  maxNumber: Number.MAX_VALUE,
  string: "string",
  longString:
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
  boolean: true,
  deeplyNested: {
    foo: "bar",
    num: 1,
    bool: false,
  },
});

const makeSchema = () =>
  S.schema({
    number: S.number,
    negNumber: S.number,
    maxNumber: S.number,
    string: S.string,
    longString: S.string,
    boolean: S.boolean,
    deeplyNested: {
      foo: S.string,
      num: S.number,
      bool: S.boolean,
    },
  });

const schema = makeSchema();
const parse = S.parser(schema);
const serialize = S.encoder(schema);

describe("object", () => {
  // Schema construction only — no parsing.
  bench("object: create", () => {
    makeSchema();
  });

  // Hot path: parse with an already-compiled parser (what users pay per call).
  bench("object: parse", () => {
    parse(data);
  });

  // Cold path: build the schema and compile its parser, then parse once.
  bench("object: create + parse", () => {
    S.parser(makeSchema())(data);
  });

  // Output -> Input. No transforms here, so this measures reverse-pipeline
  // assembly without any per-field conversion.
  bench("object: serialize", () => {
    serialize(data);
  });

  // Validate in place: narrows the input and throws on mismatch, no output.
  bench("object: assert", () => {
    S.assert(data, schema);
  });
});

// Union of an object member and a string->number transform — exercises union
// dispatch, with one branch that transforms and one that doesn't.
const union = S.union([{ box: S.string }, S.string.with(S.to, S.number)]);
const parseUnion = S.parser(union);

describe("union", () => {
  // Dispatch to the object branch (no transform).
  bench("union: parse object branch", () => {
    parseUnion({ box: "abc" });
  });

  // Dispatch to the string->number branch (runs the transform).
  bench("union: parse transform branch", () => {
    parseUnion("123");
  });
});

// Recursive tagged tree: each node holds an array of child nodes. Exercises
// S.recursive together with union dispatch and array recursion.
type Tree = { type: "node"; children: Tree[] } | string;
const tree = S.recursive<Tree>("Tree", (self) =>
  S.union([S.schema({ type: "node" as const, children: S.array(self) }), S.string])
);
const parseTree = S.parser(tree);

const makeTree = (depth: number, breadth: number): Tree =>
  depth === 0
    ? "leaf"
    : { type: "node", children: Array.from({ length: breadth }, () => makeTree(depth - 1, breadth)) };

const treeData = makeTree(3, 10);

describe("recursive", () => {
  bench("recursive: parse nested tree", () => {
    parseTree(treeData);
  });
});

// --- Historical results (archive) ---------------------------------------
// Snapshots from the retired ReScript Benchmark.js harness
// (packages/e2e/src/benchmark/Benchmark.res), kept for cross-version
// reference. They predate CodSpeed and use the old harness's names
// (S.schema / S.object / S.string), so they do not map 1:1 to the
// benchmarks above — treat them as a perf changelog, not live fixtures.
/*
V7.0.1
makeObjectSchema: 0.174ms
parseOrThrow: 1: 0.465ms
parseOrThrow: 2: 0.006ms
parseOrThrow: 3: 0.004ms
serializeWith: 1: 0.208ms
serializeWith: 2: 0.004ms
serializeWith: 3: 0.003ms
S.Error.make: 0.029ms
Parse string x 607,790,506 ops/sec ±0.21% (100 runs sampled)
Reverse convert string x 607,895,909 ops/sec ±0.23% (99 runs sampled)
Advanced object schema factory x 789,559 ops/sec ±0.28% (99 runs sampled)
Parse advanced object x 70,550,720 ops/sec ±0.51% (98 runs sampled)
Create and parse advanced object x 54,592 ops/sec ±0.49% (93 runs sampled)
Parse advanced strict object x 26,614,621 ops/sec ±0.30% (93 runs sampled)
Reverse convert advanced object x 598,233,913 ops/sec ±0.19% (95 runs sampled)
 */

/*
PR remove-definer (before s.nested)

testData1 serialize: 4.949ms
testData1 parse: 3.77ms
testData2 serialize: 0.514ms
testData2 parse: 0.646ms
makeObjectSchema: 0.142ms
parseOrThrow: 1: 0.169ms
parseOrThrow: 2: 0.004ms
parseOrThrow: 3: 0.004ms
serializeWith: 1: 0.095ms
serializeWith: 2: 0.003ms
serializeWith: 3: 0.002ms
S.Error.make: 0.031ms
S.schema - make x 1,123,935 ops/sec ±0.32% (92 runs sampled)
S.schema - make + parse x 187,355 ops/sec ±1.15% (97 runs sampled)
S.schema - parse x 94,323,009 ops/sec ±2.90% (86 runs sampled)
S.schema - parse strict x 29,137,739 ops/sec ±0.78% (95 runs sampled)
S.schema - make + reverse x 1,087,350 ops/sec ±1.33% (96 runs sampled)
S.schema - make + reverse convert x 391,049 ops/sec ±0.94% (98 runs sampled)
S.schema - reverse convert x 108,649,950 ops/sec ±0.95% (99 runs sampled)
S.schema - reverse convert (compiled) x 180,119,516 ops/sec ±6.45% (77 runs sampled)
S.schema - assert x 95,220,749 ops/sec ±3.50% (87 runs sampled)
S.schema - assert (compiled) x 106,000,705 ops/sec ±1.96% (89 runs sampled)
S.schema - assert strict x 29,048,110 ops/sec ±0.85% (95 runs sampled)
S.object - make x 1,061,761 ops/sec ±0.35% (98 runs sampled)
S.object - make + parse x 147,676 ops/sec ±0.25% (97 runs sampled)
S.object - parse x 50,794,005 ops/sec ±1.43% (95 runs sampled)
S.object - make + reverse x 254,673 ops/sec ±0.58% (96 runs sampled)
S.object - make + reverse convert x 135,036 ops/sec ±0.84% (91 runs sampled)
S.object - reverse convert x 58,534,347 ops/sec ±1.90% (85 runs sampled)
S.string - parse x 92,787,449 ops/sec ±2.85% (91 runs sampled)
S.string - reverse convert x 102,776,056 ops/sec ±2.27% (89 runs sampled)

 */

/*
V9 final touches

testData1 serialize: 3.269ms
testData1 parse: 2.357ms
testData2 serialize: 1.077ms
testData2 parse: 0.552ms
makeObjectSchema: 0.156ms
parseOrThrow: 1: 0.328ms
parseOrThrow: 2: 0.012ms
parseOrThrow: 3: 0.005ms
serializeWith: 1: 0.127ms
serializeWith: 2: 0.003ms
serializeWith: 3: 0.002ms
S.Error.make: 0.047ms
S.schema - make x 1,950,402 ops/sec ±1.29% (98 runs sampled)
S.schema - make + parse x 191,489 ops/sec ±2.24% (95 runs sampled)
S.schema - parse x 75,990,331 ops/sec ±2.68% (89 runs sampled)
S.schema - parse strict x 26,483,566 ops/sec ±1.32% (92 runs sampled)
S.schema - make + reverse x 1,354,272 ops/sec ±0.48% (98 runs sampled)
S.schema - make + reverse convert x 404,155 ops/sec ±0.72% (97 runs sampled)
S.schema - reverse convert x 103,090,924 ops/sec ±1.81% (93 runs sampled)
S.schema - reverse convert (compiled) x 181,516,443 ops/sec ±5.88% (75 runs sampled)
S.schema - assert x 97,051,911 ops/sec ±3.07% (87 runs sampled)
S.schema - assert (compiled) x 106,033,893 ops/sec ±2.03% (94 runs sampled)
S.schema - assert strict x 28,592,103 ops/sec ±1.53% (94 runs sampled)
S.object - make x 1,541,313 ops/sec ±2.32% (94 runs sampled)
S.object - make + parse x 150,339 ops/sec ±1.60% (94 runs sampled)
S.object - parse x 53,976,976 ops/sec ±1.99% (91 runs sampled)
S.object - make + reverse x 256,418 ops/sec ±0.81% (93 runs sampled)
S.object - make + reverse convert x 151,959 ops/sec ±1.52% (91 runs sampled)
S.object - reverse convert x 57,477,511 ops/sec ±2.25% (87 runs sampled)
S.string - parse x 87,622,705 ops/sec ±2.25% (91 runs sampled)
S.string - reverse convert x 95,897,043 ops/sec ±1.64% (92 runs sampled)
 */

/* 10.0.0-rc.3
 testData1 serialize: 1.536ms
testData1 parse: 1.165ms
testData2 serialize: 0.352ms
testData2 parse: 0.565ms
makeObjectSchema: 0.075ms
parseOrThrow: 1: 0.297ms
parseOrThrow: 2: 0.004ms
parseOrThrow: 3: 0.003ms
serializeWith: 1: 0.142ms
serializeWith: 2: 0.003ms
serializeWith: 3: 0.003ms
S.Error.make: 0.04ms
S.schema - make x 1,888,248 ops/sec ±1.49% (91 runs sampled)
S.schema - make + parse x 149,921 ops/sec ±1.55% (96 runs sampled)
S.schema - parse x 91,150,808 ops/sec ±3.32% (88 runs sampled)
S.schema - parse strict x 26,806,854 ops/sec ±0.60% (99 runs sampled)
S.schema - make + reverse x 1,317,399 ops/sec ±1.46% (93 runs sampled)
S.schema - make + reverse convert x 340,752 ops/sec ±0.28% (100 runs sampled)
S.schema - reverse convert x 107,500,375 ops/sec ±0.43% (101 runs sampled)
S.schema - reverse convert (compiled) x 179,908,246 ops/sec ±5.09% (78 runs sampled)
S.schema - assert x 93,252,178 ops/sec ±3.35% (90 runs sampled)
S.schema - assert (compiled) x 103,615,146 ops/sec ±2.62% (95 runs sampled)
S.schema - assert strict x 26,159,036 ops/sec ±2.75% (93 runs sampled)
S.object - make x 1,527,354 ops/sec ±1.91% (92 runs sampled)
S.object - make + parse x 133,347 ops/sec ±0.36% (97 runs sampled)
S.object - parse x 53,225,773 ops/sec ±1.85% (95 runs sampled)
S.object - make + reverse x 209,658 ops/sec ±1.42% (84 runs sampled)
S.object - make + reverse convert x 130,518 ops/sec ±0.57% (93 runs sampled)
S.object - reverse convert x 55,405,268 ops/sec ±1.92% (89 runs sampled)
S.string - parse x 64,571,685 ops/sec ±1.48% (91 runs sampled)
S.string - reverse convert x 64,480,839 ops/sec ±1.96% (89 runs sampled)
 */

/*
v11.0.0-alpha.0

testData1 serialize: 2.523ms
testData1 parse: 1.367ms
testData2 serialize: 0.866ms
testData2 parse: 0.513ms
makeObjectSchema: 0.067ms
parseOrThrow: 1: 0.287ms
parseOrThrow: 2: 0.004ms
parseOrThrow: 3: 0.003ms
serializeWith: 1: 0.083ms
serializeWith: 2: 0.002ms
serializeWith: 3: 0.001ms
S.Error.make: 0.03ms
S.schema - make x 1,842,285 ops/sec ±0.60% (94 runs sampled)
S.schema - make + parse x 110,817 ops/sec ±0.34% (90 runs sampled)
S.schema - parse x 56,094,436 ops/sec ±1.43% (93 runs sampled)
S.schema - parse strict x 23,828,192 ops/sec ±0.54% (97 runs sampled)
S.schema - make + reverse x 445,124 ops/sec ±0.33% (97 runs sampled)
S.schema - make + reverse convert x 134,718 ops/sec ±1.12% (98 runs sampled)
S.schema - reverse convert x 77,229,668 ops/sec ±2.34% (93 runs sampled)
S.schema - reverse convert (compiled) x 179,007,439 ops/sec ±6.12% (78 runs sampled)
S.schema - assert x 64,880,688 ops/sec ±2.78% (90 runs sampled)
S.schema - assert (compiled) x 103,022,830 ops/sec ±2.45% (95 runs sampled)
S.schema - assert strict x 23,551,566 ops/sec ±0.68% (96 runs sampled)
S.object - make x 261,716 ops/sec ±1.24% (95 runs sampled)
S.object - make + parse x 70,015 ops/sec ±0.90% (94 runs sampled)
S.object - parse x 40,620,418 ops/sec ±1.03% (95 runs sampled)
S.object - make + reverse x 111,013 ops/sec ±5.43% (96 runs sampled)
S.object - make + reverse convert x 67,716 ops/sec ±0.65% (93 runs sampled)
S.object - reverse convert x 44,494,112 ops/sec ±1.24% (90 runs sampled)
S.string - parse x 58,630,597 ops/sec ±1.36% (95 runs sampled)
S.string - reverse convert x 57,733,298 ops/sec ±1.52% (93 runs sampled)
 */
