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
