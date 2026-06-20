import { bench, describe } from "vitest";
import * as S from "../src/S.js";

// Match the benchmark fixture's runtime config (NaN check skewed float parsing).
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
  bench("create", () => {
    makeSchema();
  });

  // S.parser returns the compiled parse fn; this is the hot path users pay.
  bench("parse", () => {
    parse(data);
  });

  // Cold path: build the schema and parse once (compile cost included).
  bench("create + parse", () => {
    S.parser(makeSchema())(data);
  });

  // Output -> Input (serialize). No transforms here, so it exercises the
  // reverse pipeline assembly without per-field conversion.
  bench("serialize", () => {
    serialize(data);
  });

  bench("assert", () => {
    S.assert(data, schema);
  });
});

// Discriminated union of an object member and a string->number transform —
// exercises union dispatch + a transforming branch.
const union = S.union([{ box: S.string }, S.string.with(S.to, S.number)]);
const parseUnion = S.parser(union);

describe("union", () => {
  bench("parse object branch", () => {
    parseUnion({ box: "abc" });
  });

  bench("parse transform branch", () => {
    parseUnion("123");
  });
});

// Recursive schema: a tagged tree whose nodes hold an array of children.
// Exercises S.recursive + union dispatch + array recursion together.
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
  bench("parse nested tree", () => {
    parseTree(treeData);
  });
});
