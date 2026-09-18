import { CODEC_FUNCTION_NAMES } from "./catalog";
import { Random } from "./random";
import { schemaCategory } from "./schema";
import {
  CASE_VERSION,
  type CompilerCase,
  type OperationKind,
  type PrimitiveName,
  type SchemaAst,
  type ValueAst,
} from "./types";

const PRIMITIVES: readonly PrimitiveName[] = [
  "string",
  "any",
  "boolean",
  "int32",
  "number",
  "nan",
  "bigint",
  "symbol",
  "void",
  "never",
  "unknown",
  "json",
  "jsonString",
  "jsonStringWithSpace",
  "uint8Array",
  "isoDateTime",
  "port",
  "email",
  "uuid",
  "cuid",
  "url",
  "date",
];

const OPERATIONS: readonly OperationKind[] = [
  "parser",
  "decoder",
  "encoder",
  "asyncParser",
  "asyncDecoder",
  "asyncEncoder",
];

const FIELD_NAMES = ["value", "kind", "id", "constructor", 'quoted"key'] as const;

const literalValue = (random: Random): ValueAst =>
  random.pick<ValueAst>([
    { kind: "string", value: "fuzz" },
    { kind: "string", value: "" },
    { kind: "number", value: { kind: "finite", value: 0 } },
    { kind: "number", value: { kind: "negative-zero" } },
    { kind: "number", value: { kind: "nan" } },
    { kind: "bigint", value: "1" },
    { kind: "boolean", value: true },
    { kind: "null" },
    { kind: "undefined" },
  ]);

const leaf = (random: Random): SchemaAst =>
  random.bool(0.75)
    ? { kind: "primitive", name: random.pick(PRIMITIVES) }
    : { kind: "literal", value: literalValue(random) };

const genericRefinement = (random: Random, inner: SchemaAst): SchemaAst => ({
  kind: "refine",
  refinement: random.pick(["always", "never", "non-empty", "non-negative"] as const),
  inner,
});

const refined = (random: Random, inner: SchemaAst): SchemaAst => {
  const category = schemaCategory(inner, "output");
  if (category === "string" || category === "array") {
    const refinement = random.pick([
      "min-length",
      "max-length",
      "length",
      "empty",
      "nonEmpty",
      ...(category === "string" ? (["pattern"] as const) : []),
    ] as const);
    return {
      kind: "refine",
      refinement,
      argument:
        refinement === "empty" || refinement === "nonEmpty" || refinement === "pattern"
          ? undefined
          : random.int(0, 4),
      inner,
    };
  }
  if (category === "number" || category === "bigint") {
    return {
      kind: "refine",
      refinement: random.pick(["gte", "lte", "gt", "lt"] as const),
      argument: random.int(-2, 2),
      inner,
    };
  }
  return genericRefinement(random, inner);
};

const modified = (random: Random, inner: SchemaAst): SchemaAst => {
  const supportsObjectPolicy = inner.kind === "object" || inner.kind === "record";
  const modifier =
    supportsObjectPolicy
      ? random.pick(["strict", "strip", "deepStrict", "deepStrip", "noValidation", "meta"] as const)
      : random.pick(["noValidation", "meta"] as const);
  return { kind: "modifier", modifier, inner };
};

const transformed = (random: Random, depth: number): SchemaAst => {
  const source = generateSchema(random, Math.max(0, depth - 1));
  const target = generateSchema(random, Math.max(0, depth - 1));
  const mode = random.weighted([
    { value: "builtin" as const, weight: 4 },
    { value: "custom-decoder" as const, weight: 3 },
    { value: "custom-bidirectional" as const, weight: 3 },
  ]);
  if (mode === "builtin") return { kind: "to", source, target, codec: { kind: mode } };
  const decoder = random.pick(CODEC_FUNCTION_NAMES);
  if (mode === "custom-decoder") {
    return { kind: "to", source, target, codec: { kind: mode, decoder } };
  }
  return {
    kind: "to",
    source,
    target,
    codec: {
      kind: mode,
      decoder,
      encoder: random.pick(CODEC_FUNCTION_NAMES),
    },
  };
};

const unary = (random: Random, depth: number): SchemaAst => {
  const api = random.pick([
    "compactColumns",
    "list",
    "trim",
    "brand",
    "shape",
    "asyncDecoderAssert",
    "reverse",
  ] as const);
  const inner =
    api === "trim"
      ? ({ kind: "primitive", name: "string" } as const)
      : api === "compactColumns"
        ? objectSchema(random, depth, "column")
        : generateSchema(random, depth - 1);
  return {
    kind: "unary",
    api,
    inner,
    ...(api === "shape" ? { codec: random.pick(CODEC_FUNCTION_NAMES) } : {}),
  };
};

const objectSchema = (random: Random, depth: number, field: string): SchemaAst => ({
  kind: "object",
  factory: random.pick(["schema", "object"] as const),
  fields: [[field, generateSchema(random, Math.max(0, depth - 1))]],
});

export const generateSchema = (random: Random, depth: number): SchemaAst => {
  if (depth <= 0) return leaf(random);

  const kind = random.weighted([
    { value: "leaf" as const, weight: 20 },
    { value: "enum" as const, weight: 3 },
    { value: "instance" as const, weight: 2 },
    { value: "array" as const, weight: 8 },
    { value: "tuple" as const, weight: 8 },
    { value: "record" as const, weight: 6 },
    { value: "object" as const, weight: 10 },
    { value: "union" as const, weight: 12 },
    { value: "optional" as const, weight: 5 },
    { value: "nullable" as const, weight: 5 },
    { value: "nullish" as const, weight: 3 },
    { value: "refine" as const, weight: 7 },
    { value: "modifier" as const, weight: 6 },
    { value: "to" as const, weight: 16 },
    { value: "unary" as const, weight: 8 },
    { value: "merge" as const, weight: 2 },
    { value: "recursive" as const, weight: 2 },
  ]);

  switch (kind) {
    case "leaf":
      return leaf(random);
    case "enum":
      return {
        kind,
        values: Array.from({ length: random.int(1, 3) }, () => literalValue(random)),
      };
    case "instance":
      return { kind };
    case "array":
      return { kind, item: generateSchema(random, depth - 1) };
    case "tuple":
      return {
        kind,
        items: Array.from({ length: random.int(1, 3) }, () =>
          generateSchema(random, depth - 1),
        ),
      };
    case "record":
      return { kind, value: generateSchema(random, depth - 1) };
    case "object": {
      const names = [...FIELD_NAMES];
      const fields: [string, SchemaAst][] = [];
      for (let index = 0; index < random.int(1, 3); index++) {
        const nameIndex = random.int(0, names.length - 1);
        const name = names.splice(nameIndex, 1)[0]!;
        fields.push([name, generateSchema(random, depth - 1)]);
      }
      return { kind, factory: random.pick(["schema", "object"] as const), fields };
    }
    case "union":
      return {
        kind,
        members: Array.from({ length: random.int(2, 4) }, () =>
          generateSchema(random, depth - 1),
        ),
      };
    case "optional":
    case "nullable":
    case "nullish":
      return { kind, inner: generateSchema(random, depth - 1) };
    case "refine":
      return refined(random, generateSchema(random, depth - 1));
    case "modifier":
      return modified(random, generateSchema(random, depth - 1));
    case "to":
      return transformed(random, depth);
    case "unary":
      return unary(random, depth);
    case "merge":
      return {
        kind,
        left: objectSchema(random, depth, "left"),
        right: objectSchema(random, depth, "right"),
      };
    case "recursive":
      return {
        kind,
        name: `FuzzRecursive${random.int(0, 0x7fffffff)}`,
        leaf: leaf(random),
      };
  }
};

const primitiveSchema = (name: PrimitiveName): SchemaAst => ({ kind: "primitive", name });

export const baselineCases = (): CompilerCase[] => {
  const string = primitiveSchema("string");
  const number = primitiveSchema("number");
  const json = primitiveSchema("json");
  const bigint = primitiveSchema("bigint");
  const baseline = (
    id: string,
    operation: OperationKind,
    schema: SchemaAst,
  ): CompilerCase => ({
    version: CASE_VERSION,
    id: `baseline:${id}`,
    operation,
    schemas: [schema],
    runWitness: true,
  });
  return [
    {
      version: CASE_VERSION,
      id: "baseline:primitive-parser",
      operation: "parser",
      schemas: [string],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:builtin-to",
      operation: "parser",
      schemas: [{ kind: "to", source: string, target: number, codec: { kind: "builtin" } }],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:custom-decoder",
      operation: "decoder",
      schemas: [
        {
          kind: "to",
          source: string,
          target: number,
          codec: { kind: "custom-decoder", decoder: "to-number" },
        },
      ],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:custom-bidirectional-parser",
      operation: "parser",
      schemas: [
        {
          kind: "to",
          source: string,
          target: number,
          codec: {
            kind: "custom-bidirectional",
            decoder: "to-number",
            encoder: "to-string",
          },
        },
      ],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:custom-bidirectional-encoder",
      operation: "encoder",
      schemas: [
        {
          kind: "to",
          source: string,
          target: number,
          codec: {
            kind: "custom-bidirectional",
            decoder: "to-number",
            encoder: "to-string",
          },
        },
      ],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:custom-bidirectional-async-encoder",
      operation: "asyncEncoder",
      schemas: [
        {
          kind: "to",
          source: string,
          target: number,
          codec: {
            kind: "custom-bidirectional",
            decoder: "to-number",
            encoder: "to-string",
          },
        },
      ],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:union-codec",
      operation: "decoder",
      schemas: [
        { kind: "union", members: [string, number] },
        { kind: "union", members: [number, string] },
      ],
      runWitness: true,
    },
    {
      version: CASE_VERSION,
      id: "baseline:json-bigint-async",
      operation: "asyncDecoder",
      schemas: [json, bigint],
      runWitness: true,
    },
    baseline("any", "parser", primitiveSchema("any")),
    baseline("nan", "parser", primitiveSchema("nan")),
    baseline("json-string-space", "parser", primitiveSchema("jsonStringWithSpace")),
    baseline("enum", "parser", {
      kind: "enum",
      values: [
        { kind: "string", value: "one" },
        { kind: "number", value: { kind: "finite", value: 2 } },
      ],
    }),
    baseline("instance", "parser", { kind: "instance" }),
    {
      version: CASE_VERSION,
      id: "baseline:compact-columns",
      operation: "parser",
      schemas: [
        { kind: "unary", api: "compactColumns", inner: primitiveSchema("unknown") },
        {
          kind: "array",
          item: { kind: "object", factory: "object", fields: [["value", string]] },
        },
      ],
      runWitness: true,
    },
    baseline("list", "parser", { kind: "unary", api: "list", inner: string }),
    baseline("trim", "parser", { kind: "unary", api: "trim", inner: string }),
    baseline("brand", "parser", { kind: "unary", api: "brand", inner: string }),
    baseline("shape", "parser", {
      kind: "unary",
      api: "shape",
      codec: "identity",
      inner: string,
    }),
    baseline("async-assert", "asyncParser", {
      kind: "unary",
      api: "asyncDecoderAssert",
      inner: string,
    }),
    baseline("reverse", "parser", {
      kind: "unary",
      api: "reverse",
      inner: {
        kind: "to",
        source: string,
        target: number,
        codec: {
          kind: "custom-bidirectional",
          decoder: "to-number",
          encoder: "to-string",
        },
      },
    }),
    baseline("merge", "parser", {
      kind: "merge",
      left: { kind: "object", factory: "object", fields: [["left", string]] },
      right: { kind: "object", factory: "schema", fields: [["right", number]] },
    }),
    baseline("string-refinements", "parser", {
      kind: "refine",
      refinement: "pattern",
      inner: {
        kind: "refine",
        refinement: "nonEmpty",
        inner: {
          kind: "refine",
          refinement: "length",
          argument: 4,
          inner: {
            kind: "refine",
            refinement: "max-length",
            argument: 8,
            inner: { kind: "refine", refinement: "min-length", argument: 1, inner: string },
          },
        },
      },
    }),
    baseline("empty", "parser", { kind: "refine", refinement: "empty", inner: string }),
    baseline("number-refinements", "parser", {
      kind: "refine",
      refinement: "lt",
      argument: 10,
      inner: {
        kind: "refine",
        refinement: "gt",
        argument: -10,
        inner: {
          kind: "refine",
          refinement: "lte",
          argument: 10,
          inner: { kind: "refine", refinement: "gte", argument: -10, inner: number },
        },
      },
    }),
    baseline("object-policies", "parser", {
      kind: "modifier",
      modifier: "deepStrict",
      inner: {
        kind: "modifier",
        modifier: "strict",
        inner: { kind: "object", factory: "object", fields: [["value", string]] },
      },
    }),
    baseline("strip-policies", "parser", {
      kind: "modifier",
      modifier: "deepStrip",
      inner: {
        kind: "modifier",
        modifier: "strip",
        inner: { kind: "object", factory: "schema", fields: [["value", string]] },
      },
    }),
    baseline("metadata", "parser", {
      kind: "modifier",
      modifier: "noValidation",
      inner: { kind: "modifier", modifier: "meta", inner: string },
    }),
    baseline("primitive-surface", "parser", {
      kind: "tuple",
      items: [
        primitiveSchema("boolean"),
        primitiveSchema("int32"),
        primitiveSchema("bigint"),
        primitiveSchema("symbol"),
        primitiveSchema("void"),
        primitiveSchema("never"),
        primitiveSchema("unknown"),
        primitiveSchema("jsonString"),
        primitiveSchema("uint8Array"),
        primitiveSchema("isoDateTime"),
        primitiveSchema("port"),
        primitiveSchema("email"),
        primitiveSchema("uuid"),
        primitiveSchema("cuid"),
        primitiveSchema("url"),
        primitiveSchema("date"),
      ],
    }),
    baseline("composite-surface", "parser", {
      kind: "record",
      value: {
        kind: "array",
        item: {
          kind: "nullish",
          inner: { kind: "nullable", inner: { kind: "optional", inner: string } },
        },
      },
    }),
    baseline("literal-refine", "parser", {
      kind: "refine",
      refinement: "always",
      inner: { kind: "literal", value: { kind: "string", value: "fuzz" } },
    }),
    baseline("recursive", "parser", {
      kind: "recursive",
      name: "FuzzBaseline",
      leaf: string,
    }),
  ];
};

export const generateCompilerCase = (
  random: Random,
  index: number,
  maxDepth: number,
): CompilerCase => {
  const operation = random.pick(OPERATIONS);
  const schemaCount =
    operation === "parser" || operation === "asyncParser"
      ? random.int(1, 3)
      : random.int(1, 3);
  return {
    version: CASE_VERSION,
    id: `${random.initialSeed}:${index}`,
    operation,
    schemas: Array.from({ length: schemaCount }, () => generateSchema(random, maxDepth)),
    runWitness: true,
  };
};

export const generateCases = (
  seed: number,
  count: number,
  maxDepth: number,
): CompilerCase[] => {
  const baselines = baselineCases();
  const random = new Random(seed);
  const generated = Array.from({ length: Math.max(0, count - baselines.length) }, (_, index) =>
    generateCompilerCase(random, index, maxDepth),
  );
  return [...baselines.slice(0, count), ...generated];
};
