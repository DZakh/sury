// The schema page: the everyday job every one of these libraries is for -
// describe a shape, check a value against it, get a type out of it.
//
// The workload is the shape the ecosystem's cross-library benchmarks use, so a
// reader who has seen those numbers can put these beside them.
import { Type } from "@sinclair/typebox";
import { TypeCompiler } from "@sinclair/typebox/compiler";
import { Value } from "@sinclair/typebox/value";
import { type } from "arktype";
import * as S from "sury";
import * as v from "valibot";
import { z } from "zod";
import { measureBundles } from "../bundle";
import { NO, PARTLY, YES, buildFeatures, works } from "../features";
import type { Source } from "../registry";
import type { Cell, Table } from "../table";
import { timeNs } from "../time";
import { versionsOf } from "../versions";

const COLUMNS = ["Sury", "Zod", "TypeBox", "Valibot", "ArkType"];

// Nothing here changes a library's global settings. Every page is generated in
// one process, so a knob turned to make one row fairer would silently change
// what the other three pages measured - and at their defaults these five agree
// on the one thing the knob was for: `NaN` is not a number.

const VALUE = Object.freeze({
  number: 1,
  negNumber: -1,
  maxNumber: Number.MAX_VALUE,
  string: "string",
  longString: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore.",
  boolean: true,
  deeplyNested: { foo: "bar", num: 1, bool: false },
});

const SURY = S.schema({
  number: S.number,
  negNumber: S.number,
  maxNumber: S.number,
  string: S.string,
  longString: S.string,
  boolean: S.boolean,
  deeplyNested: { foo: S.string, num: S.number, bool: S.boolean },
});
const ZOD = z.object({
  number: z.number(),
  negNumber: z.number(),
  maxNumber: z.number(),
  string: z.string(),
  longString: z.string(),
  boolean: z.boolean(),
  deeplyNested: z.object({ foo: z.string(), num: z.number(), bool: z.boolean() }),
});
const TYPEBOX = Type.Object({
  number: Type.Number(),
  negNumber: Type.Number(),
  maxNumber: Type.Number(),
  string: Type.String(),
  longString: Type.String(),
  boolean: Type.Boolean(),
  deeplyNested: Type.Object({ foo: Type.String(), num: Type.Number(), bool: Type.Boolean() }),
});
const VALIBOT = v.object({
  number: v.number(),
  negNumber: v.number(),
  maxNumber: v.number(),
  string: v.string(),
  longString: v.string(),
  boolean: v.boolean(),
  deeplyNested: v.object({ foo: v.string(), num: v.number(), bool: v.boolean() }),
});
const ARKTYPE = type({
  number: "number",
  negNumber: "number",
  maxNumber: "number",
  string: "string",
  longString: "string",
  boolean: "boolean",
  deeplyNested: { foo: "string", num: "number", bool: "boolean" },
});

// The same schema in each library's own words, as a bundler sees it. Written
// out rather than derived from the values above, because what is measured is
// the graph a consumer's own source pulls in.
const FIELDS = {
  sury: "{ number: S.number, negNumber: S.number, maxNumber: S.number, string: S.string, longString: S.string, boolean: S.boolean, deeplyNested: { foo: S.string, num: S.number, bool: S.boolean } }",
  zod: "{ number: z.number(), negNumber: z.number(), maxNumber: z.number(), string: z.string(), longString: z.string(), boolean: z.boolean(), deeplyNested: z.object({ foo: z.string(), num: z.number(), bool: z.boolean() }) }",
  typebox:
    "{ number: Type.Number(), negNumber: Type.Number(), maxNumber: Type.Number(), string: Type.String(), longString: Type.String(), boolean: Type.Boolean(), deeplyNested: Type.Object({ foo: Type.String(), num: Type.Number(), bool: Type.Boolean() }) }",
  valibot:
    "{ number: v.number(), negNumber: v.number(), maxNumber: v.number(), string: v.string(), longString: v.string(), boolean: v.boolean(), deeplyNested: v.object({ foo: v.string(), num: v.number(), bool: v.boolean() }) }",
  arktype:
    '{ number: "number", negNumber: "number", maxNumber: "number", string: "string", longString: "string", boolean: "boolean", deeplyNested: { foo: "string", num: "number", bool: "boolean" } }',
};

const bundleSize = async (): Promise<Table> => {
  const sizes = await measureBundles([
    { label: "sury schema", code: `import * as S from "sury"; export const p = S.parseOrThrow(S.schema(${FIELDS.sury}));` },
    { label: "zod schema", code: `import { z } from "zod"; const M = z.object(${FIELDS.zod}); export const p = (x) => M.parse(x);` },
    {
      label: "typebox schema",
      code: `import { Type } from "@sinclair/typebox"; import { TypeCompiler } from "@sinclair/typebox/compiler"; const C = TypeCompiler.Compile(Type.Object(${FIELDS.typebox})); export const p = (x) => C.Check(x);`,
    },
    {
      label: "valibot schema",
      code: `import * as v from "valibot"; const M = v.object(${FIELDS.valibot}); export const p = (x) => v.parse(M, x);`,
    },
    {
      label: "arktype schema",
      code: `import { type } from "arktype"; const M = type(${FIELDS.arktype}); export const p = (x) => M.assert(x);`,
    },
    { label: "sury all", code: `export * from "sury";` },
    { label: "zod all", code: `export * from "zod";` },
    // TypeBox's compiler is a separate entry point, and the row above needs
    // it, so the ceiling has to include it or it reads as smaller than the
    // schema built on it.
    {
      label: "typebox all",
      code: `export * from "@sinclair/typebox"; export * from "@sinclair/typebox/compiler"; export * from "@sinclair/typebox/value";`,
    },
    { label: "valibot all", code: `export * from "valibot";` },
    { label: "arktype all", code: `export * from "arktype";` },
  ]);
  const gzip = (label: string): Cell => sizes.get(label)?.gzip ?? null;
  const each = (suffix: string): Cell[] =>
    ["sury", "zod", "typebox", "valibot", "arktype"].map((id) => gzip(`${id} ${suffix}`));
  return {
    columns: COLUMNS,
    rows: [
      {
        label: "What this schema ships",
        note: "the seven fields above, after tree-shaking",
        cells: each("schema"),
        format: "bytes",
        best: "low",
      },
      {
        label: "Everything the library exports",
        note: "the ceiling, for an app that ends up using all of it",
        cells: each("all"),
        format: "bytes",
        best: "low",
      },
    ],
    note: "<sub>Bundled with esbuild, minified and gzipped. The first row is the one a consumer pays: Sury and Valibot are built from many small functions a bundler can drop individually, so the gap between the two rows is most of the library. The second row is each library's main entry, plus the compiler and value entries for TypeBox, which is where the row above gets `TypeCompiler`.</sub>",
  };
};

type Standard = {
  "~standard": {
    version: number;
    vendor: string;
    validate: (value: unknown) => { value?: unknown; issues?: readonly { message: string; path?: readonly unknown[] }[] };
  };
};

const standardOf = (schema: unknown): Standard["~standard"] | undefined =>
  (schema as Partial<Standard>)["~standard"];

const SCHEMAS: [string, unknown][] = [
  ["Sury", SURY],
  ["Zod", ZOD],
  ["TypeBox", TYPEBOX],
  ["Valibot", VALIBOT],
  ["ArkType", ARKTYPE],
];

const features = (): Table => {
  const suryCodec = S.string.with(S.to, S.number, { decode: Number, encode: String });
  const zodCodec = z.codec(z.string(), z.number(), { decode: Number, encode: String });
  const typeboxCodec = Type.Transform(Type.String()).Decode(Number).Encode(String);
  return buildFeatures(COLUMNS, [
    {
      label: "What you see on hover",
      note: "the type your editor shows for `{ foo: string }`, which is also what an error quotes back at you",
      cells: [
        "`S.Schema<{foo: string}, {foo: string}>`",
        "`z.ZodObject<{foo: z.ZodString}, $strip>`",
        "`TObject<{foo: TString}>`",
        "`v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>`",
        "`Type<{foo: string}, {}>`",
      ],
    },
    {
      label: "Standard Schema",
      note: "the interface tRPC, TanStack and 28 others accept a schema through",
      cells: SCHEMAS.map(([, schema]) => () => standardOf(schema)?.version === 1),
    },
    {
      label: "One schema, both directions",
      note: "write the transform once and the same schema encodes the value back",
      cells: [
        () => S.encodeOrThrow(suryCodec)(7 as never) === "7",
        () => z.encode(zodCodec, 7) === "7",
        () => Value.Encode(typeboxCodec, 7 as never) === "7",
        NO,
        NO,
      ],
    },
    {
      label: "Compares two values for you",
      note: "the schema knows the shape, so comparing is one generated function instead of a blind walk",
      cells: [
        () => S.isEqualOutput(SURY as never)(VALUE as never, { ...VALUE } as never),
        NO,
        // `Value.Equal` is a structural walk that never sees the schema, so it
        // answers a different question.
        NO,
        NO,
        NO,
      ],
    },
    {
      label: "Checks a value you built yourself",
      note: "for data your own code produced, where parsing an untrusted input is the wrong tool",
      cells: [() => works(() => S.makeOutputOrThrow(SURY as never)(VALUE as never)), NO, NO, NO, NO],
    },
    {
      label: "Validates asynchronously",
      cells: [
        () => typeof S.parseAsPromiseOrReject === "function",
        () => typeof ZOD.parseAsync === "function",
        NO,
        () => typeof v.parseAsync === "function",
        NO,
      ],
    },
    {
      label: "Reports every problem, not just the first",
      note: "Sury stops at the first, which is part of why it parses as fast as it does",
      cells: SCHEMAS.map(([, schema]) => () => {
        const issues = standardOf(schema)?.validate({ ...VALUE, string: 1, deeplyNested: { foo: 1, num: 1, bool: false } })
          .issues;
        return issues !== undefined && issues.length > 1;
      }),
    },
    {
      label: "Runs where `new Function` is blocked",
      note: "a page under a strict CSP, some browser extension contexts",
      cells: [NO, `${PARTLY} opt-out`, `${PARTLY} opt-in`, YES, `${PARTLY} opt-out`],
    },
  ]);
};

const performance = (): Table => {
  const suryParse = S.parseOrThrow(SURY);
  const typeboxCheck = TypeCompiler.Compile(TYPEBOX);
  return {
    columns: COLUMNS,
    rows: [
      {
        label: "Parse with a schema you already have",
        note: "the hot path a request pays",
        cells: [
          timeNs(() => suryParse(VALUE)),
          timeNs(() => ZOD.parse(VALUE)),
          timeNs(() => typeboxCheck.Check(VALUE)),
          timeNs(() => v.parse(VALIBOT, VALUE)),
          timeNs(() => ARKTYPE.assert(VALUE)),
        ],
        format: "ns",
        best: "low",
      },
      {
        label: "Build the schema and parse once",
        note: "what a short-lived script or a cold start pays",
        cells: [
          timeNs(() =>
            S.parseOrThrow(
              S.schema({
                number: S.number,
                negNumber: S.number,
                maxNumber: S.number,
                string: S.string,
                longString: S.string,
                boolean: S.boolean,
                deeplyNested: { foo: S.string, num: S.number, bool: S.boolean },
              }),
            )(VALUE),
          ),
          timeNs(() =>
            z
              .object({
                number: z.number(),
                negNumber: z.number(),
                maxNumber: z.number(),
                string: z.string(),
                longString: z.string(),
                boolean: z.boolean(),
                deeplyNested: z.object({ foo: z.string(), num: z.number(), bool: z.boolean() }),
              })
              .parse(VALUE),
          ),
          timeNs(() => Value.Check(TYPEBOX, VALUE)),
          timeNs(() =>
            v.parse(
              v.object({
                number: v.number(),
                negNumber: v.number(),
                maxNumber: v.number(),
                string: v.string(),
                longString: v.string(),
                boolean: v.boolean(),
                deeplyNested: v.object({ foo: v.string(), num: v.number(), bool: v.boolean() }),
              }),
              VALUE,
            ),
          ),
          timeNs(() =>
            type({
              number: "number",
              negNumber: "number",
              maxNumber: "number",
              string: "string",
              longString: "string",
              boolean: "boolean",
              deeplyNested: { foo: "string", num: "number", bool: "boolean" },
            }).assert(VALUE),
          ),
        ],
        format: "ns",
        best: "low",
      },
    ],
  };
};

// Standard Schema is the interface the rest of the ecosystem reaches a schema
// through, so "does it work" is a question with an answer, not a checkbox.
// Path segments may be plain keys or objects carrying a `key`, and the spec
// allows both, so the check resolves either.
const segment = (part: unknown): unknown =>
  typeof part === "object" && part !== null && "key" in part ? (part as { key: unknown }).key : part;

const CHECKS: [string, (std: Standard["~standard"]) => boolean][] = [
  ["version is 1", (std) => std.version === 1],
  ["vendor is named", (std) => typeof std.vendor === "string" && std.vendor.length > 0],
  ["a valid value reports no issues", (std) => std.validate(VALUE).issues === undefined],
  ["a valid value comes back", (std) => JSON.stringify(std.validate(VALUE).value) === JSON.stringify(VALUE)],
  ["an invalid value reports issues", (std) => (std.validate({ ...VALUE, string: 1 }).issues?.length ?? 0) > 0],
  [
    "every issue carries a message",
    (std) => (std.validate({ ...VALUE, string: 1 }).issues ?? []).every((i) => typeof i.message === "string"),
  ],
  [
    "a nested failure carries its path",
    (std) =>
      (std.validate({ ...VALUE, deeplyNested: { foo: 1, num: 1, bool: false } }).issues ?? []).some(
        (issue) => (issue.path ?? []).map(segment).join(".") === "deeplyNested.foo",
      ),
  ],
  ["a value of the wrong type reports rather than throws", (std) => (std.validate(null).issues?.length ?? 0) > 0],
];

const conformance = (): Table => ({
  columns: COLUMNS,
  rows: [
    {
      label: "Standard Schema v1",
      note: "checks in `CHECKS` in `packages/benchmarks/topics/schema.ts`",
      cells: SCHEMAS.map(([, schema]) => {
        const std = standardOf(schema);
        if (std === undefined) return "n/a";
        let passed = 0;
        for (const [, check] of CHECKS) {
          try {
            if (check(std)) passed++;
          } catch {
            // A throw is a failure.
          }
        }
        return `${passed}/${CHECKS.length}`;
      }),
      format: "text",
    },
  ],
  note: "<sub>Each check is run against that library's own `~standard` object: what a framework holding the schema would do with it. `n/a` is a library that does not implement the interface at all, which is a different thing from implementing it and getting a check wrong.</sub>",
});

export const schema: Source = {
  id: "schema",
  title: "Schema benchmarks",
  label: "Schema",
  blurb:
    "Describing a shape, checking a value against it, and getting a TypeScript type out of it: the job every one of these libraries is for. Sury compiles the schema into a function, which is why the parse row reads the way it does.",
  versions: () => versionsOf(["sury", "zod", "@sinclair/typebox", "valibot", "arktype"]),
  performanceNote: "<sub>Median of seven rounds. TypeBox is validation only: it checks the value and returns a boolean rather than producing an output, and its second row uses the interpreted `Value.Check` because compiling per call is not what anyone does. Sury, Zod and Valibot return the parsed value; ArkType throws or returns it.</sub>",
  bundleSize,
  features: () => Promise.resolve(features()),
  conformance: () => Promise.resolve(conformance()),
  performance: () => Promise.resolve(performance()),
};
