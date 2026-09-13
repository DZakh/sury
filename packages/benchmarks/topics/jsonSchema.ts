// The JSON Schema page.
//
// Two questions live here, and the columns are the union of what answers
// either: emitting a JSON Schema from a schema you wrote, and turning a JSON
// Schema somebody handed you into something that checks values. Ajv only does
// the second, TypeBox and ArkType only the first, and a cell with nothing to
// measure says so.
import { Type } from "@sinclair/typebox";
import { Value } from "@sinclair/typebox/value";
import Ajv, { type ValidateFunction } from "ajv";
import { type } from "arktype";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import * as S from "sury";
import { z } from "zod";
import { measureBundles } from "../bundle";
import { NO, buildFeatures, rejects, works } from "../features";
import type { Source } from "../registry";
import type { Cell, Table } from "../table";
import { timeNs } from "../time";
import { versionsOf } from "../versions";

const COLUMNS = ["Sury", "Zod", "TypeBox", "ArkType", "Ajv"];

// The same three fields in each library's own words, and as the document a
// service would have published.
const SURY = S.schema({ id: S.string, age: S.number, tags: S.array(S.string) });
const ZOD = z.object({ id: z.string(), age: z.number(), tags: z.array(z.string()) });
const TYPEBOX = Type.Object({ id: Type.String(), age: Type.Number(), tags: Type.Array(Type.String()) });
const ARKTYPE = type({ id: "string", age: "number", tags: "string[]" });
const DOCUMENT = {
  type: "object",
  properties: { id: { type: "string" }, age: { type: "number" }, tags: { type: "array", items: { type: "string" } } },
  required: ["id", "age", "tags"],
} as const;
const VALUE = { id: "a", age: 1, tags: ["x"] };

const golden = (relative: string): { summary: Record<string, string | number> } =>
  JSON.parse(readFileSync(fileURLToPath(new URL(relative, import.meta.url)), "utf8")) as {
    summary: Record<string, string | number>;
  };

const FIELDS = '{ id: S.string, age: S.number, tags: S.array(S.string) }';
const DOC_LITERAL = JSON.stringify(DOCUMENT);

const bundleSize = async (): Promise<Table> => {
  const sizes = await measureBundles([
    {
      label: "sury emit",
      code: `import * as S from "sury"; const M = S.schema(${FIELDS}); export const j = S.toInputJSONSchemaOrThrow(M);`,
    },
    {
      label: "zod emit",
      code: `import { z } from "zod"; const M = z.object({ id: z.string(), age: z.number(), tags: z.array(z.string()) }); export const j = z.toJSONSchema(M);`,
    },
    {
      label: "typebox emit",
      code: `import { Type } from "@sinclair/typebox"; export const j = Type.Object({ id: Type.String(), age: Type.Number(), tags: Type.Array(Type.String()) });`,
    },
    {
      label: "arktype emit",
      code: `import { type } from "arktype"; export const j = type({ id: "string", age: "number", tags: "string[]" }).toJsonSchema();`,
    },
    {
      label: "sury read",
      code: `import * as S from "sury"; export const v = S.parseOrThrow(S.fromJSONSchemaOrThrow(${DOC_LITERAL}));`,
    },
    {
      label: "zod read",
      code: `import { z } from "zod"; export const v = z.fromJSONSchema(${DOC_LITERAL});`,
    },
    {
      label: "ajv read",
      code: `import Ajv from "ajv"; export const v = new Ajv().compile(${DOC_LITERAL});`,
    },
  ]);
  const gzip = (label: string): Cell => sizes.get(label)?.gzip ?? null;
  return {
    columns: COLUMNS,
    rows: [
      {
        label: "Emit a JSON Schema",
        cells: [gzip("sury emit"), gzip("zod emit"), gzip("typebox emit"), gzip("arktype emit"), null],
        format: "bytes",
        best: "low",
      },
      {
        label: "Check values against a published document",
        note: "the JSON Schema is the input, not the output",
        cells: [gzip("sury read"), gzip("zod read"), null, null, gzip("ajv read")],
        format: "bytes",
        best: "low",
      },
    ],
    note: "<sub>The three-field schema above, bundled with esbuild, minified and gzipped. A TypeBox schema is a JSON Schema already, which is why its emit row is the smallest and why it has no row below it: `Value.Check` only understands TypeBox's own objects, not a document from elsewhere.</sub>",
  };
};

const RECURSIVE_SURY = S.recursive("Node", (self) => S.schema({ v: S.number, next: S.optional(self) }));
const RECURSIVE_ZOD: z.ZodType = z.lazy(() => z.object({ v: z.number(), next: RECURSIVE_ZOD.optional() }));
const RECURSIVE_ARKTYPE = type({ v: "number", "next?": "this" });
const RECURSIVE_TYPEBOX = Type.Recursive((self) => Type.Object({ v: Type.Number(), next: Type.Optional(self) }));

const hasRef = (json: unknown): boolean => JSON.stringify(json).includes('"$ref"');

const features = (): Table => {
  const codec = S.string.with(S.to, S.number, Number);
  const zodCodec = z.string().transform(Number).pipe(z.number());
  return buildFeatures(COLUMNS, [
    {
      label: "Reads a published JSON Schema back into a schema",
      note: "the direction that lets a document from someone else check values and infer types",
      cells: [
        () => works(() => S.parseOrThrow(S.fromJSONSchemaOrThrow(DOCUMENT))(VALUE)),
        () => works(() => z.fromJSONSchema(DOCUMENT as never).parse(VALUE)),
        // TypeBox validates its own objects, which carry a `Kind` symbol a
        // published document does not have.
        () => works(() => Value.Check(DOCUMENT as never, VALUE)),
        () => "fromJsonSchema" in type,
        // Ajv compiles a document into a validator, which checks values but is
        // not a schema to build on, so there is no call to run.
        NO,
      ],
    },
    {
      label: "Input and output sides emit separately",
      note: "a codec's two ends are two documents: what a client sends, and what it gets back",
      cells: [
        () =>
          JSON.stringify(S.toInputJSONSchemaOrThrow(codec)) !== JSON.stringify(S.toOutputJSONSchemaOrThrow(codec)),
        () =>
          JSON.stringify(z.toJSONSchema(zodCodec, { io: "input" })) !==
          JSON.stringify(z.toJSONSchema(zodCodec, { io: "output" })),
        NO,
        NO,
        "n/a",
      ],
    },
    {
      label: "Emits draft-07 as well as 2020-12",
      cells: [
        () => S.toInputJSONSchemaOrThrow(SURY, { target: "draft-07" }).type === "object",
        () => works(() => z.toJSONSchema(ZOD, { target: "draft-7" })),
        NO,
        () => works(() => ARKTYPE.toJsonSchema({ dialect: "draft-07" } as never)),
        "n/a",
      ],
    },
    {
      label: "Emits an OpenAPI 3.0 document",
      note: "where a nullable field is `nullable: true` rather than a type union",
      cells: [
        () => works(() => S.toInputJSONSchemaOrThrow(SURY, { target: "openapi-3.0" })),
        () => works(() => z.toJSONSchema(ZOD, { target: "openapi-3.0" })),
        NO,
        NO,
        "n/a",
      ],
    },
    {
      label: "A recursive schema emits `$ref` and `$defs`",
      cells: [
        () => hasRef(S.toInputJSONSchemaOrThrow(RECURSIVE_SURY)),
        () => hasRef(z.toJSONSchema(RECURSIVE_ZOD)),
        () => hasRef(RECURSIVE_TYPEBOX),
        () => hasRef(RECURSIVE_ARKTYPE.toJsonSchema()),
        "n/a",
      ],
    },
    {
      label: "A document it cannot represent is refused, not guessed",
      note: "reading `{ type: \"string\", $ref: \"https://example.com/x\" }`, whose target is not in the document",
      cells: [
        () => rejects(() => S.fromJSONSchemaOrThrow({ $ref: "https://example.com/x" })),
        () => rejects(() => z.fromJSONSchema({ $ref: "https://example.com/x" } as never)),
        "n/a",
        "n/a",
        () => rejects(() => new Ajv().compile({ $ref: "https://example.com/x" })),
      ],
    },
  ]);
};

// Ajv caches a compiled validator by the document's text, so timing the same
// document twice on one instance measures the cache. Each call gets a
// document that differs only in its title, which is what an app does when it
// loads a second endpoint's schema, and the instance is built once - the cost
// of `new Ajv()` is startup, paid whatever the document count.
const distinct = (): (() => Record<string, unknown>) => {
  let n = 0;
  return () => ({ ...DOCUMENT, title: `t${n++}` });
};

const performance = (): Table => {
  const suryValidate = S.parseOrThrow(S.fromJSONSchemaOrThrow(DOCUMENT));
  const zodValidate = z.fromJSONSchema(DOCUMENT as never);
  const ajv = new Ajv();
  const ajvValidate: ValidateFunction = ajv.compile(DOCUMENT);
  const forSury = distinct();
  const forZod = distinct();
  const forAjv = distinct();
  return {
    columns: COLUMNS,
    rows: [
      {
        label: "Emit the JSON Schema",
        cells: [
          timeNs(() => S.toInputJSONSchemaOrThrow(SURY)),
          timeNs(() => z.toJSONSchema(ZOD)),
          null,
          timeNs(() => ARKTYPE.toJsonSchema()),
          null,
        ],
        format: "ns",
        best: "low",
      },
      {
        label: "Build a checker from the document",
        note: "paid once per document, at startup",
        cells: [
          timeNs(() => S.parseOrThrow(S.fromJSONSchemaOrThrow(forSury()))),
          timeNs(() => z.fromJSONSchema(forZod() as never)),
          null,
          null,
          timeNs(() => ajv.compile(forAjv())),
        ],
        format: "ns",
        best: "low",
      },
      {
        label: "Check one value with it",
        note: "paid per request, which is the row that matters",
        cells: [
          timeNs(() => suryValidate(VALUE)),
          timeNs(() => zodValidate.parse(VALUE)),
          null,
          null,
          timeNs(() => ajvValidate(VALUE)),
        ],
        format: "ns",
        best: "low",
      },
    ],
    note: "<sub>Median of seven rounds per cell. Sury and Zod return the checked value, Ajv returns a boolean and leaves the value alone, so the last row is not quite the same work - it is the closest thing each library offers to the same job. TypeBox schemas are JSON Schema already, so there is nothing to time in the first row, and neither it nor ArkType reads a document written elsewhere.</sub>",
  };
};

const conformance = (): Table => {
  const d7 = golden("../../json-schema-test-suite/goldens/draft7.json").summary;
  const d2020 = golden("../../json-schema-test-suite/goldens/draft2020-12.json").summary;
  const row = (label: string, summary: Record<string, string | number>, note?: string) => ({
    label,
    note,
    cells: [summary["assertions"]!, summary["passed"]!, summary["rate"]!] as Cell[],
    format: "count" as const,
  });
  return {
    columns: ["Assertions", "Passed", "Rate"],
    rows: [
      row("JSON Schema Test Suite, draft-07", d7, "the official corpus, run through `S.fromJSONSchemaOrThrow`"),
      row("JSON Schema Test Suite, 2020-12", d2020, "`$dynamicRef` and remote `$ref` are the bulk of what is left"),
      {
        label: "A converted schema converts back to itself",
        note: "`toJSONSchema` then `fromJSONSchema` then `toJSONSchema` again",
        cells: [d2020["identityAssertions"]!, d2020["identityPassed"]!, d2020["identityRate"]!],
        format: "count",
      },
    ],
    note: "<sub>The suite is pinned by commit in `packages/json-schema-test-suite/suite-ref.json` and run in CI. An assertion counted as errored is a keyword `fromJSONSchemaOrThrow` refuses rather than one it gets wrong: it throws on a document it cannot represent instead of quietly accepting values the document forbids, which is why the false-accept count is 1 and not the difference.</sub>",
  };
};

export const jsonSchema: Source = {
  id: "jsonSchema",
  title: "JSON Schema benchmarks",
  label: "JSON Schema",
  blurb:
    "Sury converts in both directions: a schema you wrote becomes a JSON Schema for the other side, and a JSON Schema somebody published becomes a schema that checks values and infers types.",
  versions: () => versionsOf(["sury", "zod", "@sinclair/typebox", "arktype", "ajv"]),
  bundleSize,
  features: () => Promise.resolve(features()),
  conformance: () => Promise.resolve(conformance()),
  performance: () => Promise.resolve(performance()),
};
