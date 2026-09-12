// The JSON encoding page.
//
// `S.jsonString` compiles the schema into the JSON text itself: the structure
// is baked in as literals and only the values are spliced, so nothing is
// allocated between the value and the string. The competitors are the two
// things a JS project otherwise reaches for - the builtin, and
// fast-json-stringify, which compiles from a JSON Schema.
//
// The workloads are written here rather than shared with
// `packages/sury/scripts/jsonStringifyBench.ts`: that script is for iterating
// on the encoder and carries cases and a second table this page has no use
// for, and sharing would make one of the two packages depend on the other for
// the sake of four object literals.
import fastJson from "fast-json-stringify";
import * as S from "sury";
import { measureBundles } from "../bundle";
import { NO, PARTLY, buildFeatures, rejects, works } from "../features";
import type { Source } from "../registry";
import type { Cell, Table } from "../table";
import { timeNs } from "../time";
import { versionsOf } from "../versions";

const COLUMNS = ["Sury", "`JSON.stringify`", "fast-json-stringify"];

type Workload = {
  label: string;
  note?: string;
  sury: () => string;
  stringify: () => string;
  fastJson: () => string;
};

const workloads = (): Workload[] => {
  const out: Workload[] = [];

  {
    const value = {
      id: 42,
      name: "Anna Nachesa",
      email: "anna@example.com",
      age: 34,
      verified: true,
      score: 12.5,
      role: "admin",
    };
    const schema = S.schema({
      id: S.number,
      name: S.string,
      email: S.string,
      age: S.number,
      verified: S.boolean,
      score: S.number,
      role: S.string,
    });
    const fj = fastJson({
      type: "object",
      properties: {
        id: { type: "integer" },
        name: { type: "string" },
        email: { type: "string" },
        age: { type: "integer" },
        verified: { type: "boolean" },
        score: { type: "number" },
        role: { type: "string" },
      },
      required: ["id", "name", "email", "age", "verified", "score", "role"],
    });
    const sury = S.encodeOrThrow(schema, S.jsonString);
    out.push({
      label: "API response",
      note: "a user profile, 7 flat fields",
      sury: () => sury(value),
      stringify: () => JSON.stringify(value),
      fastJson: () => fj(value),
    });
  }

  {
    const value = Array.from({ length: 100 }, (_, i) => ({ id: i, name: `item-${i}`, active: i % 2 === 0 }));
    const schema = S.array(S.schema({ id: S.number, name: S.string, active: S.boolean }));
    const fj = fastJson({
      type: "array",
      items: {
        type: "object",
        properties: { id: { type: "integer" }, name: { type: "string" }, active: { type: "boolean" } },
        required: ["id", "name", "active"],
      },
    });
    const sury = S.encodeOrThrow(schema, S.jsonString);
    out.push({
      label: "List endpoint",
      note: "100 rows of the same three fields",
      sury: () => sury(value),
      stringify: () => JSON.stringify(value),
      fastJson: () => fj(value),
    });
  }

  {
    const value = {
      events: Array.from({ length: 50 }, (_, i) =>
        i % 3 === 0
          ? ({ type: "click", x: i, y: i * 2 } as const)
          : i % 3 === 1
            ? ({ type: "view", path: `/page/${i}` } as const)
            : ({ type: "error", message: `boom ${i}`, code: 500 } as const),
      ),
    };
    const schema = S.schema({
      events: S.array(
        S.union([
          S.schema({ type: "click", x: S.number, y: S.number }),
          S.schema({ type: "view", path: S.string }),
          S.schema({ type: "error", message: S.string, code: S.number }),
        ]),
      ),
    });
    const fj = fastJson({
      type: "object",
      properties: {
        events: {
          type: "array",
          items: {
            anyOf: [
              {
                type: "object",
                properties: { type: { const: "click" }, x: { type: "number" }, y: { type: "number" } },
                required: ["type", "x", "y"],
              },
              {
                type: "object",
                properties: { type: { const: "view" }, path: { type: "string" } },
                required: ["type", "path"],
              },
              {
                type: "object",
                properties: { type: { const: "error" }, message: { type: "string" }, code: { type: "number" } },
                required: ["type", "message", "code"],
              },
            ],
          },
        },
      },
      required: ["events"],
    });
    const sury = S.encodeOrThrow(schema, S.jsonString);
    out.push({
      label: "Event feed",
      note: "50 events across a three-member tagged union",
      sury: () => sury(value),
      stringify: () => JSON.stringify(value),
      fastJson: () => fj(value),
    });
  }

  {
    const value = {
      id: 12345678901234567890n,
      payload: new Uint8Array([104, 101, 108, 108, 111, 33, 33, 33]),
      createdAt: new Date("2026-01-15T10:30:00.000Z"),
      label: "event",
    };
    const schema = S.schema({
      id: S.bigint,
      payload: S.base64.with(S.to, S.uint8Array),
      createdAt: S.isoDateTime.with(S.to, S.date),
      label: S.string,
    });
    // Neither competitor can represent these three types, so the mapping pass a
    // consumer would have to write is inside their timed call. Sury compiles the
    // same mapping into the encoder, and charging it to only one side would be
    // the dishonest comparison.
    const map = (v: typeof value) => ({
      id: v.id.toString(),
      payload: Buffer.from(v.payload).toString("base64"),
      createdAt: v.createdAt.toISOString(),
      label: v.label,
    });
    const fj = fastJson({
      type: "object",
      properties: {
        id: { type: "string" },
        payload: { type: "string" },
        createdAt: { type: "string" },
        label: { type: "string" },
      },
      required: ["id", "payload", "createdAt", "label"],
    });
    const sury = S.encodeOrThrow(schema, S.jsonString);
    out.push({
      label: "Types JSON has no word for",
      note: "a `bigint` id, a `Uint8Array` payload and a `Date`, mapping included",
      sury: () => sury(value as never),
      stringify: () => JSON.stringify(map(value)),
      fastJson: () => fj(map(value)),
    });
  }

  return out;
};

const SURY_ENTRY = `import * as S from "sury";
const M = S.schema({ id: S.number, name: S.string, email: S.string, age: S.number, verified: S.boolean, score: S.number, role: S.string });
export const e = S.encodeOrThrow(M, S.jsonString);`;

const SURY_BOTH = `${SURY_ENTRY}
export const d = S.decodeOrThrow(S.jsonString, M);`;

const FJS_ENTRY = `import fastJson from "fast-json-stringify";
export const e = fastJson({ type: "object", properties: { id: { type: "integer" }, name: { type: "string" }, email: { type: "string" }, age: { type: "integer" }, verified: { type: "boolean" }, score: { type: "number" }, role: { type: "string" } }, required: ["id", "name", "email", "age", "verified", "score", "role"] });`;

const bundleSize = async (): Promise<Table> => {
  const sizes = await measureBundles([
    { label: "sury encode", code: SURY_ENTRY },
    { label: "sury both", code: SURY_BOTH },
    { label: "fastJson", code: FJS_ENTRY },
  ]);
  const gzip = (label: string): Cell => sizes.get(label)!.gzip;
  return {
    columns: COLUMNS,
    rows: [
      { label: "Encode", cells: [gzip("sury encode"), 0, gzip("fastJson")], format: "bytes", best: "low" },
      {
        label: "Encode and decode",
        note: "the same schema read back, checked against itself",
        cells: [gzip("sury both"), 0, null],
        format: "bytes",
        best: "low",
      },
    ],
    note: "<sub>The API response schema above, bundled with esbuild, minified and gzipped. `JSON.stringify` is 0 because it is in the runtime already, and `JSON.parse` with it. What neither gives you is a schema, so the second row compares a checked read against an unchecked one.</sub>",
  };
};

const features = (): Table => {
  const number = S.encodeOrThrow(S.schema({ n: S.number }), S.jsonString);
  const fjNumber = fastJson({ type: "object", properties: { n: { type: "number" } } });
  const bytes = S.encodeOrThrow(S.schema({ b: S.base64.with(S.to, S.uint8Array) }), S.jsonString);
  const fjText = fastJson({ type: "object", properties: { b: { type: "string" } } });
  const big = S.encodeOrThrow(S.schema({ id: S.bigint }), S.jsonString);
  const fjBig = fastJson({ type: "object", properties: { id: { type: "string" } } });
  const PAYLOAD = new Uint8Array([1, 2, 3]);
  const BASE64 = '{"b":"AQID"}';

  return buildFeatures(COLUMNS, [
    {
      label: "`Infinity` and `NaN` are refused",
      note: "rather than written as `null`, which reads back as a missing measurement",
      cells: [
        () => rejects(() => number({ n: Infinity })),
        () => rejects(() => JSON.stringify({ n: Infinity })),
        () => rejects(() => fjNumber({ n: Infinity })),
      ],
    },
    {
      label: "A value the schema forbids is refused",
      note: 'encoding `{ n: "7" }` where the schema says number',
      cells: [
        () => rejects(() => number({ n: "7" } as never)),
        () => rejects(() => JSON.stringify({ n: "7" })),
        () => rejects(() => fjNumber({ n: "7" } as never)),
      ],
    },
    {
      label: "A `bigint` field needs no mapping pass",
      cells: [
        () => big({ id: 12n } as never) === '{"id":"12"}',
        () => works(() => JSON.stringify({ id: 12n })),
        () => fjBig({ id: 12n } as never) === '{"id":"12"}',
      ],
    },
    {
      label: "A `Uint8Array` field needs no mapping pass",
      note: "base64 in and base64 out, because the schema says so",
      cells: [
        () => bytes({ b: PAYLOAD } as never) === BASE64,
        () => JSON.stringify({ b: PAYLOAD }) === BASE64,
        () => fjText({ b: PAYLOAD } as never) === BASE64,
      ],
    },
    {
      label: "Reads the text back through the same schema",
      cells: [
        () => works(() => S.decodeOrThrow(S.jsonString, S.schema({ n: S.number }))('{"n":1}')),
        `${PARTLY} \`JSON.parse\`, which checks nothing`,
        NO,
      ],
    },
  ]);
};

const performance = (): Table => ({
  columns: COLUMNS,
  rows: workloads().map((work) => ({
    label: work.label,
    note: work.note,
    cells: [timeNs(work.sury), timeNs(work.stringify), timeNs(work.fastJson)],
    format: "ns" as const,
    best: "low" as const,
  })),
  note: "<sub>Median of seven rounds per cell. Every row ends with the same text, so the only difference is what each library had to do to get there.</sub>",
});

// Agreement with the builtin. `S.jsonString` is only useful if its text is the
// text `JSON.stringify` would have produced, and if reading that text back
// gives the value that went in. The corpus is the places an encoder that
// splices values into a literal is most likely to get it wrong.
const AGREEMENT: { schema: S.Schema<unknown, unknown>; value: unknown }[] = [
  { schema: S.schema({ n: S.number }) as never, value: { n: 0 } },
  { schema: S.schema({ n: S.number }) as never, value: { n: -1.5e-7 } },
  { schema: S.string as never, value: "" },
  { schema: S.string as never, value: 'quote " backslash \\ newline \n tab \t' },
  { schema: S.string as never, value: "\u0000\u001f\u007f" },
  { schema: S.string as never, value: "emoji \u{1f9ec} and \u{1d11e} outside the BMP" },
  { schema: S.string as never, value: "</script><!-- and & entities" },
  { schema: S.array(S.number) as never, value: [] },
  { schema: S.array(S.array(S.number)) as never, value: [[], [1], [2, 3]] },
  { schema: S.record(S.number) as never, value: {} },
  { schema: S.record(S.string) as never, value: { "": "empty key", 'a"b': "quoted key" } },
  { schema: S.schema({ a: S.optional(S.number), b: S.number }) as never, value: { b: 1 } },
  { schema: S.schema({ a: S.nullable(S.string), b: S.number }) as never, value: { a: null, b: 1 } },
  { schema: S.boolean as never, value: false },
  { schema: S.schema(null) as never, value: null },
  {
    schema: S.schema({ deep: S.schema({ deeper: S.schema({ x: S.array(S.string) }) }) }) as never,
    value: { deep: { deeper: { x: ["a", "b"] } } },
  },
];

const conformance = (): Table => {
  let sameText = 0;
  let roundTrip = 0;
  for (const { schema, value } of AGREEMENT) {
    let text: string | undefined;
    try {
      text = S.encodeOrThrow(schema, S.jsonString)(value) as string;
      if (text === JSON.stringify(value)) sameText++;
    } catch {
      text = undefined;
    }
    try {
      if (text !== undefined && JSON.stringify(S.decodeOrThrow(S.jsonString, schema)(text)) === JSON.stringify(value)) {
        roundTrip++;
      }
    } catch {
      // Not incrementing is how a case fails.
    }
  }
  const total = AGREEMENT.length;
  return {
    columns: ["Cases", "Agree", "Rate"],
    rows: [
      {
        label: "Text identical to `JSON.stringify`",
        note: "escapes, lone control characters, astral characters, empty keys, absent optionals",
        cells: [total, sameText, `${((sameText / total) * 100).toFixed(1)}%`],
        format: "count",
      },
      {
        label: "Decoding the text gives the value back",
        cells: [total, roundTrip, `${((roundTrip / total) * 100).toFixed(1)}%`],
        format: "count",
      },
    ],
    note: "<sub>The corpus is `AGREEMENT` in `packages/benchmarks/topics/jsonString.ts`. An encoder that is fast and disagrees with the builtin on one escape is not useful, so the page measures the agreement rather than asserting it.</sub>",
  };
};

export const jsonString: Source = {
  id: "jsonString",
  title: "JSON encoding benchmarks",
  label: "JSON Encoding",
  blurb:
    "`S.jsonString` compiles the schema into the JSON text: the structure is baked in as literals and only the values are spliced, so nothing is allocated between the value and the string. The same schema reads the text back, checked.",
  versions: () => versionsOf(["sury", "fast-json-stringify"]),
  bundleSize,
  features: () => Promise.resolve(features()),
  conformance: () => Promise.resolve(conformance()),
  performance: () => Promise.resolve(performance()),
};
