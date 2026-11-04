// Sury against every protobuf codec a JS project would reach for, on the
// same bytes and the same values: protobufjs reflection and static
// codegen, protobuf-es (@bufbuild/protobuf) and pbf (Mapbox). Each one is
// driven the way its README shows, with a copying finish where the library
// offers a choice, so nothing here is a benchmark-only fast path.
import { createRequire } from "node:module";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { create, createFileRegistry, fromBinary, toBinary, type DescMessage } from "@bufbuild/protobuf";
import {
  anyPack,
  anyUnpack,
  FieldDescriptorProto_Label,
  FieldDescriptorProto_Type,
  FileDescriptorProtoSchema,
  FileDescriptorSetSchema,
  file_google_protobuf_any,
  file_google_protobuf_timestamp,
  timestampFromDate,
} from "@bufbuild/protobuf/wkt";
import { PbfReader, PbfWriter } from "pbf";
import { compile as compilePbf } from "pbf/compile";
import schemaParse from "protocol-buffers-schema";
import protobuf from "protobufjs";
import descriptor from "protobufjs/ext/descriptor/index.js";
import { main as pbjsMain } from "protobufjs-cli/pbjs";
import * as S from "sury";
import { type FieldDef, suryMessage } from "./cases";
import { protoSource, protobufjsType, toPbjsValue } from "./reference";

export type Workload = {
  id: string;
  fields: FieldDef[];
  value: Record<string, unknown>;
};

const tiny: Workload = {
  id: "tiny",
  fields: [{ key: "id", number: 1, type: "uint32" }],
  value: { id: 150 },
};

const typical: Workload = {
  id: "typical",
  fields: [
    { key: "id", number: 1, type: "uint32" },
    { key: "name", number: 2, type: "string" },
    { key: "active", number: 3, type: "bool" },
    { key: "tags", number: 4, type: "string", repeated: true },
    { key: "score", number: 5, type: "double", optional: true },
    { key: "payload", number: 6, type: "bytes", optional: true },
  ],
  value: {
    id: 42,
    name: "Ada",
    active: true,
    tags: ["ml", "fp"],
    score: 0.5,
    payload: new Uint8Array([1, 2, 3]),
  },
};

const large: Workload = {
  id: "large",
  fields: [
    { key: "id", number: 1, type: "uint32" },
    { key: "blob", number: 2, type: "string" },
    { key: "nums", number: 3, type: "sint32", repeated: true },
  ],
  value: {
    id: 1,
    blob: "x".repeat(1024),
    nums: Array.from({ length: 256 }, (_, i) => i - 128),
  },
};

// protobuf.js's own `bench/cases/common` message and payload.
export const common: Workload = {
  id: "common",
  fields: [
    { key: "string", number: 1, type: "string" },
    { key: "uint32", number: 2, type: "uint32" },
    {
      key: "inner",
      number: 3,
      type: "message",
      optional: true,
      fields: [
        { key: "int32", number: 1, type: "int32" },
        {
          key: "innerInner",
          number: 2,
          type: "message",
          optional: true,
          fields: [
            { key: "long", number: 1, type: "int64" },
            { key: "enum", number: 2, type: "enum" },
            { key: "sint32", number: 3, type: "sint32" },
          ],
        },
        {
          key: "outer",
          number: 3,
          type: "message",
          optional: true,
          fields: [
            { key: "bool", number: 1, type: "bool", repeated: true },
            { key: "double", number: 2, type: "double" },
          ],
        },
      ],
    },
    { key: "float", number: 4, type: "float" },
  ],
  value: {
    string: "Lorem ipsum dolor sit amet.",
    uint32: 9000,
    inner: {
      int32: 20161110,
      innerInner: { long: (151234n << 32n) | 1051n, enum: 1, sint32: -42 },
      outer: { bool: [true, false, false, true, false, false, true], double: 204.8 },
    },
    float: 0.25,
  },
};

// A Mapbox vector-tile shaped message: packed geometry dominates.
const tile: Workload = {
  id: "tile",
  fields: [
    {
      key: "layers",
      number: 3,
      type: "message",
      repeated: true,
      fields: [
        { key: "name", number: 1, type: "string" },
        {
          key: "features",
          number: 2,
          type: "message",
          repeated: true,
          fields: [
            { key: "id", number: 1, type: "uint64" },
            { key: "tags", number: 2, type: "uint32", repeated: true },
            { key: "type", number: 3, type: "enum" },
            { key: "geometry", number: 4, type: "uint32", repeated: true },
          ],
        },
        { key: "keys", number: 3, type: "string", repeated: true },
        { key: "extent", number: 5, type: "uint32" },
      ],
    },
  ],
  value: {
    layers: [
      {
        name: "roads",
        features: Array.from({ length: 40 }, (_, i) => ({
          id: BigInt(1000 + i),
          tags: [0, 1, 2, i % 5],
          type: 2,
          geometry: Array.from({ length: 30 }, (_, j) => (i * 31 + j * 17) & 0x3fff),
        })),
        keys: ["class", "name", "oneway", "surface", "lanes"],
        extent: 4096,
      },
    ],
  },
};

export const WORKLOADS: Workload[] = [tiny, typical, large, common, tile];

type Codec = { encode: () => Uint8Array; decode: () => unknown };
type Library = { id: string; codec: (work: Workload, bytes: Uint8Array) => Promise<Codec> | Codec };

const bigintsToLong = (fields: FieldDef[], value: Record<string, unknown>): Record<string, unknown> =>
  toPbjsValue(fields, value);

export const libraries: Library[] = [
  {
    id: "sury",
    codec: (work, bytes) => {
      const schema = suryMessage(work.fields);
      const encode = S.decodeOrThrow(schema, S.protobuf);
      const decode = S.decodeOrThrow(S.protobuf, schema);
      const value = work.value;
      return { encode: () => encode(value), decode: () => decode(bytes) };
    },
  },
  {
    id: "protobufjs reflect",
    codec: (work, bytes) => {
      const type = protobufjsType(work.fields);
      const value = bigintsToLong(work.fields, work.value);
      return { encode: () => type.encode(value).finish(), decode: () => type.decode(bytes) };
    },
  },
  {
    id: "protobufjs static",
    codec: async (work, bytes) => {
      const dir = fileURLToPath(new URL("./.generated/", import.meta.url));
      mkdirSync(dir, { recursive: true });
      const proto = `${dir}${work.id}.proto`;
      const out = `${dir}${work.id}.mjs`;
      writeFileSync(proto, protoSource(work.fields));
      await new Promise<void>((resolve, reject) =>
        pbjsMain(
          ["-t", "static-module", "-w", "es6", "--no-comments", "--no-verify", "--no-convert", "--no-delimited", "--no-service", "-o", out, proto],
          (err) => (err ? reject(err) : resolve()),
        ),
      );
      const mod = (await import(out)) as { M: { encode: (v: unknown) => { finish: () => Uint8Array }; decode: (b: Uint8Array) => unknown } };
      const value = bigintsToLong(work.fields, work.value);
      return { encode: () => mod.M.encode(value).finish(), decode: () => mod.M.decode(bytes) };
    },
  },
  {
    id: "protobuf-es",
    codec: (work, bytes) => {
      const root = protobuf.parse(protoSource(work.fields)).root;
      const set = (root as unknown as { toDescriptor: (syntax: string) => unknown }).toDescriptor("proto3");
      const setBytes = (descriptor as unknown as { FileDescriptorSet: { encode: (v: unknown) => { finish: () => Uint8Array } } })
        .FileDescriptorSet.encode(set).finish();
      const registry = createFileRegistry(fromBinary(FileDescriptorSetSchema, setBytes));
      const schema = registry.getMessage("M") as DescMessage;
      const value = fromBinary(schema, bytes);
      return { encode: () => toBinary(schema, value), decode: () => fromBinary(schema, bytes) };
    },
  },
  {
    id: "pbf",
    codec: (work, bytes) => {
      const compiled = compilePbf(schemaParse(protoSource(work.fields))) as {
        readM: (pbf: PbfReader) => unknown;
        writeM: (value: unknown, pbf: PbfWriter) => void;
      };
      const value = compiled.readM(new PbfReader(bytes));
      return {
        encode: () => {
          const pbf = new PbfWriter();
          compiled.writeM(value, pbf);
          return pbf.finish();
        },
        decode: () => compiled.readM(new PbfReader(bytes)),
      };
    },
  },
];

// What a field table can't spell: a union oneof in every item, a Timestamp and
// an Any, where Sury's codec does work the others hand to the caller. Each
// library holds the value its own way, its encode packs the Any and its decode
// unpacks it, as its README shows, so every decode hands back the same message.
// pbf has no Any.
export type Custom = {
  id: string;
  schema: () => S.Schema<unknown, unknown>;
  value: unknown;
  codecs: Record<string, (bytes: Uint8Array) => Promise<Codec> | Codec>;
};

const richProto = `syntax = "proto3";
import "google/protobuf/any.proto";
import "google/protobuf/timestamp.proto";
message User { int32 id = 1; string name = 2; }
message Point { int32 x = 1; int32 y = 2; }
message Item {
  oneof kind {
    string text = 1;
    Point point = 2;
    int64 count = 3;
  }
}
message M {
  uint32 id = 1;
  google.protobuf.Timestamp at = 2;
  google.protobuf.Any detail = 3;
  repeated Item items = 4;
}
`;

const richSchema = (): S.Schema<unknown, unknown> => {
  const field = S.protobufField;
  const User = S.schema({ id: S.int32.with(field, 1), name: S.string.with(field, 2) }).with(S.protobufTypeName, "User");
  const Point = S.schema({ x: S.int32.with(field, 1), y: S.int32.with(field, 2) }).with(S.protobufTypeName, "Point");
  const Item = S.schema({
    kind: S.union([
      S.schema({ case: "text" as const, value: S.string.with(field, 1) }),
      S.schema({ case: "point" as const, value: Point.with(field, 2) }),
      S.schema({ case: "count" as const, value: S.bigint.with(field, { number: 3, type: "int64" }) }),
      S.schema({ case: undefined, value: S.optional(S.schema(undefined)) }),
    ]),
  });
  return S.schema({
    id: S.integer.with(field, { number: 1, type: "uint32" }),
    at: S.date.with(field, 2),
    detail: S.union([S.schema({ type: "user" as const, value: User }), S.schema({ type: "point" as const, value: Point })]).with(field, 3),
    items: S.array(Item).with(field, 4),
  }) as never;
};

const richItems = Array.from({ length: 12 }, (_, i) =>
  i % 3 === 0 ? { case: "text" as const, value: `item ${i}` }
  : i % 3 === 1 ? { case: "point" as const, value: { x: i, y: -i } }
  : { case: "count" as const, value: 1234567890123n + BigInt(i) },
);
const richAt = new Date(1700000000123);
const richUser = { id: 1, name: "Ada" };

type PbjsType = { encode: (v: unknown) => { finish: () => Uint8Array }; decode: (b: Uint8Array) => unknown };

const pbjsRichRoot = (): protobuf.Root => {
  type Common = { nested: { google: { nested: { protobuf: { nested: object } } } } };
  const common = (file: string) =>
    (protobuf.common as unknown as { get: (file: string) => Common }).get(file).nested.google.nested.protobuf.nested;
  const root = protobuf.Root.fromJSON({
    nested: { google: { nested: { protobuf: { nested: { ...common("google/protobuf/any.proto"), ...common("google/protobuf/timestamp.proto") } } } } },
  });
  protobuf.parse(richProto, root);
  root.resolveAll();
  return root;
};

const pbjsRichCodec = (types: { M: PbjsType; User: PbjsType; Point: PbjsType }, bytes: Uint8Array): Codec => {
  const ms = richAt.getTime();
  const value = {
    id: 7,
    at: { seconds: Math.floor(ms / 1000), nanos: (ms % 1000) * 1e6 },
    items: richItems.map((item) => ({ [item.case]: item.case === "count" ? Number(item.value) : item.value })),
  };
  const byName: Record<string, PbjsType> = { User: types.User, Point: types.Point };
  return {
    encode: () =>
      types.M.encode({ ...value, detail: { type_url: "type.googleapis.com/User", value: types.User.encode(richUser).finish() } }).finish(),
    decode: () => {
      const m = types.M.decode(bytes) as { detail: { type_url: string; value: Uint8Array } };
      const url = m.detail.type_url;
      return [m, byName[url.slice(url.lastIndexOf("/") + 1)]!.decode(m.detail.value)];
    },
  };
};

const esRichRegistry = () => {
  const { MESSAGE, INT32, UINT32, INT64, STRING } = FieldDescriptorProto_Type;
  const { OPTIONAL, REPEATED } = FieldDescriptorProto_Label;
  const f = (name: string, number: number, type: FieldDescriptorProto_Type, typeName?: string, extra: object = {}) => ({
    name,
    number,
    type,
    typeName,
    label: OPTIONAL,
    jsonName: name,
    ...extra,
  });
  const file = create(FileDescriptorProtoSchema, {
    name: "rich.proto",
    syntax: "proto3",
    dependency: ["google/protobuf/any.proto", "google/protobuf/timestamp.proto"],
    messageType: [
      { name: "User", field: [f("id", 1, INT32), f("name", 2, STRING)] },
      { name: "Point", field: [f("x", 1, INT32), f("y", 2, INT32)] },
      {
        name: "Item",
        oneofDecl: [{ name: "kind" }],
        field: [
          f("text", 1, STRING, undefined, { oneofIndex: 0 }),
          f("point", 2, MESSAGE, ".Point", { oneofIndex: 0 }),
          f("count", 3, INT64, undefined, { oneofIndex: 0 }),
        ],
      },
      {
        name: "M",
        field: [
          f("id", 1, UINT32),
          f("at", 2, MESSAGE, ".google.protobuf.Timestamp"),
          f("detail", 3, MESSAGE, ".google.protobuf.Any"),
          f("items", 4, MESSAGE, ".Item", { label: REPEATED }),
        ],
      },
    ],
  } as never);
  return createFileRegistry(file, (name) =>
    name === "google/protobuf/any.proto" ? file_google_protobuf_any.proto
    : name === "google/protobuf/timestamp.proto" ? file_google_protobuf_timestamp.proto
    : undefined,
  );
};

export const CUSTOM: Custom[] = [
  {
    id: "oneof-any",
    schema: richSchema,
    value: { id: 7, at: richAt, detail: { type: "user", value: richUser }, items: richItems.map((kind) => ({ kind })) },
    codecs: {
      "protobufjs reflect": (bytes) => {
        const root = pbjsRichRoot();
        const type = (name: string) => root.lookupType(name) as unknown as PbjsType;
        return pbjsRichCodec({ M: type("M"), User: type("User"), Point: type("Point") }, bytes);
      },
      "protobufjs static": async (bytes) => {
        const dir = fileURLToPath(new URL("./.generated/", import.meta.url));
        mkdirSync(dir, { recursive: true });
        const proto = `${dir}oneof-any.proto`;
        const out = `${dir}oneof-any.mjs`;
        writeFileSync(proto, richProto);
        await new Promise<void>((resolve, reject) =>
          pbjsMain(
            ["-t", "static-module", "-w", "es6", "--no-comments", "--no-verify", "--no-convert", "--no-delimited", "--no-service", "-o", out, proto],
            (err) => (err ? reject(err) : resolve()),
          ),
        );
        return pbjsRichCodec((await import(out)) as { M: PbjsType; User: PbjsType; Point: PbjsType }, bytes);
      },
      "protobuf-es": (bytes) => {
        const registry = esRichRegistry();
        const M = registry.getMessage("M")!;
        const User = registry.getMessage("User")!;
        const Point = registry.getMessage("Point")!;
        const user = create(User, richUser as never);
        const value = create(M, {
          id: 7,
          at: timestampFromDate(richAt),
          items: richItems.map((item) => ({
            kind: item.case === "point" ? { case: "point", value: create(Point, item.value as never) } : item,
          })),
        } as never) as unknown as { detail?: unknown };
        return {
          encode: () => {
            value.detail = anyPack(User, user as never);
            return toBinary(M, value as never);
          },
          decode: () => {
            const m = fromBinary(M, bytes) as unknown as { detail: never };
            return [m, anyUnpack(m.detail, registry)];
          },
        };
      },
    },
  },
];

export type Cell = { library: string; encodeNs: number; decodeNs: number };
export type BenchRow = { id: string; bytes: number; cells: Cell[] };

// Best of the samples rather than the median: the larger workloads allocate
// enough that a sample landing on a GC pause reads 2x, and which library
// pays it is luck of the draw.
const best = (xs: number[]): number => Math.min(...xs);

const timeNs = (fn: () => unknown, n: number): number => {
  for (let i = 0; i < Math.min(n, 500); i++) fn();
  const start = process.hrtime.bigint();
  for (let i = 0; i < n; i++) fn();
  return Number(process.hrtime.bigint() - start) / n;
};

const benchRow = async (
  id: string,
  bytes: Uint8Array,
  samples: number,
  codecOf: (library: Library) => Promise<Codec> | Codec | undefined,
): Promise<BenchRow> => {
  // Bigger messages get fewer iterations so every workload takes roughly
  // the same wall time per sample.
  const n = Math.max(3000, Math.round(600000 / (bytes.length + 20)));
  const cells: Cell[] = [];
  for (const library of libraries) {
    const codec = await codecOf(library);
    if (codec === undefined) continue;
    const encodeNs: number[] = [];
    const decodeNs: number[] = [];
    for (let i = 0; i < samples; i++) {
      encodeNs.push(timeNs(codec.encode, n));
      decodeNs.push(timeNs(codec.decode, n));
    }
    cells.push({ library: library.id, encodeNs: best(encodeNs), decodeNs: best(decodeNs) });
  }
  return { id, bytes: bytes.length, cells };
};

export const runBench = async (samples = 7, only?: string[]): Promise<BenchRow[]> => {
  const rows: BenchRow[] = [];
  for (const work of WORKLOADS) {
    if (only?.length && !only.includes(work.id)) continue;
    const bytes = S.decodeOrThrow(suryMessage(work.fields), S.protobuf)(work.value);
    rows.push(await benchRow(work.id, bytes, samples, (library) => library.codec(work, bytes)));
  }
  for (const work of CUSTOM) {
    if (only?.length && !only.includes(work.id)) continue;
    const schema = work.schema();
    const encode = S.decodeOrThrow(schema, S.protobuf);
    const decode = S.decodeOrThrow(S.protobuf, schema);
    const bytes = encode(work.value);
    rows.push(
      await benchRow(work.id, bytes, samples, (library) =>
        library.id === "sury" ? { encode: () => encode(work.value), decode: () => decode(bytes) } : work.codecs[library.id]?.(bytes),
      ),
    );
  }
  return rows;
};

// A table without the versions it was measured on is a claim nobody can
// reproduce, so the run states them rather than leaving them to whoever copies
// the numbers into a doc. Read off the installed packages, not a hand-kept
// list: `@bufbuild/protobuf` and `pbf` do not export their package.json, so
// each is resolved from a subpath the package does export.
const versionOf = (name: string, entry: string): string => {
  const require_ = createRequire(import.meta.url);
  // The package.json subpath where the package exports it - sury does, and
  // its "." export is import-only, so `require.resolve` cannot reach it.
  try {
    return (JSON.parse(readFileSync(require_.resolve(`${name}/package.json`), "utf8")) as {
      version?: string;
    }).version ?? "?";
  } catch {
    // Otherwise walk up from any subpath it does export until the manifest
    // that names it.
    try {
      let dir = path.dirname(require_.resolve(entry));
      for (;;) {
        const candidate = path.join(dir, "package.json");
        if (existsSync(candidate)) {
          const pkg = JSON.parse(readFileSync(candidate, "utf8")) as { name?: string; version?: string };
          if (pkg.name === name) return pkg.version ?? "?";
        }
        const up = path.dirname(dir);
        if (up === dir) return "?";
        dir = up;
      }
    } catch {
      return "?";
    }
  }
};

export const benchVersions = (): string[] => [
  `sury ${versionOf("sury", "sury")}`,
  `protobufjs ${versionOf("protobufjs", "protobufjs")}`,
  `protobuf-es ${versionOf("@bufbuild/protobuf", "@bufbuild/protobuf")}`,
  `pbf ${versionOf("pbf", "pbf")}`,
];

export const benchProvenance = (): string =>
  [`node ${process.version} · ${process.platform} ${process.arch}`, ...benchVersions()].join(" · ");

export const formatBench = (rows: BenchRow[]): string => {
  const lines: string[] = [benchProvenance(), ""];
  for (const row of rows) {
    lines.push(`${row.id} (${row.bytes} bytes)`);
    const bestEncode = Math.min(...row.cells.map((c) => c.encodeNs));
    const bestDecode = Math.min(...row.cells.map((c) => c.decodeNs));
    for (const cell of row.cells) {
      const enc = `${cell.encodeNs.toFixed(0).padStart(7)} ns  ${(cell.encodeNs / bestEncode).toFixed(2)}x`;
      const dec = `${cell.decodeNs.toFixed(0).padStart(7)} ns  ${(cell.decodeNs / bestDecode).toFixed(2)}x`;
      lines.push(`  ${cell.library.padEnd(20)} encode ${enc}   decode ${dec}`);
    }
  }
  return lines.join("\n");
};
