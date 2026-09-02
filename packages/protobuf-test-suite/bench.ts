// Sury against every protobuf codec a JS project would reach for, on the
// same bytes and the same values: protobufjs reflection and static
// codegen, protobuf-es (@bufbuild/protobuf) and pbf (Mapbox). Each one is
// driven the way its README shows, with a copying finish where the library
// offers a choice, so nothing here is a benchmark-only fast path.
import { mkdirSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { createFileRegistry, fromBinary, toBinary, type DescMessage } from "@bufbuild/protobuf";
import { FileDescriptorSetSchema } from "@bufbuild/protobuf/wkt";
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

const libraries: Library[] = [
  {
    id: "sury",
    codec: (work, bytes) => {
      const schema = suryMessage(work.fields);
      const encode = S.decoder(schema, S.protobuf);
      const decode = S.decoder(S.protobuf, schema);
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

export type Cell = { library: string; encodeNs: number; decodeNs: number };
export type BenchRow = { id: string; bytes: number; cells: Cell[] };

const median = (xs: number[]): number => {
  const s = [...xs].sort((a, b) => a - b);
  return s[(s.length - 1) >> 1]!;
};

const timeNs = (fn: () => unknown, n: number): number => {
  for (let i = 0; i < Math.min(n, 500); i++) fn();
  const start = process.hrtime.bigint();
  for (let i = 0; i < n; i++) fn();
  return Number(process.hrtime.bigint() - start) / n;
};

export const runBench = async (samples = 7): Promise<BenchRow[]> => {
  const rows: BenchRow[] = [];
  for (const work of WORKLOADS) {
    const schema = suryMessage(work.fields);
    const bytes = S.decoder(schema, S.protobuf)(work.value);
    // Bigger messages get fewer iterations so every workload takes roughly
    // the same wall time per sample.
    const n = Math.max(2000, Math.round(400000 / (bytes.length + 20)));
    const cells: Cell[] = [];
    for (const library of libraries) {
      const codec = await library.codec(work, bytes);
      const encodeNs: number[] = [];
      const decodeNs: number[] = [];
      for (let i = 0; i < samples; i++) {
        encodeNs.push(timeNs(codec.encode, n));
        decodeNs.push(timeNs(codec.decode, n));
      }
      cells.push({ library: library.id, encodeNs: median(encodeNs), decodeNs: median(decodeNs) });
    }
    rows.push({ id: work.id, bytes: bytes.length, cells });
  }
  return rows;
};

export const formatBench = (rows: BenchRow[]): string => {
  const lines: string[] = [];
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
