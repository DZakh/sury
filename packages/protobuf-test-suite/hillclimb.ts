import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import * as S from "sury";
import { suryMessage, type FieldDef } from "./cases";
import { protobufjsType, toPbjsValue } from "./reference";

const SAMPLES = 7;
const WARMUP = 200;
const N = 20000;

type Workload = {
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

const WORKLOADS = [tiny, typical, large];

const timeNs = (fn: () => void): number => {
  for (let i = 0; i < WARMUP; i++) fn();
  const start = process.hrtime.bigint();
  for (let i = 0; i < N; i++) fn();
  return Number(process.hrtime.bigint() - start) / N;
};

const median = (xs: number[]): number => {
  const s = [...xs].sort((a, b) => a - b);
  return s[(s.length - 1) >> 1]!;
};

export type WorkloadScore = {
  id: string;
  encodeNs: number;
  decodeNs: number;
  pbjsEncodeNs: number;
  pbjsDecodeNs: number;
  encodeRatio: number;
  decodeRatio: number;
  geoMean: number;
};

export type HillclimbScore = {
  n: number;
  samples: number;
  protobufBytes: number;
  protobufFieldBytes: number;
  typical: WorkloadScore;
  tiny: WorkloadScore;
  large: WorkloadScore;
};

const bundleRow = (name: string): number => {
  const yaml = readFileSync(
    fileURLToPath(new URL("../sury/specs/bundleSize.yaml", import.meta.url)),
    "utf8",
  );
  const match = new RegExp(`^  ${name}: (\\d+)$`, "m").exec(yaml);
  if (!match) throw new Error(`bundleSize.yaml missing ${name}`);
  return Number(match[1]);
};

const scoreWorkload = (work: Workload): WorkloadScore => {
  const schema = suryMessage(work.fields);
  const encode = S.decoder(schema, S.protobuf);
  const decode = S.decoder(S.protobuf, schema);
  const value = work.value;
  const bytes = encode(value);
  const pbjsType = protobufjsType(work.fields);
  const pbjsValue = toPbjsValue(work.fields, value);
  const encodeNs: number[] = [];
  const decodeNs: number[] = [];
  const pbjsEncodeNs: number[] = [];
  const pbjsDecodeNs: number[] = [];
  for (let i = 0; i < SAMPLES; i++) {
    encodeNs.push(timeNs(() => encode(value)));
    pbjsEncodeNs.push(timeNs(() => pbjsType.encode(pbjsValue).finish()));
    decodeNs.push(timeNs(() => decode(bytes)));
    pbjsDecodeNs.push(timeNs(() => pbjsType.decode(bytes)));
  }
  const e = median(encodeNs);
  const d = median(decodeNs);
  const pe = median(pbjsEncodeNs);
  const pd = median(pbjsDecodeNs);
  const encodeRatio = e / pe;
  const decodeRatio = d / pd;
  return {
    id: work.id,
    encodeNs: e,
    decodeNs: d,
    pbjsEncodeNs: pe,
    pbjsDecodeNs: pd,
    encodeRatio,
    decodeRatio,
    geoMean: Math.sqrt(encodeRatio * decodeRatio),
  };
};

export const runHillclimb = (): HillclimbScore => {
  const scores = WORKLOADS.map(scoreWorkload);
  const byId = Object.fromEntries(scores.map((s) => [s.id, s]));
  return {
    n: N,
    samples: SAMPLES,
    protobufBytes: bundleRow("protobuf"),
    protobufFieldBytes: bundleRow("protobufField"),
    typical: byId["typical"]!,
    tiny: byId["tiny"]!,
    large: byId["large"]!,
  };
};

export const formatHillclimb = (score: HillclimbScore): string => {
  const row = (s: WorkloadScore) =>
    `${s.id.padEnd(8)} encode ${s.encodeNs.toFixed(1)}/${s.pbjsEncodeNs.toFixed(1)} ns  r=${s.encodeRatio.toFixed(2)}   decode ${s.decodeNs.toFixed(1)}/${s.pbjsDecodeNs.toFixed(1)} ns  r=${s.decodeRatio.toFixed(2)}   geo=${s.geoMean.toFixed(2)}`;
  return [
    `n=${score.n} samples=${score.samples}  protobuf=${score.protobufBytes}B  protobufField=${score.protobufFieldBytes}B`,
    row(score.tiny),
    row(score.typical),
    row(score.large),
    `metric typical.geoMean=${score.typical.geoMean.toFixed(3)}`,
  ].join("\n");
};
