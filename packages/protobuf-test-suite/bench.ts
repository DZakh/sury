import * as S from "sury";
import { mixedCases, suryMessage } from "./cases";
import { protobufjsType, toPbjsValue } from "./reference";

export type BenchRow = {
  id: string;
  n: number;
  suryEncodeNs: number;
  pbjsEncodeNs: number;
  suryDecodeNs: number;
  pbjsDecodeNs: number;
  encodeRatio: number;
  decodeRatio: number;
};

const typical = mixedCases[0]!;

const time = (n: number, fn: () => void): number => {
  for (let i = 0; i < Math.min(n, 200); i++) fn();
  const start = process.hrtime.bigint();
  for (let i = 0; i < n; i++) fn();
  return Number(process.hrtime.bigint() - start);
};

export const runBench = (n = 20000): BenchRow => {
  const schema = suryMessage(typical.fields);
  const encode = S.decoder(schema, S.protobuf);
  const decode = S.decoder(S.protobuf, schema);
  const value = typical.value;
  const bytes = encode(value);
  const pbjsType = protobufjsType(typical.fields);
  const pbjsValue = toPbjsValue(typical.fields, value);
  const suryEncodeNs = time(n, () => {
    encode(value);
  });
  const pbjsEncodeNs = time(n, () => {
    pbjsType.encode(pbjsValue).finish();
  });
  const suryDecodeNs = time(n, () => {
    decode(bytes);
  });
  const pbjsDecodeNs = time(n, () => {
    pbjsType.decode(bytes);
  });
  return {
    id: typical.id,
    n,
    suryEncodeNs,
    pbjsEncodeNs,
    suryDecodeNs,
    pbjsDecodeNs,
    encodeRatio: suryEncodeNs / pbjsEncodeNs,
    decodeRatio: suryDecodeNs / pbjsDecodeNs,
  };
};

export const formatBench = (row: BenchRow): string => {
  const ns = (v: number) => `${(v / row.n).toFixed(1)} ns/op`;
  return [
    `${row.id} × ${row.n}`,
    `  encode  sury ${ns(row.suryEncodeNs)}  protobufjs ${ns(row.pbjsEncodeNs)}  ratio ${row.encodeRatio.toFixed(2)} (sury/pbjs)`,
    `  decode  sury ${ns(row.suryDecodeNs)}  protobufjs ${ns(row.pbjsDecodeNs)}  ratio ${row.decodeRatio.toFixed(2)} (sury/pbjs)`,
  ].join("\n");
};
