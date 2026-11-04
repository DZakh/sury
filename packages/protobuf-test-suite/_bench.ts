import { create, fromJson, toJson, fromBinary } from "@bufbuild/protobuf";
import * as S from "sury";
import { protobufEsType } from "./reference";
import { suryMessage, type FieldDef } from "./cases";
import { ENUM_NAMES } from "./reference";
const child: FieldDef[] = [{ key: "f0", number: 1, type: "string" }, { key: "f1", number: 2, type: "int32" }];
const fields: FieldDef[] = [
  { key: "f0", number: 1, type: "int32" }, { key: "f1", number: 2, type: "string" }, { key: "f2", number: 3, type: "int64" },
  { key: "f3", number: 4, type: "double" }, { key: "f4", number: 5, type: "bool" }, { key: "f5", number: 6, type: "enum" },
  { key: "f6", number: 7, type: "string", repeated: true }, { key: "f7", number: 8, type: "message", fields: child, repeated: true },
  { key: "f8", number: 9, type: "int32", map: "string" }, { key: "f9", number: 10, type: "bytes" },
];
const schema = suryMessage(fields, new Map(), new Map(), S.protobufEnum(ENUM_NAMES) as never);
const es = protobufEsType(fields, true);
const value = { f0: 150, f1: "Ada Lovelace", f2: 1234567890123n, f3: 3.14, f4: true, f5: 1, f6: ["a", "bb", "ccc"], f7: [{ f0: "x", f1: 1 }, { f0: "y", f1: 2 }, { f0: "z", f1: 3 }], f8: { a: 1, b: 2 }, f9: new Uint8Array([1, 2, 3, 4]) };
const print = S.decodeOrThrow(schema, S.protobufJson);
const read = S.decodeOrThrow(S.protobufJson, schema);
const json = print(value);
const esMsg = fromBinary(es, S.decodeOrThrow(schema, S.protobuf)(value));
const median = (f: () => unknown) => { const r: number[] = []; for (let k = 0; k < 9; k++) { const n = 200000; const t = performance.now(); for (let i = 0; i < n; i++) f(); r.push((performance.now() - t) * 1e6 / n); } return r.sort((a, b) => a - b)[4]!.toFixed(0); };
for (let i = 0; i < 50000; i++) { print(value); read(json); }
console.log("print", median(() => print(value)), "read", median(() => read(json)), "esToJson", median(() => toJson(es, esMsg)), "esFromJson", median(() => fromJson(es, json as never)));
