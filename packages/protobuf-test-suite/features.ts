// What each protobuf codec does with the same bytes, probed rather than
// claimed.
//
// The four libraries are installed here, so a feature row is a call run
// against each of them and the answer it gave. The rows are the places the
// implementations actually differ - what a 64-bit field decodes to, whether a
// value the schema cannot hold is refused, whether a reader accepts the wire
// encoding proto3 says it must - not a list of what everyone does.
import { fromBinary, toBinary } from "@bufbuild/protobuf";
import { PbfReader, PbfWriter } from "pbf";
import { compile as compilePbf } from "pbf/compile";
import schemaParse from "protocol-buffers-schema";
import protobuf from "protobufjs";
import * as S from "sury";
import { type FieldDef, suryMessage } from "./cases";
import { protoSource, protobufEsType, protobufjsType } from "./reference";

// A cell is a call to run. `string` is the escape hatch for a column where
// there is no call to make; the generator marks those.
export type CellSpec = string | (() => boolean);
export type Feature = { label: string; note?: string; cells: CellSpec[] };

export const COLUMNS = ["Sury", "protobufjs", "protobuf-es", "pbf"];

// Mirrors packages/benchmarks/features.ts, which renders them. A cell only
// reaches for one of these where there is no call to make.
const NO = "\u274c";
const PARTLY = "\u2b55";

const sury = (fields: FieldDef[]) => {
  const schema = suryMessage(fields);
  return {
    encode: S.decodeOrThrow(schema, S.protobuf),
    decode: S.decodeOrThrow(S.protobuf, schema) as (b: Uint8Array) => Record<string, unknown>,
  };
};

const es = protobufEsType;

const pbf = (fields: FieldDef[]) =>
  compilePbf(schemaParse(protoSource(fields))) as {
    readM: (reader: PbfReader) => Record<string, unknown>;
    writeM: (value: unknown, writer: PbfWriter) => void;
  };

const works = (fn: () => unknown): boolean => {
  try {
    return fn() !== false;
  } catch {
    return false;
  }
};

const rejects = (fn: () => unknown): boolean => {
  try {
    fn();
    return false;
  } catch {
    return true;
  }
};

const BIG: FieldDef[] = [{ key: "big", number: 1, type: "int64" }];
const TEXT: FieldDef[] = [{ key: "text", number: 1, type: "string" }];
const COUNT: FieldDef[] = [{ key: "count", number: 1, type: "int32" }];

// int64 = 4294967297, which is larger than a 32-bit field and exact in every
// representation, so a library that answers with a Number is not yet wrong,
// only differently typed.
const BIG_WIRE = new Uint8Array([0x08, 0x81, 0x80, 0x80, 0x80, 0x10]);
// A string field holding 0xff, which is not valid UTF-8.
const BAD_UTF8 = new Uint8Array([0x0a, 0x01, 0xff]);
// `count` as 7, plus field 5 carrying a varint nothing declares.
const UNKNOWN = new Uint8Array([0x08, 0x07, 0x28, 0x63]);
const SEVEN = new Uint8Array([0x08, 0x07]);
// One past what an int32 field can hold. A writer that neither refuses it nor
// widens the field emits bytes the other side reads as a different number.
const OVERFLOW = 2 ** 31;

const isBigint = (value: unknown): boolean => typeof value === "bigint";

// A decoded message a consumer can hand to `structuredClone`, spread, or
// compare: the schema's own fields on a plain object, with no class and no
// marker property of the library's own.
const isPlainShape = (value: unknown, keys: string[]): boolean =>
  Object.getPrototypeOf(value) === Object.prototype &&
  Object.keys(value as object).join() === keys.join();

export const FEATURES: Feature[] = [
  {
    label: "64-bit fields decode as `bigint`",
    note: "not a Number that rounds past 2^53, and not a library-specific Long",
    cells: [
      () => isBigint(sury(BIG).decode(BIG_WIRE)["big"]),
      () => isBigint((protobufjsType(BIG).decode(BIG_WIRE) as unknown as { big: unknown }).big),
      () => isBigint((fromBinary(es(BIG), BIG_WIRE) as unknown as { big: unknown }).big),
      () => isBigint(pbf(BIG).readM(new PbfReader(BIG_WIRE))["big"]),
    ],
  },
  {
    label: "Decodes to a plain object with the schema's fields",
    note: "no class instance, no marker property of the library's own",
    cells: [
      () => isPlainShape(sury(COUNT).decode(SEVEN), ["count"]),
      () => isPlainShape(protobufjsType(COUNT).decode(SEVEN), ["count"]),
      () => isPlainShape(fromBinary(es(COUNT), SEVEN), ["count"]),
      () => isPlainShape(pbf(COUNT).readM(new PbfReader(SEVEN)), ["count"]),
    ],
  },
  {
    label: "An out-of-range `int32` is refused on encode",
    note: "rather than writing bytes the other side reads as a different number",
    cells: [
      () => rejects(() => sury(COUNT).encode({ count: OVERFLOW })),
      () => rejects(() => protobufjsType(COUNT).encode({ count: OVERFLOW }).finish()),
      () => rejects(() => toBinary(es(COUNT), { $typeName: "M", count: OVERFLOW } as never)),
      () =>
        rejects(() => {
          const writer = new PbfWriter();
          pbf(COUNT).writeM({ count: OVERFLOW }, writer);
          return writer.finish();
        }),
    ],
  },
  {
    label: "Invalid UTF-8 in a `string` field is rejected",
    cells: [
      () => rejects(() => sury(TEXT).decode(BAD_UTF8)),
      () => rejects(() => protobufjsType(TEXT).decode(BAD_UTF8)),
      () => rejects(() => fromBinary(es(TEXT), BAD_UTF8)),
      () => rejects(() => pbf(TEXT).readM(new PbfReader(BAD_UTF8))),
    ],
  },
  {
    label: "Unknown fields are skipped",
    note: "what every proto3 reader has to do for a message written by a newer sender",
    cells: [
      () => sury(COUNT).decode(UNKNOWN)["count"] === 7,
      () => (protobufjsType(COUNT).decode(UNKNOWN) as unknown as { count: number }).count === 7,
      () => (fromBinary(es(COUNT), UNKNOWN) as unknown as { count: number }).count === 7,
      () => pbf(COUNT).readM(new PbfReader(UNKNOWN))["count"] === 7,
    ],
  },
  {
    label: "The same schema validates a JS value",
    note: "one description of the message, rather than one for the wire and another for the checks",
    cells: [
      () => works(() => S.parseOrThrow(suryMessage(COUNT))({ count: 7 }) !== undefined) &&
        rejects(() => S.parseOrThrow(suryMessage(COUNT))({ count: "7" })),
      () => protobufjsType(COUNT).verify({ count: 7 }) === null && protobufjsType(COUNT).verify({ count: "7" }) !== null,
      // Neither library offers a call that checks a JS value against the
      // message, so there is nothing to run: these are claims, not probes.
      NO,
      NO,
    ],
  },
  {
    label: "Prints the `.proto` the other side needs",
    cells: [
      () => S.toProtoOrThrow(suryMessage(COUNT), { name: "M" }).includes("int32 count = 1;"),
      `${PARTLY} via the \`protobufjs-cli\` package, not the runtime`,
      NO,
      NO,
    ],
  },
];
