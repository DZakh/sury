// ProtoJSON over the graphs `fuzz.ts` generates, against protobuf-es's
// `toJson` and `fromJson` on the same `.proto` with the enum declared: Sury
// prints a value as protobuf-es prints the message its bytes are, reads what
// protobuf-es printed back as the same value, and given that JSON spelled
// another way - a number as a string, a 64-bit integer as a number, base64url,
// a field as null, a leaf of the wrong type, a key it doesn't know - accepts
// it exactly when protobuf-es does and reads the same message.
import { type DescMessage, fromBinary, fromJson, type JsonValue, toBinary, toJson } from "@bufbuild/protobuf";
import * as S from "sury";
import { type FieldDef, suryMessage } from "./cases";
import { ENUM_NAMES, protobufEsType } from "./reference";
import { equalValue, show } from "./runner";

type Random = () => number;
export type JsonFinding = { kind: string; seed: number; detail: string };

const int = (r: Random, n: number): number => Math.floor(r() * n);
const pick = <T>(r: Random, xs: readonly T[]): T => xs[int(r, xs.length)]!;

// One spelling per key: a comparison of two JSON values ignores key order.
export const canonical = (value: unknown): string =>
  JSON.stringify(value, (_, v) =>
    v !== null && typeof v === "object" && !Array.isArray(v) ? Object.fromEntries(Object.keys(v).sort().map((k) => [k, v[k]])) : v,
  );

const names = Object.keys(ENUM_NAMES);

// Another spelling of a leaf, or a value no field of its kind takes.
const respell = (r: Random, value: unknown): unknown => {
  const any = (): unknown => pick(r, [null, true, {}, [], "x", "", 1.5, -1, 1e40, "1e2", " 1", "0x10", "NaN", "-0"]);
  if (typeof value === "number") return pick(r, [String(value), `${value}e0`, value + 0.5, -value, any()]);
  if (typeof value === "string") {
    if (/^-?\d+$/.test(value)) return pick(r, [Number(value), `${value}.0`, `0${value}`, ` ${value}`, any()]);
    if (names.includes(value)) return pick(r, [ENUM_NAMES[value as keyof typeof ENUM_NAMES], value.toLowerCase(), any()]);
    if (/^[A-Za-z0-9+/]+=*$/.test(value)) return pick(r, [value.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, ""), value.replace(/=+$/, ""), `${value}=`, any()]);
    return any();
  }
  return any();
};

// Every place a value sits: `holder[key]`.
const places = (value: unknown, out: [holder: Record<string, unknown> | unknown[], key: string | number][] = []) => {
  if (Array.isArray(value)) value.forEach((item, idx) => (out.push([value, idx]), places(item, out)));
  else if (value !== null && typeof value === "object") {
    for (const key of Object.keys(value)) {
      out.push([value as Record<string, unknown>, key]);
      places((value as Record<string, unknown>)[key], out);
    }
  }
  return out;
};

const mutateJson = (r: Random, json: JsonValue): JsonValue => {
  const copy = structuredClone(json) as Record<string, unknown>;
  const at = places(copy);
  if (at.length === 0 || int(r, 8) === 0) {
    const objects = [copy, ...at.map(([holder, key]) => (holder as Record<string, unknown>)[key as string]).filter((v) => v !== null && typeof v === "object" && !Array.isArray(v))];
    (pick(r, objects) as Record<string, unknown>).zz = respell(r, 1);
    return copy as JsonValue;
  }
  const [holder, key] = pick(r, at);
  (holder as Record<string, unknown>)[key as string] = respell(r, (holder as Record<string, unknown>)[key as string]);
  return copy as JsonValue;
};

const reason = (e: unknown): string => String((e as S.Error).reason ?? (e as Error).message);

// Its own stream, as every family beside the round trip.
export const jsonChecks = (
  findings: JsonFinding[],
  r: Random,
  seed: number,
  fields: FieldDef[],
  values: () => Record<string, unknown>,
  count: number,
  mutants: number,
): number => {
  let schema: S.Schema<unknown, unknown>;
  let esType: DescMessage;
  try {
    schema = suryMessage(fields, new Map(), new Map(), S.protobufEnum(ENUM_NAMES) as never);
    esType = protobufEsType(fields, true);
  } catch (e) {
    findings.push({ kind: "json: compile", seed, detail: (e as Error).message });
    return 0;
  }
  const encode = S.decodeOrThrow(schema, S.protobuf);
  const decode = S.decodeOrThrow(S.protobuf, schema);
  const print = S.decodeOrThrow(schema, S.protobufJson);
  const read = S.decodeOrThrow(S.protobufJson, schema);
  let checks = 0;
  for (let v = 0; v < count; v++) {
    const value = values();
    checks++;
    const bytes = encode(value).slice();
    const esJson = toJson(esType, fromBinary(esType, bytes));
    let suryJson: unknown;
    try {
      suryJson = print(value);
    } catch (e) {
      findings.push({ kind: `json: Sury can't print what protobuf-es prints (${reason(e).replace(/\d+/g, "#")})`, seed, detail: `value=${show(value)}` });
      continue;
    }
    if (canonical(suryJson) !== canonical(esJson)) {
      findings.push({ kind: "json: Sury prints another JSON than protobuf-es", seed, detail: `sury=${canonical(suryJson).slice(0, 400)} es=${canonical(esJson).slice(0, 400)}` });
      continue;
    }
    const expected = decode(bytes);
    try {
      if (!equalValue(read(esJson), expected)) {
        findings.push({ kind: "json: Sury reads protobuf-es's JSON as another value", seed, detail: `json=${canonical(esJson).slice(0, 400)}` });
        continue;
      }
    } catch (e) {
      findings.push({ kind: `json: Sury refuses protobuf-es's JSON (${reason(e).replace(/\d+/g, "#")})`, seed, detail: `json=${canonical(esJson).slice(0, 400)}` });
      continue;
    }
    for (let m = 0; m < mutants; m++) {
      const mutant = mutateJson(r, esJson);
      checks++;
      let sury: unknown;
      let es: Uint8Array | undefined;
      let suryError: unknown;
      let esError: unknown;
      try {
        sury = read(mutant);
      } catch (e) {
        suryError = e;
      }
      try {
        es = toBinary(esType, fromJson(esType, mutant, { ignoreUnknownFields: true }));
      } catch (e) {
        esError = e;
      }
      if ((suryError === undefined) !== (esError === undefined)) {
        const why = suryError === undefined ? (esError as Error).message.replace(/^.*?: /, "") : reason(suryError);
        findings.push({
          kind: `json acceptance: ${suryError === undefined ? "only sury" : "only protobuf-es"} accepts (${why.replace(/\d+/g, "#").replace(/"[^"]*"/g, "S")})`,
          seed,
          detail: `json=${JSON.stringify(mutant).slice(0, 500)}`,
        });
        continue;
      }
      if (es !== undefined && !equalValue(decode(es), sury)) {
        findings.push({ kind: "json: Sury and protobuf-es read one JSON as two messages", seed, detail: `json=${JSON.stringify(mutant).slice(0, 500)}` });
      }
    }
  }
  return checks;
};
