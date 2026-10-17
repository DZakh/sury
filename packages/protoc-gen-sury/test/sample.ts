// Values for a message, drawn from its descriptor in the shape both
// protobuf-es and the generated schemas hold it: the one the parity gate hands
// to each side. Seeded, so a finding reproduces.
import type { Element, Field, Message, Scalar } from "../src/model";
import { isStruct, messageOf, wrapperScalar } from "../src/shared";

export type Rng = () => number;

export const rngOf = (seed: number): Rng => {
  let state = seed >>> 0 || 1;
  return () => {
    state ^= state << 13;
    state ^= state >>> 17;
    state ^= state << 5;
    return (state >>> 0) / 4294967296;
  };
};

const pick = <T>(rng: Rng, items: readonly T[]): T => items[Math.floor(rng() * items.length)]!;

// Edges each scalar has, beside its zero. `-0` is left out: protobuf-es elides
// it as the zero of an implicit field where Sury writes it, and that is the
// one place the two disagree by design.
const pools: Record<Scalar, readonly unknown[]> = {
  double: [0, 1.5, -2.25, 1e308, Number.MIN_VALUE, Infinity, -Infinity, Number.NaN],
  float: [0, 1.5, -2.25, Math.fround(3.4e38), Math.fround(1e-40), Infinity, Number.NaN],
  int32: [0, 1, -1, 150, 2147483647, -2147483648],
  sint32: [0, 1, -1, 2147483647, -2147483648],
  sfixed32: [0, 1, -1, 2147483647, -2147483648],
  uint32: [0, 1, 127, 128, 4294967295],
  fixed32: [0, 1, 4294967295],
  int64: [0n, 1n, -1n, 9223372036854775807n, -9223372036854775808n],
  sint64: [0n, 1n, -1n, 9223372036854775807n, -9223372036854775808n],
  sfixed64: [0n, -1n, 9223372036854775807n, -9223372036854775808n],
  uint64: [0n, 1n, 18446744073709551615n],
  fixed64: [0n, 18446744073709551615n],
  bool: [false, true],
  string: ["", "a", "Ada Lovelace", "ü🙂 \u0000"],
  bytes: [new Uint8Array(0), new Uint8Array([0]), new Uint8Array([1, 2, 255])],
};

const scalar = (rng: Rng, type: Scalar, asString: boolean): unknown => {
  const value = pick(rng, pools[type]);
  return asString && typeof value === "bigint" ? `${value}` : value instanceof Uint8Array ? value.slice() : value;
};

const mapKey = (rng: Rng, type: Scalar): string => {
  const value = pick(rng, pools[type]);
  return typeof value === "boolean" ? `${value}` : `${value}`;
};

const json = (rng: Rng, depth: number): unknown => {
  const kind = Math.floor(rng() * (depth > 2 ? 4 : 6));
  if (kind === 0) return null;
  if (kind === 1) return pick(rng, [0, 1.5, -3]);
  if (kind === 2) return pick(rng, ["", "x", "ü"]);
  if (kind === 3) return rng() < 0.5;
  if (kind === 4) return Array.from({ length: Math.floor(rng() * 3) }, () => json(rng, depth + 1));
  return jsonObject(rng, depth + 1);
};

const jsonObject = (rng: Rng, depth: number): Record<string, unknown> => {
  const out: Record<string, unknown> = {};
  for (let idx = Math.floor(rng() * 3); idx > 0; idx--) out[pick(rng, ["a", "b", "key", "ü"])] = json(rng, depth);
  return out;
};

export const sampleMessage = (rng: Rng, message: Message, depth = 0): Record<string, unknown> => {
  const out: Record<string, unknown> = {};
  const element = (field: Field, el: Element): unknown => {
    if (el.kind === "scalar") return scalar(rng, el.scalar, field.longAsString && field.mapKey === undefined);
    if (el.kind === "enum") return pick(rng, el.enum.values).number;
    if (isStruct(field, el)) return jsonObject(rng, 0);
    return sampleMessage(rng, messageOf(el), depth + 1);
  };
  const deep = depth >= 3;
  for (const member of message.members) {
    if (member.kind === "oneof") {
      const candidates = member.fields.filter((f) => !deep || f.element.kind !== "message" || isStruct(f, f.element));
      const chosen = rng() < 0.25 || candidates.length === 0 ? undefined : pick(rng, candidates);
      out[member.localName] = chosen === undefined ? { case: undefined } : { case: chosen.localName, value: element(chosen, chosen.element) };
      continue;
    }
    const field = member;
    const nests = field.element.kind === "message" && !isStruct(field, field.element);
    if (field.mapKey !== undefined) {
      const map: Record<string, unknown> = {};
      if (!(deep && nests)) for (let idx = Math.floor(rng() * 3); idx > 0; idx--) map[mapKey(rng, field.mapKey)] = element(field, field.element);
      out[field.localName] = map;
    } else if (field.list) {
      out[field.localName] = deep && nests ? [] : Array.from({ length: Math.floor(rng() * 3) }, () => element(field, field.element));
    } else {
      const wrapped = wrapperScalar(field);
      if (wrapped !== undefined) {
        if (rng() < 0.6) out[field.localName] = scalar(rng, wrapped, false);
      } else if (field.element.kind === "message") {
        if (!(deep && nests) && rng() < 0.6) out[field.localName] = element(field, field.element);
      } else if (!field.optional || rng() < 0.6) out[field.localName] = element(field, field.element);
    }
  }
  return out;
};

// One comparable spelling of a value either side produced: `$` properties and
// absent keys dropped, keys sorted, and what JSON can't hold spelled out.
export const normalize = (value: unknown): unknown => {
  if (typeof value === "bigint") return `${value}n`;
  if (typeof value === "number") return Number.isNaN(value) ? "NaN" : Object.is(value, -0) ? "-0" : value === Infinity ? "Infinity" : value === -Infinity ? "-Infinity" : value;
  if (value instanceof Uint8Array) return { bytes: [...value] };
  if (Array.isArray(value)) return value.map(normalize);
  if (value !== null && typeof value === "object") {
    const out: Record<string, unknown> = {};
    for (const key of Object.keys(value).sort()) {
      const item = (value as Record<string, unknown>)[key];
      if (key.startsWith("$") && !key.endsWith("$")) continue;
      if (item === undefined) continue;
      out[key] = normalize(item);
    }
    return out;
  }
  return value;
};
