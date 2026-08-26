import { NO_WITNESS } from "./types";

export { NO_WITNESS };

type Schema = {
  type?: string;
  format?: string;
  const?: unknown;
  class?: new (...args: never[]) => unknown;
  anyOf?: Schema[];
  properties?: Record<string, Schema>;
  required?: string[];
  items?: Schema[];
  additionalItems?: Schema | string;
  $ref?: unknown;
};

const FORMAT_STRING: Record<string, string> = {
  email: "jane@example.com",
  uuid: "00000000-0000-0000-0000-000000000000",
  cuid: "cabcdefghijk",
  uri: "https://example.com",
  "uri-reference": "/a",
  "uri-template": "/{id}",
  iri: "https://example.com",
  "iri-reference": "/a",
  "idn-email": "jane@example.com",
  hostname: "example.com",
  "idn-hostname": "example.com",
  ipv4: "127.0.0.1",
  ipv6: "::1",
  "date-time": "2020-01-01T00:00:00Z",
  date: "2020-01-01",
  time: "00:00:00Z",
  duration: "P1D",
  "json-pointer": "/a",
  "relative-json-pointer": "0",
  json: "1",
};

const instanceWitness = (ctor: unknown): unknown => {
  if (ctor === Date) return new Date(0);
  if (ctor === URL) return new URL("https://example.com/");
  if (ctor === Error) return new Error("e");
  if (ctor === Uint8Array) return new Uint8Array([1]);
  if (typeof Blob !== "undefined" && ctor === Blob) return new Blob(["x"]);
  if (typeof File !== "undefined" && ctor === File) {
    return new File(["x"], "a.txt");
  }
  return NO_WITNESS;
};

export const witnessOf = (schema: unknown): unknown => {
  const s = schema as Schema;
  if (!s || typeof s !== "object") return schema;
  if (s.type === "never") return NO_WITNESS;
  if (s.type === "nan") return NaN;
  if (s.type === "undefined") return undefined;
  if (s.type === "null") return null;
  if ("const" in s) return s.const;
  switch (s.type) {
    case "string":
      return (s.format && FORMAT_STRING[s.format]) || "x";
    case "number":
      if (s.format === "port") return 80;
      return 1;
    case "bigint":
      return 1n;
    case "boolean":
      return true;
    case "symbol":
      return Symbol.for("fuzz");
    case "unknown":
      return "x";
    case "ref":
      return 1;
    case "instance":
      return instanceWitness(s.class);
    case "anyOf": {
      const variants = s.anyOf ?? [];
      for (const variant of variants) {
        const inner = witnessOf(variant);
        if (inner !== NO_WITNESS && inner !== undefined && inner !== null) {
          return inner;
        }
      }
      for (const variant of variants) {
        const inner = witnessOf(variant);
        if (inner !== NO_WITNESS) return inner;
      }
      return NO_WITNESS;
    }
    case "object": {
      const out: Record<string, unknown> = {};
      const properties = s.properties ?? {};
      const keys = s.required ?? Object.keys(properties);
      for (const key of keys) {
        const field = properties[key];
        if (!field) continue;
        const inner = witnessOf(field);
        if (inner === NO_WITNESS) return NO_WITNESS;
        out[key] = inner;
      }
      return out;
    }
    case "array": {
      if (Array.isArray(s.items) && s.items.length) {
        const items: unknown[] = [];
        for (const item of s.items) {
          const inner = witnessOf(item);
          if (inner === NO_WITNESS) return NO_WITNESS;
          items.push(inner);
        }
        return items;
      }
      if (s.additionalItems && typeof s.additionalItems === "object") {
        const inner = witnessOf(s.additionalItems);
        if (inner === NO_WITNESS) return NO_WITNESS;
        return [inner];
      }
      return [];
    }
    default:
      return NO_WITNESS;
  }
};

export const JUNK: readonly unknown[] = [
  "a",
  "",
  0,
  -0,
  1,
  true,
  false,
  null,
  undefined,
  10n,
  NaN,
  {},
  [],
  { TAG: "Z", _0: "x" },
  { kind: "z", v: "s" },
  { TAG: "Four", _0: "x" },
];
