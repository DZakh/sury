import type { Rng } from "./generate";

// The value sampler shared by the fuzzers, and the structural oracle beside it.
// One copy: `fuzz:eq` asks whether the comparator agrees with a schema-blind
// walk, `fuzz:codec` asks whether an operation's result still conforms - both
// need values a schema admits, and a sampler that drifted between them would
// make the two runs disagree about what was even tested.
//
// The walk reads a schema's own internals and never follows `.to`, so it draws
// the INPUT side of whatever it is handed. An Output value is the same walk
// over `S.reverse(schema)`, whose head is that Output shape - which is the only
// reason a transforming schema can be sampled from both ends at all.

export type Internal = {
  type?: string;
  format?: string;
  const?: unknown;
  class?: unknown;
  anyOf?: Internal[];
  properties?: Record<string, Internal>;
  items?: Internal[];
  additionalItems?: Internal | string;
  $ref?: unknown;
};

// ---- the oracle ------------------------------------------------------------
//
// SameValueZero at the leaves, so it agrees with the emit on NaN (equal to
// itself) and on -0 (equal to 0). Built-ins whose value is their content are
// read as content; anything else carrying an identity of its own compares by
// identity, which is the only thing a synchronous walk can say about it.
export const structural = (a: unknown, b: unknown): boolean => {
  if (a === b) return true;
  if (a !== a) return b !== b;
  if (!a || !b || typeof a !== "object" || typeof b !== "object") return false;
  const proto = Object.getPrototypeOf(a);
  if (proto !== Object.getPrototypeOf(b)) return false;
  const ao = a as Record<string, unknown>;
  const bo = b as Record<string, unknown>;
  if (Array.isArray(a) || (ArrayBuffer.isView(a) && typeof (a as unknown as ArrayLike<unknown>).length === "number")) {
    const n = (a as unknown as ArrayLike<unknown>).length;
    if (n !== (b as unknown as ArrayLike<unknown>).length) return false;
    for (let i = 0; i < n; i++) if (!structural(ao[i], bo[i])) return false;
    return true;
  }
  if (proto === Date.prototype) return +(a as Date) === +(b as Date);
  if (proto === URL.prototype) return `${a}` === `${b}`;
  if (proto === Set.prototype) {
    const as = a as Set<unknown>;
    const bs = b as Set<unknown>;
    return as.size === bs.size && [...as].every((v) => bs.has(v));
  }
  if (typeof FormData !== "undefined" && proto === FormData.prototype) {
    const ae = [...(a as FormData)];
    const be = [...(b as FormData)];
    return ae.length === be.length && ae.every((e, i) => e[0] === be[i]![0] && e[1] === be[i]![1]);
  }
  if (typeof URLSearchParams !== "undefined" && proto === URLSearchParams.prototype) {
    const ae = [...(a as URLSearchParams)];
    const be = [...(b as URLSearchParams)];
    return ae.length === be.length && ae.every((e, i) => e[0] === be[i]![0] && e[1] === be[i]![1]);
  }
  if (proto !== null && proto !== Object.prototype) return false;
  // Key sets must match. An absent key and an `undefined` one are the same
  // value only where a schema declares the property optional, and this walk
  // does not read the schema - while `S.record(S.void)` is a case where they
  // are genuinely two values, since a record's keys are its content. The
  // sampler writes every declared property, optional ones included, so no pair
  // it builds turns on the distinction.
  const keys = Object.keys(ao);
  if (keys.length !== Object.keys(bo).length) return false;
  for (const key of keys) if (!(key in bo) || !structural(ao[key], bo[key])) return false;
  return true;
};

// ---- sampling --------------------------------------------------------------

export const NO_SAMPLE = Symbol("no-sample");

const FORMAT_STRINGS: Record<string, string[]> = {
  email: ["jane@example.com", "bob@example.com"],
  uuid: ["00000000-0000-0000-0000-000000000000", "11111111-1111-4111-8111-111111111111"],
  uri: ["https://example.com", "https://other.example.com"],
  "uri-reference": ["/a", "/b"],
  "uri-template": ["/{id}", "/{name}"],
  iri: ["https://example.com", "https://other.example.com"],
  "iri-reference": ["/a", "/b"],
  "idn-email": ["jane@example.com", "bob@example.com"],
  hostname: ["example.com", "other.example.com"],
  "idn-hostname": ["example.com", "other.example.com"],
  ipv4: ["127.0.0.1", "10.0.0.1"],
  ipv6: ["::1", "::2"],
  "date-time": ["2020-01-01T00:00:00Z", "2021-06-02T03:04:05Z"],
  date: ["2020-01-01", "2021-06-02"],
  time: ["00:00:00Z", "03:04:05Z"],
  duration: ["P1D", "P2D"],
  "json-pointer": ["/a", "/b"],
  "relative-json-pointer": ["0", "1"],
  json: ["1", '"x"'],
  cuid: ["cabcdefghijk", "clmnopqrstuv"],
  cuid2: ["abcdefghijk", "lmnopqrstuv"],
  ulid: ["01ARZ3NDEKTSV4RRFFQ69G5FAV", "01BX5ZZKBKACTAV9WEVGEMMVRZ"],
  ksuid: ["0ujsswThIGTUYm2K8FjOOfXtY1K", "0ujsszwN8NRY24YaXiTIE2VWDTS"],
  xid: ["9m4e2mr0ui3e8a215n4g", "9m4e2mr0ui3e8a215n50"],
  nanoid: ["V1StGXR8_Z5jdHi6B-myT", "IcOyv9-nQ0e6mQKvVc3jH"],
  hex: ["ab", "cd"],
  base64: ["aGk=", "eW8="],
  base64url: ["aGk", "eW8"],
  mac: ["00:00:00:00:00:00", "00:00:00:00:00:01"],
  e164: ["+15551234567", "+15557654321"],
  cidrv4: ["10.0.0.0/8", "192.168.0.0/16"],
  cidrv6: ["::/0", "2001:db8::/32"],
};

// Two of everything, so a schema that admits more than one value gets samples
// that differ as well as samples that match.
const instanceSample = (ctor: unknown, pick: number): unknown => {
  if (ctor === Date) return new Date(pick ? 86400000 : 0);
  if (ctor === URL) return new URL(pick ? "https://b.example.com/" : "https://a.example.com/");
  if (ctor === Error) return new Error(pick ? "b" : "a");
  if (ctor === Uint8Array) return new Uint8Array(pick ? [2, 3] : [1]);
  if (typeof Blob !== "undefined" && ctor === Blob) return new Blob([pick ? "y" : "x"]);
  if (typeof File !== "undefined" && ctor === File)
    return new File([pick ? "y" : "x"], pick ? "b.txt" : "a.txt");
  if (typeof FormData !== "undefined" && ctor === FormData) {
    const form = new FormData();
    form.append("a", pick ? "2" : "1");
    form.append("a", "shared");
    return form;
  }
  if (typeof URLSearchParams !== "undefined" && ctor === URLSearchParams) {
    const params = new URLSearchParams();
    params.append("a", pick ? "2" : "1");
    params.append("a", "shared");
    return params;
  }
  return NO_SAMPLE;
};

// A value the schema admits, drawn from `rng`. Two calls with rngs on the same
// seed build the same value TWICE - two objects, not one reference - which is
// the pair reflexivity is about.
export const sample = (schema: unknown, rng: Rng, depth = 0): unknown => {
  const s = schema as Internal;
  if (!s || typeof s !== "object") return schema;
  if ("const" in s) return s.const;
  const pick = rng() < 0.5 ? 0 : 1;
  switch (s.type) {
    case "never":
      return NO_SAMPLE;
    case "nan":
      return NaN;
    case "undefined":
      return undefined;
    case "null":
      return null;
    case "boolean":
      return !pick;
    case "symbol":
      return Symbol.for(pick ? "fuzz-b" : "fuzz-a");
    case "bigint":
      return pick ? 2n : 1n;
    case "number":
      if (s.format === "port") return pick ? 8080 : 80;
      if (s.format === "int32") return pick ? 7 : 1;
      return pick ? 2 : 1;
    case "string": {
      const options = s.format ? FORMAT_STRINGS[s.format] : undefined;
      if (options) return options[pick]!;
      // An unrecognised format would be a string the refinement rejects, and a
      // value the schema does not admit is the sampler's bug, not a finding.
      return s.format ? NO_SAMPLE : pick ? "y" : "x";
    }
    case "unknown":
      return pick ? { deep: [1, { x: NaN }] } : "x";
    case "instance":
      return instanceSample(s.class, pick);
    case "ref":
      // `S.json` and the recursive schemas: no shape to walk, and the fallback
      // is what compares them.
      return pick ? [1, { a: null }] : "x";
    case "anyOf": {
      const members = s.anyOf ?? [];
      if (!members.length) return NO_SAMPLE;
      // Every member gets picked over a run, which is what puts a value down
      // each arm of a dispatch.
      for (let tries = 0; tries < members.length; tries++) {
        const member = members[Math.floor(rng() * members.length) % members.length]!;
        const inner = sample(member, rng, depth + 1);
        if (inner !== NO_SAMPLE) return inner;
      }
      return NO_SAMPLE;
    }
    case "object": {
      const out: Record<string, unknown> = {};
      const properties = s.properties ?? {};
      for (const key of Object.keys(properties)) {
        const inner = sample(properties[key]!, rng, depth + 1);
        if (inner === NO_SAMPLE) return NO_SAMPLE;
        out[key] = inner;
      }
      const rest = s.additionalItems;
      if (rest !== undefined && typeof rest !== "string") {
        for (let i = 0; i < pick + 1; i++) {
          const inner = sample(rest, rng, depth + 1);
          if (inner === NO_SAMPLE) return NO_SAMPLE;
          out[`k${i}`] = inner;
        }
      }
      return out;
    }
    case "array": {
      const out: unknown[] = [];
      for (const item of s.items ?? []) {
        const inner = sample(item, rng, depth + 1);
        if (inner === NO_SAMPLE) return NO_SAMPLE;
        out.push(inner);
      }
      const rest = s.additionalItems;
      if (rest !== undefined && typeof rest !== "string") {
        for (let i = 0; i < pick + 1; i++) {
          const inner = sample(rest, rng, depth + 1);
          if (inner === NO_SAMPLE) return NO_SAMPLE;
          out.push(inner);
        }
      }
      return out;
    }
    default:
      return NO_SAMPLE;
  }
};

export const show = (value: unknown): string => {
  if (typeof value === "bigint") return `${value}n`;
  if (typeof value === "symbol") return value.toString();
  if (typeof FormData !== "undefined" && value instanceof FormData)
    return `FormData(${[...value].map(([k, v]) => `${k}=${String(v)}`).join(",")})`;
  if (typeof URLSearchParams !== "undefined" && value instanceof URLSearchParams)
    return `URLSearchParams(${[...value].map(([k, v]) => `${k}=${v}`).join(",")})`;
  if (value instanceof Set) return `Set(${[...value].map(String).join(",")})`;
  if (value instanceof Date) return `Date(${value.toISOString()})`;
  if (value instanceof URL) return `URL(${value.href})`;
  if (value instanceof Error) return `${value.constructor.name}(${value.message})`;
  try {
    const text = JSON.stringify(value, (_k, v) => (typeof v === "bigint" ? `${v}n` : v));
    return text === undefined ? String(value) : text;
  } catch {
    return String(value);
  }
};

