// `S.isEqualInput` / `S.isEqualOutput` / `S.compareInput` / `S.compareOutput` fuzzer.
//
//   pnpm --filter=sury fuzz:eq
//   pnpm --filter=sury fuzz:eq --seeds=40 --cases=2000
//   pnpm --filter=sury fuzz:eq --seed=24 --show-known
//
// A spec pins the comparator's generated code and the answers it gives for the
// values that spec writes down. Neither says anything about a schema no spec
// has, and the emit is a tree of special cases - a union narrow, a tagged
// dispatch, a hoisted loop, a structural fallback - where the wrong branch is
// invisible until some value takes it. So schemas come from the union fuzzer's
// grammar, values are sampled from each schema, and the answers are held to the
// properties an equivalence has to satisfy whatever the emit chose:
//
//   reflexive    a value equals a separately built copy of itself. The one
//                property with no escape: it is what the `a === b` the emit
//                opens with cannot answer, and what NaN breaks.
//   symmetric    `eq(a,b)` is `eq(b,a)`. A dispatch that narrows on `a` and
//                forgets to narrow on `b` fails here and nowhere else.
//   transitive   equal to the same value means equal to each other.
//   oracle       agrees with a structural walk written here, without reference
//                to the schema.
//   compare-zero `compare(a,b)===0` exactly when `isEqual(a,b)`. The two
//                compiles share a walker; this is the invariant that sharing
//                exists to keep.
//   duality      `isEqualInput(schema)` is `isEqualOutput(reverse(schema))`.
//                One comparator, reached two ways.
//   congruence   two inputs the Input side calls equal decode to two outputs
//                the Output side calls equal. A decoder is a function, so the
//                only way this breaks is an Input comparator that is looser
//                than the decode it feeds.
//
// KNOWN lists the cases that do not hold, keyed by what the run prints, with
// the reason written by hand. The run fails on an unlisted finding and on a
// listed one that has started to hold.

import * as S from "../index.mjs";
import { generateSchema, rngFromSeed, type Rng } from "./unionFuzz/generate";
import type { Sury } from "./unionFuzz/types";

const KNOWN: Record<string, string> = {};

type Internal = {
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
const structural = (a: unknown, b: unknown): boolean => {
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

const NO_SAMPLE = Symbol("no-sample");

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
const sample = (schema: unknown, rng: Rng, depth = 0): unknown => {
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

const show = (value: unknown): string => {
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

// ---- the run ---------------------------------------------------------------

const arg = (name: string, fallback: string): number => {
  const hit = process.argv.find((a) => a.startsWith(`--${name}=`));
  const value = Number(hit === undefined ? fallback : hit.slice(name.length + 3));
  if (!Number.isFinite(value)) throw new Error(`--${name} must be a number`);
  return value;
};

const cases = arg("cases", "600");
const seed = arg("seed", "1");
// The grammar branches on every draw, so consecutive seeds reach regions one
// long stream does not: a bug found at seed 24 in 1500 cases was still not
// found at seed 1 in 150000. Coverage comes from sweeping seeds, and `--cases`
// is how many schemas each one draws.
const seeds = arg("seeds", "1");
const SAMPLES = 4;

const findings: string[] = [];
const used = new Set<string>();

// A finding is keyed by everything but the values, so one entry covers a case
// however the sampler reached it.
const report = (key: string, detail: string): void => {
  used.add(key);
  if (KNOWN[key] === undefined) findings.push(`${key}: ${detail}`);
};

const holds = (key: string): void => {
  used.add(key);
  if (KNOWN[key] !== undefined) findings.push(`${key}: listed in KNOWN but holds - delete the entry`);
};

let compared = 0;
let sampled = 0;
let skipped = 0;
let congruences = 0;

const sury = S as unknown as Sury;
let stream = seed;
let next = rngFromSeed(stream);

for (let c = 0; c < cases * seeds; c++) {
  const at = c % cases;
  if (c && !at) next = rngFromSeed(++stream);
  const { id, schema } = generateSchema(sury, next);

  let isEqualOutput: (a: unknown, b: unknown) => boolean;
  let isEqualInput: (a: unknown, b: unknown) => boolean;
  let compareOutput: (a: unknown, b: unknown) => number;
  let conforms: (v: unknown) => boolean;
  try {
    isEqualOutput = S.isEqualOutput(schema as never) as (a: unknown, b: unknown) => boolean;
    isEqualInput = S.isEqualInput(schema as never) as (a: unknown, b: unknown) => boolean;
    compareOutput = S.compareOutput(schema as never) as (a: unknown, b: unknown) => number;
    conforms = S.isOutput(schema as never) as (v: unknown) => boolean;
  } catch (error) {
    report(`${id}: compile`, `building the comparator threw - ${(error as Error).message.split("\n")[0]}`);
    continue;
  }

  // The same slot sampled twice, from two rngs on one seed: two values built
  // by the same draws, and never the same object.
  const values: unknown[][] = [];
  for (let slot = 0; slot < SAMPLES; slot++) {
    const slotSeed = (stream + at * 97 + slot * 7919) | 0;
    const first = sample(schema, rngFromSeed(slotSeed));
    const second = sample(schema, rngFromSeed(slotSeed));
    // A value the schema does not admit says nothing about a comparator that
    // is allowed to assume conformance.
    if (first === NO_SAMPLE || !conforms(first) || !conforms(second)) {
      skipped++;
      continue;
    }
    sampled++;
    values.push([first, second]);
  }
  if (!values.length) continue;

  const ask = (
    fn: (a: unknown, b: unknown) => boolean,
    what: string,
    a: unknown,
    b: unknown,
  ): boolean | undefined => {
    try {
      return fn(a, b);
    } catch (error) {
      report(`${id}: ${what}`, `threw on (${show(a)}, ${show(b)}) - ${(error as Error).message.split("\n")[0]}`);
      return undefined;
    }
  };

  let reflexive = true;
  for (const [first, second] of values) {
    // Gated on the oracle, which is the arbiter here too: a Blob, an Error, a
    // user class has nothing but its identity, so a rebuilt copy is a different
    // value and both sides say so. Reflexivity is about the values that DO have
    // structure - the pair the `a === b` the emit opens with cannot answer.
    if (!structural(first, second)) continue;
    const answer = ask(isEqualOutput, "reflexive", first, second);
    if (answer === undefined) reflexive = false;
    else if (answer !== true) {
      reflexive = false;
      report(
        `${id}: reflexive`,
        `answered ${show(answer)} for ${show(first)} against a separately built copy of itself`,
      );
    }
  }
  if (reflexive) holds(`${id}: reflexive`);

  let symmetric = true;
  let agrees = true;
  for (let i = 0; i < values.length; i++) {
    for (let j = 0; j < values.length; j++) {
      const a = values[i]![0];
      const b = values[j]![1];
      const forward = ask(isEqualOutput, "symmetric", a, b);
      const back = ask(isEqualOutput, "symmetric", b, a);
      if (forward === undefined || back === undefined) {
        symmetric = false;
        agrees = false;
        continue;
      }
      compared++;
      if (forward !== back) {
        symmetric = false;
        report(
          `${id}: symmetric`,
          `${show(a)} vs ${show(b)} reads ${forward} one way and ${back} the other`,
        );
      }
      const want = structural(a, b);
      if (forward !== want) {
        agrees = false;
        report(
          `${id}: oracle`,
          `${show(a)} vs ${show(b)}: comparator ${forward}, structural ${want}`,
        );
      }
      let cmp: unknown;
      try {
        cmp = compareOutput(a, b);
      } catch (error) {
        report(`${id}: compare-zero`, `threw - ${(error as Error).message.split("\n")[0]}`);
        continue;
      }
      if (cmp !== -1 && cmp !== 0 && cmp !== 1) {
        report(`${id}: compare-zero`, `${show(a)} vs ${show(b)} answered ${show(cmp)}, not -1|0|1`);
      } else if ((cmp === 0) !== want) {
        report(
          `${id}: compare-zero`,
          `${show(a)} vs ${show(b)}: compare ${show(cmp)}, isEqual ${forward}`,
        );
      }
    }
  }
  if (symmetric) holds(`${id}: symmetric`);
  if (agrees) holds(`${id}: oracle`);

  let transitive = true;
  for (let i = 0; i < values.length; i++)
    for (let j = 0; j < values.length; j++)
      for (let k = 0; k < values.length; k++) {
        const a = values[i]![0];
        const b = values[j]![0];
        const c2 = values[k]![1];
        if (
          ask(isEqualOutput, "transitive", a, b) === true &&
          ask(isEqualOutput, "transitive", b, c2) === true &&
          ask(isEqualOutput, "transitive", a, c2) !== true
        ) {
          transitive = false;
          report(
            `${id}: transitive`,
            `${show(a)} equals ${show(b)} equals ${show(c2)}, but the first and last do not`,
          );
        }
      }
  if (transitive) holds(`${id}: transitive`);

  // Reversing swaps the sides, so the Input comparator of a schema and the
  // Output comparator of its reverse are the same question asked twice.
  let dual = true;
  try {
    const reversedOutput = S.isEqualOutput(S.reverse(schema as never) as never) as (
      a: unknown,
      b: unknown,
    ) => boolean;
    for (const [first, second] of values) {
      const direct = ask(isEqualInput, "duality", first, second);
      if (direct === undefined) {
        dual = false;
        continue;
      }
      const viaReverse = reversedOutput(first, second);
      if (direct !== viaReverse) {
        dual = false;
        report(
          `${id}: duality`,
          `isEqualInput answered ${direct} for ${show(first)} but isEqualOutput of the reverse ` +
            `answered ${viaReverse}`,
        );
      }
    }
  } catch (error) {
    dual = false;
    report(`${id}: duality`, `reversing threw - ${(error as Error).message.split("\n")[0]}`);
  }
  if (dual) holds(`${id}: duality`);

  // A decoder is a function: what the Input side calls one value has to leave
  // the decode as one value too. Only a comparator looser than the decode it
  // feeds can break this.
  let congruent = true;
  let decode: ((v: unknown) => unknown) | undefined;
  try {
    decode = S.decodeOrThrow(schema as never) as unknown as (v: unknown) => unknown;
  } catch {
    decode = undefined;
  }
  if (decode) {
    for (let i = 0; i < values.length; i++)
      for (let j = 0; j < values.length; j++) {
        const a = values[i]![0];
        const b = values[j]![1];
        const equalIn = ask(isEqualInput, "congruence", a, b);
        if (equalIn === undefined) {
          congruent = false;
          continue;
        }
        if (equalIn !== true) continue;
        let da: unknown;
        let db: unknown;
        try {
          da = decode(a);
          db = decode(b);
        } catch {
          // An input the decode rejects is one the Input comparator was never
          // promised, and `isInput` is not what selected these samples.
          continue;
        }
        congruences++;
        if (ask(isEqualOutput, "congruence", da, db) !== true) {
          congruent = false;
          report(
            `${id}: congruence`,
            `${show(a)} and ${show(b)} are equal on the Input side but decode to ${show(da)} ` +
              `and ${show(db)}, which are not equal on the Output side`,
          );
        }
      }
  }
  if (congruent) holds(`${id}: congruence`);
}

for (const key of Object.keys(KNOWN))
  if (!used.has(key)) findings.push(`${key}: listed in KNOWN but no such case ran - the grammar moved under it`);

if (process.argv.includes("--show-known")) {
  console.log("\ncases known not to hold:");
  for (const [key, reason] of Object.entries(KNOWN)) console.log(`  ${key}\n    ${reason}`);
  console.log("");
}

console.log(
  `${compared} comparisons over ${sampled} values from ${cases * seeds} schemas ` +
    `(${skipped} slots the sampler could not fill), ${congruences} decode congruences ` +
    `(${cases} cases on ${seeds > 1 ? `seeds ${seed}-${seed + seeds - 1}` : `seed ${seed}`})`,
);
if (findings.length) {
  const shown = findings.slice(0, 40);
  console.log(`\n${findings.length} finding(s):`);
  for (const finding of shown) console.log(`  ${finding}`);
  if (findings.length > shown.length) console.log(`  … ${findings.length - shown.length} more`);
  process.exitCode = 1;
} else {
  console.log("No findings.");
}
