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
//   antisymmetric `compare(a,b)` is `-compare(b,a)`. What makes the answer a
//                sort comparator rather than a difference test, and the reason
//                compare refuses the schemas it has no order for: a schema it
//                accepts has to hold this for every pair. A schema it refuses
//                is skipped for the compare properties and checked for the
//                rest.
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
import { generateSchema, rngFromSeed } from "./unionFuzz/generate";
import { NO_SAMPLE, sample, show, structural } from "./unionFuzz/sample";
import type { Sury } from "./unionFuzz/types";

const KNOWN: Record<string, string> = {};

// A Sury refusal, as opposed to a crash: the class, not the wording.
const isRefusal = (error: unknown): boolean =>
  error instanceof (S as unknown as { Error: new () => Error }).Error ||
  (error as { code?: unknown }).code !== undefined;

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
  let id: string;
  let schema: unknown;
  try {
    ({ id, schema } = generateSchema(sury, next));
  } catch (error) {
    // Building a schema is not what this fuzzer is about, but a grammar that
    // cannot produce one has nothing to compare - and the throw is a finding
    // wherever it comes from.
    findings.push(`creation: ${(error as Error).message.split("\n")[0]}`);
    continue;
  }

  let isEqualOutput: (a: unknown, b: unknown) => boolean;
  let isEqualInput: (a: unknown, b: unknown) => boolean;
  let compareOutput: ((a: unknown, b: unknown) => number) | undefined;
  let conforms: (v: unknown) => boolean;
  try {
    isEqualOutput = S.isEqualOutput(schema as never) as (a: unknown, b: unknown) => boolean;
    isEqualInput = S.isEqualInput(schema as never) as (a: unknown, b: unknown) => boolean;
    conforms = S.isOutput(schema as never) as (v: unknown) => boolean;
  } catch (error) {
    // A schema the comparator has no answer for refuses while compiling, which
    // is the contract its own spec records. `S.never` inside a field is one:
    // nothing inhabits it, so there is no pair to compare.
    if (!isRefusal(error)) {
      report(`${id}: compile`, `building the comparator threw - ${(error as Error).message.split("\n")[0]}`);
    }
    continue;
  }
  // A schema with no order is refused when the comparator is compiled, which
  // is the contract every spec of such a schema pins. Anything else thrown here
  // is a finding.
  try {
    compareOutput = S.compareOutput(schema as never) as (a: unknown, b: unknown) => number;
  } catch (error) {
    const message = (error as Error).message;
    if (!message.startsWith("[Sury] Can't compare "))
      report(`${id}: compile`, `building the comparator threw - ${message.split("\n")[0]}`);
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
  let antisymmetric = compareOutput !== undefined;
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
      if (compareOutput === undefined) continue;
      let cmp: unknown;
      let cmpBack: unknown;
      try {
        cmp = compareOutput(a, b);
        cmpBack = compareOutput(b, a);
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
      if (cmp !== -(cmpBack as number)) {
        antisymmetric = false;
        report(
          `${id}: antisymmetric`,
          `${show(a)} vs ${show(b)}: compare ${show(cmp)} one way and ${show(cmpBack)} the other`,
        );
      }
    }
  }
  if (symmetric) holds(`${id}: symmetric`);
  if (antisymmetric) holds(`${id}: antisymmetric`);
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
