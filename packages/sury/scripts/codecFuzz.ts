// The decode/encode fuzzer.
//
//   pnpm --filter=sury fuzz:codec
//   pnpm --filter=sury fuzz:codec --seeds=40 --cases=2000
//   pnpm --filter=sury fuzz:codec --seed=24 --show-known
//
// A spec pins the code one schema generates and the values that spec writes
// down. What no spec can pin is the agreement between the two sides of a
// schema, because the interesting disagreements need a shape nobody thought to
// write: a container whose transform lives in its items rather than on its
// `.to`, a default written in the Output form, a union under either. So schemas
// come from the union fuzzer's grammar, values are sampled from both ends, and
// the answers are held to the properties that hold whatever the emit chose:
//
//   creation     building the schema does not throw. A default the sampler drew
//                off the Output side is one the schema has to accept, so a
//                throw here is a finding rather than a case to skip (#452).
//   conformance  an operation's result lands on the side it claims: a decode
//                produces a value the schema's own Output validator accepts, an
//                encode one its Input validator accepts. The property that
//                needs no oracle - the schema is asked about its own answer -
//                and the one a half-applied transform always breaks.
//   agreement    `parse` and `decode` answer the same thing for an input the
//                Input side accepts. They differ only in whether they validate,
//                so a transform that one runs and the other skips shows here
//                and nowhere else.
//   round-trip   `decode(encode(o))` is `o`. An encode that undoes less, or
//                more, than the decode it mirrors fails this even when both
//                ends conform.
//   duality      `encode(schema)` is `decode(reverse(schema))`. One operation,
//                reached two ways, so a reverse that rebuilds rather than
//                swaps is caught against itself.
//
// Equality is the structural walk from `sample.ts`, never `isEqual*`: that
// comparator is what `fuzz:eq` is testing, and a property holding only because
// both sides are wrong is not a property.
//
// The default invocation is the gate: one seed, and KNOWN below is settled
// against exactly it. `--seeds=N` widens the search and is exploratory - a
// deeper sweep still turns up pre-existing bugs, and a finding there is a case
// to write down, not a reason to add a pattern until the gate goes quiet.

import * as S from "../index.mjs";
import { generateSchema, rngFromSeed } from "./unionFuzz/generate";
import { NO_SAMPLE, sample, show, structural } from "./unionFuzz/sample";
import type { Sury } from "./unionFuzz/types";

// The cases that do not hold, with the reason written by hand. Keyed by a
// SUBSTRING of what the run prints rather than a whole id: an id names one
// draw of one seed, and the grammar moves under it every time a shape is
// added, so exact keys turn every grammar change into a wall of stale
// entries. A pattern names the shape the bug is about. The run fails on a
// finding no pattern covers, and on a pattern nothing matches any more -
// which is how a fixed bug tells you to delete its entry.
const KNOWN: Record<string, string> = {
  // `S.to` with a custom coder leaves the source's format refinement on the
  // Output side: `isOutput` of `Schema<uuid, number>` emits the uuid pattern
  // and runs it over the number. The decode is right, the validator is wrong,
  // so every value of such a schema fails conformance.
  ".with(to): conformance": "isOutput runs the source's format check over the target value",
  // A default makes the value always present, but when the item is ITSELF
  // optional or nullable the Output type still admits the absent case. So
  // `isOutput` accepts the absent value, and encoding it has nothing to hand
  // back. The values are right and the output type is too wide.
  "{fieldOr(f,nullish(": "a default over an already-optional item leaves the absent case in its output type",
  "{fieldOr(f,optional(": "a default over an already-optional item leaves the absent case in its output type",
};

const knownFor = (key: string): string | undefined =>
  Object.keys(KNOWN).find((pattern) => key.includes(pattern));

const matched = new Set<string>();
const arg = (name: string, fallback: string): number => {
  const hit = process.argv.find((a) => a.startsWith(`--${name}=`));
  const value = hit ? Number(hit.slice(name.length + 3)) : Number(fallback);
  if (!Number.isFinite(value) || value < 1) throw new Error(`--${name} must be a positive number`);
  return value;
};

const seed = arg("seed", "1");
const cases = arg("cases", "600");
const seeds = arg("seeds", "1");
const SAMPLES = 3;

const findings: string[] = [];

const report = (key: string, detail: string): void => {
  const pattern = knownFor(key);
  if (pattern === undefined) findings.push(`${key}: ${detail}`);
  else matched.add(pattern);
};

const reason = (error: unknown): string => (error as Error).message.split("\n")[0]!;

// An operation a schema legitimately has no answer for refuses while compiling,
// which is a contract its own spec records rather than a finding here. The test
// is the exception's class, not its wording: a `SyntaxError` out of `new
// Function` is the compiler emitting code that will not parse, which is a
// finding however it reads.
const refused = (error: unknown): boolean =>
  error instanceof (S as unknown as { Error: new () => Error }).Error ||
  (error as { code?: unknown }).code !== undefined;

type Op = ((value: unknown) => unknown) | undefined;

const compile = (id: string, what: string, build: () => Op): Op | null => {
  try {
    return build();
  } catch (error) {
    if (refused(error)) return null;
    report(`${id}: ${what}`, `compiling threw - ${reason(error)}`);
    return null;
  }
};

const sury = S as unknown as Sury;
let stream = seed;
let next = rngFromSeed(stream);

let schemas = 0;
let checked = 0;
let unfilled = 0;

for (let c = 0; c < cases * seeds; c++) {
  const at = c % cases;
  if (c && !at) next = rngFromSeed(++stream);

  let id = "?";
  let schema: unknown;
  let lossy = false;
  try {
    const member = generateSchema(sury, next);
    id = member.id;
    schema = member.schema;
    lossy = !!member.lossy;
  } catch (error) {
    // `generateSchema` names the member it was building in the message only by
    // luck, so the id is whatever it reached - enough to rebuild it by seed.
    report(`${id}: creation`, `building the schema threw - ${reason(error)}`);
    continue;
  }
  schemas++;

  let reversed: unknown;
  try {
    reversed = S.reverse(schema as never);
  } catch (error) {
    report(`${id}: duality`, `reversing threw - ${reason(error)}`);
    continue;
  }

  const decode = compile(id, "decode", () => S.decodeOrThrow(schema as never) as unknown as Op);
  const encode = compile(id, "encode", () => S.encodeOrThrow(schema as never) as unknown as Op);
  const parse = compile(id, "agreement", () => S.parseOrThrow(schema as never) as unknown as Op);
  const viaReverse = compile(id, "duality", () => S.decodeOrThrow(reversed as never) as unknown as Op);
  const isInput = compile(id, "conformance", () => S.isInput(schema as never) as unknown as Op);
  const isOutput = compile(id, "conformance", () => S.isOutput(schema as never) as unknown as Op);
  // Without both validators there is no side to hold an answer against, and
  // the refusal that denied them is the schema's own contract.
  if (!isInput || !isOutput) continue;

  // Input values walk the schema, Output values walk its reverse. A sample the
  // corresponding validator rejects is the sampler falling short of the shape,
  // which says nothing about the operations.
  const inputs: unknown[] = [];
  const outputs: unknown[] = [];
  for (let slot = 0; slot < SAMPLES; slot++) {
    const slotSeed = (stream + at * 97 + slot * 7919) | 0;
    const i = sample(schema, rngFromSeed(slotSeed));
    if (i !== NO_SAMPLE && isInput(i) === true) inputs.push(i);
    else unfilled++;
    const o = sample(reversed, rngFromSeed(slotSeed));
    if (o !== NO_SAMPLE && isOutput(o) === true) outputs.push(o);
    else unfilled++;
  }

  // A promise is a shape none of these properties are written for, and the
  // grammar produces one only by way of a schema whose async-ness is its point.
  const settled = (value: unknown): boolean => !(value instanceof Promise);

  if (decode) {
    for (const i of inputs) {
      let decoded: unknown;
      try {
        decoded = decode(i);
      } catch (error) {
        // `decode` trusts its input, so a value `isInput` accepted must get
        // through it. A throw is the two disagreeing about the same side.
        report(`${id}: conformance`, `decode threw on ${show(i)}, which isInput accepts - ${reason(error)}`);
        continue;
      }
      if (!settled(decoded)) continue;
      checked++;
      if (isOutput(decoded) !== true) {
        report(
          `${id}: conformance`,
          `decode turned ${show(i)} into ${show(decoded)}, which the schema's own isOutput rejects`,
        );
      }
      if (!parse) continue;
      let parsed: unknown;
      try {
        parsed = parse(i);
      } catch (error) {
        report(`${id}: agreement`, `parse threw on ${show(i)} but decode did not - ${reason(error)}`);
        continue;
      }
      if (settled(parsed) && !structural(parsed, decoded)) {
        report(
          `${id}: agreement`,
          `parse answered ${show(parsed)} and decode ${show(decoded)} for ${show(i)}`,
        );
      }
    }
  }

  if (encode) {
    for (const o of outputs) {
      let encoded: unknown;
      try {
        encoded = encode(o);
      } catch (error) {
        report(`${id}: conformance`, `encode threw on ${show(o)}, which isOutput accepts - ${reason(error)}`);
        continue;
      }
      if (!settled(encoded)) continue;
      checked++;
      if (isInput(encoded) !== true) {
        report(
          `${id}: conformance`,
          `encode turned ${show(o)} into ${show(encoded)}, which the schema's own isInput rejects`,
        );
      }
      if (viaReverse) {
        let mirror: unknown;
        try {
          mirror = viaReverse(o);
        } catch (error) {
          report(`${id}: duality`, `decoding the reverse threw on ${show(o)} but encode did not - ${reason(error)}`);
          mirror = NO_SAMPLE;
        }
        if (mirror !== NO_SAMPLE && settled(mirror) && !structural(mirror, encoded)) {
          report(
            `${id}: duality`,
            `encode answered ${show(encoded)} for ${show(o)} but decoding the reverse answered ${show(mirror)}`,
          );
        }
      }
      // A lossy conversion has no round trip to hold it to; the grammar says
      // which members carry one.
      if (!decode || lossy) continue;
      let back: unknown;
      try {
        back = decode(encoded);
      } catch (error) {
        report(`${id}: round-trip`, `decoding the encode of ${show(o)} threw - ${reason(error)}`);
        continue;
      }
      if (settled(back) && !structural(back, o)) {
        report(`${id}: round-trip`, `${show(o)} encoded to ${show(encoded)} and decoded back to ${show(back)}`);
      }
    }
  }
}

for (const pattern of Object.keys(KNOWN))
  if (!matched.has(pattern))
    findings.push(
      `${pattern}: listed in KNOWN but nothing matched it - either it is fixed (delete the ` +
        `entry) or the grammar no longer reaches the shape (widen it)`,
    );

if (process.argv.includes("--show-known")) {
  console.log("\ncases known not to hold:");
  for (const [key, why] of Object.entries(KNOWN)) console.log(`  ${key}\n    ${why}`);
  console.log("");
}

console.log(
  `${checked} operation results over ${schemas} schemas ` +
    `(${unfilled} slots the sampler could not fill) ` +
    `(${cases} cases on ${seeds > 1 ? `seeds ${seed}-${seed + seeds - 1}` : `seed ${seed}`})`,
);
if (findings.length) {
  // Grouped by property, so a sweep that turns up one bug in forty shapes reads
  // as one line rather than forty.
  const byProperty: Record<string, number> = {};
  for (const finding of findings) {
    const key = finding.slice(finding.lastIndexOf(": ", finding.indexOf(": ") + 1) + 2);
    byProperty[key.split(":")[0]!] = (byProperty[key.split(":")[0]!] ?? 0) + 1;
  }
  const shown = findings.slice(0, arg("show", "40"));
  console.log(`\n${findings.length} finding(s):`);
  for (const finding of shown) console.log(`  ${finding}`);
  if (findings.length > shown.length) console.log(`  … ${findings.length - shown.length} more`);
  process.exitCode = 1;
} else {
  console.log("No findings.");
}
