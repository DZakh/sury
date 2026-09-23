// The schema fuzzer: one generated schema, both of its sides sampled, and every
// family of properties that can be asked of a single schema held against it.
//
//   pnpm --filter=sury fuzz:schema                      the gate, as CI runs it
//   pnpm --filter=sury fuzz:schema --only=eq --seed=24   a narrower search
//
// Families (`schemaFuzz/`), each with its own properties:
//
//   eq      the comparators: an equivalence, an order, one comparator reached
//           from both sides, and no looser than the decode it feeds.
//   codec   decode and encode: each lands on the side it claims, parse agrees
//           with decode, encode undoes decode, and reverse is the same encode.
//
// One more property belongs to the runner, since every family needs it first:
//
//   creation     building the schema does not throw. A default the grammar
//                drew off the Output side is one the schema has to accept, so a
//                throw here is a finding rather than a draw to skip (#452).
//
// Why one runner: the families need the same schema, the same values and the
// same refusal rules, and two runners drew and sampled everything twice. It is
// also what gives each family both sides: Input values are the sampler over the
// schema, Output values the sampler over its reverse. The comparator fuzzer
// used to draw one sample and filter it through `isOutput`, which rejected
// every draw of any schema that transforms.
//
// `fuzz:union` stays apart. It asks a different question - does the compiled
// union dispatch like trying each member in turn - of a list of members rather
// than one schema, against a reference that re-runs each member. `fuzz:formdata`
// is exhaustive over its own wrapper-by-leaf grid and needs no seed.
//
// The grammar branches on every draw, so consecutive seeds reach regions one
// long stream does not: a bug found at seed 24 in 1500 cases was still not
// found at seed 1 in 150000. `--seeds=N` is what widens the search.
//
// What is known not to hold is `scripts/knownBugs.ts`, shared with `fuzz:union`.
// A finding it does not cover fails the run. The default invocation is the gate
// - forty seeds of 1500 schemas, both families - and only the gate also fails
// on an entry nothing matched, since a narrower search reaching less says
// nothing about a bug. Any of `--seed`, `--seeds`, `--cases` or `--only` makes
// the run a search rather than the gate. A finding becomes a spec.

import * as S from "../index.mjs";
import { codec } from "./schemaFuzz/codec";
import { knownFor, staleFor } from "./knownBugs";
import { type Ctx, type Family, reason } from "./schemaFuzz/context";
import { eq } from "./schemaFuzz/eq";
import { generateSchema, rngFromSeed, takeRefused } from "./unionFuzz/generate";
import { NO_SAMPLE, sample } from "./unionFuzz/sample";
import type { Sury } from "./unionFuzz/types";

const FAMILIES: Record<string, Family> = { eq, codec };

const arg = (name: string, fallback: string): number => {
  const hit = process.argv.find((a) => a.startsWith(`--${name}=`));
  const value = Number(hit === undefined ? fallback : hit.slice(name.length + 3));
  if (!Number.isFinite(value) || value < 1) throw new Error(`--${name} must be a positive number`);
  return value;
};

const gate = !process.argv.some((a) => /^--(seed|seeds|cases|only)=/.test(a));
const seed = arg("seed", "1");
const seeds = arg("seeds", "40");
const cases = arg("cases", "1500");
const show = arg("show", "40");
const only = process.argv.find((a) => a.startsWith("--only="))?.slice(7).split(",");
const running = Object.entries(FAMILIES).filter(([name]) => !only || only.includes(name));
if (only && running.length !== only.length) {
  throw new Error(`--only takes ${Object.keys(FAMILIES).join(", ")}`);
}
const SAMPLES = 4;

// Refused rather than crashed: a SuryError (or anything carrying its `code`) is
// the schema's own contract. Judged by class, not wording: a `SyntaxError` out
// of `new Function` is the compiler emitting code that will not parse, which is
// a finding however it reads.
const refused = (error: unknown): boolean =>
  error instanceof (S as unknown as { Error: new () => Error }).Error ||
  (error as { code?: unknown }).code !== undefined;

const findings: string[] = [];
const counts: Record<string, number> = {};
const count = (what: string, n = 1): void => {
  counts[what] = (counts[what] ?? 0) + n;
};

const sury = S as unknown as Sury;
let stream = seed;
let next = rngFromSeed(stream);

for (let c = 0; c < cases * seeds; c++) {
  const at = c % cases;
  if (c && !at) next = rngFromSeed(++stream);

  const member = generateSchema(sury, next);
  // A default the library refused. The grammar built the shape without it and
  // carried on, so the draw still lines up with any other build's.
  for (const refusal of takeRefused()) findings.push(`creation: ${refusal}`);
  const { id, schema } = member;
  count("schemas");

  let reversed: unknown;
  let isInput: (value: unknown) => unknown;
  let isOutput: (value: unknown) => unknown;
  try {
    reversed = S.reverse(schema as never);
    isInput = S.isInput(schema as never) as unknown as typeof isInput;
    isOutput = S.isOutput(schema as never) as unknown as typeof isOutput;
  } catch (error) {
    // Without both validators there is no side to sample or hold an answer
    // against, and a refusal that denied them is the schema's own contract.
    if (!refused(error)) findings.push(`${id}: setup: ${reason(error)}`);
    continue;
  }

  const inputs: [unknown, unknown][] = [];
  const outputs: [unknown, unknown][] = [];
  for (let slot = 0; slot < SAMPLES; slot++) {
    const slotSeed = (stream + at * 97 + slot * 7919) | 0;
    for (const [from, accepts, into] of [
      [schema, isInput, inputs],
      [reversed, isOutput, outputs],
    ] as const) {
      const first = sample(from, rngFromSeed(slotSeed));
      const second = sample(from, rngFromSeed(slotSeed));
      // A sample the side rejects is the sampler falling short of a bound or a
      // pattern, which says nothing about the operations.
      if (first !== NO_SAMPLE && accepts(first) === true && accepts(second) === true) {
        into.push([first, second]);
        count("values");
      } else count("unfilled");
    }
  }

  for (const [name, family] of running) {
    const report = (property: string, detail: string): void => {
      if (!knownFor(name as "eq" | "codec", id, property, detail)) findings.push(`${id}: ${property}: ${detail}`);
    };
    const ctx: Ctx = {
      S: sury,
      id,
      schema,
      reversed,
      lossy: !!member.lossy,
      inputs,
      outputs,
      isInput,
      isOutput,
      report,
      compile: (property, build) => {
        try {
          return build() as never;
        } catch (error) {
          if (!refused(error)) report(property, `compiling threw - ${reason(error)}`);
          return undefined;
        }
      },
      count,
    };
    family.check(ctx);
  }
}

if (gate) findings.push(...staleFor(running.map(([name]) => name as "eq" | "codec")));

console.log(
  `${Object.entries(counts)
    .map(([what, n]) => `${n} ${what}`)
    .join(", ")} (${running.map(([name]) => name).join(" + ")}; ${cases} cases on ` +
    `${seeds > 1 ? `seeds ${seed}-${seed + seeds - 1}` : `seed ${seed}`})`,
);
if (findings.length) {
  console.log(`\n${findings.length} finding(s):`);
  for (const finding of findings.slice(0, show)) console.log(`  ${finding}`);
  if (findings.length > show) console.log(`  … ${findings.length - show} more (--show=N)`);
  process.exitCode = 1;
} else {
  console.log("No findings.");
}
