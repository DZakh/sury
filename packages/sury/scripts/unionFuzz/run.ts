import { classify, describeOutcome, outcomeOf, show } from "./outcome";
import type { MemberSpec } from "./generate";
import {
  compiledEncode,
  compiledParse,
  flattenVariants,
  referenceEncode,
  referenceParse,
} from "./reference";
import type { DiffClass, Outcome, Sury } from "./types";
import { JUNK, NO_WITNESS, witnessOf } from "./witness";

// `is`/`result` compare an answering outcome to the same compile's
// `parseOrThrow`, not to the reference: see `outcomeDiffs`.
export type Direction = "parse" | "encode" | "is" | "result";

export type Comparison = {
  direction: Direction;
  input: unknown;
  compiled: Outcome;
  reference: Outcome;
  class: DiffClass;
};

export const compareValue = (
  S: Sury,
  unionSchema: unknown,
  input: unknown,
  direction: Direction,
): { compiled: Outcome; reference: Outcome } =>
  direction === "parse"
    ? {
        compiled: compiledParse(S, unionSchema, input),
        reference: referenceParse(S, unionSchema, input),
      }
    : {
        compiled: compiledEncode(S, unionSchema, input),
        reference: referenceEncode(S, unionSchema, input),
      };

const asDiff = (
  direction: Direction,
  input: unknown,
  compiled: Outcome,
  reference: Outcome,
): Comparison | undefined => {
  if (describeOutcome(compiled) === describeOutcome(reference)) return undefined;
  return {
    direction,
    input,
    compiled,
    reference,
    class: classify(reference, compiled),
  };
};

export const diffsForValue = (
  S: Sury,
  unionSchema: unknown,
  input: unknown,
  encode = true,
): { diffs: Comparison[]; compared: number } => {
  const parse = compareValue(S, unionSchema, input, "parse");
  const diffs: Comparison[] = [];
  let compared = 1;
  const parseDiff = asDiff("parse", input, parse.compiled, parse.reference);
  if (parseDiff) diffs.push(parseDiff);
  if (encode && parse.compiled.ok && parse.reference.ok && !parseDiff) {
    let output: unknown = input;
    try {
      output = S.parseOrThrow(unionSchema)(input);
    } catch {
      output = input;
    }
    const encoded = compareValue(S, unionSchema, output, "encode");
    compared += 1;
    const encodeDiff = asDiff(
      "encode",
      output,
      encoded.compiled,
      encoded.reference,
    );
    if (encodeDiff) diffs.push(encodeDiff);
  }
  return { diffs, compared };
};

const memberWitnesses = (
  members: readonly MemberSpec[],
): { value: unknown; encode: boolean }[] => {
  const values: { value: unknown; encode: boolean }[] = [];
  const seen = new Set<string>();
  const add = (value: unknown, encode: boolean) => {
    const key = show(value);
    if (seen.has(key)) return;
    seen.add(key);
    values.push({ value, encode });
  };
  for (const member of flattenVariants(members.map((m) => m.schema))) {
    const w = witnessOf(member);
    if (w !== NO_WITNESS) add(w, true);
  }
  for (const junk of JUNK) add(junk, false);
  return values;
};

export const diffsForUnion = (
  S: Sury,
  members: readonly MemberSpec[],
): { diffs: Comparison[]; compared: number; skipped: number } => {
  let unionSchema: unknown;
  try {
    unionSchema = S.union(members.map((m) => m.schema));
  } catch (error) {
    console.log(`  skipped ${describeMembers(members)}: ${String(error)}`);
    return { diffs: [], compared: 0, skipped: 1 };
  }
  const diffs: Comparison[] = [];
  let compared = 0;
  const inputs = memberWitnesses(members);
  for (const input of inputs) {
    const next = diffsForValue(S, unionSchema, input.value, input.encode);
    diffs.push(...next.diffs);
    compared += next.compared;
  }
  const outcomes = outcomeDiffs(S, unionSchema, inputs.map((input) => input.value));
  diffs.push(...outcomes.diffs);
  compared += outcomes.compared;
  return { diffs, compared, skipped: 0 };
};

// The outcomes that answer a failure rather than throw it - `isInput`,
// `parseAsResult` - leave the body by a different exit than `parseOrThrow`
// does, and have to agree with it on every value: the same acceptance, and for
// a Sury failure the same message. A foreign exception is wrapped by
// `parseAsResult` on purpose, so there only acceptance is compared. The union
// is also asked as an array item and an object field, where its failure
// leaves through a loop or a field's path.
export const outcomeDiffs = (
  S: Sury,
  unionSchema: unknown,
  values: readonly unknown[],
): { diffs: Comparison[]; compared: number } => {
  const diffs: Comparison[] = [];
  let compared = 0;
  const contexts: [unknown, (value: unknown) => unknown][] = [
    [unionSchema, (value) => value],
    [S.array(unionSchema), (value) => [value, value]],
    [S.schema({ f: unionSchema }), (value) => ({ f: value })],
  ];
  for (const [schema, wrap] of contexts) {
    for (const value of values) {
      const input = wrap(value);
      const expected = outcomeOf(S, () => S.parseOrThrow(schema)(input));
      const is = outcomeOf(S, () => S.isInput(schema)(input));
      const result = outcomeOf(S, () => S.parseAsResult(schema)(input));
      compared += 2;
      // `is` answers only the acceptance.
      if (!is.ok || (is.value === "true") !== expected.ok) {
        diffs.push({ direction: "is", input, compiled: is, reference: expected, class: "outcome" });
      }
      let answer: Outcome = result;
      if (result.ok) {
        const r = S.parseAsResult(schema)(input);
        answer = r.success
          ? { ok: true, value: show(r.value) }
          : { ok: false, kind: "sury", message: r.error.message, reasons: r.error.unionErrors?.length ?? 0 };
      }
      const agrees =
        answer.ok === expected.ok &&
        (expected.ok || expected.kind === "foreign" || describeOutcome(answer) === describeOutcome(expected));
      if (!result.ok || !agrees) {
        diffs.push({ direction: "result", input, compiled: answer, reference: expected, class: "outcome" });
      }
    }
  }
  return { diffs, compared };
};

export type RunStats = {
  compared: number;
  diffs: number;
  skipped: number;
  byClass: Record<DiffClass, number>;
};

export const emptyStats = (): RunStats => ({
  compared: 0,
  diffs: 0,
  skipped: 0,
  byClass: { acceptance: 0, "exception-kind": 0, reasons: 0, message: 0, outcome: 0 },
});

export const describeMembers = (members: readonly MemberSpec[]): string =>
  `S.union([${members.map((m) => m.id).join(", ")}])`;
