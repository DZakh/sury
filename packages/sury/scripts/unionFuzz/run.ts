import { classify, describeOutcome, show } from "./outcome";
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

export type Direction = "parse" | "encode";

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
      output = S.parser(unionSchema)(input);
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
  for (const junk of JUNK) add(junk, junk === null || junk === undefined);
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
  for (const input of memberWitnesses(members)) {
    const next = diffsForValue(S, unionSchema, input.value, input.encode);
    diffs.push(...next.diffs);
    compared += next.compared;
  }
  return { diffs, compared, skipped: 0 };
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
  byClass: { acceptance: 0, "exception-kind": 0, reasons: 0, message: 0 },
});

export const describeMembers = (members: readonly MemberSpec[]): string =>
  `S.union([${members.map((m) => m.id).join(", ")}])`;
