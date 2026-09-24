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

// The directions past `encode` compare an answering outcome to the same
// compile's `parseOrThrow`, not to the reference: see `outcomeDiffs`.
export type Direction = "parse" | "encode" | "is" | "result" | "validate" | "promise";

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
// `parseAsResult`, `~standard.validate`, the promise ones - have to agree with
// the same compile's `parseOrThrow` on every value: the same acceptance, and
// for a Sury failure the same message where the outcome carries one. A foreign
// exception is an answer too (Result and `validate` wrap it, the promise
// rejects with it), so none of them may throw synchronously. The union is also
// asked as an array item and an object field, where its failure leaves through
// a loop or a field's path, and handed a value whose every read throws.
const hostile = new Proxy(
  {},
  {
    // Printable, so a diff on it can be reported.
    get: (_, key) => {
      if (key === "toJSON") return () => "hostile";
      if (typeof key === "symbol") return undefined;
      throw new TypeError("hostile read");
    },
  },
);

const failed = (error: any): Outcome => ({
  ok: false,
  kind: "sury",
  message: error.message,
  reasons: error.unionErrors?.length ?? 0,
});

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
    let parse, is, result, promise, validate: (input: unknown) => any;
    try {
      parse = S.parseOrThrow(schema);
      is = S.isInput(schema);
      result = S.parseAsResult(schema);
      promise = S.parseAsPromiseOrReject(schema);
      validate = (schema as any)["~standard"].validate;
    } catch {
      // A wrapper the union can't be compiled into answers nothing to compare.
      continue;
    }
    for (const value of [...values, hostile]) {
      const input = wrap(value);
      const expected = outcomeOf(S, () => parse(input));
      const push = (direction: Direction, compiled: Outcome) =>
        diffs.push({ direction, input, compiled, reference: expected, class: "outcome" });
      compared += 4;

      const answer = outcomeOf(S, () => is(input));
      if (!answer.ok || (answer.value === "true") !== expected.ok) push("is", answer);

      let resulted: Outcome;
      try {
        const r = result(input);
        resulted = r.success ? { ok: true, value: show(r.value) } : failed(r.error);
      } catch (error: any) {
        resulted = { ok: false, kind: "foreign", name: "thrown", message: String(error?.message ?? error) };
      }
      if (
        resulted.ok !== expected.ok ||
        (!resulted.ok && resulted.kind === "foreign") ||
        (!expected.ok && expected.kind === "sury" && describeOutcome(resulted) !== describeOutcome(expected))
      ) {
        push("result", resulted);
      }

      try {
        const r = validate(input);
        if (!r.issues !== expected.ok) push("validate", r.issues ? failed(r.issues[0]) : { ok: true, value: show(r.value) });
      } catch (error: any) {
        push("validate", { ok: false, kind: "foreign", name: "thrown", message: String(error?.message ?? error) });
      }

      try {
        (promise(input) as Promise<unknown>).catch(() => {});
      } catch (error: any) {
        push("promise", { ok: false, kind: "foreign", name: "thrown", message: String(error?.message ?? error) });
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
