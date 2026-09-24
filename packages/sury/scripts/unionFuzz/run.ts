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

export const diffsForUnion = async (
  S: Sury,
  members: readonly MemberSpec[],
): Promise<{ diffs: Comparison[]; compared: number; skipped: number }> => {
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
  const outcomes = await outcomeDiffs(S, unionSchema, inputs.map((input) => input.value));
  diffs.push(...outcomes.diffs);
  compared += outcomes.compared;
  return { diffs, compared, skipped: 0 };
};

// The outcomes that answer a failure rather than throw it - `isInput`,
// `parseAsResult`, `~standard.validate`, and the promise ones - have to agree
// with the same compile's `parseOrThrow` on every value: the same acceptance,
// and for a Sury failure the same message where the outcome carries one
// (`validate` carries only the reason, so it is held to acceptance). A foreign
// exception is an answer too - Result and `validate` wrap it, the promise
// rejects with it - so none of them may throw synchronously. The union is also
// asked as an array item and an object field, where its failure leaves through
// a loop or a field's path, and handed a value whose every read throws.
const hostile = new Proxy(
  {},
  {
    // Printable, so a diff on it can be reported, and not a thenable, so a
    // promise can resolve with it.
    get: (_, key) => {
      if (key === "toJSON") return () => "hostile";
      if (key === "then" || typeof key === "symbol") return undefined;
      throw new TypeError("hostile read");
    },
  },
);

const thrown = (error: any): Outcome => ({
  ok: false,
  kind: "foreign",
  name: "thrown synchronously",
  message: String(error?.message ?? error),
});

const ofResult = (r: any): Outcome =>
  r.success
    ? { ok: true, value: show(r.value) }
    : { ok: false, kind: "sury", message: r.error.message, reasons: r.error.unionErrors?.length ?? 0 };

export const outcomeDiffs = async (
  S: Sury,
  unionSchema: unknown,
  values: readonly unknown[],
): Promise<{ diffs: Comparison[]; compared: number }> => {
  const diffs: Comparison[] = [];
  let compared = 0;
  const contexts: [unknown, (value: unknown) => unknown][] = [
    [unionSchema, (value) => value],
    [S.array(unionSchema), (value) => [value, value]],
    [S.schema({ f: unionSchema }), (value) => ({ f: value })],
  ];
  for (const [schema, wrap] of contexts) {
    let ops: {
      parse: (input: unknown) => unknown;
      is: (input: unknown) => boolean;
      result: (input: unknown) => any;
      validate: (input: unknown) => any;
      isAsync: (input: unknown) => Promise<boolean>;
      resultAsync: (input: unknown) => Promise<any>;
      promise: (input: unknown) => Promise<unknown>;
    };
    try {
      ops = {
        parse: S.parseOrThrow(schema),
        is: S.isInput(schema),
        result: S.parseAsResult(schema),
        validate: (schema as any)["~standard"].validate,
        isAsync: S.isInputAsPromise(schema),
        resultAsync: S.parseAsResultPromise(schema),
        promise: S.parseAsPromiseOrReject(schema),
      };
    } catch {
      // A wrapper the union can't be compiled into answers nothing to compare.
      continue;
    }
    for (const value of [...values, hostile]) {
      const input = wrap(value);
      const expected = outcomeOf(S, () => ops.parse(input));
      const push = (direction: Direction, compiled: Outcome) =>
        diffs.push({ direction, input, compiled, reference: expected, class: "outcome" });
      // A Sury failure agrees only with the same message; a foreign one is
      // wrapped or rejected with, so there only acceptance is compared.
      const same = (answer: Outcome) =>
        answer.ok === expected.ok &&
        (expected.ok || expected.kind === "foreign" || describeOutcome(answer) === describeOutcome(expected));
      // Only `parseAsPromiseOrReject` may reject; any outcome throwing
      // synchronously is a diff whatever `parseOrThrow` said.
      const settle = async (direction: Direction, produce: () => unknown, read: (v: any) => Outcome) => {
        compared++;
        let answer: Outcome;
        let broken = false;
        try {
          const produced = produce();
          if (produced instanceof Promise) {
            answer = await produced.then(read, (error) => {
              if (direction !== "promise") broken = true;
              return error instanceof S.Error
                ? { ok: false, kind: "sury", message: error.message, reasons: error.unionErrors?.length ?? 0 }
                : { ok: false, kind: "foreign", name: error?.constructor?.name ?? "unknown", message: String(error?.message ?? error) };
            });
          } else {
            answer = read(produced);
          }
        } catch (error) {
          answer = thrown(error);
          broken = true;
        }
        if (broken || !same(answer)) push(direction, answer);
      };
      const boolean = (answer: boolean): Outcome =>
        answer === expected.ok ? expected : answer ? { ok: true, value: "true" } : { ok: false, kind: "sury", message: "false", reasons: 0 };
      await settle("is", () => ops.is(input), boolean);
      await settle("is", () => ops.isAsync(input), boolean);
      await settle("result", () => ops.result(input), ofResult);
      await settle("result", () => ops.resultAsync(input), ofResult);
      await settle("validate", () => ops.validate(input), (r) =>
        r.issues ? (expected.ok ? { ok: false, kind: "sury", message: r.issues[0].message, reasons: 0 } : expected) : expected.ok ? expected : { ok: true, value: show(r.value) },
      );
      await settle("promise", () => ops.promise(input), (r) => ({ ok: true, value: show(r) }));
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
