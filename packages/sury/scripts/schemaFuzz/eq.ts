// The comparator family: `S.isEqualInput` / `S.isEqualOutput` /
// `S.compareInput` / `S.compareOutput`.
//
// A spec pins the comparator's generated code and the answers it gives for the
// values that spec writes down. Neither says anything about a schema no spec
// has, and the emit is a tree of special cases - a union narrow, a tagged
// dispatch, a hoisted loop, a structural fallback - where the wrong branch is
// invisible until some value takes it. So the answers are held to the
// properties an equivalence has whatever the emit chose:
//
//   reflexive    a value equals a separately built copy of itself. The one
//                property with no escape: it is what the `a === b` the emit
//                opens with cannot answer, and what NaN breaks.
//   symmetric    `eq(a,b)` is `eq(b,a)`. A dispatch that narrows on `a` and
//                forgets to narrow on `b` fails here and nowhere else.
//   transitive   equal to the same value means equal to each other.
//   oracle       agrees with the structural walk in `sample.ts`, written
//                without reference to the schema.
//   compare-zero `compare(a,b)===0` exactly when `isEqual(a,b)`. The two
//                compiles share a walker; this is the invariant that sharing
//                exists to keep.
//   antisymmetric `compare(a,b)` is `-compare(b,a)`. What makes the answer a
//                sort comparator rather than a difference test, and the reason
//                compare refuses the schemas it has no order for.
//   eq-duality   `isEqualInput(schema)` is `isEqualOutput(reverse(schema))`.
//                One comparator, reached two ways.
//   congruence   two inputs the Input side calls equal decode to two outputs
//                the Output side calls equal. A decoder is a function, so the
//                only way this breaks is an Input comparator that is looser
//                than the decode it feeds.
//
// The Output properties run on Output samples and the last two on Input
// samples. Before the two families shared a runner, one sample served both,
// drawn through the schema's own shape and filtered by `isOutput` - which for
// any schema that transforms rejected every draw, so none of them was tested.

import { show, structural } from "../unionFuzz/sample";
import { type Ctx, type Family, reason } from "./context";

type Eq = (a: unknown, b: unknown) => boolean;
type Cmp = (a: unknown, b: unknown) => number;

const check = (ctx: Ctx): void => {
  const { S, schema, reversed, inputs, outputs, report, compile, count } = ctx;

  const isEqualOutput = compile<Eq>("compile", () => S.isEqualOutput(schema));
  const isEqualInput = compile<Eq>("compile", () => S.isEqualInput(schema));
  if (!isEqualOutput || !isEqualInput) return;
  // A schema with no order refuses when its comparator is compiled, which is
  // the contract every spec of such a schema pins. That refusal is a plain
  // `[Sury]` panic rather than a SuryError, so it is recognised by wording.
  let compareOutput: Cmp | undefined;
  try {
    compareOutput = S.compareOutput(schema) as Cmp;
  } catch (error) {
    if (!reason(error).startsWith("[Sury] Can't compare "))
      report("compile", `building the comparator threw - ${reason(error)}`);
  }

  const ask = (fn: Eq, what: string, a: unknown, b: unknown): boolean | undefined => {
    try {
      return fn(a, b);
    } catch (error) {
      report(what, `threw on (${show(a)}, ${show(b)}) - ${reason(error)}`);
      return undefined;
    }
  };

  for (const [first, second] of outputs) {
    // Gated on the oracle: a Blob, an Error, a user class has nothing but its
    // identity, so a rebuilt copy is a different value and both sides say so.
    if (!structural(first, second)) continue;
    const answer = ask(isEqualOutput, "reflexive", first, second);
    if (answer !== undefined && answer !== true) {
      report("reflexive", `answered ${show(answer)} for ${show(first)} against a separately built copy of itself`);
    }
  }

  for (let i = 0; i < outputs.length; i++) {
    for (let j = 0; j < outputs.length; j++) {
      const a = outputs[i]![0];
      const b = outputs[j]![1];
      const forward = ask(isEqualOutput, "symmetric", a, b);
      const back = ask(isEqualOutput, "symmetric", b, a);
      if (forward === undefined || back === undefined) continue;
      count("comparisons");
      if (forward !== back) {
        report("symmetric", `${show(a)} vs ${show(b)} reads ${forward} one way and ${back} the other`);
      }
      const want = structural(a, b);
      if (forward !== want) {
        report("oracle", `${show(a)} vs ${show(b)}: comparator ${forward}, structural ${want}`);
      }
      if (compareOutput === undefined) continue;
      let cmp: unknown;
      let cmpBack: unknown;
      try {
        cmp = compareOutput(a, b);
        cmpBack = compareOutput(b, a);
      } catch (error) {
        report("compare-zero", `threw - ${reason(error)}`);
        continue;
      }
      if (cmp !== -1 && cmp !== 0 && cmp !== 1) {
        report("compare-zero", `${show(a)} vs ${show(b)} answered ${show(cmp)}, not -1|0|1`);
      } else if ((cmp === 0) !== want) {
        report("compare-zero", `${show(a)} vs ${show(b)}: compare ${show(cmp)}, isEqual ${forward}`);
      }
      if (cmp !== -(cmpBack as number)) {
        report(
          "antisymmetric",
          `${show(a)} vs ${show(b)}: compare ${show(cmp)} one way and ${show(cmpBack)} the other`,
        );
      }
    }
  }

  for (let i = 0; i < outputs.length; i++)
    for (let j = 0; j < outputs.length; j++)
      for (let k = 0; k < outputs.length; k++) {
        const a = outputs[i]![0];
        const b = outputs[j]![0];
        const c = outputs[k]![1];
        if (
          ask(isEqualOutput, "transitive", a, b) === true &&
          ask(isEqualOutput, "transitive", b, c) === true &&
          ask(isEqualOutput, "transitive", a, c) !== true
        ) {
          report("transitive", `${show(a)} equals ${show(b)} equals ${show(c)}, but the first and last do not`);
        }
      }

  // Reversing swaps the sides, so the Input comparator of a schema and the
  // Output comparator of its reverse are the same question asked twice.
  const reversedOutput = compile<Eq>("eq-duality", () => S.isEqualOutput(reversed));
  if (reversedOutput) {
    for (const [first, second] of inputs) {
      const direct = ask(isEqualInput, "eq-duality", first, second);
      if (direct === undefined) continue;
      const viaReverse = ask(reversedOutput, "eq-duality", first, second);
      if (viaReverse === undefined) continue;
      if (direct !== viaReverse) {
        report(
          "eq-duality",
          `isEqualInput answered ${direct} for ${show(first)} but isEqualOutput of the reverse answered ${viaReverse}`,
        );
      }
    }
  }

  const decode = compile<(v: unknown) => unknown>("congruence", () => S.decodeOrThrow(schema));
  if (!decode) return;
  for (let i = 0; i < inputs.length; i++)
    for (let j = 0; j < inputs.length; j++) {
      const a = inputs[i]![0];
      const b = inputs[j]![1];
      if (ask(isEqualInput, "congruence", a, b) !== true) continue;
      let da: unknown;
      let db: unknown;
      try {
        da = decode(a);
        db = decode(b);
      } catch {
        // A throwing decode is `conformance`'s finding in the codec family.
        continue;
      }
      if (da instanceof Promise || db instanceof Promise) continue;
      count("congruences");
      if (ask(isEqualOutput, "congruence", da, db) !== true) {
        report(
          "congruence",
          `${show(a)} and ${show(b)} are equal on the Input side but decode to ${show(da)} ` +
            `and ${show(db)}, which are not equal on the Output side`,
        );
      }
    }
};

export const eq: Family = { check };
