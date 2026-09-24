// The codec family: `decode` and `encode` held against the schema's own
// validators and against each other.
//
// What no spec can pin is the agreement between the two sides of a schema,
// because the interesting disagreements need a shape nobody thought to write:
// a container whose transform lives in its items rather than on its `.to`, a
// default written in the Output form, a union under either (#452).
//
//   conformance  an operation's result lands on the side it claims: a decode
//                produces a value the schema's own `isOutput` accepts, an
//                encode one its `isInput` accepts. It needs no oracle - the
//                schema is asked about its own answer - and a half-applied
//                transform always breaks it.
//   agreement    `parse` and `decode` answer the same thing for an input the
//                Input side accepts. They differ only in whether they validate,
//                so a transform one runs and the other skips shows here and
//                nowhere else.
//   round-trip   `decode(encode(o))` is `o`. An encode that undoes less, or
//                more, than the decode it mirrors fails this even when both
//                ends conform. Skipped for a member the grammar marks lossy.
//   reverse      `encode(schema)` is `decode(reverse(schema))`. One operation
//                reached two ways, so a reverse that rebuilds rather than swaps
//                is caught against itself.
//
// Equality here is the structural walk, never `isEqual*`: that comparator is
// what the eq family tests, and a property holding only because both sides are
// wrong is not a property.

import { show, structural } from "../unionFuzz/sample";
import { type Ctx, type Family, reason } from "./context";

type Op = (value: unknown) => unknown;

const settled = (value: unknown): boolean => !(value instanceof Promise);

const check = (ctx: Ctx): void => {
  const { S, schema, reversed, lossy, inputs, outputs, isInput, isOutput, report, compile, count } = ctx;

  const decode = compile<Op>("decode", () => S.decodeOrThrow(schema));
  const encode = compile<Op>("encode", () => S.encodeOrThrow(schema));
  const parse = compile<Op>("agreement", () => S.parseOrThrow(schema));
  const viaReverse = compile<Op>("reverse", () => S.decodeOrThrow(reversed));

  if (decode) {
    for (const [i] of inputs) {
      let decoded: unknown;
      try {
        decoded = decode(i);
      } catch (error) {
        // `decode` trusts its input, so a value `isInput` accepted must get
        // through it. A throw is the two disagreeing about the same side.
        report("conformance", `decode threw on ${show(i)}, which isInput accepts - ${reason(error)}`);
        continue;
      }
      if (!settled(decoded)) continue;
      count("results");
      if (isOutput(decoded) !== true) {
        report("conformance", `decode turned ${show(i)} into ${show(decoded)}, which the schema's own isOutput rejects`);
      }
      if (!parse) continue;
      let parsed: unknown;
      try {
        parsed = parse(i);
      } catch (error) {
        report("agreement", `parse threw on ${show(i)} but decode did not - ${reason(error)}`);
        continue;
      }
      if (settled(parsed) && !structural(parsed, decoded)) {
        report("agreement", `parse answered ${show(parsed)} and decode ${show(decoded)} for ${show(i)}`);
      }
    }
  }

  if (!encode) return;
  for (const [o] of outputs) {
    let encoded: unknown;
    try {
      encoded = encode(o);
    } catch (error) {
      report("conformance", `encode threw on ${show(o)}, which isOutput accepts - ${reason(error)}`);
      continue;
    }
    if (!settled(encoded)) continue;
    count("results");
    if (isInput(encoded) !== true) {
      report("conformance", `encode turned ${show(o)} into ${show(encoded)}, which the schema's own isInput rejects`);
    }
    if (viaReverse) {
      try {
        const mirror = viaReverse(o);
        if (settled(mirror) && !structural(mirror, encoded)) {
          report("reverse", `encode answered ${show(encoded)} for ${show(o)} but decoding the reverse answered ${show(mirror)}`);
        }
      } catch (error) {
        report("reverse", `decoding the reverse threw on ${show(o)} but encode did not - ${reason(error)}`);
      }
    }
    if (!decode || lossy) continue;
    let back: unknown;
    try {
      back = decode(encoded);
    } catch (error) {
      report("round-trip", `decoding the encode of ${show(o)} threw - ${reason(error)}`);
      continue;
    }
    if (settled(back) && !structural(back, o)) {
      report("round-trip", `${show(o)} encoded to ${show(encoded)} and decoded back to ${show(back)}`);
    }
  }
};

export const codec: Family = { check };
