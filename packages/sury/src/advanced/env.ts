// `S.env` — one `process.env` entry. A shell has no way to spell "empty
// string", so `FOO=` and an absent `FOO` mean the same thing to everyone
// writing a `.env` file. That makes a blank value the one thing every
// conversion out of env has to intercept: the built-in string coercions would
// silently turn it into `0`, `0n` or a passing `S.string`, which is the whole
// class of bug this schema exists to stop.
//
// The rules live on the conversions, not on the type, which is what lets a
// target opt back in (`S.min`) or claim blank as its own (`S.optional`). Bare
// `S.env` is just the raw string.
//
// Absence itself is not modelled here — `process.env.FOO` is `undefined`, not
// `""`, and that is `S.optional(S.env)` (or `S.record(S.env)` over the map).

import {
  baseSchema,
  cached,
  flagUnsafeHas,
  type Internal,
  isLiteral,
  stringTag,
  tagFlagBigint,
  tagFlagBoolean,
  tagFlagNumber,
  tagFlags,
  tagFlagString,
  tagFlagUndefined,
  U,
  type Val,
} from "../base";
import {
  B_embedInvalidInput,
  B_failWithErrorMessage,
  B_nextConst,
  B_refine,
  failInvalidType,
} from "../builder";
import { B_nextVar, stringDecoderFn } from "../primitives";

const envFormat = "env";

// Blank, not just empty: `+" "` is `0` and `BigInt(" ")` is `0n`, so a variable
// holding nothing but a stray space from a `.env` line would otherwise decode
// to a perfectly valid zero. Blank is normalized to unset rather than trimmed —
// trimming isn't injective, and `S.trim` already exists for callers who want it.
const isBlank = (inputVar: string) => `${inputVar}.trim()===""`;

export const env = (): Internal => {
  return cached("env", stringTag, (s) => {
    // Built here, not at module scope: a top-level `B_failWithErrorMessage(…)`
    // call is opaque to esbuild, which then keeps this whole module in the
    // bundle of a consumer who only imports `S.string`.
    const nonBlankCheck = {
      c: (inputVar: string) => `${inputVar}.trim()!==""`,
      f: B_failWithErrorMessage("format", "Expected a non-empty environment variable"),
    };

    s.format = envFormat;

    s.decoder = (input: Val): Val => {
      // Mirrors the undefined branch of the encoder below, so `""` <-> unset
      // survives a round trip: without it the generic literal coercion encodes
      // the undefined variant back as the string `"undefined"`, which parse
      // would then reject. Only the undefined *literal* source is redirected —
      // a plain string source is genuinely ambiguous here (inside a union the
      // dispatch has already narrowed the input to `string`, so a real
      // `S.string` -> env conversion is indistinguishable from the bare
      // `S.option(S.env)` read, which must keep taking `""`).
      if (
        flagUnsafeHas(tagFlags[input.s.type]!, tagFlagUndefined) &&
        isLiteral(input.s)
      ) {
        const emptyString = baseSchema(stringTag, false);
        emptyString.const = "";
        return B_nextConst(input, emptyString, input.e);
      }
      return stringDecoderFn(input);
    };

    s.encoder = (input: Val, target: Internal): Val => {
      // env -> env is a pass-through, not a conversion. The parse loop calls an
      // encoder whenever `s !== e`, and a union member or a `.to` chain link
      // carries a *copy* of this schema — without this the empty check would be
      // re-emitted once per hop (`S.option(S.env)` alone emitted three).
      if (target.format === envFormat) {
        return input;
      }

      const targetTagFlag = tagFlags[target.type]!;

      if (flagUnsafeHas(targetTagFlag, tagFlagUndefined) && isLiteral(target)) {
        // The generic string->undefined literal coercion spells undefined
        // `"undefined"`; for an env var it is spelled `""`. Mirrors
        // literalDecoder's string branch with that one const swapped, so
        // `S.option`'s dispatch falls through to this variant on empty.
        const emptyString = baseSchema(stringTag, false);
        emptyString.const = "";
        const emptyVal = B_nextConst(input, emptyString, emptyString);
        emptyVal.vc = [
          {
            c: isBlank,
            f: failInvalidType,
          },
        ];
        return B_nextConst(emptyVal, target, target);
      }

      if (flagUnsafeHas(targetTagFlag, tagFlagBoolean)) {
        // Widens the built-in "true"/"false" coercion to the tokens a shell
        // actually carries.
        const output = B_nextVar(input, target);
        const inputVar = input.v();
        output.cp = `let ${output.i};(${output.i}=${inputVar}==="true"||${inputVar}==="1")||${inputVar}==="false"||${inputVar}==="0"||${B_embedInvalidInput(
          input,
          target,
        )};`;
        return output;
      }

      if (
        flagUnsafeHas(targetTagFlag, tagFlagNumber | tagFlagBigint) ||
        // An explicit length bound is the opt-in for taking a blank as a value.
        (flagUnsafeHas(targetTagFlag, tagFlagString) && target.minLength === U)
      ) {
        return B_refine(input, input.s, [nonBlankCheck]);
      }

      return input;
    };
  });
};
