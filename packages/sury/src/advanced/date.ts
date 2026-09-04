// `S.date` — an ISO string on the JSON side, a `Date` on ours.

import {
  initSchema,
  instanceTag,
  type Internal,
  stringTag,
  tagFlags,
  type Val
} from "../base";
import {
 _var,
 B_embedInvalidInput,
 B_next,
 B_refine,
 B_unsupportedDecode,
 B_varWithoutAllocation,
 failInvalidType
} from "../builder";
import {
 instanceDecoder,
 parse
} from "../parse";
import {
 stringDecoderFn
} from "../primitives";

export const invalidDateRefine = (input: Val): Val => {
  return B_refine(input, input.e, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType,
    },
  ]);
}

// The `toISOString()` result, described once. It outlives the encoder call: it
// becomes the enclosing object's property schema and is reached later as another
// operation's target, so it needs a real decoder (#369) and a stable identity
// for the seq-keyed operation cache — a fresh copy per compilation was both the
// bug and a cache miss.
const dateTimeString: Internal = /* @__PURE__ */ initSchema(
  stringTag,
  stringDecoderFn,
  (s) => {
    s.format = "date-time";
    // `toISOString()` emits only digits, `-:.TZ` and a sign.
    s.escapeFree = true;
  },
);

// The decoder names `date` rather than the `init` callback's `s`: it is built
// before the schema exists, and only ever runs after.
export const date: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if ((inputTagFlag & 2)) {
      return invalidDateRefine(B_next(input, `new Date(${input.i})`, date));
    } else if ((inputTagFlag & 1)) {
      return invalidDateRefine(instanceDecoder(input));
    } else if ((inputTagFlag & 8192) && input.s.class === date.class) {
      return input;
    } else {
      return B_unsupportedDecode(input, input.s, input.e);
    }
  },
  (s) => {
    s.class = Date;

    // Encoder: Date → string (via toISOString) when target is string
    s.encoder = (input, target) => {
      const toTagFlag = tagFlags[target.type]!;
      if ((toTagFlag & 2)) {
        // `toISOString()` throws a bare RangeError on an invalid Date, which
        // carries no path and never matches `S.Raised` — so the throw is
        // caught and reported against the Date node (`input.s`), which names
        // `Date` in the error. A try/catch costs a valid Date nothing, where a
        // `getTime()` check would run on every encode.
        // The B_refine wrap is what makes the produced string the subject of
        // the target's checks (see the note in advanced/url.ts). Without it
        // `S.isoDateTime.with(S.to, S.date)` tests the datetime regex against
        // the `Date`, which stringifies to "Wed Jan 01 2020 …" and never
        // matches.
        // `noValidation` on the Date is the promise it is valid, so the raw
        // call stays.
        if (input.s.noValidation) {
          return parse(B_refine(B_next(input, `${input.i}.toISOString()`, dateTimeString, target)));
        }
        const outputVar = B_varWithoutAllocation(input.g);
        const output = B_next(input, outputVar, dateTimeString, target);
        output.v = _var;
        output.cp = `let ${outputVar};try{${outputVar}=${input.v()}.toISOString()}catch(_){${B_embedInvalidInput(
          input,
          input.s,
        )}}`;
        return parse(B_refine(output));
      } else {
        return input;
      }
    };
  },
);
