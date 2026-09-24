// `S.date` - an ISO string on the JSON side, a `Date` on ours.

import {
  initSchema,
  instanceTag,
  type Internal,
  stringTag,
  tagFlags,
  type Val
} from "../base";
import {
 B_failInvalidInput,
 B_next,
 B_nextVar,
 B_refine,
 B_unsupportedDecode,
 failInvalidType
} from "../builder";
import {
 instanceDecoder,
 parse
} from "../parse";
import {
 stringDecoderFn
} from "../primitives";

const invalidDateRefine = (input: Val): Val => {
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
// for the seq-keyed operation cache - a fresh copy per compilation was both the
// bug and a cache miss.
const dateTimeString: Internal = /* @__PURE__ */ initSchema(
  stringTag,
  stringDecoderFn,
  (s) => {
    s.format = "date-time";
    // `toISOString()` emits only digits, `-:.TZ` and a sign.
    s.flags = s.flags | 32;
  },
);

// The decoder names `date` rather than the `init` callback's `s`: it is built
// before the schema exists, and only ever runs after.
export const date: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if ((inputTagFlag & 2)) {
      // The conversion is checked, and what failed is blamed on the text that
      // was handed over, not on the `Invalid Date` it produced - the same
      // shape the number coercion uses. The instance branch below blames its
      // own value, which there really is an invalid Date. With nothing to
      // check, the conversion stays an expression its reader can inline.
      if (input.e.noValidation) {
        return B_next(input, `new Date(${input.i})`, date);
      }
      const output = B_nextVar(input, date, input.e);
      const inputVar = input.v();
      output.cp = `let ${output.i}=new Date(${inputVar});`;
      output.vc = [{ c: () => `!Number.isNaN(${output.i}.getTime())`, f: failInvalidType }];
      return output;
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
        // carries no path and never matches `S.Raised` - so the throw is
        // caught and reported against the Date node (`input.s`), which names
        // `Date` in the error. A try/catch costs a valid Date nothing, where a
        // `getTime()` check would run on every encode.
        // The B_refine wrap is what makes the produced string the subject of
        // the target's checks (see the note in advanced/url.ts). Without it
        // `S.isoDateTime.with(S.to, S.date)` tests the datetime regex against
        // the `Date`, which stringifies to "Wed Jan 01 2020 …" and never
        // matches.
        // `noValidation` on the Date is the promise it is valid, so the raw
        if (input.s.noValidation) {
          return parse(B_refine(B_next(input, `${input.i}.toISOString()`, dateTimeString, target)));
        }
        const output = B_nextVar(input, dateTimeString, target);
        output.cp = `let ${output.i};try{${output.i}=${input.v()}.toISOString()}catch(_){${B_failInvalidInput(
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
