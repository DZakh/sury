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
 B_next,
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

export const invalidDateRefine = (input: Val, expected: Internal = input.e): Val => {
  return B_refine(input, expected, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType,
    },
  ], expected);
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
        // See the note in advanced/url.ts: the B_refine wrap is what makes the
        // produced string the subject of the target's checks. Without it
        // `S.isoDateTime.with(S.to, S.date)` tests the datetime regex against the
        // `Date`, which stringifies to "Wed Jan 01 2020 …" and never matches.
        // `toISOString()` throws a bare RangeError on an invalid Date, which
        // carries no path and never matches `S.Raised` — so validate first.
        // The check belongs to the Date node (`input.s`), not the string
        // target: that names `Date` in the error and lets its `noValidation`
        // drop the check.
        const checked = invalidDateRefine(input, input.s);
        return parse(
          B_refine(
            B_next(checked, `${checked.i}.toISOString()`, dateTimeString, target)
          )
        );
      } else {
        return input;
      }
    };
  },
);
