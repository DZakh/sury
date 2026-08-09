// `S.date` — an ISO string on the JSON side, a `Date` on ours.

import {
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  stringTag,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  tagFlagUnknown,
  type Val,
} from "../base";
import { B_next, B_refine, B_unsupportedDecode, failInvalidType } from "../builder";
import { instanceDecoder, parse } from "../parse";
import { stringDecoderFn } from "../primitives";

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
  },
);

// The decoder names `date` rather than the `init` callback's `s`: it is built
// before the schema exists, and only ever runs after.
export const date: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return invalidDateRefine(B_next(input, `new Date(${input.i})`, date));
    } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      return invalidDateRefine(instanceDecoder(input));
    } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === date.class) {
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
      if (flagUnsafeHas(toTagFlag, tagFlagString)) {
        // See the note in advanced/url.ts: the B_refine wrap is what makes the
        // produced string the subject of the target's checks. Without it
        // `S.isoDateTime.with(S.to, S.date)` tests the datetime regex against the
        // `Date`, which stringifies to "Wed Jan 01 2020 …" and never matches.
        return parse(
          B_refine(
            B_next(input, `${input.i}.toISOString()`, dateTimeString, target),
          ),
        );
      } else {
        return input;
      }
    };
  },
);
