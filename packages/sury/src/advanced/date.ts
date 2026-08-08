// `S.date` — an ISO string on the JSON side, a `Date` on ours.

import {
  baseSchema,
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

export const invalidDateRefine = (input: Val): Val => {
  return B_refine(input, input.e, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType,
    },
  ]);
}

export const date: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = Date;
  s.decoder = (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return invalidDateRefine(B_next(input, `new Date(${input.i})`, s));
    } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      return invalidDateRefine(instanceDecoder(input));
    } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === s.class) {
      return input;
    } else {
      return B_unsupportedDecode(input, input.s, input.e);
    }
  };

  // Encoder: Date → string (via toISOString) when target is string
  s.encoder = (input, target) => {
    const toTagFlag = tagFlags[target.type]!;
    if (flagUnsafeHas(toTagFlag, tagFlagString)) {
      const dateTimeString = baseSchema(stringTag, false);
      dateTimeString.format = "date-time";
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
});
