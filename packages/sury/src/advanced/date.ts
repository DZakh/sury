// `S.date` — an ISO string on the JSON side, a `Date` on ours.

import {
  baseSchema,
  cached,
  flagUnsafeHas,
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

export const date = (): Internal => {
  return cached(instanceTag, instanceTag, (s) => {
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
        return parse(
          B_next(input, `${input.i}.toISOString()`, dateTimeString, target),
        );
      } else {
        return input;
      }
    };
  });
}
