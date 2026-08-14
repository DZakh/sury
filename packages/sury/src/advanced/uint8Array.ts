// `S.uint8Array` — a UTF-8 string on the JSON side, bytes on ours.

import {
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  tagFlagUnknown,
  U,
  type Val,
} from "../base";
import { B_embed, B_next } from "../builder";
import { instanceDecoder } from "../parse";
import { string } from "../primitives";

// The decoder names `uint8Array` rather than the `init` callback's `s`: it is
// built before the schema exists, and only ever runs after.
export const uint8Array: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (inputArg: Val): Val => {
    const inputTagFlag = tagFlags[inputArg.s.type]!;
    let input = inputArg;

    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      input = B_next(
        input,
        `${B_embed(input, new TextEncoder())}.encode(${input.i})`,
        uint8Array,
      );
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagUnknown | tagFlagInstance))) {
      input = instanceDecoder(input);
    }

    if (
      inputArg.e.to !== U &&
      inputArg.e.parser === U &&
      flagUnsafeHas(tagFlags[inputArg.e.to.type]!, tagFlagString)
    ) {
      input = B_next(
        input,
        `${B_embed(input, new TextDecoder())}.decode(${input.i})`,
        string,
      );
    }
    return input;
  },
  (s) => {
    s.class = Uint8Array;
  },
);
