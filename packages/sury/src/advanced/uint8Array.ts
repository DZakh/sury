// `S.uint8Array` — a base64 string on the JSON side, bytes on ours.

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

export const uint8Array: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = Uint8Array;
  s.decoder = (inputArg: Val): Val => {
    const inputTagFlag = tagFlags[inputArg.s.type]!;
    let input = inputArg;

    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      input = B_next(
        input,
        `${B_embed(input, new TextEncoder())}.encode(${input.i})`,
        s,
      );
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagUnknown | tagFlagInstance))) {
      input = instanceDecoder(input);
    }

    if (inputArg.e.to !== U && inputArg.e.parser === U) {
      const to = inputArg.e.to;
      const toTagFlag = tagFlags[to.type]!;
      if (flagUnsafeHas(toTagFlag, tagFlagString)) {
        input = B_next(
          input,
          `${B_embed(input, new TextDecoder())}.decode(${input.i})`,
          string,
        );
      }
      return input;
    } else {
      return input;
    }
  };
});
