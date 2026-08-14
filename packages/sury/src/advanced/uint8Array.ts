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

    // A string target is decoded as UTF-8 text — unless it is one of the base64
    // formats, which represent the same bytes rather than describing them, and
    // build themselves from the instance in advanced/base64.ts. Compared by
    // format name rather than by schema identity so that importing this module
    // never drags the base64 encoders into a bundle that only wanted UTF-8.
    const toFormat = inputArg.e.to?.format;
    if (
      inputArg.e.to !== U &&
      inputArg.e.parser === U &&
      toFormat !== "base64" &&
      toFormat !== "base64url" &&
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
