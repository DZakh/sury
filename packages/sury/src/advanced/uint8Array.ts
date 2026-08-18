// `S.uint8Array` — bytes. Both readings of a bytes link live here rather than
// on the formats they meet (CONTENT_CODEC_SPEC.md): a value position stores
// bytes as base64, and a plain string target is the text those bytes spell.
// `S.jsonString` therefore never names a base64 helper, and a bundle that never
// mentions bytes never ships one.

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
import { B_embed, B_next, B_readsPayload, B_unsupportedDecode } from "../builder";
import { instanceDecoder } from "../parse";
import { string } from "../primitives";
import { base64, base64ToBytes, bytesToBase64 } from "../refinements";

// The decoder names `uint8Array` rather than the `init` callback's `s`: it is
// built before the schema exists, and only ever runs after.
export const uint8Array: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const source = input.s;
    const sourceTagFlag = tagFlags[source.type]!;

    if (flagUnsafeHas(sourceTagFlag, tagFlagString)) {
      return B_next(
        input,
        source.content === base64
          ? `${B_embed(input, base64ToBytes)}(${input.i})`
          : `${B_embed(input, new TextEncoder())}.encode(${input.i})`,
        uint8Array,
      );
    }
    if (flagUnsafeHas(sourceTagFlag, (tagFlagUnknown | tagFlagInstance))) {
      return instanceDecoder(input);
    }
    // Without this arm a `S.uint8Array.with(S.to, S.number)` encode handed the
    // number back typed as bytes.
    return B_unsupportedDecode(input, source, input.e);
  },
  (s) => {
    s.class = Uint8Array;
    s.content = base64;

    // Packing (a value position, or base64 itself as the target) writes base64;
    // anything else that wants a string wants the text the bytes spell, which
    // is also what a format opened by rule 3 is handed.
    s.encoder = (input, target) => {
      if (!flagUnsafeHas(tagFlags[target.type]!, tagFlagString)) {
        return input;
      }
      const packs =
        target.content !== U && (target.content === base64 || !B_readsPayload(target));
      return B_next(
        input,
        packs
          ? `${B_embed(input, bytesToBase64)}(${input.i})`
          : `${B_embed(input, new TextDecoder())}.decode(${input.i})`,
        packs ? base64 : string,
      );
    };
  },
);
