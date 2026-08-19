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
  setContent,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  tagFlagUnknown,
  U,
  type Val,
} from "../base";
import {
  B_computed,
  B_embed,
  B_readOnce,
  B_readsPayload,
  B_refine,
  B_unsupportedDecode,
} from "../builder";
import { instanceDecoder } from "../parse";
import { openedText, string } from "../primitives";
import { base64, base64ToBytes, bytesToBase64 } from "../refinements";

// The decoder names `uint8Array` rather than the `init` callback's `s`: it is
// built before the schema exists, and only ever runs after.
export const uint8Array: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const source = input.s;
    const sourceTagFlag = tagFlags[source.type]!;

    if (flagUnsafeHas(sourceTagFlag, tagFlagString)) {
      const value = B_readOnce(input);
      return B_computed(
        input,
        source.content === base64
          ? `${B_embed(input, base64ToBytes)}(${value})`
          : `${B_embed(input, new TextEncoder())}.encode(${value})`,
        uint8Array,
      );
    }
    if (flagUnsafeHas(sourceTagFlag, (tagFlagUnknown | tagFlagInstance))) {
      return instanceDecoder(input);
    }
    // Without this arm a `S.uint8Array.with(S.to, S.number)` encode handed the
    // number back typed as bytes. A value that reaches the conversions above
    // and isn't bytes is a different story — an encode trusts its declared
    // type, and the platform's own exception is what `S.date` gives for the
    // same lie, so neither is wrapped.
    return B_unsupportedDecode(input, source, input.e);
  },
  (s) => {
    s.class = Uint8Array;
    setContent(s, base64);

    s.encoder = (input, target) => {
      const targetTagFlag = tagFlags[target.type]!;
      if (flagUnsafeHas(targetTagFlag, tagFlagInstance)) {
        // Another binary carrier holds these very bytes, rather than a
        // rendering of them — leave the value alone and let it take them.
        return input;
      }
      // A value position (or base64 itself) stores the bytes as base64. The
      // test comes before the string one because a JSON document is a value
      // position without being string-tagged.
      if (target.content !== U && (target.content === base64 || !B_readsPayload(target))) {
        // The `B_refine` wrap is what makes the produced text the subject of
        // the format's own checks, rather than the bytes that went in.
        return B_refine(
          B_computed(input, `${B_embed(input, bytesToBase64)}(${B_readOnce(input)})`, base64),
        );
      }
      // Anything else that wants a string wants the text the bytes spell —
      // which, for a format being opened (rule 3), is its document. Wrapped
      // like the branch above, so the target's own checks read the text rather
      // than the bytes that produced it.
      return flagUnsafeHas(targetTagFlag, tagFlagString)
        ? B_refine(
            B_computed(
              input,
              `${B_embed(input, new TextDecoder())}.decode(${B_readOnce(input)})`,
              target.content !== U ? openedText(target) : string,
            ),
          )
        : input;
    };
  },
);
