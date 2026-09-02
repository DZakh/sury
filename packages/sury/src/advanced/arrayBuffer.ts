// `S.arrayBuffer` — memory of one's own. A bytes carrier may hand out a view
// of a buffer it shares (`S.protobuf` writes messages into one slab), so
// converting to this schema is how a caller takes ownership: the view's
// buffer as is when the view covers all of it, otherwise a copy sized to the
// view. Back to bytes is a view over the buffer, which allocates nothing.

import {
  initSchema,
  instanceTag,
  type Internal,
  tagFlags,
  type Val
} from "../base";
import {
  B_computed,
  B_embed,
  B_next,
  B_readOnce,
  B_unsupportedDecode
} from "../builder";
import {
 instanceDecoder
} from "../parse";

const own = (view: Uint8Array): ArrayBuffer =>
  view.byteOffset === 0 && view.byteLength === view.buffer.byteLength
    ? (view.buffer as ArrayBuffer)
    : (view.slice().buffer as ArrayBuffer);

export const arrayBuffer: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const source = input.s;
    const sourceTagFlag = tagFlags[source.type]!;
    if ((sourceTagFlag & 8192) && source.class === Uint8Array) {
      return B_next(input, `${B_embed(input, own)}(${B_readOnce(input)})`, arrayBuffer);
    }
    return (sourceTagFlag & (1 | 8192 | 32768))
      ? instanceDecoder(input)
      : B_unsupportedDecode(input, source, input.e);
  },
  (s) => {
    s.class = ArrayBuffer;
    s.encoder = (input, target) =>
      (tagFlags[target.type]! & 8192) && target.class === Uint8Array
        ? B_computed(input, `new Uint8Array(${B_readOnce(input)})`, target)
        : input;
  },
);
