// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.

import { initSchema, instanceTag, type Internal } from "../base";
import { instanceDecoder } from "../parse";

// Reached through `globalThis` rather than as a bare global: `Blob` landed in
// Node 18 and `File` in Node 20, and a module-level bare reference would throw
// at import time on a runtime that has neither — including for a consumer who
// imports Sury and never touches these two. Undefined instead surfaces at the
// `instanceof`, where the caller did ask for it.
export const blob: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.Blob;
  s.decoder = instanceDecoder;
});

export const file: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.File;
  s.decoder = instanceDecoder;
});
