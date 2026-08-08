// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.

import { type Builder, initSchema, instanceTag, type Internal, panic, U } from "../base";
import { instanceDecoder } from "../parse";

// Missing, the class would reach `i instanceof e[0]` and fail there with a
// TypeError naming neither the schema nor the reason, so the decoder says it
// instead. It runs when an operation is compiled — the first moment the schema
// is actually used, and still early enough to be a build error rather than a
// per-value one.
const unsupported = (name: string): Builder => () =>
  panic(`S.${name} is not supported in this runtime`);

// The global is read *inside* the initializer, not passed into it: a member
// expression at module scope is not something esbuild will drop (the getter
// could have effects), so hoisting it out of the `@__PURE__` call put both
// reads in every consumer's bundle — ~90 bytes on exports that never mention a
// blob. `globalThis.` rather than a bare `Blob` because the reference has to
// survive a runtime that has neither: `Blob` landed in Node 18 and `File` in
// Node 20, and a bare one would throw at import.
export const blob: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.Blob;
  s.decoder = s.class !== U ? instanceDecoder : unsupported("blob");
});

export const file: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.File;
  s.decoder = s.class !== U ? instanceDecoder : unsupported("file");
});
