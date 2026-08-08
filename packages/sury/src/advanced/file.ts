// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.

import { initSchema, instanceTag, type Internal, panic, U } from "../base";
import { instanceDecoder } from "../parse";

// A runtime without the global would otherwise leave `class` undefined, and
// every reader of it — the rendering, the JSON Schema emit, `String(schema)` —
// dereferences it for a `.name`. The stand-in keeps those answering, and the
// decoder is what reports the real problem, once, when an operation is
// compiled. It is never reached by an `instanceof`: the decoder panics first.
//
// Not a throwing getter in `class`'s place, which would read as the tidier fix:
// `copySchema` builds every derived schema with `Object.assign`, so the throw
// would land on `.with(…)`, on reverse, and on anything that serializes a
// schema, turning "this can't compile here" into "touching this explodes".
// `name` is the class's, so the rendering reads the same either way; the export
// it belongs to is that lowercased.
const unsupportIfMissing = (s: Internal, name: string): void => {
  if (s.class === U) {
    s.class = { name };
    s.decoder = () => panic(`S.${name.toLowerCase()} is not supported in this runtime`);
  }
};

// The global is read *inside* the initializer, not passed into it: a member
// expression at module scope is not something esbuild will drop (the getter
// could have effects), so hoisting it out of the `@__PURE__` call put both
// reads in every consumer's bundle — ~90 bytes on exports that never mention a
// blob. `globalThis.` rather than a bare `Blob` because the reference has to
// survive a runtime that has neither: `Blob` landed in Node 18 and `File` in
// Node 20, and a bare one would throw at import.
export const blob: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.Blob;
  s.decoder = instanceDecoder;
  unsupportIfMissing(s, "Blob");
});

export const file: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.File;
  s.decoder = instanceDecoder;
  unsupportIfMissing(s, "File");
});
