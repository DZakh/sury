// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.

import { initSchema, instanceTag, type Internal, openApi30, panic, U } from "../base";
import type { JSONSchemaT } from "../jsonschema";
import { instanceDecoder } from "../parse";

// On a runtime that has no such global there is no schema to be had, so `class`
// reports that instead of sitting there as `undefined` for its readers to
// dereference. Every route that needs the class goes through it — the decoder's
// `instanceof`, the rendering, and `copySchema`'s `Object.assign` for `.with(…)`
// and `reverse` — so all of them answer with this one sentence rather than a
// TypeError, or worse, a schema that builds and fails later. The JSON Schema
// emit below is the exception on purpose: it describes the wire, which is worth
// generating on a runtime that could never hold the value.
//
// Enumerable, so the `Object.assign` copy is one of the routes it covers.
// `console.log` still works: `util.inspect` shows an accessor rather than
// invoking it.
const unsupported = (s: Internal, name: string): void => {
  Object.defineProperty(s, "class", {
    enumerable: true,
    get: () => panic(`S.${name} is not supported in this runtime`),
  });
};

// The one thing about these schemas a structural conversion can't reach: the
// value is octets, which no JSON type describes, so both dialects describe the
// carrier instead and disagree on how. OpenAPI 3.0 has `format: "binary"` for
// exactly this and no content keywords at all; JSON Schema has no such format
// and says it with `contentMediaType`. `minSize`/`maxSize` have no keyword in
// either and stay off — a byte count is not `minLength`, which counts
// characters.
const binaryJSONSchema = (_schema: Internal, target: string): JSONSchemaT =>
  target === openApi30
    ? { type: "string", format: "binary" }
    : { type: "string", contentMediaType: "application/octet-stream" };

// The global is read *inside* the initializer, not passed into it: a member
// expression at module scope is not something esbuild will drop (the getter
// could have effects), so hoisting it out of the `@__PURE__` call put both
// reads in every consumer's bundle — ~90 bytes on exports that never mention a
// blob. `globalThis.` rather than a bare `Blob` because the reference has to
// survive a runtime that has neither: `Blob` landed in Node 18 and `File` in
// Node 20, and a bare one would throw at import.
export const blob: Internal = /* @__PURE__ */ initSchema(instanceTag, instanceDecoder, (s) => {
  s.class = globalThis.Blob;
  s.jsonSchema = binaryJSONSchema;
  if (s.class === U) unsupported(s, "blob");
});

export const file: Internal = /* @__PURE__ */ initSchema(instanceTag, instanceDecoder, (s) => {
  s.class = globalThis.File;
  s.jsonSchema = binaryJSONSchema;
  if (s.class === U) unsupported(s, "file");
});
