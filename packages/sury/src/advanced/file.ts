// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.

import { initSchema, instanceTag, type Internal, openApi30, panic, U } from "../base";
import type { JSONSchemaT } from "../jsonschema";
import { instanceDecoder } from "../parse";

// On a runtime that has no such global there is no schema to be had, so `class`
// reports that instead of sitting there as `undefined` for its readers to
// dereference. Every route into the schema goes through `class` — the decoder's
// `instanceof`, the rendering and the JSON Schema emit via `.name`, and
// `copySchema`'s `Object.assign` for `.with(…)` and `reverse` — so all of them
// answer with this one sentence rather than a TypeError, or worse, a schema
// that builds and fails later — converting a schema that only decodes to one
// included, since the encode-reverse copies the target to get there.
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

// No `type`: octets have none, so the carrier that decodes to a blob is the
// side with a type to give and this only says what it carries. `minSize` and
// `maxSize` stay off — neither dialect bounds a byte count, and `minLength`
// counts characters.
const binaryJSONSchema = (_schema: Internal, target: string): JSONSchemaT =>
  target === openApi30
    ? { format: "binary" }
    : { contentMediaType: "application/octet-stream" };

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
