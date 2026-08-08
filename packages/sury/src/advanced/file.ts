// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.
//
// Both also carry their content codec: `.with(S.to, S.string)` reads the bytes
// as text and `.with(S.to, S.uint8Array)` reads them raw, and reversing either
// builds the container back around the value.

import {
  type Builder,
  type Encoder,
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  panic,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  U,
  type Val,
} from "../base";
import {
  B_embed,
  B_markAsync,
  B_next,
  B_varWithoutAllocation,
} from "../builder";
import { instanceDecoder } from "../parse";

// On a runtime that has no such global there is no schema to be had, so `class`
// reports that instead of sitting there as `undefined` for its readers to
// dereference. Every route into the schema goes through `class` — the decoder's
// `instanceof`, the rendering and the JSON Schema emit via `.name`, and
// `copySchema`'s `Object.assign` for `.with(…)` and `reverse` — so all of them
// answer with this one sentence rather than a TypeError, or worse, a schema
// that builds and fails later.
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

// `.text()` and `.arrayBuffer()` are the only way in — a Blob's bytes are never
// available synchronously — so the content codec is async in this direction
// while its reverse (the constructor call below) stays sync.
//
// The result takes the target as its own schema rather than a bare `string` /
// `Uint8Array`: the methods are typed, so there is nothing left to check, and
// naming the target is what lets the rest of the chain read the value as what
// it asked for — `S.file.with(S.to, S.jsonString.with(S.to, config))` continues
// into a `JSON.parse`, where a plain string would have been re-stringified.
const contentEncoder: Encoder = (input, target) => {
  const toTagFlag = tagFlags[target.type]!;
  let expression: string | undefined = U;
  if (flagUnsafeHas(toTagFlag, tagFlagString)) {
    expression = `${input.i}.text()`;
  } else if (flagUnsafeHas(toTagFlag, tagFlagInstance) && target.class === Uint8Array) {
    const bufferVar = B_varWithoutAllocation(input.g);
    expression = `${input.i}.arrayBuffer().then(${bufferVar}=>new Uint8Array(${bufferVar}))`;
  }
  if (expression === U) {
    return input;
  }
  const output = B_next(input, expression, target);
  B_markAsync(output);
  return output;
};

// `rest` is the trailing constructor argument the two containers differ by: a
// `File` has to be named, a `Blob` has no slot for one. Nothing carries a name
// across the codec — a string doesn't have one — so the reverse builds a File
// with an empty name and leaves naming it to the caller.
const contentDecoder = (rest: string): Builder => (input: Val): Val => {
  const inputTagFlag = tagFlags[input.s.type]!;
  if (
    flagUnsafeHas(inputTagFlag, tagFlagString) ||
    (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === Uint8Array)
  ) {
    return B_next(
      input,
      `new ${B_embed(input, input.e.class)}([${input.i}]${rest})`,
      input.e,
    );
  }
  return instanceDecoder(input);
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
  s.content = true;
  s.decoder = contentDecoder("");
  s.encoder = contentEncoder;
  if (s.class === U) unsupported(s, "blob");
});

export const file: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  s.class = globalThis.File;
  s.content = true;
  s.decoder = contentDecoder(`,""`);
  s.encoder = contentEncoder;
  if (s.class === U) unsupported(s, "file");
});
