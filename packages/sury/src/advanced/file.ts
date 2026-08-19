// `S.blob` / `S.file` — the binary containers a form submission or a fetch body
// carries. `File` extends `Blob`, so a file value satisfies `S.blob` through the
// same `instanceof` the decoder already emits.
//
// Their payload is bytes, and reading it is asynchronous, so every conversion
// out of one is (CONTENT_CODEC_SPEC.md). Writing one is not: the constructor
// takes the parts as they are.

import {
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  openApi30,
  panic,
  setContent,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  U,
  type Val,
} from "../base";
import {
  B_computed,
  B_embed,
  B_failWithArg,
  B_makeInvalidConversionDetails,
  B_markAsync,
  B_next,
  B_readOnce,
  B_readsPayload,
} from "../builder";
import type { JSONSchemaT } from "../jsonschema";
import { instanceDecoder } from "../parse";
import { openedText, string } from "../primitives";
import { base64, base64ToBytes, bytesToBase64 } from "../refinements";

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

// One promise per read, chained inside the expression rather than left for the
// parse loop to unwrap: the awaited value is what the target asked for, not the
// `ArrayBuffer` the platform hands back.
//
// The method is read off `B_readOnce`'s var rather than the val's expression:
// that is what makes `.`'s precedence a non-question, where a val handed over as
// a ternary would otherwise have the method read off the wrong branch.
const read = (input: Val, call: string, schema: Internal): Val => {
  // Caught the way `B_conversion` catches a coder's rejection: a read that
  // fails (the backing file moved, say) is a `DOMException`, and letting one
  // escape hands the enclosing array or dict a plain Error to stamp a path onto.
  const failure = B_failWithArg(
    input,
    (cause: unknown) => B_makeInvalidConversionDetails(input, schema, cause),
    `x`,
  );
  const output = B_computed(input, `${B_readOnce(input)}${call}.catch(x=>${failure})`, schema);
  B_markAsync(input, output);
  return output;
};

// `global` is the constructor's name and `name` the export's; `nameArg` is what
// the constructor wants past the parts. Packing a file loses
// its name — the reverse builds an unnamed one, since a name belongs on
// `S.file` itself rather than on a conversion.
// @__NO_SIDE_EFFECTS__
const binarySchema = (name: string, global: string, nameArg: string): Internal =>
  initSchema(
    instanceTag,
    (input: Val): Val => {
      const source = input.s;
      const sourceTagFlag = tagFlags[source.type]!;
      const value = B_readOnce(input);
      const parts = flagUnsafeHas(sourceTagFlag, tagFlagString)
        ? source.content === base64
          ? `${B_embed(input, base64ToBytes)}(${value})`
          : value
        : flagUnsafeHas(sourceTagFlag, tagFlagInstance) && source.class === Uint8Array
          ? value
          : U;
      if (parts !== U) {
        return B_next(input, `new ${B_embed(input, input.e.class)}([${parts}]${nameArg})`, input.e);
      }
      // `File` extends `Blob`, so a file already satisfies `S.blob` — a widening
      // `instanceDecoder`'s exact-class match refuses. The other direction still
      // does: not every blob is a file.
      return flagUnsafeHas(sourceTagFlag, tagFlagInstance) &&
        (source.class as { prototype?: unknown }).prototype instanceof
          (input.e.class as new () => unknown)
        ? input
        : instanceDecoder(input);
    },
    (s) => {
      // The global is read *inside* the initializer, not passed into it: a
      // member expression at module scope is not something esbuild will drop
      // (the getter could have effects), so hoisting it out of the `@__PURE__`
      // call put both reads in every consumer's bundle — ~90 bytes on exports
      // that never mention a blob. `globalThis.` rather than a bare `Blob`
      // because the reference has to survive a runtime that has neither: `Blob`
      // landed in Node 18 and `File` in Node 20, and a bare one would throw at
      // import.
      s.class = (globalThis as unknown as Record<string, unknown>)[global];
      setContent(s, base64);
      s.jsonSchema = binaryJSONSchema;
      if (s.class === U) {
        unsupported(s, name);
      }

      s.encoder = (input, target) => {
        const targetTagFlag = tagFlags[target.type]!;
        if (flagUnsafeHas(targetTagFlag, tagFlagInstance)) {
          // Bytes are the payload, so a bytes target takes them as they are;
          // any other instance is not this carrier's business.
          return target.class === Uint8Array
            ? read(input, `.arrayBuffer().then(b=>new Uint8Array(b))`, target)
            : input;
        }
        // A value position (or base64 itself) stores the bytes as base64;
        // anything else after a string wants the text they spell, which is also
        // what a format opened by rule 3 is handed.
        if (target.content !== U && (target.content === base64 || !B_readsPayload(target))) {
          return read(
            input,
            `.arrayBuffer().then(b=>${B_embed(input, bytesToBase64)}(new Uint8Array(b)))`,
            base64,
          );
        }
        // A format being opened (rule 3) is handed its own document, so it
        // parses the text instead of escaping it.
        return flagUnsafeHas(targetTagFlag, tagFlagString)
          ? read(input, `.text()`, target.content !== U ? openedText(target) : string)
          : input;
      };
    },
  );

export const blob: Internal = /* @__PURE__ */ binarySchema("blob", "Blob", "");
export const file: Internal = /* @__PURE__ */ binarySchema("file", "File", `,""`);
