// `S.uint8Array` — base64 on the JSON side, bytes on ours.

import {
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  jsonName,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  tagFlagUnknown,
  U,
  type Val,
} from "../base";
import { B_embed, B_next, B_refine } from "../builder";
import { instanceDecoder, parse } from "../parse";
import { string } from "../primitives";
import { base64, B_fromBytes, B_toBytes } from "./base64";

// A JSON document has no byte type, so bytes cross into one as base64 — the
// representation `contentEncoding: "base64"`, the proto3 JSON mapping and
// OpenAPI's `format: "byte"` all name. UTF-8 would be lossy: TextDecoder is
// non-fatal, so every sequence that isn't valid UTF-8 becomes U+FFFD and the
// bytes can't be recovered (specs/codec-uint8array-jsonstring.yaml).
//
// `S.json` names itself, `S.jsonString` carries the format — matched rather
// than imported, which is what keeps this module off advanced/json.ts.
const isJson = (schema: Internal): boolean =>
  schema.format === "json" || schema.name === jsonName;

// The decoder names `uint8Array` rather than the `init` callback's `s`: it is
// built before the schema exists, and only ever runs after.
export const uint8Array: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (inputArg: Val): Val => {
    const inputTagFlag = tagFlags[inputArg.s.type]!;
    let input = inputArg;

    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      input = B_next(
        input,
        isJson(inputArg.s) || inputArg.s.format === "base64"
          ? B_toBytes(input, false)
          : `${B_embed(input, new TextEncoder())}.encode(${input.i})`,
        uint8Array,
      );
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagUnknown | tagFlagInstance))) {
      input = instanceDecoder(input);
    }

    const to = inputArg.e.to;
    if (to !== U && inputArg.e.parser === U) {
      if (isJson(to)) {
        input = B_next(input, B_fromBytes(input, false), base64);
      } else if (
        // The base64 formats represent these same bytes rather than describing
        // them, and build themselves from the instance in advanced/base64.ts.
        to.format !== "base64" &&
        to.format !== "base64url" &&
        flagUnsafeHas(tagFlags[to.type]!, tagFlagString)
      ) {
        input = B_next(
          input,
          `${B_embed(input, new TextDecoder())}.decode(${input.i})`,
          string,
        );
      }
    }
    return input;
  },
  (s) => {
    s.class = Uint8Array;
    s.jsonAs = base64;
    // Reached when the bytes sit inside a container being converted to JSON,
    // where the target arrives here rather than as the decoder's `e.to`. See
    // the note in advanced/url.ts for why the B_refine wrap is what makes the
    // produced string, not the instance, the subject of the target's checks.
    s.encoder = (input, target) =>
      isJson(target)
        ? parse(B_refine(B_next(input, B_fromBytes(input, false), base64, target)))
        : input;
  },
);
