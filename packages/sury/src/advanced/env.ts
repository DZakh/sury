// `S.env` - an environment variable value, `string | undefined` on both sides
// the way `process.env` reports it: `undefined` is the unset var. A string
// format so JSON Schema is `{ type: "string" }`, not an instance with no
// document form. Empty string is absent unless the target keeps it with
// `S.minLength(0)`. A bare `S.string` is ambiguous.

import { type Check, initSchema, stringTag, tagFlags, U, type Internal, type Val } from "../base";
import { B_next, B_refine, B_unsupportedDecode, failInvalidType } from "../builder";
import { string, typeofCond } from "../primitives";
import { convertTextEntry, isAbsent } from "./entries";

const definedCheck: Check = { c: (inputVar) => `${inputVar}!==void 0`, f: failInvalidType };

// A text target has no place for the unset var: an absent one reads it as its
// absent arm, a coercion or a jsonString rejects it itself.
const rejectsUnset = (target: Internal): boolean =>
  (tagFlags[target.type]! & 2) !== 0 && target.format !== "json" && !isAbsent(target);

// Walks the chain and the scopes it was taken from: a union case scopes the
// group's narrow, whose check became the case condition.
const unchecked = (input: Val): boolean => {
  for (let val: Val | undefined = input; val; val = val.prev) {
    if (
      (val.vc && val.vc.some((check) => check.f === failInvalidType)) ||
      (val.b && !unchecked(val.b))
    ) {
      return false;
    }
  }
  return true;
};

const envDecoder = (input: Val): Val => {
  const flag = tagFlags[input.s.type]!;
  if (input.s === input.e || input.s.format === "env" || flag & 16) {
    return input;
  }
  if (flag & 1) {
    // A parse into a text target checks the text once, here.
    return B_refine(input, input.e, [
      {
        c:
          input.e.to !== U && rejectsUnset(input.e.to)
            ? typeofCond(stringTag)
            : (inputVar) => `(typeof ${inputVar}==="string"||${inputVar}===void 0)`,
        f: failInvalidType,
      },
    ]);
  }
  if (flag & 32) {
    // The unset var a nullable target read as `null`.
    return B_next(input, "void 0", input.e);
  }
  if (flag & 2) {
    return B_refine(input, input.e);
  }
  if (flag & (4 | 8 | 1024)) {
    return B_next(input, `""+${input.i}`, input.e);
  }
  return B_unsupportedDecode(input, input.s, input.e);
};

export const env: Internal = /* @__PURE__ */ initSchema(stringTag, envDecoder, (s) => {
  s.format = "env";
  s.name = "env";
  s.encoder = (input, target) =>
    convertTextEntry(
      // The env itself as the source, not a field a loop wrapped (`u`) or a
      // narrow a conversion produced, into a text target, unless a parse or a
      // union case checked the text already.
      (input.s === s || input.s.to === target) && !input.u && rejectsUnset(target) && unchecked(input)
        ? B_refine(input, s, [definedCheck], target)
        : input,
      target,
      string,
      true,
    );
});
